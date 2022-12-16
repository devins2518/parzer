const std = @import("std");
const utils = @import("utils.zig");

pub fn ParseFn(comptime T: type) type {
    return fn (*[]const u8, anytype) ParseFnRet(T);
}

pub fn getChildParseFn(comptime T: type, comptime field: anytype) ParseFn(field.field_type) {
    const Field = field.field_type;
    const FieldInfo = @typeInfo(Field);
    const has_parser = @hasDecl(T, field.name ++ "Parser");
    const has_parse = if (comptime utils.isStructOrUnion(Field))
        @hasDecl(Field, "parse")
    else
        false;
    return switch (FieldInfo) {
        .Struct, .Union => if (has_parser)
            @field(T, field.name ++ "Parser")
        else if (has_parse)
            @field(Field, "parse")
        else
            Parser(Field).parseRest,
        .Int => if (has_parser)
            @field(T, field.name ++ "Parser")
        else
            @compileError("Parser not found for " ++ @typeName(T) ++ "." ++ @typeName(field.field_type)),
        .Pointer => |Ptr| if (Ptr.size == .One and Ptr.is_const)
            struct {
                fn f(bytes: *[]const u8, extra: anytype) ParseFnRet(field.field_type) {
                    const Child = Ptr.child;
                    comptime assertExtraHasAllocator(extra);
                    const parseFn = getParseFn(Child);
                    const allocator = extra.allocator;
                    const ptr = try allocator.create(Child);
                    ptr.* = try parseFn(bytes, extra);
                    return ptr;
                }
            }.f
        else
            @compileError("Unsupported pointer parsing type: Pointer.size == " ++ @tagName(FieldInfo.Pointer.size)),
        else => @compileError("Unsupported parsing type: " ++ @tagName(FieldInfo)),
    };
}

pub fn getParseFn(comptime T: type) ParseFn(T) {
    return if (@hasDecl(T, "parse"))
        T.parse
    else
        Parser(T).parseRest;
}

pub fn ParseFnRet(comptime T: type) type {
    return ParseError(T)!T;
}

pub fn ParseError(comptime T: type) type {
    const TyInfo = @typeInfo(T);
    var errors = error{ UnexpectedEof, UnexpectedToken };
    if (comptime utils.isStructOrUnion(T)) {
        if (@hasDecl(T, "ParserError"))
            errors = errors || T.ParserError;

        const Fields = if (comptime utils.isStruct(T)) TyInfo.Struct.fields else TyInfo.Union.fields;
        for (Fields) |field| {
            errors = errors || ParseError(field.field_type);
        }
    } else if (TyInfo == .Pointer) {
        errors = errors || std.mem.Allocator.Error || ParseError(TyInfo.Pointer.child);
    }

    return errors;
}

pub fn Parser(comptime T: type) type {
    const TyInfo = @typeInfo(T);
    if (comptime !utils.isStructOrUnion(T)) @compileError("Parser() can only parse into structs or unions. Found: " ++ @tagName(TyInfo));
    if (comptime utils.isUnion(T) and TyInfo.Union.tag_type == null) @compileError("Parser() cannot parse an untagged union. Found: " ++ @typeName(T));
    const Fields = switch (TyInfo) {
        .Struct => |s| s.fields,
        .Union => |u| u.fields,
        else => unreachable,
    };

    return struct {
        const Error = ParseError(T);

        pub fn parse(bytes: []const u8, extra: anytype) ParseFnRet(T) {
            var b = bytes;
            return getParseFn(T)(&b, extra);
        }
        // null means that parsing will continue for rest of the struct.
        pub fn parseWhile(bytes: *[]const u8, extra: anytype, comptime stop: ?[]const u8) ParseFnRet(T) {
            var ret: T = undefined;
            inline for (Fields) |field| {
                if (stop != null and field.name == stop.?) break;
                const parseFn = getChildParseFn(T, field);
                if (comptime utils.isStruct(T)) {
                    @field(ret, field.name) = try parseFn(bytes, extra);
                } else if (comptime utils.isUnion(T)) {
                    const old_bytes = bytes.*;
                    const maybe_parsed = parseFn(bytes, extra);
                    if (maybe_parsed) |parsed| {
                        ret = @unionInit(T, field.name, parsed);
                        break;
                    } else |_| {
                        bytes.* = old_bytes;
                    }
                } else unreachable;
            }
            return ret;
        }
        pub fn parseRest(bytes: *[]const u8, extra: anytype) ParseFnRet(T) {
            return parseWhile(bytes, extra, null);
        }
    };
}

pub fn SingleValue(comptime T: type, comptime val: T) type {
    return struct {
        _: u1 = 0,
        pub fn parse(bytes: *[]const u8, _: anytype) ParseFnRet(@This()) {
            return if (T == u8) blk: {
                try eatChar(bytes, val);
                break :blk @This(){};
            } else |e| e;
        }
    };
}

pub fn eatChar(bytes: *[]const u8, char: u8) ParseFnRet(void) {
    return if (bytes.*[0] == char) {
        bytes.* = bytes.*[1..];
    } else error.UnexpectedToken;
}

// TODO: Investigate ways to make this better
// OneOf(u8, &.{0x00, 0x01}) will create union { @"0", @"1" }; which isn't great to work with.
pub fn OneOf(comptime T: type, comptime slice: []const T) type {
    const Type = std.builtin.Type;
    const Union = Type.Union;
    const Enum = Type.Enum;
    const Decl = Type.Declaration;
    const UnionField = Type.UnionField;
    const EnumField = Type.EnumField;
    var UnionFields: [slice.len]UnionField = undefined;
    var EnumFields: [slice.len]EnumField = undefined;
    inline for (slice) |ty, i| {
        const name = std.fmt.comptimePrint("{}", .{ty});
        if (T == type) {
            UnionFields[i] = UnionField{ .name = name, .field_type = ty, .alignment = @alignOf(ty) };
            EnumFields[i] = EnumField{ .name = name, .value = i };
        } else {
            UnionFields[i] = UnionField{ .name = name, .field_type = SingleValue(T, ty), .alignment = @alignOf(T) };
            EnumFields[i] = EnumField{ .name = name, .value = i };
        }
    }
    return @Type(Type{ .Union = Union{
        .layout = .Auto,
        .tag_type = @Type(Type{ .Enum = Enum{
            .layout = .Auto,
            .tag_type = std.math.IntFittingRange(0, EnumFields.len - 1),
            .fields = &EnumFields,
            .decls = &[_]Decl{},
            .is_exhaustive = true,
        } }),
        .fields = &UnionFields,
        .decls = &[_]Decl{},
    } });
}

pub fn OneOrMore(comptime T: type) type {
    return struct {
        const ParserError = std.mem.Allocator.Error || ParseError(T);
        first: T,
        rest: []const T,

        pub fn parse(bytes: *[]const u8, extra: anytype) ParseFnRet(@This()) {
            comptime assertExtraHasAllocator(extra);
            const allocator = extra.allocator;
            const parseFn = getParseFn(T);
            const first = if (parseFn(bytes, extra)) |t|
                t
            else |_|
                return error.UnexpectedToken;

            var list = std.ArrayList(T).init(allocator);
            while (true) {
                const t = parseFn(bytes, extra);
                if (t) |ok|
                    try list.append(ok)
                else |_|
                    return @This(){ .first = first, .rest = try list.toOwnedSlice() };
            }
        }
        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            allocator.free(self.rest);
        }
    };
}

pub fn ZeroOrMore(comptime T: type) type {
    return struct {
        const ParserError = std.mem.Allocator.Error || ParseError(T);
        rest: []const T,

        pub fn parse(bytes: *[]const u8, extra: anytype) ParseFnRet(@This()) {
            comptime assertExtraHasAllocator(extra);
            const allocator = extra.allocator;
            var list = std.ArrayList(T).init(allocator);

            while (true) {
                const t = getParseFn(T)(bytes, extra);
                if (t) |ok|
                    try list.append(ok)
                else |_|
                    return @This(){ .rest = try list.toOwnedSlice() };
            }
        }
        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            allocator.free(self.rest);
        }
    };
}

pub fn assertExtraHasAllocator(extra: anytype) void {
    const T = @TypeOf(extra);
    if (!comptime utils.isStruct(T)) @compileError("Extra must be a struct");
    if (@hasField(T, "allocator")) {
        const AllocatorTy = @TypeOf(extra.allocator);
        if (AllocatorTy == std.mem.Allocator)
            return
        else
            @compileError("Extra.allocator must be of type std.mem.Allocator found " ++ @typeName(AllocatorTy));
    } else @compileError("Extra must have a field named allocator.");
}

pub fn parseInt(comptime T: type, len: usize, comptime radix: u8) ParseFn(T) {
    return struct {
        fn f(bytes: *[]const u8, _: anytype) ParseFnRet(T) {
            return if (bytes.len < len)
                error.UnexpectedEof
            else if (std.fmt.parseInt(u8, bytes.*[0..len], radix)) |ret| blk: {
                bytes.* = bytes.*[len..];
                break :blk ret;
            } else |_| error.UnexpectedToken;
        }
    }.f;
}

test "basic parse - workaround within struct to properly parse" {
    // TODO: investigate parser which compacts struct with multiple SingleValues while
    // maintaining parse order
    const Color = struct {
        hex: SingleValue(u8, '#') = .{},
        red: u8,
        green: u8,
        blue: u8,

        const redParser = parseInt(u8, 2, 16);
        const greenParser = parseInt(u8, 2, 16);
        const blueParser = parseInt(u8, 2, 16);
    };

    const ColorParser = Parser(Color);

    const expected = Color{ .red = 47, .green = 20, .blue = 223 };
    const parsed = try ColorParser.parse("#2F14DF", .{});

    try std.testing.expectEqual(parsed, expected);
}

test "basic parse - custom parse fn to properly parse" {
    const Color = struct {
        const Self = @This();
        const ColorParser = Parser(Self);
        red: u8,
        green: u8,
        blue: u8,

        const redParser = parseInt(u8, 2, 16);
        const greenParser = parseInt(u8, 2, 16);
        const blueParser = parseInt(u8, 2, 16);
        pub fn parse(bytes: *[]const u8, extra: anytype) ParseFnRet(Self) {
            try eatChar(bytes, '#');
            return ColorParser.parseRest(bytes, extra);
        }
    };

    const ColorParser = Parser(Color);

    const expected = Color{ .red = 47, .green = 20, .blue = 223 };
    const parsed = try ColorParser.parse("#2F14DF", .{});

    try std.testing.expectEqual(parsed, expected);
}

test "basic parse - zero or more" {
    const a = SingleValue(u8, 'a');
    const A = struct {
        a: ZeroOrMore(a),
    };

    const AParser = Parser(A);

    const expected = A{ .a = .{ .rest = &.{ .{}, .{}, .{} } } };
    const parsed = try AParser.parse("aaab", .{ .allocator = std.testing.allocator });
    defer parsed.a.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(a, parsed.a.rest, expected.a.rest);
}

test "basic parse - one or more" {
    const a = SingleValue(u8, 'a');
    const A = struct {
        a: OneOrMore(a),
    };

    const AParser = Parser(A);

    const expected = A{ .a = .{ .first = .{}, .rest = &.{ .{}, .{} } } };
    const parsed = try AParser.parse("aaab", .{ .allocator = std.testing.allocator });
    defer parsed.a.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(a, parsed.a.rest, expected.a.rest);
}

test "basic parse - pointers" {
    const a = SingleValue(u8, 'a');
    const APtrs = struct {
        a: *const a,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            allocator.destroy(self.a);
        }
    };

    const AParser = Parser(APtrs);

    const expected = APtrs{ .a = &.{} };
    const parsed = try AParser.parse("aaab", .{ .allocator = std.testing.allocator });
    defer parsed.deinit(std.testing.allocator);

    try std.testing.expectEqual(expected.a.*, parsed.a.*);
}

test "basic parse - union" {
    const Color = union(enum) {
        double: struct {
            hex: SingleValue(u8, '#') = .{},
            red: u8,
            green: u8,
            blue: u8,
            const redParser = parseInt(u8, 2, 16);
            const greenParser = parseInt(u8, 2, 16);
            const blueParser = parseInt(u8, 2, 16);
        },
        single: struct {
            hex: SingleValue(u8, '#') = .{},
            red: u8,
            green: u8,
            blue: u8,
            const redParser = parseInt(u8, 1, 16);
            const greenParser = parseInt(u8, 1, 16);
            const blueParser = parseInt(u8, 1, 16);
        },
    };

    const ColorParser = Parser(Color);

    {
        const expected = Color{ .single = .{ .red = 0xa, .green = 0xb, .blue = 0xc } };
        const parsed = try ColorParser.parse("#abc", .{});
        try std.testing.expectEqual(parsed, expected);
    }
    {
        const expected = Color{ .double = .{ .red = 0x2f, .green = 0x14, .blue = 0xdf } };
        const parsed = try ColorParser.parse("#2F14DF", .{});
        try std.testing.expectEqual(parsed, expected);
    }
}

test "basic parse - union within struct" {
    const Color = struct {
        values: union(enum) {
            double: struct {
                hex: SingleValue(u8, '#') = .{},
                red: u8,
                green: u8,
                blue: u8,
                const redParser = parseInt(u8, 2, 16);
                const greenParser = parseInt(u8, 2, 16);
                const blueParser = parseInt(u8, 2, 16);
            },
            single: struct {
                hex: SingleValue(u8, '#') = .{},
                red: u8,
                green: u8,
                blue: u8,
                const redParser = parseInt(u8, 1, 16);
                const greenParser = parseInt(u8, 1, 16);
                const blueParser = parseInt(u8, 1, 16);
            },
        },
    };

    const ColorParser = Parser(Color);

    {
        const expected = Color{ .values = .{ .single = .{ .red = 0xa, .green = 0xb, .blue = 0xc } } };
        const parsed = try ColorParser.parse("#abc", .{});
        try std.testing.expectEqual(parsed, expected);
    }
    {
        const expected = Color{ .values = .{ .double = .{ .red = 0x2f, .green = 0x14, .blue = 0xdf } } };
        const parsed = try ColorParser.parse("#2F14DF", .{});
        try std.testing.expectEqual(parsed, expected);
    }
}

test "basic parse - one of" {
    const Control = struct {
        cntrl: OneOf(u8, &[_]u8{
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
            0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
            0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
            0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
            0x7f,
        }),
    };

    const ControlParser = Parser(Control);

    const expected = Control{ .cntrl = .{ .@"0" = .{} } };
    const parsed = try ControlParser.parse("\x00", .{});
    try std.testing.expectEqual(parsed, expected);
}

test "basic parse - one of types" {
    const Slash = struct {
        slash: OneOf(type, &[_]type{
            SingleValue(u8, '\\'),
            SingleValue(u8, '/'),
        }),
    };

    const SlashParser = Parser(Slash);

    {
        const expected = Slash{ .slash = .{ .@"main.SingleValue(u8,92)" = .{} } };
        const parsed = try SlashParser.parse("\\", .{});
        try std.testing.expectEqual(parsed, expected);
    }
    {
        const expected = Slash{ .slash = .{ .@"main.SingleValue(u8,47)" = .{} } };
        const parsed = try SlashParser.parse("/", .{});
        try std.testing.expectEqual(parsed, expected);
    }
}

test "assert SingleValue zero-size" {
    try std.testing.expect(@sizeOf(SingleValue([]const u8, "test")) == 1);
}

test "parser error" {
    const Foo = struct {
        const ParserError = error{foo};
        bar: struct {
            const ParserError = error{bar};
        },
        foo: u8,
        baz: union {
            const ParserError = error{baz};
            baz: void,
        },
    };
    const FooError = ParseError(Foo);
    const FooErrorSet = @typeInfo(FooError).ErrorSet.?;

    const ExpectError = error{ UnexpectedEof, UnexpectedToken, foo, bar, baz };
    const ExpectErrorSet = @typeInfo(ExpectError).ErrorSet.?;

    for (ExpectErrorSet) |e, i|
        try std.testing.expectEqualStrings(e.name, FooErrorSet[i].name);
}

test "static analysis" {
    std.testing.refAllDeclsRecursive(@This());
}
