const std = @import("std");
const stage1 = @import("builtin").zig_backend == .stage1;

pub fn ParseFn(comptime T: type) type {
    return fn (*[]const u8, anytype) ParseFnRet(T);
}

pub fn getChildParseFn(comptime T: type, comptime field: anytype) ParseFn(T) {
    const Field = field.field_type;
    const FieldInfo = @typeInfo(Field);
    const has_parser = @hasDecl(T, field.name ++ "Parser");
    const has_parse = if (comptime isStructOrUnion(Field))
        @hasDecl(Field, "parse")
    else
        false;
    return switch (FieldInfo) {
        .Struct => if (has_parser)
            @field(T, field.name ++ "Parser")
        else if (has_parse)
            @field(Field, "parse")
        else
            Parser(Field).parseRest,
        .Int => if (has_parser)
            @field(T, field.name ++ "Parser")
        else
            @compileError("Parser not found for " ++ @typeName(T) ++ "." ++ @typeName(field.field_type)),
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
    var errors = error{ UnexpectedEof, UnexpectedToken };
    if (isStructOrUnion(T)) {
        const TyInfo = @typeInfo(T);
        if (@hasDecl(T, "ParserError"))
            errors = errors || T.ParserError;

        const Fields = if (isStruct(T)) TyInfo.Struct.fields else TyInfo.Union.fields;
        for (Fields) |field| {
            if (isStructOrUnion(field.field_type))
                errors = errors || ParseError(field.field_type);
        }
    }

    return errors;
}

pub fn Parser(comptime T: type) type {
    const TyInfo = @typeInfo(T);
    if (!isStructOrUnion(T)) @compileError("Parser() can only parse into structs or unions. Found: " ++ @tagName(TyInfo));
    const Fields = switch (TyInfo) {
        .Struct => |s| s.fields,
        .Union => |u| u.fields,
        else => unreachable,
    };

    return struct {
        const Error = ParseError(T);

        fn parse(bytes: []const u8, extra: anytype) ParseFnRet(T) {
            var b = bytes;
            return getParseFn(T)(&b, extra);
        }
        // null means that parsing will continue for rest of the struct.
        fn parseWhile(bytes: *[]const u8, extra: anytype, comptime stop: ?[]const u8) ParseFnRet(T) {
            var ret: T = undefined;
            inline for (Fields) |field| {
                if (stop != null and field.name == stop.?) break;
                @field(ret, field.name) = try getChildParseFn(T, field)(bytes, extra);
            }
            return ret;
        }
        fn parseRest(bytes: *[]const u8, extra: anytype) ParseFnRet(T) {
            return parseWhile(bytes, extra, null);
        }
    };
}

pub fn SingleValue(comptime T: type, comptime val: T) type {
    const U = if (stage1) T else void;
    const Val = if (stage1) val else {};
    return struct {
        _: U = Val,
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

pub fn ZeroOrMore(comptime T: type) type {
    return struct {
        const ParserError = std.mem.Allocator.Error || ParseError(T);
        rest: []const T,

        fn parse(bytes: *[]const u8, extra: anytype) ParseFnRet(@This()) {
            assertExtraHasAllocator(extra);
            const allocator = extra.allocator;
            var list = std.ArrayList(T).init(allocator);

            while (true) {
                const t = getParseFn(T)(bytes, extra);
                if (t) |ok|
                    try list.append(ok)
                else |_|
                    return @This(){ .rest = list.toOwnedSlice() };
            }
        }
        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            allocator.free(self.rest);
        }
    };
}

pub fn assertExtraHasAllocator(extra: anytype) void {
    const T = @TypeOf(extra);
    if (!comptime isStruct(T)) @compileError("Extra must be a struct");
    if (@hasField(T, "allocator")) {
        const AllocatorTy = @TypeOf(extra.allocator);
        if (AllocatorTy == std.mem.Allocator)
            return
        else
            @compileError("Extra.allocator must be of type std.mem.Allocator found " ++ @typeName(AllocatorTy));
    } else @compileError("Extra must have a field named allocator.");
}

fn parseInt(comptime T: type, len: usize, comptime radix: u8) ParseFn(T) {
    return struct {
        fn f(bytes: *[]const u8, _: anytype) ParseFnRet(T) {
            return if (std.fmt.parseInt(u8, bytes.*[0..len], radix)) |ret| blk: {
                bytes.* = bytes.*[len..];
                break :blk ret;
            } else |_| error.UnexpectedToken;
        }
    }.f;
}

fn isStructOrUnion(comptime T: type) bool {
    return isStruct(T) or isUnion(T);
}

fn isStruct(comptime T: type) bool {
    return @typeInfo(T) == .Struct;
}

fn isUnion(comptime T: type) bool {
    return @typeInfo(T) == .Union;
}

test "basic parse - workaround within struct to properly parse" {
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

test "assert SingleValue zero-size" {
    if (!stage1)
        try std.testing.expect(@sizeOf(SingleValue([]const u8, "test")) == 0);
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

    try std.testing.expectEqualSlices(std.builtin.Type.Error, FooErrorSet, ExpectErrorSet);
}

test "static analysis" {
    std.testing.refAllDeclsRecursive(@This());
}
