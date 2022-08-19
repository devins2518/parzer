const std = @import("std");
const stage1 = @import("builtin").zig_backend == .stage1;

pub fn ParseFn(comptime T: type) type {
    return fn ([]const u8, *usize, anytype) ParseFnRet(T);
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

        fn parse(bytes: []const u8, extra: anytype) !T {
            var idx: usize = 0;
            return if (@hasDecl(T, "parse"))
                T.parse(bytes, &idx, extra)
            else
                parseRest(bytes, &idx, extra);
        }
        // null means that parsing will continue for rest of the struct.
        fn parseWhile(ret: *T, bytes: []const u8, idx: *usize, extra: anytype, comptime stop: ?[]const u8) ParseError(T)!void {
            inline for (Fields) |field| {
                if (stop != null and field.name == stop.?) break;
                const FieldInfo = @typeInfo(field.field_type);
                const has_parser = @hasDecl(T, field.name ++ "Parser");
                const has_parse = if (comptime isStructOrUnion(field.field_type))
                    @hasDecl(field.field_type, "parse")
                else
                    false;
                @field(ret, field.name) = switch (FieldInfo) {
                    .Struct => if (has_parser)
                        try @field(T, field.name ++ "Parser")(bytes, idx, extra)
                    else if (has_parse)
                        try @field(field.field_type, "parse")(bytes, idx, extra)
                    else
                        undefined,
                    .Int => if (has_parser)
                        try @field(T, field.name ++ "Parser")(bytes, idx, extra)
                    else
                        undefined,
                    else => @compileError("Unsupported parsing type: " ++ @tagName(FieldInfo)),
                };
            }
        }
        fn parseRest(bytes: []const u8, idx: *usize, extra: anytype) ParseFnRet(T) {
            var ret: T = undefined;
            try parseWhile(&ret, bytes, idx, extra, null);
            return ret;
        }
    };
}

pub fn SingleValue(comptime T: type, comptime val: T) type {
    const U = if (stage1) T else void;
    const Val = if (stage1) val else {};
    return struct {
        _: U = Val,
        pub fn parse(bytes: []const u8, idx: *usize, _: anytype) ParseFnRet(@This()) {
            return if (T == u8) blk: {
                try eatChar(bytes, idx, val);
                break :blk @This(){};
            } else |e| e;
        }
    };
}

pub fn eatChar(bytes: []const u8, idx: *usize, char: u8) ParseFnRet(void) {
    return if (bytes[idx.*] == char) {
        idx.* += 1;
    } else error.UnexpectedToken;
}

fn parseInt(comptime T: type, len: usize, comptime radix: u8) ParseFn(T) {
    return struct {
        fn f(bytes: []const u8, idx: *usize, _: anytype) ParseFnRet(T) {
            return if (std.fmt.parseInt(u8, bytes[idx.* .. idx.* + len], radix)) |ret| blk: {
                idx.* += len;
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
        pub fn parse(bytes: []const u8, idx: *usize, extra: anytype) ParseFnRet(Self) {
            try eatChar(bytes, idx, '#');
            return ColorParser.parseRest(bytes, idx, extra);
        }
    };

    const ColorParser = Parser(Color);

    const expected = Color{ .red = 47, .green = 20, .blue = 223 };
    const parsed = try ColorParser.parse("#2F14DF", .{});

    try std.testing.expectEqual(parsed, expected);
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
