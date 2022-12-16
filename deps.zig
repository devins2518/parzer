const std = @import("std");
const Pkg = std.build.Pkg;
const FileSource = std.build.FileSource;


pub const exports = struct {
    pub const parzer = Pkg{
        .name = "parzer",
        .source = FileSource{ .path = "src/lib.zig" },
    };
};
