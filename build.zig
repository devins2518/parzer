const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // TODO: allocating ZSTs is kinda broken on stage2
    b.use_stage1 = true;
    const mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});

    const lib = b.addStaticLibrary("parzer", "src/lib.zig");
    lib.setBuildMode(mode);
    lib.install();

    const main_tests = b.addTest("src/lib.zig");
    main_tests.setBuildMode(mode);
    main_tests.setTarget(target);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
