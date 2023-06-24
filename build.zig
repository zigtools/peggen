const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "peggen",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);
    // TODO remove. only used for importing ./out.zig
    exe.main_pkg_path = ".";

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/tests.zig" },
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run unit tests");
    const test_run = b.addRunArtifact(exe_tests);
    test_run.has_side_effects = true;
    test_step.dependOn(&test_run.step);

    const interval_test_exe = b.addExecutable(.{
        .name = "interval-test",
        .root_source_file = .{ .path = "src/tests/interval.zig" },
        .target = target,
        .optimize = optimize,
    });
    interval_test_exe.main_pkg_path = ".";
    b.installArtifact(interval_test_exe);
}
