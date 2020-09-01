const Builder = @import("std").build.Builder;

const std = @import("std");
const warn = std.debug.warn;

// This build.zig is only used as an example of using header_gen

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});

    const mode = b.standardReleaseOptions();

    // HEADER GEN BUILD STEP
    const exe = b.addExecutable("example", "src/example/exports.zig");
    exe.main_pkg_path = "src/";
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.addPackagePath("header_gen", "src/header_gen.zig");
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("headergen", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
