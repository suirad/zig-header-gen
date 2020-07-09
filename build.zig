const Builder = @import("std").build.Builder;
const header_gen = @import("header_gen.zig");

const std = @import("std");
const warn = std.debug.warn;

// This build.zig is only used as an example of using header_gen

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});

    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("example", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // --- Using header_gen
    const gen = header_gen.HeaderGen("src/exports.zig").init();
    gen.exec(header_gen.C_Generator);
}
