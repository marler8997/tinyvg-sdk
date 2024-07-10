const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const xml_dep = b.dependency("zig_xml", .{});
    const xml_mod = xml_dep.module("zig-xml");

    const exe = b.addExecutable(.{
        .name = "svg2tvgt",
        .root_source_file = b.path("src/svg2tvgt.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("xml", xml_mod);
    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
