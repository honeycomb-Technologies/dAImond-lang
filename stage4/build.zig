const std = @import("std");

// Stage 4 build file: builds ONLY the integration test harness.
// The harness spawns the Stage 4 compiler binary (built separately,
// see run_parity.sh), so it does not need LLVM.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Integration tests — reuses Stage 0 test case definitions
    const stage0_runner_mod = b.addModule("stage0_runner", .{
        .root_source_file = .{ .cwd_relative = "../stage0/tests/runner.zig" },
    });

    const integration_exe = b.addExecutable(.{
        .name = "test-integration",
        .root_source_file = b.path("tests/runner.zig"),
        .target = target,
        .optimize = optimize,
    });
    integration_exe.root_module.addImport("stage0_runner", stage0_runner_mod);

    b.installArtifact(integration_exe);

    const run_integration = b.addRunArtifact(integration_exe);
    run_integration.step.dependOn(b.getInstallStep());

    const integration_step = b.step("test-integration", "Run Stage 4 LLVM integration tests");
    integration_step.dependOn(&run_integration.step);
}
