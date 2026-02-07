const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Add Stage 0 frontend as a module (re-exports lexer, parser, ast, checker, etc.)
    const frontend_mod = b.addModule("frontend", .{
        .root_source_file = .{ .cwd_relative = "../stage0/src/frontend.zig" },
    });

    // Main executable - dAImond compiler with LLVM backend
    const exe = b.addExecutable(.{
        .name = "daimond-llvm",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe.root_module.addImport("frontend", frontend_mod);

    // Link against LLVM
    exe.linkLibC();
    exe.linkSystemLibrary("LLVM-21");
    exe.addIncludePath(.{ .cwd_relative = "/usr/include" });

    b.installArtifact(exe);

    // Run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the dAImond LLVM compiler");
    run_step.dependOn(&run_cmd.step);

    // Unit tests for IR
    const ir_tests = b.addTest(.{
        .root_source_file = b.path("src/ir.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_ir_tests = b.addRunArtifact(ir_tests);

    // Unit tests for LLVM bindings
    const llvm_tests = b.addTest(.{
        .root_source_file = b.path("src/llvm_bindings.zig"),
        .target = target,
        .optimize = optimize,
    });
    llvm_tests.linkLibC();
    llvm_tests.linkSystemLibrary("LLVM-21");
    llvm_tests.addIncludePath(.{ .cwd_relative = "/usr/include" });
    const run_llvm_tests = b.addRunArtifact(llvm_tests);

    // Unit tests for IR generation (needs frontend)
    const ir_gen_tests = b.addTest(.{
        .root_source_file = b.path("src/ir_gen.zig"),
        .target = target,
        .optimize = optimize,
    });
    ir_gen_tests.root_module.addImport("frontend", frontend_mod);
    const run_ir_gen_tests = b.addRunArtifact(ir_gen_tests);

    // Unit tests for LLVM generation
    const llvm_gen_tests = b.addTest(.{
        .root_source_file = b.path("src/llvm_gen.zig"),
        .target = target,
        .optimize = optimize,
    });
    llvm_gen_tests.linkLibC();
    llvm_gen_tests.linkSystemLibrary("LLVM-21");
    llvm_gen_tests.addIncludePath(.{ .cwd_relative = "/usr/include" });
    const run_llvm_gen_tests = b.addRunArtifact(llvm_gen_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_ir_tests.step);
    test_step.dependOn(&run_llvm_tests.step);
    test_step.dependOn(&run_ir_gen_tests.step);
    test_step.dependOn(&run_llvm_gen_tests.step);

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

    const run_integration = b.addRunArtifact(integration_exe);
    run_integration.step.dependOn(b.getInstallStep());

    const integration_step = b.step("test-integration", "Run Stage 3 LLVM integration tests");
    integration_step.dependOn(&run_integration.step);
}
