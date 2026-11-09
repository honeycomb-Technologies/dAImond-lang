const std = @import("std");

/// Detected LLVM configuration (via llvm-config).
const LlvmConfig = struct {
    /// System library name, e.g. "LLVM-23" (fallback: "LLVM").
    lib_name: []const u8,
    libdir: ?[]const u8,
    includedir: ?[]const u8,
};

/// Run `llvm-config <flag>` and return its trimmed stdout, or null on failure.
fn runLlvmConfig(b: *std.Build, flag: []const u8) ?[]const u8 {
    var code: u8 = undefined;
    const out = b.runAllowFail(&.{ "llvm-config", flag }, &code, .Ignore) catch return null;
    if (code != 0) return null;
    const trimmed = std.mem.trim(u8, out, " \t\r\n");
    if (trimmed.len == 0) return null;
    return trimmed;
}

/// Detect LLVM via llvm-config: parse the -lLLVM-N token from `--libs`,
/// plus `--libdir` / `--includedir`. Falls back to plain "LLVM" and
/// "/usr/include" (the previous hard-coded behavior) if detection fails.
fn detectLlvm(b: *std.Build) LlvmConfig {
    var lib_name: []const u8 = "LLVM";
    if (runLlvmConfig(b, "--libs")) |libs| {
        var it = std.mem.tokenizeAny(u8, libs, " \t");
        var plain: ?[]const u8 = null;
        while (it.next()) |tok| {
            if (std.mem.startsWith(u8, tok, "-lLLVM-")) {
                lib_name = tok[2..];
                plain = null;
                break;
            } else if (std.mem.eql(u8, tok, "-lLLVM")) {
                plain = tok[2..];
            }
        } else if (plain) |p| {
            lib_name = p;
        }
    }
    return .{
        .lib_name = lib_name,
        .libdir = runLlvmConfig(b, "--libdir"),
        .includedir = runLlvmConfig(b, "--includedir") orelse "/usr/include",
    };
}

/// Link a compile step against the detected LLVM.
fn linkLlvm(step: *std.Build.Step.Compile, llvm: LlvmConfig) void {
    step.linkLibC();
    step.linkSystemLibrary(llvm.lib_name);
    if (llvm.libdir) |libdir| {
        step.addLibraryPath(.{ .cwd_relative = libdir });
    }
    if (llvm.includedir) |includedir| {
        step.addIncludePath(.{ .cwd_relative = includedir });
    }
}

pub fn build(b: *std.Build) void {
    const llvm = detectLlvm(b);
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

    // Link against LLVM (detected via llvm-config)
    linkLlvm(exe, llvm);

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
    linkLlvm(llvm_tests, llvm);
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
    linkLlvm(llvm_gen_tests, llvm);
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
