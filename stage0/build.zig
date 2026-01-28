const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Main executable
    const exe = b.addExecutable(.{
        .name = "daimond",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    // Run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the dAImond compiler");
    run_step.dependOn(&run_cmd.step);

    // Unit tests for lexer
    const lexer_tests = b.addTest(.{
        .root_source_file = b.path("src/lexer.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_lexer_tests = b.addRunArtifact(lexer_tests);

    // Unit tests for errors
    const errors_tests = b.addTest(.{
        .root_source_file = b.path("src/errors.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_errors_tests = b.addRunArtifact(errors_tests);

    // Unit tests for AST
    const ast_tests = b.addTest(.{
        .root_source_file = b.path("src/ast.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_ast_tests = b.addRunArtifact(ast_tests);

    // Unit tests for types
    const types_tests = b.addTest(.{
        .root_source_file = b.path("src/types.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_types_tests = b.addRunArtifact(types_tests);

    // Unit tests for parser
    const parser_tests = b.addTest(.{
        .root_source_file = b.path("src/parser.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_parser_tests = b.addRunArtifact(parser_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lexer_tests.step);
    test_step.dependOn(&run_errors_tests.step);
    test_step.dependOn(&run_ast_tests.step);
    test_step.dependOn(&run_types_tests.step);
    test_step.dependOn(&run_parser_tests.step);
}
