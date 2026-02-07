//! dAImond Stage 3 (LLVM) Integration Test Runner
//!
//! Reuses test case definitions from Stage 0's runner, but invokes
//! the Stage 3 LLVM compiler (daimond-llvm) instead of the Stage 0 compiler.

const std = @import("std");
const stage0_runner = @import("stage0_runner");

// Re-export all test types
const TestCase = stage0_runner.TestCase;
const TestFile = stage0_runner.TestFile;
const TestResult = stage0_runner.TestResult;
const runTest = stage0_runner.runTest;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Stage 3 compiler path
    const compiler_path = "zig-out/bin/daimond-llvm";

    const stdout = std.io.getStdOut().writer();
    var total_passed: usize = 0;
    var total_failed: usize = 0;
    var total_skipped: usize = 0;

    // Run all test suites — same as Stage 0
    const suites = [_]struct { name: []const u8, tests: []const TestCase }{
        .{ .name = "Basic", .tests = &stage0_runner.basic_tests },
        .{ .name = "Functions", .tests = &stage0_runner.function_tests },
        .{ .name = "Variables", .tests = &stage0_runner.variable_tests },
        .{ .name = "Control Flow", .tests = &stage0_runner.control_flow_tests },
        .{ .name = "Loops", .tests = &stage0_runner.loop_tests },
        .{ .name = "Comments", .tests = &stage0_runner.comment_tests },
        .{ .name = "Arithmetic", .tests = &stage0_runner.arithmetic_tests },
        .{ .name = "Function Returns", .tests = &stage0_runner.function_return_tests },
        .{ .name = "Structs", .tests = &stage0_runner.struct_tests },
        .{ .name = "Enums", .tests = &stage0_runner.enum_tests },
        .{ .name = "Extended Loops", .tests = &stage0_runner.loop_extended_tests },
        .{ .name = "Match", .tests = &stage0_runner.match_tests },
        .{ .name = "Impl Methods", .tests = &stage0_runner.impl_tests },
        .{ .name = "Strings", .tests = &stage0_runner.string_tests },
        .{ .name = "Enum Match", .tests = &stage0_runner.enum_match_tests },
        .{ .name = "String Ops", .tests = &stage0_runner.string_ops_tests },
        .{ .name = "Lists", .tests = &stage0_runner.list_tests },
        .{ .name = "File I/O", .tests = &stage0_runner.io_tests },
        .{ .name = "Process", .tests = &stage0_runner.process_tests },
        .{ .name = "Utilities", .tests = &stage0_runner.utility_tests },
        .{ .name = "Mut Params", .tests = &stage0_runner.mut_param_tests },
        .{ .name = "Forward Refs", .tests = &stage0_runner.forward_ref_tests },
        .{ .name = "Box Types", .tests = &stage0_runner.box_tests },
        .{ .name = "Map Types", .tests = &stage0_runner.map_tests },
        .{ .name = "Generic Functions", .tests = &stage0_runner.generic_function_tests },
        .{ .name = "Imports", .tests = &stage0_runner.import_tests },
        .{ .name = "Option/Result", .tests = &stage0_runner.option_result_tests },
        .{ .name = "Try Operator", .tests = &stage0_runner.try_operator_tests },
        .{ .name = "String Utils", .tests = &stage0_runner.string_util_tests },
        .{ .name = "Path Utils", .tests = &stage0_runner.path_util_tests },
        .{ .name = "Higher-Order Fns", .tests = &stage0_runner.higher_order_tests },
        .{ .name = "Compile Errors", .tests = &stage0_runner.compile_error_tests },
        .{ .name = "List Methods", .tests = &stage0_runner.list_method_tests },
        .{ .name = "Match Option/Result", .tests = &stage0_runner.match_option_result_tests },
        .{ .name = "Map Iteration", .tests = &stage0_runner.map_iteration_tests },
        .{ .name = "Lambdas", .tests = &stage0_runner.lambda_tests },
        .{ .name = "String Ops Extra", .tests = &stage0_runner.string_ops_extra_tests },
        .{ .name = "Traits", .tests = &stage0_runner.trait_tests },
        .{ .name = "Extern Functions", .tests = &stage0_runner.extern_tests },
        .{ .name = "Numeric Types", .tests = &stage0_runner.numeric_type_tests },
        .{ .name = "Operator Overloading", .tests = &stage0_runner.operator_overload_tests },
        .{ .name = "Enum Dot Notation", .tests = &stage0_runner.enum_dot_tests },
        .{ .name = "Nested If", .tests = &stage0_runner.nested_if_tests },
        .{ .name = "String Interpolation", .tests = &stage0_runner.string_interp_tests },
        .{ .name = "Closures", .tests = &stage0_runner.closure_tests },
        .{ .name = "Regions", .tests = &stage0_runner.region_tests },
        .{ .name = "Comptime", .tests = &stage0_runner.comptime_tests },
        .{ .name = "Effects", .tests = &stage0_runner.effect_tests },
        .{ .name = "Dyn Trait", .tests = &stage0_runner.dyn_trait_tests },
        .{ .name = "Bare Self", .tests = &stage0_runner.bare_self_tests },
        .{ .name = "SIMD", .tests = &stage0_runner.simd_tests },
        .{ .name = "Async/Await", .tests = &stage0_runner.async_await_tests },
        .{ .name = "Async Phase B", .tests = &stage0_runner.async_phase_b_tests },
        .{ .name = "Array Sizes", .tests = &stage0_runner.array_size_tests },
        .{ .name = "FS Builtins", .tests = &stage0_runner.fs_builtin_tests },
        .{ .name = "Division Safety", .tests = &stage0_runner.div_safety_tests },
        .{ .name = "Bounds Checking", .tests = &stage0_runner.bounds_check_tests },
        .{ .name = "Numeric Casts Extended", .tests = &stage0_runner.numeric_cast_tests },
        .{ .name = "String Edge Cases", .tests = &stage0_runner.string_edge_tests },
        .{ .name = "Nested Generics", .tests = &stage0_runner.nested_generic_tests },
        .{ .name = "Regions Extended", .tests = &stage0_runner.region_extended_tests },
        .{ .name = "Closures Extended", .tests = &stage0_runner.closure_extended_tests },
        .{ .name = "Op Overload Extended", .tests = &stage0_runner.op_overload_extended_tests },
        .{ .name = "Comprehensive", .tests = &stage0_runner.comprehensive_test },
    };

    try stdout.print("\n============================================================\n", .{});
    try stdout.print("  dAImond Stage 3 (LLVM) Integration Tests\n", .{});
    try stdout.print("============================================================\n\n", .{});

    for (suites) |suite| {
        try stdout.print("--- {s} Tests ---\n", .{suite.name});

        for (suite.tests) |case| {
            if (case.skip) {
                try stdout.print("  SKIP: {s}\n", .{case.name});
                total_skipped += 1;
                continue;
            }

            const result = runTest(allocator, case, compiler_path) catch |err| {
                try stdout.print("  FAIL: {s} - Error: {}\n", .{ case.name, err });
                total_failed += 1;
                continue;
            };

            if (result.passed) {
                try stdout.print("  PASS: {s}\n", .{result.name});
                total_passed += 1;
            } else {
                try stdout.print("  FAIL: {s}\n        {s}\n", .{ result.name, result.message });
                total_failed += 1;
            }
        }
        try stdout.print("\n", .{});
    }

    try stdout.print("============================================================\n", .{});
    try stdout.print("  Results: {d} passed, {d} failed, {d} skipped\n", .{ total_passed, total_failed, total_skipped });
    try stdout.print("============================================================\n", .{});

    if (total_failed > 0) {
        return error.TestsFailed;
    }
}
