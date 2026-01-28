//! dAImond Integration Test Runner
//!
//! This module provides infrastructure for end-to-end testing of the dAImond compiler.
//! It compiles test programs and verifies their output matches expectations.

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const TestCase = struct {
    name: []const u8,
    source: []const u8,
    expected_output: []const u8,
    expected_exit_code: u8 = 0,
    expect_compile_error: bool = false,
};

pub const TestResult = struct {
    name: []const u8,
    passed: bool,
    message: []const u8,
};

/// Run a single test case
pub fn runTest(allocator: Allocator, case: TestCase, compiler_path: []const u8) !TestResult {
    // Create temp directory for test files
    var tmp_dir_path: [std.fs.max_path_bytes]u8 = undefined;
    const tmp_dir = try std.fmt.bufPrint(&tmp_dir_path, "/tmp/daimond_test_{d}", .{std.time.milliTimestamp()});

    std.fs.cwd().makeDir(tmp_dir) catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };
    defer std.fs.cwd().deleteTree(tmp_dir) catch {};

    // Write source to temp file
    const source_path = try std.fmt.allocPrint(allocator, "{s}/test.dm", .{tmp_dir});
    defer allocator.free(source_path);

    const output_path = try std.fmt.allocPrint(allocator, "{s}/test_bin", .{tmp_dir});
    defer allocator.free(output_path);

    {
        const file = try std.fs.cwd().createFile(source_path, .{});
        defer file.close();
        try file.writeAll(case.source);
    }

    // Compile the test
    var compile_args = [_][]const u8{
        compiler_path,
        source_path,
        "-o",
        output_path,
    };

    var compile_proc = std.process.Child.init(&compile_args, allocator);
    compile_proc.stderr_behavior = .Pipe;
    compile_proc.stdout_behavior = .Pipe;

    try compile_proc.spawn();
    const compile_result = try compile_proc.wait();

    if (case.expect_compile_error) {
        if (compile_result.Exited == 0) {
            return .{
                .name = case.name,
                .passed = false,
                .message = "Expected compile error but compilation succeeded",
            };
        }
        return .{
            .name = case.name,
            .passed = true,
            .message = "Compile error as expected",
        };
    }

    if (compile_result.Exited != 0) {
        return .{
            .name = case.name,
            .passed = false,
            .message = "Compilation failed",
        };
    }

    // Run the compiled binary
    var run_args = [_][]const u8{output_path};
    var run_proc = std.process.Child.init(&run_args, allocator);
    run_proc.stderr_behavior = .Pipe;
    run_proc.stdout_behavior = .Pipe;

    try run_proc.spawn();

    // Read stdout
    var stdout_list = std.ArrayList(u8).init(allocator);
    defer stdout_list.deinit();

    if (run_proc.stdout) |stdout| {
        try stdout.reader().readAllArrayList(&stdout_list, 1024 * 1024);
    }

    const run_result = try run_proc.wait();

    // Check exit code
    if (run_result.Exited != case.expected_exit_code) {
        const msg = try std.fmt.allocPrint(allocator, "Expected exit code {d}, got {d}", .{ case.expected_exit_code, run_result.Exited });
        return .{
            .name = case.name,
            .passed = false,
            .message = msg,
        };
    }

    // Check output
    if (!std.mem.eql(u8, stdout_list.items, case.expected_output)) {
        const msg = try std.fmt.allocPrint(allocator, "Output mismatch.\nExpected: '{s}'\nGot: '{s}'", .{ case.expected_output, stdout_list.items });
        return .{
            .name = case.name,
            .passed = false,
            .message = msg,
        };
    }

    return .{
        .name = case.name,
        .passed = true,
        .message = "OK",
    };
}

/// Run all test cases and report results
pub fn runAllTests(allocator: Allocator, cases: []const TestCase, compiler_path: []const u8) !void {
    const stdout = std.io.getStdOut().writer();

    var passed: usize = 0;
    var failed: usize = 0;

    try stdout.print("\n=== dAImond Integration Tests ===\n\n", .{});

    for (cases) |case| {
        const result = runTest(allocator, case, compiler_path) catch |err| {
            try stdout.print("FAIL: {s} - Error: {}\n", .{ case.name, err });
            failed += 1;
            continue;
        };

        if (result.passed) {
            try stdout.print("PASS: {s}\n", .{result.name});
            passed += 1;
        } else {
            try stdout.print("FAIL: {s} - {s}\n", .{ result.name, result.message });
            failed += 1;
        }
    }

    try stdout.print("\n=== Results: {d} passed, {d} failed ===\n", .{ passed, failed });

    if (failed > 0) {
        return error.TestsFailed;
    }
}

// ============================================================================
// Test Cases
// ============================================================================

pub const basic_tests = [_]TestCase{
    .{
        .name = "hello_world",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    print("Hello, World!")
        \\}
        ,
        .expected_output = "Hello, World!",
    },
    .{
        .name = "println",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    println("Line 1")
        \\    println("Line 2")
        \\}
        ,
        .expected_output = "Line 1\nLine 2\n",
    },
    .{
        .name = "multiple_prints",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    print("A")
        \\    print("B")
        \\    print("C")
        \\}
        ,
        .expected_output = "ABC",
    },
    .{
        .name = "empty_main",
        .source =
        \\module test
        \\
        \\fn main() {
        \\}
        ,
        .expected_output = "",
    },
};

pub const function_tests = [_]TestCase{
    .{
        .name = "call_helper_function",
        .source =
        \\module test
        \\
        \\fn greet() {
        \\    print("Hello from greet!")
        \\}
        \\
        \\fn main() {
        \\    greet()
        \\}
        ,
        .expected_output = "Hello from greet!",
    },
    .{
        .name = "multiple_function_calls",
        .source =
        \\module test
        \\
        \\fn a() { print("A") }
        \\fn b() { print("B") }
        \\fn c() { print("C") }
        \\
        \\fn main() {
        \\    a()
        \\    b()
        \\    c()
        \\}
        ,
        .expected_output = "ABC",
    },
};

pub const variable_tests = [_]TestCase{
    .{
        .name = "let_binding_string",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let msg = "Hello from variable!"
        \\    print(msg)
        \\}
        ,
        .expected_output = "Hello from variable!",
    },
    .{
        .name = "multiple_let_bindings",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let a = "A"
        \\    let b = "B"
        \\    let c = "C"
        \\    print(a)
        \\    print(b)
        \\    print(c)
        \\}
        ,
        .expected_output = "ABC",
    },
    .{
        .name = "let_with_type_annotation",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let msg: string = "Typed!"
        \\    print(msg)
        \\}
        ,
        .expected_output = "Typed!",
    },
};

// Tests for control flow
pub const control_flow_tests = [_]TestCase{
    .{
        .name = "if_true",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    if true {
        \\        print("yes")
        \\    }
        \\}
        ,
        .expected_output = "yes",
    },
    .{
        .name = "if_false",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    if false {
        \\        print("no")
        \\    }
        \\    print("done")
        \\}
        ,
        .expected_output = "done",
    },
    .{
        .name = "if_else",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    if false {
        \\        print("then")
        \\    } else {
        \\        print("else")
        \\    }
        \\}
        ,
        .expected_output = "else",
    },
    .{
        .name = "nested_if",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    if true {
        \\        if true {
        \\            print("nested")
        \\        }
        \\    }
        \\}
        ,
        .expected_output = "nested",
    },
};

// Tests for loops
pub const loop_tests = [_]TestCase{
    .{
        .name = "while_loop_simple",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let mut i: int = 0
        \\    while i < 3 {
        \\        print("X")
        \\        i = i + 1
        \\    }
        \\}
        ,
        .expected_output = "XXX",
    },
    .{
        .name = "while_false",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    while false {
        \\        print("never")
        \\    }
        \\    print("done")
        \\}
        ,
        .expected_output = "done",
    },
};

// Tests for comments
pub const comment_tests = [_]TestCase{
    .{
        .name = "line_comment",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    // This is a comment
        \\    print("works")
        \\}
        ,
        .expected_output = "works",
    },
    .{
        .name = "inline_comment",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    print("before") // comment after
        \\    print("after")
        \\}
        ,
        .expected_output = "beforeafter",
    },
};

// Tests that are expected to fail compilation
pub const compile_error_tests = [_]TestCase{
    .{
        .name = "syntax_error",
        .source =
        \\module test
        \\fn main( {
        \\}
        ,
        .expected_output = "",
        .expect_compile_error = true,
    },
    .{
        .name = "undefined_function",
        .source =
        \\module test
        \\fn main() {
        \\    undefined_func()
        \\}
        ,
        .expected_output = "",
        .expect_compile_error = true,
    },
    .{
        .name = "missing_main",
        .source =
        \\module test
        \\
        \\fn not_main() {
        \\    print("missing main")
        \\}
        ,
        .expected_output = "",
        .expect_compile_error = true,
    },
    .{
        .name = "unterminated_string",
        .source =
        \\module test
        \\fn main() {
        \\    print("unterminated)
        \\}
        ,
        .expected_output = "",
        .expect_compile_error = true,
    },
};

// Tests for features not yet implemented (will fail until implemented)
pub const future_tests = [_]TestCase{
    // These tests document what we want to support
};

// Main entry point for running tests
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Find compiler path
    const compiler_path = "zig-out/bin/daimond";

    const stdout = std.io.getStdOut().writer();
    var total_passed: usize = 0;
    var total_failed: usize = 0;

    // Run all test suites
    const suites = [_]struct { name: []const u8, tests: []const TestCase }{
        .{ .name = "Basic", .tests = &basic_tests },
        .{ .name = "Functions", .tests = &function_tests },
        .{ .name = "Variables", .tests = &variable_tests },
        .{ .name = "Control Flow", .tests = &control_flow_tests },
        .{ .name = "Loops", .tests = &loop_tests },
        .{ .name = "Comments", .tests = &comment_tests },
        .{ .name = "Compile Errors", .tests = &compile_error_tests },
    };

    try stdout.print("\n============================================================\n", .{});
    try stdout.print("  dAImond Stage 0 Integration Tests\n", .{});
    try stdout.print("============================================================\n\n", .{});

    for (suites) |suite| {
        try stdout.print("--- {s} Tests ---\n", .{suite.name});

        for (suite.tests) |case| {
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
    try stdout.print("  Results: {d} passed, {d} failed\n", .{ total_passed, total_failed });
    try stdout.print("============================================================\n", .{});

    if (total_failed > 0) {
        return error.TestsFailed;
    }
}

test "runner infrastructure" {
    // Basic test to ensure the runner compiles
    const case = TestCase{
        .name = "test",
        .source = "fn main() {}",
        .expected_output = "",
    };
    _ = case;
}
