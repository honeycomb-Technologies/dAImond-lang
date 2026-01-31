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

    const compile_exit_code: u32 = switch (compile_result) {
        .Exited => |code| code,
        .Signal => 255,
        else => 254,
    };

    if (case.expect_compile_error) {
        if (compile_exit_code == 0) {
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

    if (compile_exit_code != 0) {
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
    const run_exit_code: u32 = switch (run_result) {
        .Exited => |code| code,
        .Signal => |sig| @as(u32, sig) + 128,
        else => 254,
    };
    if (run_exit_code != case.expected_exit_code) {
        const msg = try std.fmt.allocPrint(allocator, "Expected exit code {d}, got {d}", .{ case.expected_exit_code, run_exit_code });
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

// Tests for arithmetic and comparison operations
pub const arithmetic_tests = [_]TestCase{
    .{
        .name = "arithmetic_add",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let x: int = 2 + 3
        \\    println(x)
        \\}
        ,
        .expected_output = "5\n",
    },
    .{
        .name = "arithmetic_sub",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let x: int = 10 - 3
        \\    println(x)
        \\}
        ,
        .expected_output = "7\n",
    },
    .{
        .name = "arithmetic_mul",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let x: int = 4 * 5
        \\    println(x)
        \\}
        ,
        .expected_output = "20\n",
    },
    .{
        .name = "arithmetic_div",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let x: int = 20 / 4
        \\    println(x)
        \\}
        ,
        .expected_output = "5\n",
    },
    .{
        .name = "arithmetic_mod",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let x: int = 17 % 5
        \\    println(x)
        \\}
        ,
        .expected_output = "2\n",
    },
    .{
        .name = "arithmetic_complex",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let x: int = (2 + 3) * 4
        \\    println(x)
        \\}
        ,
        .expected_output = "20\n",
    },
    .{
        .name = "arithmetic_negative",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let x: int = -5
        \\    println(x)
        \\}
        ,
        .expected_output = "-5\n",
    },
    .{
        .name = "comparison_ops",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    println(1 < 2)
        \\    println(2 > 1)
        \\    println(1 == 1)
        \\    println(1 != 2)
        \\    println(1 <= 1)
        \\    println(2 >= 2)
        \\}
        ,
        .expected_output = "true\ntrue\ntrue\ntrue\ntrue\ntrue\n",
    },
    .{
        .name = "boolean_logic",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    println(true and true)
        \\    println(true and false)
        \\    println(false or true)
        \\    println(false or false)
        \\    println(not false)
        \\    println(not true)
        \\}
        ,
        .expected_output = "true\nfalse\ntrue\nfalse\ntrue\nfalse\n",
    },
    .{
        .name = "print_integer",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    print(42)
        \\}
        ,
        .expected_output = "42",
    },
    .{
        .name = "print_boolean",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    print(true)
        \\    print(false)
        \\}
        ,
        .expected_output = "truefalse",
    },
};

// Tests for function return values
pub const function_return_tests = [_]TestCase{
    .{
        .name = "function_return_int",
        .source =
        \\module test
        \\
        \\fn add(a: int, b: int) -> int {
        \\    return a + b
        \\}
        \\
        \\fn main() {
        \\    let result = add(3, 4)
        \\    println(result)
        \\}
        ,
        .expected_output = "7\n",
    },
    .{
        .name = "function_return_bool",
        .source =
        \\module test
        \\
        \\fn is_positive(n: int) -> bool {
        \\    return n > 0
        \\}
        \\
        \\fn main() {
        \\    println(is_positive(5))
        \\    println(is_positive(-1))
        \\}
        ,
        .expected_output = "true\nfalse\n",
    },
    .{
        .name = "function_recursive",
        .source =
        \\module test
        \\
        \\fn factorial(n: int) -> int {
        \\    if n <= 1 {
        \\        return 1
        \\    }
        \\    return n * factorial(n - 1)
        \\}
        \\
        \\fn main() {
        \\    println(factorial(5))
        \\}
        ,
        .expected_output = "120\n",
    },
    .{
        .name = "function_multiple_params",
        .source =
        \\module test
        \\
        \\fn max(a: int, b: int) -> int {
        \\    if a > b {
        \\        return a
        \\    }
        \\    return b
        \\}
        \\
        \\fn main() {
        \\    println(max(3, 7))
        \\    println(max(10, 2))
        \\}
        ,
        .expected_output = "7\n10\n",
    },
};

pub const struct_tests = [_]TestCase{
    .{
        .name = "struct_create_access",
        .source =
        \\module test
        \\
        \\struct Point {
        \\    x: int,
        \\    y: int
        \\}
        \\
        \\fn main() {
        \\    let p = Point { x: 10, y: 20 }
        \\    println(p.x)
        \\    println(p.y)
        \\}
        ,
        .expected_output = "10\n20\n",
    },
    .{
        .name = "struct_pass_to_function",
        .source =
        \\module test
        \\
        \\struct Point {
        \\    x: int,
        \\    y: int
        \\}
        \\
        \\fn sum_point(p: Point) -> int {
        \\    return p.x + p.y
        \\}
        \\
        \\fn main() {
        \\    let p = Point { x: 3, y: 7 }
        \\    println(sum_point(p))
        \\}
        ,
        .expected_output = "10\n",
    },
    .{
        .name = "struct_return_from_function",
        .source =
        \\module test
        \\
        \\struct Point {
        \\    x: int,
        \\    y: int
        \\}
        \\
        \\fn make_point(x: int, y: int) -> Point {
        \\    return Point { x: x, y: y }
        \\}
        \\
        \\fn main() {
        \\    let p = make_point(42, 99)
        \\    println(p.x)
        \\    println(p.y)
        \\}
        ,
        .expected_output = "42\n99\n",
    },
    .{
        .name = "struct_field_mutation",
        .source =
        \\module test
        \\
        \\struct Counter {
        \\    value: int
        \\}
        \\
        \\fn main() {
        \\    let mut c = Counter { value: 0 }
        \\    c.value = 5
        \\    println(c.value)
        \\}
        ,
        .expected_output = "5\n",
    },
};

pub const enum_tests = [_]TestCase{
    .{
        .name = "simple_enum",
        .source =
        \\module test
        \\
        \\enum Color {
        \\    Red,
        \\    Green,
        \\    Blue
        \\}
        \\
        \\fn color_value(c: Color) -> int {
        \\    if c == Color::Red {
        \\        return 1
        \\    }
        \\    if c == Color::Green {
        \\        return 2
        \\    }
        \\    return 3
        \\}
        \\
        \\fn main() {
        \\    let c = Color::Red
        \\    println(color_value(c))
        \\}
        ,
        .expected_output = "1\n",
    },
    .{
        .name = "enum_with_payload",
        .source =
        \\module test
        \\
        \\enum Maybe {
        \\    Nothing,
        \\    Just(int)
        \\}
        \\
        \\fn get_value(opt: Maybe) -> int {
        \\    return 42
        \\}
        \\
        \\fn main() {
        \\    let x = Maybe::Just(42)
        \\    println(get_value(x))
        \\}
        ,
        .expected_output = "42\n",
    },
};

pub const loop_extended_tests = [_]TestCase{
    .{
        .name = "for_range_exclusive",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let mut sum = 0
        \\    for i in 0..5 {
        \\        sum = sum + i
        \\    }
        \\    println(sum)
        \\}
        ,
        .expected_output = "10\n",
    },
    .{
        .name = "for_range_inclusive",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let mut sum = 0
        \\    for i in 0..=5 {
        \\        sum = sum + i
        \\    }
        \\    println(sum)
        \\}
        ,
        .expected_output = "15\n",
    },
    .{
        .name = "loop_break",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let mut count = 0
        \\    loop {
        \\        if count >= 5 {
        \\            break
        \\        }
        \\        count = count + 1
        \\    }
        \\    println(count)
        \\}
        ,
        .expected_output = "5\n",
    },
    .{
        .name = "while_continue",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let mut sum = 0
        \\    let mut i = 0
        \\    while i < 10 {
        \\        i = i + 1
        \\        if i % 2 == 0 {
        \\            continue
        \\        }
        \\        sum = sum + i
        \\    }
        \\    println(sum)
        \\}
        ,
        .expected_output = "25\n",
    },
    .{
        .name = "nested_loops",
        .source =
        \\module test
        \\
        \\fn main() {
        \\    let mut total = 0
        \\    for i in 0..3 {
        \\        for j in 0..3 {
        \\            total = total + 1
        \\        }
        \\    }
        \\    println(total)
        \\}
        ,
        .expected_output = "9\n",
    },
};

pub const match_tests = [_]TestCase{
    .{
        .name = "match_integer",
        .source =
        \\module test
        \\
        \\fn describe(n: int) -> int {
        \\    match n {
        \\        1 => { return 10 }
        \\        2 => { return 20 }
        \\        _ => { return 0 }
        \\    }
        \\    return -1
        \\}
        \\
        \\fn main() {
        \\    println(describe(1))
        \\    println(describe(2))
        \\    println(describe(99))
        \\}
        ,
        .expected_output = "10\n20\n0\n",
    },
    .{
        .name = "match_wildcard",
        .source =
        \\module test
        \\
        \\fn classify(n: int) -> int {
        \\    match n {
        \\        0 => { return 0 }
        \\        _ => { return 1 }
        \\    }
        \\    return -1
        \\}
        \\
        \\fn main() {
        \\    println(classify(0))
        \\    println(classify(42))
        \\}
        ,
        .expected_output = "0\n1\n",
    },
    .{
        .name = "match_with_guard",
        .source =
        \\module test
        \\
        \\fn check(n: int) -> int {
        \\    match n {
        \\        x if x > 10 => { return 2 }
        \\        x if x > 0 => { return 1 }
        \\        _ => { return 0 }
        \\    }
        \\    return -1
        \\}
        \\
        \\fn main() {
        \\    println(check(20))
        \\    println(check(5))
        \\    println(check(-1))
        \\}
        ,
        .expected_output = "2\n1\n0\n",
    },
};

pub const impl_tests = [_]TestCase{
    .{
        .name = "impl_basic_method",
        .source =
            \\module test
            \\
            \\struct Point {
            \\    x: int,
            \\    y: int,
            \\}
            \\
            \\impl Point {
            \\    fn sum(self: &Self) -> int {
            \\        return self.x + self.y
            \\    }
            \\}
            \\
            \\fn main() {
            \\    let p = Point { x: 10, y: 20 }
            \\    println(p.sum())
            \\}
        ,
        .expected_output = "30\n",
    },
    .{
        .name = "impl_self_field_access",
        .source =
            \\module test
            \\
            \\struct Rect {
            \\    w: int,
            \\    h: int,
            \\}
            \\
            \\impl Rect {
            \\    fn area(self: &Self) -> int {
            \\        return self.w * self.h
            \\    }
            \\
            \\    fn width(self: &Self) -> int {
            \\        return self.w
            \\    }
            \\}
            \\
            \\fn main() {
            \\    let r = Rect { w: 5, h: 8 }
            \\    println(r.area())
            \\    println(r.width())
            \\}
        ,
        .expected_output = "40\n5\n",
    },
    .{
        .name = "impl_method_with_params",
        .source =
            \\module test
            \\
            \\struct Counter {
            \\    value: int,
            \\}
            \\
            \\impl Counter {
            \\    fn add(self: &Self, n: int) -> int {
            \\        return self.value + n
            \\    }
            \\
            \\    fn multiply(self: &Self, n: int) -> int {
            \\        return self.value * n
            \\    }
            \\}
            \\
            \\fn main() {
            \\    let c = Counter { value: 7 }
            \\    println(c.add(3))
            \\    println(c.multiply(4))
            \\}
        ,
        .expected_output = "10\n28\n",
    },
    .{
        .name = "impl_multiple_methods",
        .source =
            \\module test
            \\
            \\struct Vec2 {
            \\    x: int,
            \\    y: int,
            \\}
            \\
            \\impl Vec2 {
            \\    fn get_x(self: &Self) -> int {
            \\        return self.x
            \\    }
            \\
            \\    fn get_y(self: &Self) -> int {
            \\        return self.y
            \\    }
            \\
            \\    fn dot(self: &Self, other_x: int, other_y: int) -> int {
            \\        return self.x * other_x + self.y * other_y
            \\    }
            \\}
            \\
            \\fn main() {
            \\    let v = Vec2 { x: 3, y: 4 }
            \\    println(v.get_x())
            \\    println(v.get_y())
            \\    println(v.dot(5, 6))
            \\}
        ,
        .expected_output = "3\n4\n39\n",
    },
};

pub const string_tests = [_]TestCase{
    .{
        .name = "string_concatenation",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let a = "hello"
            \\    let b = " world"
            \\    let c = a + b
            \\    println(c)
            \\}
        ,
        .expected_output = "hello world\n",
    },
    .{
        .name = "string_length",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let s = "hello"
            \\    println(len(s))
            \\}
        ,
        .expected_output = "5\n",
    },
    .{
        .name = "int_to_string",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    println(int_to_string(42))
            \\    println(int_to_string(-7))
            \\}
        ,
        .expected_output = "42\n-7\n",
    },
    .{
        .name = "bool_to_string",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    println(bool_to_string(true))
            \\    println(bool_to_string(false))
            \\}
        ,
        .expected_output = "true\nfalse\n",
    },
};

pub const enum_match_tests = [_]TestCase{
    .{
        .name = "match_enum_payload",
        .source =
            \\module test
            \\
            \\enum Shape {
            \\    Circle(int),
            \\    Square(int),
            \\    Empty
            \\}
            \\
            \\fn area(s: Shape) -> int {
            \\    match s {
            \\        Shape::Circle(r) => { return r * r * 3 }
            \\        Shape::Square(side) => { return side * side }
            \\        _ => { return 0 }
            \\    }
            \\    return 0
            \\}
            \\
            \\fn main() {
            \\    let c = Shape::Circle(5)
            \\    let sq = Shape::Square(4)
            \\    let e = Shape::Empty
            \\    println(area(c))
            \\    println(area(sq))
            \\    println(area(e))
            \\}
        ,
        .expected_output = "75\n16\n0\n",
    },
    .{
        .name = "match_simple_enum",
        .source =
            \\module test
            \\
            \\enum Color {
            \\    Red,
            \\    Green,
            \\    Blue
            \\}
            \\
            \\fn color_name(c: Color) -> int {
            \\    match c {
            \\        Color::Red => { return 1 }
            \\        Color::Green => { return 2 }
            \\        Color::Blue => { return 3 }
            \\    }
            \\    return 0
            \\}
            \\
            \\fn main() {
            \\    println(color_name(Color::Red))
            \\    println(color_name(Color::Green))
            \\    println(color_name(Color::Blue))
            \\}
        ,
        .expected_output = "1\n2\n3\n",
    },
    .{
        .name = "match_enum_wildcard",
        .source =
            \\module test
            \\
            \\enum Result {
            \\    Ok(int),
            \\    Err(int)
            \\}
            \\
            \\fn is_ok(r: Result) -> int {
            \\    match r {
            \\        Result::Ok(v) => { return v }
            \\        _ => { return -1 }
            \\    }
            \\    return -2
            \\}
            \\
            \\fn main() {
            \\    let ok = Result::Ok(42)
            \\    let err = Result::Err(99)
            \\    println(is_ok(ok))
            \\    println(is_ok(err))
            \\}
        ,
        .expected_output = "42\n-1\n",
    },
    .{
        .name = "enum_pass_and_return",
        .source =
            \\module test
            \\
            \\enum Option {
            \\    Some(int),
            \\    None
            \\}
            \\
            \\fn double_option(opt: Option) -> Option {
            \\    match opt {
            \\        Option::Some(v) => { return Option::Some(v * 2) }
            \\        _ => { return Option::None }
            \\    }
            \\    return Option::None
            \\}
            \\
            \\fn get_value(opt: Option) -> int {
            \\    match opt {
            \\        Option::Some(v) => { return v }
            \\        _ => { return 0 }
            \\    }
            \\    return 0
            \\}
            \\
            \\fn main() {
            \\    let a = Option::Some(21)
            \\    let b = double_option(a)
            \\    println(get_value(b))
            \\    let c = Option::None
            \\    let d = double_option(c)
            \\    println(get_value(d))
            \\}
        ,
        .expected_output = "42\n0\n",
    },
};

// Phase A: String Operations
pub const string_ops_tests = [_]TestCase{
    .{
        .name = "string_indexing",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let s = "hello"
            \\    let c = s[1]
            \\    println(char_to_string(c))
            \\}
        ,
        .expected_output = "e\n",
    },
    .{
        .name = "string_substr",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let s = "hello world"
            \\    let sub = substr(s, 6, 5)
            \\    println(sub)
            \\}
        ,
        .expected_output = "world\n",
    },
    .{
        .name = "string_compare",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    println("abc" < "abd")
            \\    println("xyz" > "abc")
            \\    println("abc" <= "abc")
            \\    println("abc" >= "abd")
            \\}
        ,
        .expected_output = "true\ntrue\ntrue\nfalse\n",
    },
    .{
        .name = "char_classify",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let s = "a5 Z"
            \\    println(is_alpha(s[0]))
            \\    println(is_digit(s[1]))
            \\    println(is_whitespace(s[2]))
            \\    println(is_alnum(s[3]))
            \\}
        ,
        .expected_output = "true\ntrue\ntrue\ntrue\n",
    },
    .{
        .name = "char_to_string_test",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let s = "hello"
            \\    let c = char_to_string(s[0])
            \\    println(c)
            \\}
        ,
        .expected_output = "h\n",
    },
    .{
        .name = "string_iteration",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    for c in "abc" {
            \\        print(char_to_string(c))
            \\    }
            \\    println("")
            \\}
        ,
        .expected_output = "abc\n",
    },
};

// Phase B: Dynamic Lists
pub const list_tests = [_]TestCase{
    .{
        .name = "list_create_push_get",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let mut items: List[int] = List_new()
            \\    items.push(10)
            \\    items.push(20)
            \\    items.push(30)
            \\    println(items[0])
            \\    println(items[1])
            \\    println(items[2])
            \\}
        ,
        .expected_output = "10\n20\n30\n",
    },
    .{
        .name = "list_len",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let mut items: List[int] = List_new()
            \\    println(len(items))
            \\    items.push(1)
            \\    items.push(2)
            \\    items.push(3)
            \\    println(len(items))
            \\}
        ,
        .expected_output = "0\n3\n",
    },
    .{
        .name = "list_iteration",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let mut items: List[int] = List_new()
            \\    items.push(1)
            \\    items.push(2)
            \\    items.push(3)
            \\    let mut sum: int = 0
            \\    for x in items {
            \\        sum = sum + x
            \\    }
            \\    println(sum)
            \\}
        ,
        .expected_output = "6\n",
    },
    .{
        .name = "list_of_strings",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let mut words: List[String] = List_new()
            \\    words.push("hello")
            \\    words.push(" ")
            \\    words.push("world")
            \\    print(words[0])
            \\    print(words[1])
            \\    println(words[2])
            \\}
        ,
        .expected_output = "hello world\n",
    },
    .{
        .name = "list_of_structs",
        .source =
            \\module test
            \\
            \\struct Point {
            \\    x: int,
            \\    y: int
            \\}
            \\
            \\fn main() {
            \\    let mut points: List[Point] = List_new()
            \\    points.push(Point { x: 1, y: 2 })
            \\    points.push(Point { x: 3, y: 4 })
            \\    let p = points[0]
            \\    println(p.x)
            \\    println(p.y)
            \\    let q = points[1]
            \\    println(q.x)
            \\    println(q.y)
            \\}
        ,
        .expected_output = "1\n2\n3\n4\n",
    },
};

// Phase C: File I/O, Stderr, Exit, Args
pub const io_tests = [_]TestCase{
    .{
        .name = "file_read_write",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    file_write("/tmp/daimond_test_io.txt", "hello from daimond")
            \\    let content = file_read("/tmp/daimond_test_io.txt")
            \\    println(content)
            \\}
        ,
        .expected_output = "hello from daimond\n",
    },
    .{
        .name = "exit_code",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    exit(42)
            \\}
        ,
        .expected_output = "",
        .expected_exit_code = 42,
    },
    .{
        .name = "args_len_test",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let n = args_len()
            \\    if n > 0 {
            \\        println("has args")
            \\    }
            \\}
        ,
        .expected_output = "has args\n",
    },
    .{
        .name = "eprint_test",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    eprintln("error msg")
            \\    println("ok")
            \\}
        ,
        .expected_output = "ok\n",
    },
};

// Phase D: Process Execution
pub const process_tests = [_]TestCase{
    .{
        .name = "system_call",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let result = system("true")
            \\    println(result)
            \\}
        ,
        .expected_output = "0\n",
    },
};

// Phase E: Remaining Utilities
pub const utility_tests = [_]TestCase{
    .{
        .name = "string_find_test",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let pos = string_find("hello world", "world")
            \\    println(pos)
            \\    let notfound = string_find("hello", "xyz")
            \\    println(notfound)
            \\}
        ,
        .expected_output = "6\n-1\n",
    },
    .{
        .name = "starts_ends_with",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    println(starts_with("hello", "hel"))
            \\    println(starts_with("hello", "xyz"))
            \\    println(ends_with("hello", "llo"))
            \\    println(ends_with("hello", "xyz"))
            \\}
        ,
        .expected_output = "true\nfalse\ntrue\nfalse\n",
    },
    .{
        .name = "parse_int_test",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    let n = parse_int("42")
            \\    println(n)
            \\    let m = parse_int("-7")
            \\    println(m)
            \\}
        ,
        .expected_output = "42\n-7\n",
    },
    .{
        .name = "string_contains_test",
        .source =
            \\module test
            \\
            \\fn main() {
            \\    println(string_contains("hello world", "world"))
            \\    println(string_contains("hello world", "xyz"))
            \\    println(string_contains("abc", "bc"))
            \\}
        ,
        .expected_output = "true\nfalse\ntrue\n",
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
        .{ .name = "Arithmetic", .tests = &arithmetic_tests },
        .{ .name = "Function Returns", .tests = &function_return_tests },
        .{ .name = "Structs", .tests = &struct_tests },
        .{ .name = "Enums", .tests = &enum_tests },
        .{ .name = "Extended Loops", .tests = &loop_extended_tests },
        .{ .name = "Match", .tests = &match_tests },
        .{ .name = "Impl Methods", .tests = &impl_tests },
        .{ .name = "Strings", .tests = &string_tests },
        .{ .name = "Enum Match", .tests = &enum_match_tests },
        .{ .name = "String Ops", .tests = &string_ops_tests },
        .{ .name = "Lists", .tests = &list_tests },
        .{ .name = "File I/O", .tests = &io_tests },
        .{ .name = "Process", .tests = &process_tests },
        .{ .name = "Utilities", .tests = &utility_tests },
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
