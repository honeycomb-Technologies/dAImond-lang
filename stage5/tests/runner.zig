//! dAImond Stage 5 (LLVM) Integration Test Runner
//!
//! Reuses test case definitions from Stage 0's runner, but invokes
//! the Stage 5 LLVM compiler binary instead of the Stage 0 compiler.
//! The compiler path is read from the DAIMOND_STAGE5_BIN environment
//! variable, defaulting to "/tmp/stage5_bin" (see run_parity.sh).
//!
//! Run from the stage5/ directory so the Stage 5 compiler can locate
//! the C runtime and LLVM wrappers via its relative-path strategies.

const std = @import("std");
const stage0_runner = @import("stage0_runner");

// Re-export all test types
const TestCase = stage0_runner.TestCase;
const TestFile = stage0_runner.TestFile;
const TestResult = stage0_runner.TestResult;
const runTest = stage0_runner.runTest;

/// Generic structs are a Stage 5 feature: Stage 0 cannot parse a
/// generic struct literal, so these cases are not shared.
const generic_struct_tests = [_]TestCase{
    .{
        .name = "generic_struct_instances",
        .source =
            \\module test
            \\
            \\struct Pair[A, B] {
            \\    first: A,
            \\    second: B
            \\}
            \\
            \\struct Bag[T] {
            \\    items: List[T],
            \\    pairs: List[Pair[T, int]]
            \\}
            \\
            \\impl Bag[T] {
            \\    fn add(mut self, x: T) {
            \\        self.items.push(x)
            \\        let p = Pair[T, int] { first: x, second: self.items.len() }
            \\        self.pairs.push(p)
            \\    }
            \\    fn last(self) -> T {
            \\        return self.items[self.items.len() - 1]
            \\    }
            \\    fn count(self) -> int {
            \\        return self.items.len()
            \\    }
            \\    fn twice(mut self, x: T) -> int {
            \\        self.add(x)
            \\        self.add(x)
            \\        return self.count()
            \\    }
            \\    fn copy_of(self) -> Self {
            \\        let mut out = Bag[T] { items: [], pairs: [] }
            \\        for it in self.items {
            \\            out.add(it)
            \\        }
            \\        return out
            \\    }
            \\}
            \\
            \\struct Holder {
            \\    names: Bag[string],
            \\    nums: Bag[int]
            \\}
            \\
            \\fn make_bag() -> Bag[string] {
            \\    let mut b = Bag[string] { items: [], pairs: [] }
            \\    b.add("x")
            \\    return b
            \\}
            \\
            \\fn total(b: Bag[int]) -> int {
            \\    let mut s = 0
            \\    for v in b.items { s = s + v }
            \\    return s
            \\}
            \\
            \\fn grow(mut b: Bag[int], n: int) {
            \\    b.add(n)
            \\}
            \\
            \\fn main() {
            \\    let mut h = Holder { names: make_bag(), nums: Bag[int] { items: [], pairs: [] } }
            \\    h.names.add("y")
            \\    h.nums.add(10)
            \\    h.nums.add(32)
            \\    grow(h.nums, 100)
            \\    println(h.names.last())
            \\    println(int_to_string(h.names.count()))
            \\    println(int_to_string(total(h.nums)))
            \\    println(int_to_string(h.nums.pairs[2].second))
            \\    println(h.names.pairs[1].first)
            \\    let n = h.names.twice("z")
            \\    println(int_to_string(n))
            \\    let c = h.names.copy_of()
            \\    println(int_to_string(c.count()) + " " + c.last())
            \\    let mut bags: List[Bag[int]] = []
            \\    bags.push(h.nums)
            \\    bags[0].add(7)
            \\    println(int_to_string(bags[0].count()) + " " + int_to_string(h.nums.count()))
            \\    let p: Pair[string, Bag[int]] = Pair[string, Bag[int]] { first: "k", second: bags[0] }
            \\    println(p.first + int_to_string(p.second.last()))
            \\}
        ,
        .expected_output = "y\n2\n142\n3\ny\n4\n4 z\n4 3\nk7\n",
    },
    .{
        .name = "generic_struct_without_type_args",
        .source =
            \\module test
            \\struct Box2[T] { v: T }
            \\fn main() {
            \\    let b = Box2 { v: 1 }
            \\    println(int_to_string(b.v))
            \\}
        ,
        .expected_output = "",
        .expect_compile_error = true,
        .expected_error_contains = &.{"generic struct 'Box2' needs type arguments"},
    },
    .{
        .name = "generic_struct_wrong_arg_count",
        .source =
            \\module test
            \\struct Pair[A, B] { a: A, b: B }
            \\fn main() {
            \\    let p = Pair[int] { a: 1, b: 2 }
            \\    println(int_to_string(p.a))
            \\}
        ,
        .expected_output = "",
        .expect_compile_error = true,
        .expected_error_contains = &.{"'Pair' takes 2 type arguments, got 1"},
    },
};

/// Map[K, V] in Stage 5 is runtime/map.dm. Stage 0 has no byte_at,
/// and its keys() order is the hash table's, not insertion order.
const stage5_map_tests = [_]TestCase{
    .{
        .name = "map_keys_in_insertion_order",
        .source =
            \\module test
            \\fn main() {
            \\    let mut m: Map[string, int] = Map_new()
            \\    m.insert("z", 1)
            \\    m.insert("a", 2)
            \\    m.insert("m", 3)
            \\    m.remove("a")
            \\    m.insert("a", 4)
            \\    m["a"] += 10
            \\    let mut out = ""
            \\    for k in m {
            \\        out = out + k + int_to_string(m[k])
            \\    }
            \\    println(out)
            \\}
        ,
        .expected_output = "z1m3a14\n",
    },
    .{
        .name = "byte_at_and_bounds",
        .source =
            \\module test
            \\fn main() {
            \\    let s = "Az~"
            \\    println(int_to_string(byte_at(s, 0)) + " " + int_to_string(byte_at(s, 1)) + " " + int_to_string(byte_at(s, 2)))
            \\    println(int_to_string(byte_at(s, 3)))
            \\}
        ,
        .expected_output = "65 122 126\n",
        .expected_exit_code = 1,
    },
    .{
        .name = "map_new_needs_a_type",
        .source =
            \\module test
            \\fn main() {
            \\    let m = Map_new()
            \\    println("x")
            \\}
        ,
        .expected_output = "",
        .expect_compile_error = true,
        .expected_error_contains = &.{"cannot tell which Map type Map_new() creates here"},
    },
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Stage 5 compiler path (overridable via environment)
    const compiler_path = std.process.getEnvVarOwned(allocator, "DAIMOND_STAGE5_BIN") catch "/tmp/stage5_bin";

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
        .{ .name = "Generic Structs", .tests = &generic_struct_tests },
        .{ .name = "Maps (Stage 5)", .tests = &stage5_map_tests },
    };

    try stdout.print("\n============================================================\n", .{});
    try stdout.print("  dAImond Stage 5 (LLVM) Integration Tests\n", .{});
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
