// Naive recursive fibonacci
// Computes fib(40) to test function call overhead

const std = @import("std");

fn fib(n: i64) i64 {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

pub fn main() !void {
    const result = fib(40);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{result});
}
