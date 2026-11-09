const std = @import("std");

fn fib(n: i64) i64 {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

var results: [4]i64 = .{ 0, 0, 0, 0 };

fn worker(idx: usize) void {
    const ns = [_]i64{ 35, 36, 37, 38 };
    results[idx] = fib(ns[idx]);
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    var threads: [4]std.Thread = undefined;
    for (0..4) |i| {
        threads[i] = try std.Thread.spawn(.{}, worker, .{i});
    }

    for (0..4) |i| {
        threads[i].join();
    }

    var sum: i64 = 0;
    for (results) |r| {
        sum += r;
    }
    try stdout.print("{}\n", .{sum});
}
