// List Stress Benchmark
// Push 10 million integers onto a dynamic array, compute sum
// Tests dynamic array resize performance

const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const n: usize = 10_000_000;
    var items = std.ArrayList(i64).init(allocator);
    defer items.deinit();

    for (0..n) |i| {
        try items.append(@intCast(i));
    }

    var sum: i64 = 0;
    for (0..items.items.len) |j| {
        sum += items.items[j];
    }

    try stdout.print("sum: {}\n", .{sum});
    try stdout.print("len: {}\n", .{items.items.len});
}
