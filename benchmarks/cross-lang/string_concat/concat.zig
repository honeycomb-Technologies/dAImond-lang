// String Concatenation Benchmark
// Concatenate a single character 100,000 times
// Tests string allocation overhead
//
// Uses ArrayList(u8) (Zig's natural dynamic string building approach).

const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const n: usize = 100_000;
    var s = std.ArrayList(u8).init(allocator);
    defer s.deinit();

    for (0..n) |_| {
        try s.append('a');
    }

    try stdout.print("length: {}\n", .{s.items.len});
}
