const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;

    var m = std.StringHashMap(i64).init(allocator);
    defer m.deinit();
    try m.ensureTotalCapacity(1500000);

    // We need to store keys since StringHashMap stores slices
    var keys = std.ArrayList([]u8).init(allocator);
    defer {
        for (keys.items) |k| allocator.free(k);
        keys.deinit();
    }

    var buf: [32]u8 = undefined;

    // Insert 1,000,000 entries
    var i: i64 = 0;
    while (i < 1000000) : (i += 1) {
        const s = std.fmt.bufPrint(&buf, "key_{}", .{i}) catch unreachable;
        const key = try allocator.dupe(u8, s);
        try keys.append(key);
        try m.put(key, i);
    }
    try stdout.print("inserted: 1000000\n", .{});

    // Lookup all entries, sum values
    var sum: i64 = 0;
    i = 0;
    while (i < 1000000) : (i += 1) {
        const s = std.fmt.bufPrint(&buf, "key_{}", .{i}) catch unreachable;
        sum += m.get(s).?;
    }
    try stdout.print("lookup sum: {}\n", .{sum});

    // Delete entries where i % 3 == 0
    i = 0;
    while (i < 1000000) : (i += 1) {
        if (@mod(i, 3) == 0) {
            const s = std.fmt.bufPrint(&buf, "key_{}", .{i}) catch unreachable;
            _ = m.fetchRemove(s);
        }
    }
    try stdout.print("final: {}\n", .{m.count()});
}
