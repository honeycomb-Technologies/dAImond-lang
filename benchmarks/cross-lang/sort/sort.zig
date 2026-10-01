const std = @import("std");

var seed: i64 = 42;

fn nextRand() i64 {
    seed = @mod(seed *% 1103515245 +% 12345, 2147483648);
    return seed;
}

fn partition(arr: []i64, low: usize, high: usize) usize {
    const pivot = arr[high];
    var i: isize = @as(isize, @intCast(low)) - 1;
    var j: usize = low;
    while (j < high) : (j += 1) {
        if (arr[j] <= pivot) {
            i += 1;
            const ii: usize = @intCast(i);
            const tmp = arr[ii];
            arr[ii] = arr[j];
            arr[j] = tmp;
        }
    }
    const pi: usize = @intCast(i + 1);
    const tmp = arr[pi];
    arr[pi] = arr[high];
    arr[high] = tmp;
    return pi;
}

fn quicksort(arr: []i64, low_in: isize, high_in: isize) void {
    if (low_in < high_in) {
        const lo: usize = @intCast(low_in);
        const hi: usize = @intCast(high_in);
        const p = partition(arr, lo, hi);
        quicksort(arr, low_in, @as(isize, @intCast(p)) - 1);
        quicksort(arr, @as(isize, @intCast(p)) + 1, high_in);
    }
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;
    const n: usize = 2000000;

    const arr = try allocator.alloc(i64, n);
    defer allocator.free(arr);

    // Generate random numbers
    var i: usize = 0;
    while (i < n) : (i += 1) {
        arr[i] = nextRand();
    }

    // Sort
    quicksort(arr, 0, @as(isize, @intCast(n)) - 1);

    // Print first 5 sorted elements
    i = 0;
    while (i < 5) : (i += 1) {
        try stdout.print("sorted[{}]: {}\n", .{ i, arr[i] });
    }

    // Compute checksum: sum of first 1000 elements
    var checksum: i64 = 0;
    i = 0;
    while (i < 1000) : (i += 1) {
        checksum += arr[i];
    }
    try stdout.print("checksum: {}\n", .{checksum});
}
