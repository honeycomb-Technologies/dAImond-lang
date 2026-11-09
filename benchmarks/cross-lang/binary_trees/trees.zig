// Binary Trees Benchmark
// Allocate and traverse complete binary trees of depth 21
// Tests allocation/deallocation performance

const std = @import("std");

const TreeNode = struct {
    left: ?*TreeNode,
    right: ?*TreeNode,
};

fn makeTree(allocator: std.mem.Allocator, depth: i32) !*TreeNode {
    const node = try allocator.create(TreeNode);
    if (depth > 0) {
        node.left = try makeTree(allocator, depth - 1);
        node.right = try makeTree(allocator, depth - 1);
    } else {
        node.left = null;
        node.right = null;
    }
    return node;
}

fn countNodes(node: ?*TreeNode) i64 {
    const n = node orelse return 0;
    return 1 + countNodes(n.left) + countNodes(n.right);
}

fn freeTree(allocator: std.mem.Allocator, node: ?*TreeNode) void {
    const n = node orelse return;
    freeTree(allocator, n.left);
    freeTree(allocator, n.right);
    allocator.destroy(n);
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const max_depth: i32 = 21;
    const iterations: i32 = 10;
    var count: i64 = 0;

    var i: i32 = 0;
    while (i < iterations) : (i += 1) {
        const tree = try makeTree(allocator, max_depth);
        count = countNodes(tree);
        freeTree(allocator, tree);
    }

    try stdout.print("nodes: {}\n", .{count});
    try stdout.print("iterations: {}\n", .{iterations});
}
