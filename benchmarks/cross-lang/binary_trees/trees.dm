-- Binary Trees Benchmark
-- Allocate and traverse complete binary trees of depth 21
-- Tests allocation/deallocation performance

struct TreeNode {
    left: Box[TreeNode],
    right: Box[TreeNode],
    depth: int,
}

fn make_tree(d: int) -> Box[TreeNode] {
    if d == 0 {
        return Box_new(TreeNode { left: Box_null(), right: Box_null(), depth: 0 })
    }
    return Box_new(TreeNode { left: make_tree(d - 1), right: make_tree(d - 1), depth: d })
}

fn count_nodes(node: Box[TreeNode]) -> int {
    if node.depth == 0 {
        return 1
    }
    return 1 + count_nodes(node.left) + count_nodes(node.right)
}

fn main() {
    let max_depth = 21
    let iterations = 10
    let mut count = 0

    let mut i = 0
    while i < iterations {
        let tree = make_tree(max_depth)
        count = count_nodes(tree)
        i = i + 1
    }

    println("nodes: " + int_to_string(count))
    println("iterations: " + int_to_string(iterations))
}
