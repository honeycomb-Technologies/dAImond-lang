// Binary Trees Benchmark
// Allocate and traverse complete binary trees of depth 21
// Tests allocation/deallocation performance

struct TreeNode {
    left: Option<Box<TreeNode>>,
    right: Option<Box<TreeNode>>,
}

fn make_tree(depth: i32) -> Option<Box<TreeNode>> {
    if depth > 0 {
        Some(Box::new(TreeNode {
            left: make_tree(depth - 1),
            right: make_tree(depth - 1),
        }))
    } else {
        Some(Box::new(TreeNode {
            left: None,
            right: None,
        }))
    }
}

fn count_nodes(node: &Option<Box<TreeNode>>) -> i64 {
    match node {
        None => 0,
        Some(n) => 1 + count_nodes(&n.left) + count_nodes(&n.right),
    }
}

fn main() {
    let max_depth = 21;
    let iterations = 10;
    let mut count: i64 = 0;

    for _ in 0..iterations {
        let tree = make_tree(max_depth);
        count = count_nodes(&tree);
    }

    println!("nodes: {}", count);
    println!("iterations: {}", iterations);
}
