module test

struct TreeNode {
    value: int,
    left: Box[TreeNode],
    right: Box[TreeNode],
}

fn tree_sum(node: Box[TreeNode]) -> int {
    if node == Box_null() {
        return 0
    }
    let v = node.value
    let l = tree_sum(node.left)
    let r = tree_sum(node.right)
    return v + l + r
}

fn main() {
    let leaf1: Box[TreeNode] = Box_new(TreeNode { value: 1, left: Box_null(), right: Box_null() })
    let leaf2: Box[TreeNode] = Box_new(TreeNode { value: 3, left: Box_null(), right: Box_null() })
    let root: Box[TreeNode] = Box_new(TreeNode { value: 2, left: leaf1, right: leaf2 })
    println(tree_sum(root))
}