// Binary Trees Benchmark
// Allocate and traverse complete binary trees of depth 21
// Tests allocation/deallocation performance

package main

import "fmt"

type TreeNode struct {
	left  *TreeNode
	right *TreeNode
}

func makeTree(depth int) *TreeNode {
	node := &TreeNode{}
	if depth > 0 {
		node.left = makeTree(depth - 1)
		node.right = makeTree(depth - 1)
	}
	return node
}

func countNodes(node *TreeNode) int64 {
	if node == nil {
		return 0
	}
	return 1 + countNodes(node.left) + countNodes(node.right)
}

func main() {
	maxDepth := 21
	iterations := 10
	var count int64

	for i := 0; i < iterations; i++ {
		tree := makeTree(maxDepth)
		count = countNodes(tree)
	}

	fmt.Printf("nodes: %d\n", count)
	fmt.Printf("iterations: %d\n", iterations)
}
