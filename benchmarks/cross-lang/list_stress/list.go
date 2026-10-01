// List Stress Benchmark
// Push 10 million integers onto a dynamic array, compute sum
// Tests dynamic array resize performance

package main

import "fmt"

func main() {
	n := 10000000
	items := make([]int64, 0)

	for i := 0; i < n; i++ {
		items = append(items, int64(i))
	}

	var sum int64
	for j := 0; j < len(items); j++ {
		sum += items[j]
	}

	fmt.Printf("sum: %d\n", sum)
	fmt.Printf("len: %d\n", len(items))
}
