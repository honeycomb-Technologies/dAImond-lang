// String Concatenation Benchmark
// Concatenate a single character 100,000 times
// Tests string allocation overhead
//
// Uses s += "a" (Go's natural approach; strings are immutable so each
// concatenation allocates a new string, making this O(n^2)).

package main

import "fmt"

func main() {
	n := 100000
	s := ""

	for i := 0; i < n; i++ {
		s += "a"
	}

	fmt.Printf("length: %d\n", len(s))
}
