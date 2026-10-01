// Naive recursive fibonacci
// Computes fib(40) to test function call overhead

package main

import "fmt"

func fib(n int64) int64 {
	if n <= 1 {
		return n
	}
	return fib(n-1) + fib(n-2)
}

func main() {
	result := fib(40)
	fmt.Println(result)
}
