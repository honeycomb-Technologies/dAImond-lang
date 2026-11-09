package main

import (
	"fmt"
	"sync"
)

func fib(n int) int {
	if n <= 1 {
		return n
	}
	return fib(n-1) + fib(n-2)
}

func main() {
	var wg sync.WaitGroup
	results := make([]int, 4)
	ns := []int{35, 36, 37, 38}

	for i := 0; i < 4; i++ {
		wg.Add(1)
		go func(idx int) {
			defer wg.Done()
			results[idx] = fib(ns[idx])
		}(i)
	}

	wg.Wait()

	sum := 0
	for _, r := range results {
		sum += r
	}
	fmt.Println(sum)
}
