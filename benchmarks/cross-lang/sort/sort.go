package main

import "fmt"

var seed int64 = 42

func nextRand() int64 {
	seed = (seed*1103515245 + 12345) & 0x7FFFFFFF
	return seed
}

func partition(arr []int64, low, high int) int {
	pivot := arr[high]
	i := low - 1
	for j := low; j < high; j++ {
		if arr[j] <= pivot {
			i++
			arr[i], arr[j] = arr[j], arr[i]
		}
	}
	arr[i+1], arr[high] = arr[high], arr[i+1]
	return i + 1
}

func quicksort(arr []int64, low, high int) {
	if low < high {
		p := partition(arr, low, high)
		quicksort(arr, low, p-1)
		quicksort(arr, p+1, high)
	}
}

func main() {
	n := 2000000
	arr := make([]int64, n)

	// Generate random numbers
	for i := 0; i < n; i++ {
		arr[i] = nextRand()
	}

	// Sort
	quicksort(arr, 0, n-1)

	// Print first 5 sorted elements
	for i := 0; i < 5; i++ {
		fmt.Printf("sorted[%d]: %d\n", i, arr[i])
	}

	// Compute checksum: sum of first 1000 elements
	var checksum int64
	for i := 0; i < 1000; i++ {
		checksum += arr[i]
	}
	fmt.Printf("checksum: %d\n", checksum)
}
