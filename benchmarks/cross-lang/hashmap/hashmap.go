package main

import "fmt"

func main() {
	m := make(map[string]int64, 1000000)

	// Insert 1,000,000 entries
	for i := int64(0); i < 1000000; i++ {
		m[fmt.Sprintf("key_%d", i)] = i
	}
	fmt.Println("inserted: 1000000")

	// Lookup all entries, sum values
	var sum int64
	for i := int64(0); i < 1000000; i++ {
		sum += m[fmt.Sprintf("key_%d", i)]
	}
	fmt.Printf("lookup sum: %d\n", sum)

	// Delete entries where i % 3 == 0
	for i := int64(0); i < 1000000; i++ {
		if i%3 == 0 {
			delete(m, fmt.Sprintf("key_%d", i))
		}
	}
	fmt.Printf("final: %d\n", len(m))
}
