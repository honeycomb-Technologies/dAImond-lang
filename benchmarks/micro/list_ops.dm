-- Micro Benchmark: List Operations
-- Push/pop 1,000,000 elements.
-- Tests dynamic array resize and memory allocation.

module list_ops_bench

fn main() {
    let mut items: List[int] = []

    -- Push 1M elements
    let mut i = 0
    while i < 1000000 {
        items.push(i)
        i = i + 1
    }
    println("pushed " + int_to_string(items.len()) + " items")

    -- Pop all elements
    let mut sum = 0
    while items.len() > 0 {
        let val = items.pop()
        sum = sum + val
    }
    println("sum = " + int_to_string(sum))
}
