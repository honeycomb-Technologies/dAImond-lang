-- List Stress Benchmark
-- Push 10 million integers onto a dynamic array, compute sum
-- Tests dynamic array resize performance

fn main() {
    let mut items: List[int] = []
    let n = 10000000

    let mut i = 0
    while i < n {
        items.push(i)
        i = i + 1
    }

    let mut sum = 0
    let mut j = 0
    while j < items.len() {
        sum = sum + items[j]
        j = j + 1
    }

    println("sum: " + int_to_string(sum))
    println("len: " + int_to_string(items.len()))
}
