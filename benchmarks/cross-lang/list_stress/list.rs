// List Stress Benchmark
// Push 10 million integers onto a dynamic array, compute sum
// Tests dynamic array resize performance

fn main() {
    let n: i64 = 10_000_000;
    let mut items: Vec<i64> = Vec::new();

    for i in 0..n {
        items.push(i);
    }

    let mut sum: i64 = 0;
    for j in 0..items.len() {
        sum += items[j];
    }

    println!("sum: {}", sum);
    println!("len: {}", items.len());
}
