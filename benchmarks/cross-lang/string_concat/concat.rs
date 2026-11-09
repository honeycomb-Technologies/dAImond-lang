// String Concatenation Benchmark
// Concatenate a single character 100,000 times
// Tests string allocation overhead
//
// Uses String::push (Rust's natural mutable string building approach).

fn main() {
    let n = 100_000;
    let mut s = String::new();

    for _ in 0..n {
        s.push('a');
    }

    println!("length: {}", s.len());
}
