-- String Concatenation Benchmark
-- Concatenate a single character 100,000 times
-- Tests string allocation overhead

fn main() {
    let n = 100000
    let mut s = ""

    let mut i = 0
    while i < n {
        s = s + "a"
        i = i + 1
    }

    println("length: " + int_to_string(len(s)))
}
