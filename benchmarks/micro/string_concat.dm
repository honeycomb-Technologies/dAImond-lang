-- Micro Benchmark: String Concatenation
-- Concatenates strings 100,000 times.
-- Tests string allocation and copying performance.

module string_concat_bench

fn main() {
    let mut result = ""
    let mut i = 0
    while i < 100000 {
        result = result + "x"
        i = i + 1
    }
    println("length = " + int_to_string(len(result)))
}
