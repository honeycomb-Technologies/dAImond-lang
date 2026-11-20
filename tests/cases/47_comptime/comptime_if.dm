module test

const X = comptime { if true { 42 } else { 0 } }

fn main() {
    println(int_to_string(X))
}