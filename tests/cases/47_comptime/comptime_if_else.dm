module test

const X = comptime { let a = 5; if a > 3 { a * 2 } else { a } }

fn main() {
    println(int_to_string(X))
}