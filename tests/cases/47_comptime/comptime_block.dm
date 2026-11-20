module test

const X = comptime { let mut x = 0; x = x + 1; x = x + 2; x }

fn main() {
    println(int_to_string(X))
}