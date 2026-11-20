module test

const X = comptime { let x = 3; let y = 4; x + y }

fn main() {
    println(int_to_string(X))
}