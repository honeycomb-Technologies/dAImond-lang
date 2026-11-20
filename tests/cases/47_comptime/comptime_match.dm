module test

const X = comptime { let v = 2; match v { 1 => 10, 2 => 20, _ => 0 } }

fn main() {
    println(int_to_string(X))
}