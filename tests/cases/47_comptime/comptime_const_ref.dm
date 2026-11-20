module test

const A = comptime 10
const B = comptime A + 5

fn main() {
    println(int_to_string(B))
}