module test

const N = comptime len("hello")

fn main() {
    println(int_to_string(N))
}