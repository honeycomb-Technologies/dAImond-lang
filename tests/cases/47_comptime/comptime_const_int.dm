module test

const SIZE = comptime 4 * 1024

fn main() {
    println(int_to_string(SIZE))
}