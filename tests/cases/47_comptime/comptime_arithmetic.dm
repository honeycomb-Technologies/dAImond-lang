module test

fn main() {
    let x = comptime 2 + 3 * 4
    println(int_to_string(x))
}