module test

fn main() {
    let x = comptime 10 > 5
    println(bool_to_string(x))
}