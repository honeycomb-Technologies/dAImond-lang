module test

fn main() {
    let x = 10
    let f = |y: int| -> int x + y
    println(int_to_string(f(5)))
}