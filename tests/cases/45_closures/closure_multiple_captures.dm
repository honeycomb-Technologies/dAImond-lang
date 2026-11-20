module test

fn main() {
    let a = 100
    let b = 200
    let f = |x: int| -> int a + b + x
    println(int_to_string(f(3)))
}