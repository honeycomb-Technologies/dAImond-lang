module test

fn increment(mut n: int) {
    n = n + 1
}

fn main() {
    let mut x = 10
    increment(x)
    println(x)
}