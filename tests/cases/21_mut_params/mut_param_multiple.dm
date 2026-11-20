module test

fn swap(mut a: int, mut b: int) {
    let tmp = a
    a = b
    b = tmp
}

fn main() {
    let mut x = 1
    let mut y = 2
    swap(x, y)
    println(x)
    println(y)
}