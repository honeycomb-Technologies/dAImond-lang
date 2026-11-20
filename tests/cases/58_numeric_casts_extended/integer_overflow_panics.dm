module test
fn grow(start: int, steps: int) -> int {
    let mut x = start
    let mut i = 0
    while i < steps {
        x = x * 3 + 1
        i = i + 1
    }
    return x
}
fn main() {
    println(int_to_string(grow(1, 30)))
    println(int_to_string(grow(1, 60)))
    println("not reached")
}