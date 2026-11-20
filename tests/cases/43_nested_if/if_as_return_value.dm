module test

fn abs(x: int) -> int {
    if x < 0 {
        return 0 - x
    }
    return x
}

fn main() {
    println(int_to_string(abs(-5)))
    println(int_to_string(abs(3)))
}