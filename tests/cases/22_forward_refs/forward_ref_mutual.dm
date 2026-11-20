module test

fn is_even(n: int) -> bool {
    if n == 0 {
        return true
    }
    return is_odd(n - 1)
}

fn is_odd(n: int) -> bool {
    if n == 0 {
        return false
    }
    return is_even(n - 1)
}

fn main() {
    println(is_even(4))
    println(is_odd(3))
}