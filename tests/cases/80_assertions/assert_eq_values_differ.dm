module assert_eq_values_differ

fn double(n: int) -> int {
    return n * 2
}

fn main() -> int {
    assert_eq(double(2), 4)
    println("before")
    assert_eq(double(2), 5)
    println("after")
    return 0
}
