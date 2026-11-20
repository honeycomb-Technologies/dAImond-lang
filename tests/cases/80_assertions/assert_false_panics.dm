module assert_false_panics

fn main() -> int {
    println("before")
    assert(len("abc") == 4)
    println("after")
    return 0
}
