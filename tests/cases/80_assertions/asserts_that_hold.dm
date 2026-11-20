module asserts_that_hold

struct Point {
    x: int,
    y: int
}

fn main() -> int {
    assert(1 + 1 == 2)
    assert(len("abc") == 3)
    assert_eq(3, 1 + 2)
    assert_eq(1.5, 0.5 + 1.0)
    assert_eq(true, 2 > 1)
    assert_eq("a" + "b", "ab")
    let p = Point { x: 1, y: 2 }
    assert_eq(p.x + p.y, 3)
    println("all hold")
    return 0
}
