module test

struct Point {
    x: int,
    y: int,
}

fn make(x: int, y: int) -> Point {
    return Point { x, y }
}

fn main() {
    let p = make(3, 4)
    println(p.x + p.y)
    let x = 10
    let q = Point { x, y: 5 }
    println(q.x + q.y)
}