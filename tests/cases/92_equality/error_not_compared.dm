module error_not_compared

struct Point {
    x: int,
    y: int
}

struct Other {
    x: int,
    y: int
}

fn main() {
    let p = Point { x: 1, y: 2 }
    let q = Other { x: 1, y: 2 }
    if p == q { println("points") }
    if p < p { println("order") }
    let o: Option[int] = Some(1)
    if o >= o { println("order of options") }
}
