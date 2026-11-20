module test

struct Point {
    x: int,
    y: int
}

impl Point {
    fn eq(self: &Self, other: Point) -> bool {
        return self.x == other.x and self.y == other.y
    }
}

fn main() {
    let a = Point { x: 1, y: 2 }
    let b = Point { x: 1, y: 2 }
    let c = Point { x: 3, y: 4 }
    if a == b {
        println("equal")
    } else {
        println("not equal")
    }
    if a == c {
        println("equal")
    } else {
        println("not equal")
    }
}