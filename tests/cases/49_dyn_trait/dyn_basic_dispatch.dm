module test

trait Display {
    fn show(self: &Self) -> string
}

struct Point {
    x: int,
    y: int
}

struct Circle {
    radius: int
}

impl Display for Point {
    fn show(self: &Self) -> string {
        return "Point"
    }
}

impl Display for Circle {
    fn show(self: &Self) -> string {
        return "Circle"
    }
}

fn main() {
    let p = Point { x: 1, y: 2 }
    let c = Circle { radius: 5 }
    let d1: dyn Display = p
    let d2: dyn Display = c
    println(d1.show())
    println(d2.show())
}