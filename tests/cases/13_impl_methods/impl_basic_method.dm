module test

struct Point {
    x: int,
    y: int,
}

impl Point {
    fn sum(self: &Self) -> int {
        return self.x + self.y
    }
}

fn main() {
    let p = Point { x: 10, y: 20 }
    println(p.sum())
}