module test

struct Point { x: int, y: int }
struct Circle { radius: int }

trait HasArea {
    fn area(self: &Self) -> int
}

impl HasArea for Point {
    fn area(self: &Self) -> int {
        return self.x * self.y
    }
}

impl HasArea for Circle {
    fn area(self: &Self) -> int {
        return self.radius * self.radius * 3
    }
}

fn main() {
    let p = Point { x: 3, y: 4 }
    let c = Circle { radius: 5 }
    println(int_to_string(p.area()))
    println(int_to_string(c.area()))
}