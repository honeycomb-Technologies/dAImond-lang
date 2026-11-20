module test

struct Vec2 {
    x: int,
    y: int
}

impl Vec2 {
    fn add(self: &Self, other: Vec2) -> Vec2 {
        return Vec2 { x: self.x + other.x, y: self.y + other.y }
    }
}

fn main() {
    let a = Vec2 { x: 1, y: 2 }
    let b = Vec2 { x: 3, y: 4 }
    let c = a + b
    println(int_to_string(c.x))
    println(int_to_string(c.y))
}