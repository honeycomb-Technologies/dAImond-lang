module test

struct Vec2 {
    x: int,
    y: int,
}

impl Vec2 {
    fn get_x(self: &Self) -> int {
        return self.x
    }

    fn get_y(self: &Self) -> int {
        return self.y
    }

    fn dot(self: &Self, other_x: int, other_y: int) -> int {
        return self.x * other_x + self.y * other_y
    }
}

fn main() {
    let v = Vec2 { x: 3, y: 4 }
    println(v.get_x())
    println(v.get_y())
    println(v.dot(5, 6))
}