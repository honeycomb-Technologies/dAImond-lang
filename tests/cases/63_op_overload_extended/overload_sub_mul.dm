struct Vec2 {
    x: float,
    y: float
}
impl Vec2 {
    fn sub(self, other: Vec2) -> Vec2 {
        return Vec2 { x: self.x - other.x, y: self.y - other.y }
    }
}
fn main() {
    let a = Vec2 { x: 5.0, y: 3.0 }
    let b = Vec2 { x: 2.0, y: 1.0 }
    let c = a - b
    println(float_to_string(c.x))
    println(float_to_string(c.y))
}