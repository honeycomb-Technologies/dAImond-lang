module test

struct Point { x: int, y: int }

impl Point {
    fn sum(self) -> int {
        return self.x + self.y + self.z
    }
}

enum Color { Red, Green }

impl Color {
    fn code(self) -> int {
        return 1
    }
}

fn main() -> int {
    let p = Point { x: 1, y: 2 }
    println(p.z)
    let q = Point { x: 1 }
    let r = Point { x: 1, y: 2, z: 3 }
    let s = Point { x: "one", y: 2.5 }
    println(p.total())
    let c = Color.Red
    let d = Color.Blue
    let e = Color.Blue(3)
    println(c.name())
    return p.sum() + c.code()
}
