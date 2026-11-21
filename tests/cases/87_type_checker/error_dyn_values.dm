module test

trait Shape {
    fn area(self) -> float
}

struct Square { side: float }
struct Label { text: string }

impl Shape for Square {
    fn area(self) -> float {
        return self.side * self.side
    }
}

fn main() -> int {
    let square = Square { side: 2.0 }
    let label = Label { text: "a" }
    let a: dyn Shape = square
    let b: dyn Shape = label
    let other = Square { side: 3.0 }
    let mut c: dyn Shape = square
    c = other
    println(a.area() + c.area())
    return 0
}
