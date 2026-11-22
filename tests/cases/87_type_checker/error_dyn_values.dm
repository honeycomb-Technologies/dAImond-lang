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

fn show(s: dyn Shape) -> float {
    return s.area()
}

trait Unused {
    fn nothing(self) -> int
}

trait Copier {
    fn copy(self) -> Self
}

impl Copier for Square {
    fn copy(self) -> Square {
        return Square { side: self.side }
    }
}

fn unused(u: dyn Unused) -> int {
    return u.nothing()
}

fn unknown(u: dyn Nothing) -> int {
    return 0
}

fn copied(c: dyn Copier) -> float {
    let twin = c.copy()
    return 0.0
}

fn main() -> int {
    let square = Square { side: 2.0 }
    let label = Label { text: "a" }
    let a: dyn Shape = square
    let b: dyn Shape = label
    let c = label as dyn Shape
    println(float_to_string(show(label)))
    println(float_to_string(a.perimeter()))
    let squares = [square]
    let shapes: List[dyn Shape] = squares
    return 0
}
