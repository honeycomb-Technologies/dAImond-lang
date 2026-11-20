module test

struct Circle {
    radius: int,
}

trait Describable {
    fn describe(self: &Self) -> string
}

impl Describable for Circle {
    fn describe(self: &Self) -> string {
        return "circle"
    }
}

fn main() {
    let c = Circle { radius: 5 }
    println(c.describe())
}