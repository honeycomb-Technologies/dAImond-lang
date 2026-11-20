module bare_self

struct Point {
    x: int,
    y: int,
}

impl Point {
    fn get_x(self) -> int {
        return self.x
    }

    fn set_x(mut self, val: int) {
        self.x = val
    }

    fn with_type(self: Point) -> int {
        return self.y
    }
}

fn main() {
    let mut p = Point { x: 10, y: 20 }
    println(int_to_string(p.get_x()))
    p.set_x(42)
    println(int_to_string(p.get_x()))
    println(int_to_string(p.with_type()))
}
