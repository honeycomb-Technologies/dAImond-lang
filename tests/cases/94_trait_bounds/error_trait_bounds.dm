module error_trait_bounds

trait Show {
    fn show(self) -> string
}

trait Named {
    fn name(self) -> string
}

struct Point {
    x: int
}

struct Plain {
    x: int
}

impl Show for Point {
    fn show(self) -> string {
        return int_to_string(self.x)
    }
}

fn shown[T: Show](x: T) -> string {
    return x.show()
}

fn described[T: Show + Named](x: T) -> string {
    return x.name() + x.show()
}

fn all[T: Show](xs: List[T]) -> string {
    return "all"
}

fn lost[T: Missing](x: T) -> int {
    return 0
}

fn main() {
    println(shown(Point { x: 1 }))
    println(shown(Plain { x: 1 }))
    println(shown(5))
    println(described(Point { x: 1 }))
    println(all([Plain { x: 2 }]))
    println(all(["text"]))
}
