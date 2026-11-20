module symbols_lists_declarations

import lib
import utils { add }

const LIMIT: int = 10

struct Point[T] {
    x: T,
    y: T
}

enum Shape {
    Circle(float),
    Rect(float, float),
    Empty
}

trait Area {
    fn area(self) -> float
}

impl Area for Shape {
    fn area(self) -> float {
        return 0.0
    }
}

impl Point[T] {
    fn new(x: T, y: T) -> Point[T] {
        return Point[T] { x: x, y: y }
    }
    fn swap(mut self, other: Point[T]) with [IO] {
        println("swap")
    }
}

extern fn printf(fmt: string, ...) -> i32

fn first[T](xs: List[T],
            fallback: T) -> T {
    if xs.len() > 0 { return xs[0] }
    return fallback
}

fn square(n: int) -> int = n * n

fn main() -> int {
    return 0
}
