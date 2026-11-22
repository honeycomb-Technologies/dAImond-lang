module error_comptime_not_evaluated

struct Point {
    x: int,
    y: int
}

fn origin() -> Point {
    return Point { x: 0, y: 0 }
}

const X = comptime origin().x

fn main() {
    println(int_to_string(X))
}
