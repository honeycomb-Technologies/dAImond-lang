module test

struct Point { x: int, y: int }

impl Point {
    fn new(x: int, y: int) -> Point {
        return Point { x: x, y: y }
    }
    fn moved(self, dx: int) -> Point {
        return Point { x: self.x + dx, y: self.y }
    }
}

struct Pair[A, B] { first: A, second: B }

impl Pair[A, B] {
    fn with_first(self, a: A) -> Pair[A, B] {
        return Pair[A, B] { first: a, second: self.second }
    }
}

enum Shape { Circle(float), Rect(int, int) }

fn total(xs: List[int]) -> int {
    return xs.len()
}

fn first[T](xs: List[T], fallback: T) -> T {
    if xs.len() > 0 { return xs[0] }
    return fallback
}

fn apply(f: fn(int) -> int, v: int) -> int {
    return f("four") + f(v, v)
}

fn main() -> int {
    let p = Point { x: 1, y: 2 }
    let mut names: List[string] = []
    names.push(3)
    println(total(names))
    println(total(p))
    println(first(names, 0))
    let q = p.moved("left")
    let r = p.moved()
    let s = Point.new(1, "two")
    let t = Point.new(1)
    let pair = Pair[int, string] { first: 1, second: "one" }
    let other = pair.with_first("two")
    let mut ages: Map[string, int] = Map_new()
    ages.insert("a", "one")
    println(ages.get(1))
    let shape = Shape.Rect(1, "wide")
    let double = |x: int| -> int x * 2
    println(double("two"))
    println(double(1, 2))
    return 0
}
