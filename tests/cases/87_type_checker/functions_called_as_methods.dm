module test

struct Point { x: int, y: int }

impl Point {
    fn sum(self) -> int {
        return self.x + self.y
    }
}

fn double(n: int) -> int {
    return n * 2
}

fn shout(s: string) -> string {
    return s + "!"
}

fn longest(xs: List[string], fallback: string) -> string {
    let mut best = fallback
    for x in xs {
        if len(x) > len(best) { best = x }
    }
    return best
}

fn first[T](xs: List[T]) -> T {
    return xs[0]
}

fn scaled(p: Point, by: int) -> Point {
    return Point { x: p.x * by, y: p.y * by }
}

fn apply(f: fn(int) -> int, v: int) -> int {
    return f(v)
}

fn main() {
    -- x.f(a) is f(x, a) where the type of x has no method f
    let k = 5
    println(k.double())
    println("abc".shout())
    println("abc".shout().shout())
    let words = ["a", "abc", "ab"]
    println(words.longest(""))
    println(words.first())
    let nums = [7, 8]
    println(nums.first().double())
    let p = Point { x: 1, y: 2 }
    println(p.scaled(3).sum())
    -- A method comes first
    println(p.sum())
    -- A function's name is a value of the function's type
    println(apply(double, 4))
    let f: fn(int) -> int = double
    println(f(10))
}
