module test

-- A call of a generic function is compiled for the type arguments the
-- checker finds from its arguments

struct P { x: int, name: string }
struct Pair[A, B] { first: A, second: B }
enum Shape { Circle(float), Rect(int, int), Empty }
fn id[T](x: T) -> T { return x }
fn first[T](xs: List[T]) -> T { return xs[0] }
fn last[T](xs: List[T]) -> T { return xs[xs.len() - 1] }
fn pick[T](c: bool, a: T, b: T) -> T {
    if c { return a }
    return b
}
fn wrap[T](x: T) -> Option[T] { return Some(x) }
fn count[T](xs: List[T]) -> int { return xs.len() }
fn second[A, B](a: A, b: B) -> B { return b }
fn or_else[T](o: Option[T], fallback: T) -> T {
    match o {
        Some(v) => { return v }
        None => { return fallback }
    }
}
fn area(s: Shape) -> float {
    match s {
        Shape.Circle(r) => { return 3.0 * r * r }
        Shape.Rect(w, h) => { return 1.0 }
        Shape.Empty => { return 0.0 }
    }
}
fn main() -> int {
    let nums = [3, 4, 5]
    let words = ["a", "bb", "ccc"]
    let floats = [1.5, 2.5]
    println(first(nums) + last(nums) + count(words))
    println(first(words) + last(words))
    println(first(floats) + last(floats))
    let small: i32 = 7
    println(id(small) + 1)
    println(pick(true, "yes", "no") + pick(false, "yes", "no"))
    println(pick(nums.len() > 2, 10, 20))
    println(second(1, "two") + second("one", "2"))
    println(second("x", 2.5))
    let ps = [P { x: 1, name: "one" }, P { x: 2, name: "two" }]
    println(last(ps).name + int_to_string(first(ps).x + last(ps).x))
    let shapes = [Shape.Circle(1.0), Shape.Rect(2, 3), Shape.Empty]
    println(area(first(shapes)) + area(last(shapes)))
    println(area(id(Shape.Circle(2.0))))
    let pr = Pair[int, string] { first: 1, second: "pair" }
    println(id(pr).second)
    match wrap("w") {
        Some(s) => println(s),
        None => println("none")
    }
    let w = wrap(41)
    println(or_else(w, 0) + 1)
    println(or_else(None, "fallback") + or_else(Some("given"), "fallback"))
    let mut lists: List[List[int]] = []
    lists.push(nums)
    let nine = [9]
    lists.push(nine)
    println(first(lists).len() + last(lists)[0])
    let b = pick(true, Box_new(P { x: 8, name: "boxed" }), Box_null())
    println(b.name)
    let v = if nums.len() > 1 {
        let base = "gr"
        base + "een"
    } else {
        "none"
    }
    println(v)
    return 0
}
