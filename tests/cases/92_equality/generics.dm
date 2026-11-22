module generics

-- `==` in a generic function compares what the type argument is

struct Point {
    x: int,
    y: int
}

struct Pair[A, B] {
    first: A,
    second: B
}

enum Opt[T] {
    Some(T),
    Nothing
}

fn same[T](a: T, b: T) -> bool {
    return a == b
}

fn differ[T](a: T, b: T) -> bool {
    return a != b
}

fn count[T](xs: List[T], v: T) -> int {
    let mut n = 0
    for x in xs {
        if x == v { n = n + 1 }
    }
    return n
}

fn show(what: string, b: bool) {
    println(what + ": " + bool_to_string(b))
}

fn main() {
    show("ints", same(1, 1))
    show("strings", same("a" + "b", "ab"))
    show("structs", same(Point { x: 1, y: 2 }, Point { x: 1, y: 2 }))
    show("other structs", same(Point { x: 1, y: 2 }, Point { x: 2, y: 2 }))
    show("lists", same([1, 2], [1, 2]))
    show("other lists", differ([1, 2], [1, 3]))
    let o: Option[int] = Some(1)
    show("options", same(o, Some(1)))
    println(int_to_string(count([Point { x: 1, y: 1 }, Point { x: 0, y: 0 }, Point { x: 1, y: 1 }], Point { x: 1, y: 1 })))
    println(int_to_string(count(["a", "b", "a"], "a")))

    -- Instances of generic structs and enums
    let p = Pair[int, string] { first: 1, second: "one" }
    let q = Pair[int, string] { first: 1, second: "o" + "ne" }
    let r = Pair[int, string] { first: 2, second: "one" }
    show("pairs", p == q)
    show("other pairs", p == r)
    show("pairs through T", same(p, q))
    let a: Opt[string] = Opt.Some("x")
    let b: Opt[string] = Opt.Some("x")
    let c: Opt[string] = Opt.Nothing
    show("generic enums", a == b)
    show("other generic enums", a == c)
    show("generic enums differ", a != c)
}
