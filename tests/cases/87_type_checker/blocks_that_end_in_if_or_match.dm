module test

enum Shape { Circle(int), Square(int), Dot }

struct Big { a: int, b: int, c: int, d: int, e: int, f: int }

fn big() -> Big {
    return Big { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6 }
}

fn one() -> int {
    return 1
}

fn text() -> string {
    return "some text that is longer than a number"
}

const LIMIT = comptime { if true { 42 } else { 0 } }

fn grade(n: int) -> string {
    -- The block ends in an `if`: its value is the block's
    let word = {
        let half = n / 2
        if half > 40 { "high" } else if half > 20 { "middle" } else { "low" }
    }
    return word
}

fn area(s: Shape) -> int {
    -- An arm's block ends in an `if`, and the `if`'s in a `match`
    let a = match s {
        Shape.Circle(r) => {
            if r > 10 { 300 } else { 3 * r * r }
        }
        Shape.Square(w) => {
            let twice = w * 2
            match twice {
                0 => 0,
                _ => w * w
            }
        }
        Shape.Dot => 0
    }
    return a
}

fn pick(flag: bool, o: Option[int]) -> Option[int] {
    let chosen = if flag {
        match o {
            Some(v) => Some(v + 1),
            None => None
        }
    } else {
        if one() == 1 { None } else { Some(0) }
    }
    return chosen
}

fn show(o: Option[int]) -> string {
    match o {
        Some(v) => { return "some " + int_to_string(v) }
        None => { return "none" }
    }
    return ""
}

fn first[T](xs: List[T], fallback: T) -> T {
    -- In a generic function too
    let v = {
        if xs.len() > 0 { xs[0] } else { fallback }
    }
    return v
}

fn main() {
    println(LIMIT)
    println(grade(100))
    println(grade(50))
    println(grade(2))
    println(area(Shape.Circle(2)))
    println(area(Shape.Circle(20)))
    println(area(Shape.Square(5)))
    println(area(Shape.Dot))
    println(show(pick(true, Some(4))))
    println(show(pick(true, None)))
    println(show(pick(false, Some(4))))
    println(first([7, 8], 0))
    let none: List[string] = []
    println(first(none, "empty"))

    -- A `match` and an `if` that are statements give no value: their
    -- branches may be of different types, and nothing is kept of them
    let x = 2
    let guard = 77
    match x {
        1 => one(),
        _ => big()
    }
    match x {
        1 => { one() }
        _ => { text() }
    }
    match x {
        2 => { if guard > 0 { big() } else { text() } }
        _ => {}
    }
    {
        if x > 1 { text() } else { one() }
    }
    println(guard)
}
