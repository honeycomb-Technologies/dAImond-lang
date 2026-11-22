module test

enum Shape {
    Circle(int),
    Rect(int, int),
    Empty
}

struct Reading { name: string, value: Option[int] }

fn area(s: Shape) -> int {
    if let Shape.Circle(r) = s {
        return 3 * r * r
    } else if let Shape.Rect(w, h) = s {
        return w * h
    }
    return 0
}

fn describe(o: Option[int]) -> string {
    -- As a value, with both branches
    let text = if let Some(n) = o { "some " + int_to_string(n) } else { "none" }
    return text
}

fn parse(s: string) -> Result[int, string] {
    if s == "" { return Err("empty") }
    return Ok(parse_int(s))
}

fn first_even(xs: List[int]) -> Option[int] {
    for x in xs {
        if x % 2 == 0 { return Some(x) }
    }
    return None
}

fn chain(a: Option[int], b: Result[int, string], flag: bool) -> string {
    if flag {
        return "flag"
    } else if let Some(x) = a {
        return "a " + int_to_string(x)
    } else if let Ok(y) = b {
        return "b " + int_to_string(y)
    } else if let Err(e) = b {
        return "err " + e
    } else {
        return "nothing"
    }
}

fn main() {
    println(int_to_string(area(Shape.Circle(2))))
    println(int_to_string(area(Shape.Rect(3, 4))))
    println(int_to_string(area(Shape.Empty)))
    println(describe(Some(5)))
    println(describe(None))

    -- Without an else, as a statement
    let mut total = 0
    if let Some(n) = first_even([1, 3, 6, 8]) {
        total = total + n
    }
    if let Some(n) = first_even([1, 3]) {
        total = total + 100 * n
    }
    println(int_to_string(total))

    -- A Result, and the error's value
    if let Ok(v) = parse("42") { println("ok " + int_to_string(v)) }
    if let Err(e) = parse("") { println("err " + e) } else { println("no error") }
    if let Err(e) = parse("7") { println("err " + e) } else { println("no error") }

    -- A literal among a variant's values is a test
    let s = Shape.Rect(1, 9)
    if let Shape.Rect(1, h) = s { println("unit width, height " + int_to_string(h)) }
    if let Shape.Rect(2, h) = s { println("wrong " + int_to_string(h)) } else { println("not width 2") }

    -- A literal pattern, and a name, which always matches
    let n = 3
    if let 3 = n { println("three") }
    if let 4 = n { println("four") } else { println("not four") }
    if let m = n + 1 { println("m " + int_to_string(m)) }

    -- Mixed with plain ifs
    println(chain(None, Ok(1), true))
    println(chain(Some(2), Ok(1), false))
    println(chain(None, Ok(3), false))
    println(chain(None, Err("bad"), false))

    -- In a loop, with break, and on a field
    let readings = [Reading { name: "a", value: Some(1) }, Reading { name: "b", value: None }, Reading { name: "c", value: Some(30) }]
    let mut sum = 0
    for r in readings {
        if let Some(v) = r.value {
            if v > 10 { break }
            sum = sum + v
        } else {
            println("no value for " + r.name)
        }
    }
    println(int_to_string(sum))

    -- Nested, and as the value of a function's last expression
    let nested: Option[Option[int]] = Some(Some(9))
    if let Some(inner) = nested {
        if let Some(k) = inner { println("inner " + int_to_string(k)) }
    }
    let mut i = 0
    let mut q: List[Option[int]] = [Some(1), Some(2), None, Some(4)]
    while i < q.len() {
        if let Some(v) = q[i] { print(int_to_string(v) + " ") } else { print("- ") }
        i = i + 1
    }
    println("")
}
