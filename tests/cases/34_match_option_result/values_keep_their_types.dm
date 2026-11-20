module main

-- What Ok, Err and Some carry keeps its type in the arm, and a call
-- can be matched without a variable for its result

struct Point {
    x: int,
    y: int
}

fn parse(text: string) -> Result[Point, string] {
    if len(text) == 0 {
        return Err("empty")
    }
    return Ok(Point { x: len(text), y: 2 })
}

fn firsts(xs: List[string]) -> Option[List[string]] {
    if xs.len() == 0 {
        return None
    }
    let mut out: List[string] = []
    out.push(xs[0])
    return Some(out)
}

fn half(n: int) -> Option[float] {
    if n == 0 {
        return None
    }
    return Some(0.5)
}

fn ratio(a: int, b: int) -> Result[float, string] {
    if b == 0 {
        return Err("no ratio")
    }
    return Ok(1.5)
}

fn main() -> int {
    match parse("abc") {
        Ok(p) => println("ok " + int_to_string(p.x + p.y)),
        Err(e) => println("err " + e)
    }
    match parse("") {
        Ok(p) => println("ok " + int_to_string(p.x + p.y)),
        Err(e) => println("err " + e + "!")
    }
    let words = ["a", "b"]
    match firsts(words) {
        Some(got) => println("some " + int_to_string(got.len()) + got[0]),
        None => println("none")
    }
    let unannotated = parse("four")
    match unannotated {
        Ok(p) => println("ok " + int_to_string(p.x)),
        Err(e) => println("err " + e)
    }
    match half(3) {
        Some(h) => println(float_to_string(h * 3.0)),
        None => println("none")
    }
    match ratio(3, 2) {
        Ok(r) => println(float_to_string(r + 1.0)),
        Err(e) => println(e)
    }
    match ratio(3, 0) {
        Ok(r) => println(float_to_string(r + 1.0)),
        Err(e) => println(e)
    }
    return 0
}
