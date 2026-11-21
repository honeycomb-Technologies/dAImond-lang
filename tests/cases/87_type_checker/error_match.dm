module test

enum Color { Red, Green, Blue(int) }

fn name(c: Color) -> string {
    match c {
        Color.Red => { return "red" }
        Color.Green => { return "green" }
    }
    return "?"
}

fn value(o: Option[int]) -> int {
    let v = match o { Some(x) => x }
    return v
}

fn status(r: Result[int, string]) -> int {
    match r {
        Ok(v) if v > 0 => { return 1 }
        Err(e) => { return 2 }
    }
    return 0
}

fn word(b: bool) -> string {
    return match b { true => "yes" }
}

fn small(n: int) -> string {
    return match n { 1 => "one", 2 => "two" }
}

fn mixed(n: int) -> string {
    let v = match n { 1 => "one", _ => 2 }
    let w = if n > 0 { "positive" } else { 0 }
    return v
}

fn shade(c: Color) -> string {
    match c {
        Color.Blue(n) if n > 5 => { return "deep blue" }
        Color.Blue(n) => { return "blue" }
        _ => { return "other" }
    }
}

fn main() -> int {
    let n = 3
    match n {
        "three" => println("three"),
        _ => println("other")
    }
    match "text" {
        1 => println("one"),
        true => println("true"),
        _ => println("other")
    }
    -- A match that gives no value may leave numbers and strings out
    match n {
        1 => println("one"),
        2 => println("two")
    }
    println(name(Color.Blue(1)) + small(n) + mixed(n) + word(true))
    return value(None) + status(Ok(1))
}
