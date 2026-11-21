module test

-- None, Some(x), Ok(x) and Err(x) have the type of the place they go
-- to, whatever the function around them returns

fn declared() {
    let r: Result[int, string] = Ok(3)
    let e: Result[int, string] = Err("bad")
    match r {
        Ok(v) => println(v + 1),
        Err(m) => println("err " + m)
    }
    match e {
        Ok(v) => println(v + 1),
        Err(m) => println("err " + m)
    }
    let o: Option[string] = Some("text")
    let n: Option[string] = None
    match o {
        Some(s) => println(s),
        None => println("none")
    }
    match n {
        Some(s) => println(s),
        None => println("none")
    }
}

fn pick(n: int) -> string {
    let o = if n > 0 { Some(n) } else { None }
    match o {
        Some(v) => { return "some " + int_to_string(v) }
        None => { return "none" }
    }
}

fn div(a: int, b: int) -> Result[int, string] {
    if b == 0 { return Err("div by zero") }
    return Ok(a / b)
}
fn find(xs: List[string], s: string) -> Option[int] {
    let mut i = 0
    while i < xs.len() {
        if xs[i] == s { return Some(i) }
        i = i + 1
    }
    return None
}
fn match_values() {
    let a = match div(6, 3) { Ok(v) => "ok " + int_to_string(v), Err(e) => e }
    let b = match div(6, 0) { Ok(v) => "ok " + int_to_string(v), Err(e) => e }
    println(a + ", " + b)
    let names = ["x", "y"]
    let c = match find(names, "y") { Some(i) => "at " + int_to_string(i), None => "missing" }
    let d = match find(names, "z") { Some(i) => "at " + int_to_string(i), None => "missing" }
    println(c + ", " + d)
    let f = match find(names, "y") { Some(i) => 1.5, None => 0.5 }
    println(f)
}

struct Holder { r: Result[int, string], o: Option[string] }
fn show(o: Option[string]) -> string {
    match o {
        Some(s) => { return s }
        None => { return "none" }
    }
}
fn code(r: Result[int, string]) -> string {
    match r {
        Ok(v) => { return int_to_string(v) }
        Err(e) => { return e }
    }
}
fn places() {
    println(show(None) + show(Some("x")))
    println(code(Ok(7)) + code(Err("bad")))
    let h = Holder { r: Ok(1), o: Some("held") }
    println(code(h.r) + show(h.o))
    let g = Holder { r: Err("e"), o: None }
    println(code(g.r) + show(g.o))
    let mut os: List[Option[string]] = []
    os.push(Some("a"))
    os.push(None)
    println(show(os[0]) + show(os[1]))
    let ls: List[Option[string]] = [Some("l"), None]
    println(show(ls[0]) + show(ls[1]))
    let mut slot: Option[string] = None
    slot = Some("set")
    println(show(slot))
    slot = None
    println(show(slot))
}

fn main() -> int {
    declared()
    println(pick(3))
    println(pick(0))
    match_values()
    places()
    return 0
}
