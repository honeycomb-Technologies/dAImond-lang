module try_option

-- `value?` on an Option is the value of a Some; a None leaves the
-- function, which returns None. The function may return an Option of
-- another type than the one `?` is used on.

struct Point {
    x: int,
    name: string
}

fn doubled(n: int) -> Option[int] {
    if n > 0 { return Some(n * 2) }
    return None
}

fn point(n: int) -> Option[Point] {
    if n > 0 { return Some(Point { x: n, name: "p" + int_to_string(n) }) }
    return None
}

fn described(n: int) -> Option[string] {
    let v = doubled(n)?
    return Some("got " + int_to_string(v))
}

fn measured(n: int) -> Option[int] {
    let p = point(n)?
    return Some(p.x + len(p.name))
}

fn same_type(n: int) -> Option[int] {
    let v = doubled(n)?
    let w = doubled(v - 10)?
    return Some(v + w)
}

fn named(n: int) -> Option[Point] {
    let p = point(n)?
    let q = point(p.x - 1)?
    return Some(Point { x: p.x + q.x, name: p.name + q.name })
}

fn in_expression(n: int) -> Option[int] {
    return Some(doubled(n)? + doubled(n + 1)? * 10)
}

fn show(o: Option[string]) -> string {
    match o {
        Some(s) => { return s }
        None => { return "none" }
    }
}

fn show_int(o: Option[int]) -> string {
    match o {
        Some(n) => { return int_to_string(n) }
        None => { return "none" }
    }
}

fn main() {
    println(show(described(4)))
    println(show(described(0)))
    println(show_int(measured(5)))
    println(show_int(measured(0)))
    println(show_int(same_type(6)))
    println(show_int(same_type(5)))
    println(show_int(same_type(-1)))
    match named(3) {
        Some(p) => println(p.name + " " + int_to_string(p.x)),
        None => println("none")
    }
    match named(1) {
        Some(p) => println(p.name),
        None => println("none")
    }
    println(show_int(in_expression(1)))
    println(show_int(in_expression(0)))
    println(show_int(in_expression(-1)))

    -- In a lambda, which returns an Option itself
    let halved = |n: int| -> Option[int] {
        let d = doubled(n)?
        return Some(d / 4)
    }
    println(show_int(halved(8)) + " " + show_int(halved(-8)))
}
