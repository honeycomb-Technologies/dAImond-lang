module test

-- The value of a match has the type the checker found for it, also
-- where a name its arms bind was bound with another type before

fn small() -> Option[i32] {
    let n: i32 = 7
    return Some(n)
}
fn lit() -> Option[i32] {
    return Some(5)
}
fn fl() -> Result[float, string] {
    return Ok(2.5)
}
fn f32r() -> Option[f32] {
    let x: f32 = 1.5 as f32
    return Some(x)
}
fn widths() {
    let o: Option[i32] = Some(5)
    match o {
        Some(v) => println(v as int),
        None => println("none")
    }
    match small() {
        Some(v) => println(v as int + 1),
        None => println("none")
    }
    match lit() {
        Some(v) => println(v as int + 2),
        None => println("none")
    }
    let u: Option[u8] = Some(200 as u8)
    match u {
        Some(v) => println(v as int),
        None => println("none")
    }
    match fl() {
        Ok(v) => println(v),
        Err(e) => println(e)
    }
    let b: Option[bool] = Some(true)
    match b {
        Some(v) => println(v),
        None => println("none")
    }
    let w: Option[int] = Some(3)
    let big = match w { Some(v) => v * 2, None => 0 }
    println(big)
}

fn names_bound_twice() {
    let b: Option[bool] = Some(true)
    match b {
        Some(v) => println(v),
        None => println("none")
    }
    let w: Option[int] = Some(3)
    let big = match w { Some(v) => v * 2, None => 0 }
    println(big)
    let s: Option[string] = Some("text")
    let t = match s { Some(v) => v + "!", None => "" }
    println(t)
    let u = if big > 1 {
        let v = 2.5
        v * 2.0
    } else {
        0.5
    }
    println(u)
}

fn opt(n: int) -> Option[int] {
    if n > 0 { return Some(n) }
    return None
}
fn flag() -> Option[bool] { return Some(true) }
fn names_bound_twice_from_calls() {
    match flag() {
        Some(v) => println(v),
        None => println("none")
    }
    let big = match opt(3) { Some(v) => v * 2, None => 0 }
    println(big)
}

fn main() -> int {
    widths()
    names_bound_twice()
    names_bound_twice_from_calls()
    return 0
}
