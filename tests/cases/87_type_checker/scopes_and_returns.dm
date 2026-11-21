module test

const LIMIT: int = 3
const NAME = "dm"
fn pick(n: int) -> string {
    if LIMIT < n { return "big" } else { return "small" }
}
fn spin(n: int) -> int {
    let mut i = 0
    loop {
        if i >= n { return i }
        i = i + 1
    }
}
fn forever(n: int) -> int {
    let mut i = n
    while true {
        i = i + 1
        if i > 10 { return i }
    }
}
fn must(n: int) -> int {
    if n > 0 { return n }
    panic("not positive")
}
fn by_match(n: int) -> string {
    match n {
        0 => { return "zero" }
        _ => { return "many" }
    }
}
fn bump(mut n: int) -> int {
    n = n + 1
    return n
}
fn nothing() {
    return
}
fn main() -> int {
    let x = 1
    if x == 1 {
        let x = "inner"
        println(x)
    }
    let x = "shadow"
    println(x + NAME)
    println(pick(5) + pick(1))
    println(spin(4) + forever(2) + must(3) + bump(1))
    println(by_match(0))
    nothing()
    for i in 0..3 {
        let sq = i * i
        print(int_to_string(sq) + " ")
    }
    println("")
    let v = if x == "shadow" { 10 } else { 20 }
    let w = match v { 10 => "ten", _ => "other" }
    println(w)
    let mut total: float = 0.0
    total += 1.5
    total = total * 2.0
    println(total)
    let small: i32 = 7
    let big: int = small + 1
    let u: u8 = 200 as u8
    println(big + (u as int))
    return 0
}
