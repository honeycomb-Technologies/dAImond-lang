module test

fn log(s: string) {
    println(s)
}

fn sign(n: int) -> string {
    let s = if n > 0 { "positive" }
    return s
}

fn label(n: int, loud: bool) -> string {
    let s = match n {
        0 => { if loud { "ZERO" } else { "zero" } }
        1 => {
            let one = "one"
        }
        2 => { }
        3 => log("three"),
        4 => { panic("four") }
        5 => {
            return "five"
        }
        _ => "many"
    }
    return s
}

fn nested(a: bool, b: bool) -> int {
    return if a {
        match b {
            true => 1,
            false => { log("false") }
        }
    } else {
        2
    }
}

fn used(n: int) -> int {
    let mut total = 0
    while total < 10 {
        let step = if n > 0 { n } else { break }
        total = total + step
    }
    -- An `if` whose value is not used needs no `else`
    match n {
        1 => if total > 5 { log("big") },
        _ => log("other")
    }
    println(sign(if n > 1 { 1 }))
    return total
}

fn main() -> int {
    println(sign(1) + label(0, true))
    return nested(true, true) + used(1)
}
