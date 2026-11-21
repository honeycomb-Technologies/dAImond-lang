module test

fn nothing() {
    println("nothing")
}

fn text() -> int {
    return "text"
}

fn truth() -> bool {
    return 1
}

fn result() -> Result[int, string] {
    return Ok("one")
}

fn option() -> Option[string] {
    return Some(1)
}

fn empty() -> int {
    return
}

fn full() {
    return 5
}

fn of_nothing() -> int {
    return nothing()
}

fn some_paths(n: int) -> int {
    if n > 0 { return 1 }
}

fn last_expression() -> int { 42 }

fn loop_with_break(n: int) -> int {
    let mut i = 0
    while i < n {
        if i == 3 { return i }
        i = i + 1
    }
}

fn every_path(n: int) -> int {
    if n > 0 {
        return 1
    } else if n < 0 {
        return -1
    } else {
        panic("zero")
    }
}

fn forever() -> int {
    let mut i = 0
    loop {
        i = i + 1
        if i > 3 { return i }
    }
}

fn main() -> int {
    println(every_path(1) + forever())
}
