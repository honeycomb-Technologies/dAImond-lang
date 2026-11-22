module test

fn one() -> int {
    return 1
}

fn main() -> int {
    let c = one() == 1
    let a = {
        if c { 1 } else { "one" }
    }
    let b = {
        if c { 1 }
    }
    let d: string = {
        match one() {
            1 => 10,
            _ => 20
        }
    }
    let e = {
        if c { println("yes") } else { println("no") }
    }
    let f = {
        let g = 1
    }
    return 0
}
