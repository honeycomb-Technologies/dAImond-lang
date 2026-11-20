module test

fn describe(n: int) -> int {
    match n {
        1 => { return 10 }
        2 => { return 20 }
        _ => { return 0 }
    }
    return -1
}

fn main() {
    println(describe(1))
    println(describe(2))
    println(describe(99))
}