module test

fn check(n: int) -> int {
    match n {
        x if x > 10 => { return 2 }
        x if x > 0 => { return 1 }
        _ => { return 0 }
    }
    return -1
}

fn main() {
    println(check(20))
    println(check(5))
    println(check(-1))
}