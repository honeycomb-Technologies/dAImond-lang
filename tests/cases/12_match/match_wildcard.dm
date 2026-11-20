module test

fn classify(n: int) -> int {
    match n {
        0 => { return 0 }
        _ => { return 1 }
    }
    return -1
}

fn main() {
    println(classify(0))
    println(classify(42))
}