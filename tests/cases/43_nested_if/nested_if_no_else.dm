module test

fn check(x: int) -> string {
    if x > 10 {
        if x > 20 {
            return "big"
        }
    }
    return "small"
}

fn main() {
    println(check(25))
    println(check(15))
    println(check(5))
}