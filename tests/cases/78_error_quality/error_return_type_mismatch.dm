module test

fn name_of(n: int) -> string {
    if n == 0 {
        return "zero"
    }
    return n
}

fn main() -> int {
    println(name_of(1))
    return 0
}
