module test

fn main() -> int {
    let k = 2
    let shout = |x: int| -> int "loud"
    let scale = |x: int| -> int x * k
    let label = |x: Missing| -> string "x"
    println(scale(3))
    println(scale(k) + "x")
    return 0
}
