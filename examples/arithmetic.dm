-- Basic arithmetic test
module arithmetic

fn add(a: int, b: int) -> int {
    return a + b
}

fn main() {
    let x = 10
    let y = 20
    let sum = add(x, y)
    println("Sum: " + int_to_string(sum))
}
