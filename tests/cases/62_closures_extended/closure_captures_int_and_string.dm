fn main() {
    let prefix = "num: "
    let offset = 100
    let f = |x: int| -> string { return prefix + int_to_string(x + offset) }
    println(f(5))
    println(f(10))
}