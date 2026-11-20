module test

fn identity[T](x: T) -> T {
    return x
}

fn main() {
    let a = identity(42)
    let b = identity("hello")
    println(int_to_string(a))
    println(b)
}