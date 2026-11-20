module implicit_generics

fn identity[T](x: T) -> T {
    return x
}

fn max_val[T](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}

fn main() {
    let a = identity(42)
    let b = identity("hello")
    println(int_to_string(a))
    println(b)
    let c = max_val(10, 20)
    println(int_to_string(c))
}
