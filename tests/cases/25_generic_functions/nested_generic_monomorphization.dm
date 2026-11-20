module test

fn double[T](x: T) -> T {
    return x + x
}

fn apply_double[T](x: T) -> T {
    return double(x)
}

fn main() {
    println(int_to_string(apply_double(5)))
    println(apply_double("ab"))
}