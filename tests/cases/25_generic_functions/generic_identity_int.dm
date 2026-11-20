module test

fn identity[T](x: T) -> T {
    return x
}

fn main() {
    println(identity[int](42))
}