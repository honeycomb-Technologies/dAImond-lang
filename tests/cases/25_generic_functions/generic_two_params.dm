module test

fn first[A, B](a: A, b: B) -> A {
    return a
}

fn main() {
    println(first[int, string](42, "hello"))
}