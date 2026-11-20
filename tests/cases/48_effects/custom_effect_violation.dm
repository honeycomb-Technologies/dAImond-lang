module test
fn impure() with [CustomSideEffect, IO, Console] {
    println("side effect")
}
fn pure_fn() with [IO, Console] -> int {
    impure()
    return 42
}
fn main() with [IO, Console] {
    println(int_to_string(pure_fn()))
}