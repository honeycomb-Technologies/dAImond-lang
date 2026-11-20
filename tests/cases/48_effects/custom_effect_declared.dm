module test
fn greet() with [MyEffect, IO, Console] {
    println("hello from custom effect")
}
fn main() with [MyEffect, IO, Console] {
    greet()
}