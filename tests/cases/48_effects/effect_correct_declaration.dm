module test
fn greet(name: string) with [IO, Console] {
    println("Hello " + name)
}
fn main() {
    greet("world")
}