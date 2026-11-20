module test

fn main() {
    let greeting = "Hello"
    let f = |name: string| -> string greeting + " " + name
    println(f("World"))
}