module test

const S = comptime "hello" + " " + "world"

fn main() {
    println(S)
}