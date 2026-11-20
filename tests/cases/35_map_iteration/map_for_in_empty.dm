module test

fn main() {
    let mut m: Map[string, int] = Map_new()
    for k in m {
        println("should not print")
    }
    println("done")
}