module test

fn main() {
    region mem {
        let s = "hello" + " " + "world"
        println(s)
    }
    println("done")
}