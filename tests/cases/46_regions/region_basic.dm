module test

fn main() {
    region r {
        let x = 42
        println(int_to_string(x))
    }
    println("after region")
}