fn main() {
    region outer {
        println("outer start")
        region inner {
            println("inner")
        }
        println("outer end")
    }
    println("done")
}