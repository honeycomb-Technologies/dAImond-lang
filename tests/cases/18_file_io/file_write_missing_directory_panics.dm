module test

fn main() {
    println("before")
    file_write(args_get(0) + ".missing/x", "x")
    println("not reached")
}