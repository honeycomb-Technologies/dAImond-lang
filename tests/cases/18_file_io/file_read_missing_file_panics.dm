module test

fn main() {
    println("before")
    let s = file_read(args_get(0) + ".missing")
    println("not reached")
}