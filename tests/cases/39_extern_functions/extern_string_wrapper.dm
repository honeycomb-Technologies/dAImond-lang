module test

extern fn getenv(name: string) -> string

fn main() {
    let mut path: string = ""
    unsafe {
        path = getenv("PATH")
    }
    if len(path) > 0 {
        println("has path")
    } else {
        println("no path")
    }
}