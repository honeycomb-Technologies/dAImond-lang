module r3

fn f() -> string {
    region r {
        return "x" + "y"
    }
    return "never"
}

fn main() {
    println(f())
}
