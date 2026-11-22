module test

struct P {
    name: string
}

fn nothing() {
}

fn main() {
    let p = P { name: "ann" }
    println(f"{p}")
    println(f"{nothing()}")
    println(f"{[1, 2]}")
}
