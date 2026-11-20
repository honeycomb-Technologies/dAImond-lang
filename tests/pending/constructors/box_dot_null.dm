module test

struct Wrapper {
    value: int,
}

fn main() {
    let b: Box[Wrapper] = Box.null()
    if b == Box.null() {
        println("null")
    }
    let c: Box[Wrapper] = Box.new(Wrapper { value: 1 })
    if c != Box.null() {
        println("not null")
    }
}