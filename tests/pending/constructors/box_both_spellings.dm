module test

struct Wrapper {
    value: int,
}

fn main() {
    let a: Box[Wrapper] = Box_new(Wrapper { value: 10 })
    let b: Box[Wrapper] = Box.new(Wrapper { value: 32 })
    let n1: Box[Wrapper] = Box_null()
    let n2: Box[Wrapper] = Box.null()
    println(a.value + b.value)
    if n1 == Box.null() and n2 == Box_null() {
        println("both null")
    }
}