module test

struct Node {
    value: int,
}

fn main() {
    let b: Box[Node] = Box_null()
    if b == Box_null() {
        println("null")
    }
    let c: Box[Node] = Box_new(Node { value: 42 })
    if c != Box_null() {
        println("not null")
    }
}