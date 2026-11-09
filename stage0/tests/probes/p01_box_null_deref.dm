module probe

struct Node {
    value: int,
}

fn main() {
    let b: Box[Node] = Box_null()
    println(int_to_string(b.value))
}
