module probe

struct Node {
    value: int,
}

fn main() {
    let fake = 12345 as Box[Node]
    println(int_to_string(fake.value))
}
