module test

struct ConsData {
    value: int,
    next: Box[ConsData],
}

fn sum_list(node: Box[ConsData]) -> int {
    if node == Box_null() {
        return 0
    }
    return node.value + sum_list(node.next)
}

fn main() {
    let c: Box[ConsData] = Box_new(ConsData { value: 3, next: Box_null() })
    let b: Box[ConsData] = Box_new(ConsData { value: 2, next: c })
    let a: Box[ConsData] = Box_new(ConsData { value: 1, next: b })
    println(sum_list(a))
}