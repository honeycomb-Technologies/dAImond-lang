module test

fn count_mut(mut l: List[int]) -> int {
    return l.len()
}

fn main() {
    let mut l: List[int] = []
    l.push(1)
    l.push(2)
    l.push(3)
    println(len(l))
    println(l.len())
    println(l.pop())
    println(len(l) + l.len())
    println(count_mut(l))
}