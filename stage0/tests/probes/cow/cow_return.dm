module e5

fn give(xs: List[int]) -> List[int] {
    return xs
}

fn main() {
    let mut a: List[int] = []
    a.push(7)
    let mut b = give(a)
    b.push(8)
    b[0] = 70
    println(int_to_string(len(a)))
    println(int_to_string(a[0]))
    println(int_to_string(len(b)))
    println(int_to_string(b[0]))
}
