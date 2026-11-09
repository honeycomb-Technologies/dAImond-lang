module e2

fn mutate(xs: List[int]) -> int {
    let mut m = xs
    m.push(777)
    m[0] = 555
    return len(m)
}

fn main() {
    let mut a: List[int] = []
    a.push(10)
    a.push(20)
    let n = mutate(a)
    println(int_to_string(n))
    println(int_to_string(len(a)))
    println(int_to_string(a[0]))
}
