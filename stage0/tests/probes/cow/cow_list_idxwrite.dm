module e1

fn main() {
    let mut a: List[int] = []
    a.push(1)
    a.push(2)
    let b = a
    a[0] = 99
    println(int_to_string(b[0]))
    println(int_to_string(a[0]))
    println(int_to_string(len(b)))
}
