module alias_uaf

fn main() {
    let mut a: List[int] = []
    a.push(123)
    let b = a
    let mut i = 0
    while i < 10000 {
        a.push(i)
        i = i + 1
    }
    println(int_to_string(b[0]))
}
