fn main() {
    let mut inner: List[int] = []
    inner.push(42)
    let mut outer: List[List[int]] = []
    outer.push(inner)
    let mut i = 0
    while i < 10000 {
        inner.push(i)
        i = i + 1
    }
    println(int_to_string(len(outer[0])))
    println(int_to_string(outer[0][0]))
    println(int_to_string(len(inner)))
}
