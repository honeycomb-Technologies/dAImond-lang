module test

fn main() {
    let mut a: List[int] = []
    a.push(1)
    a[0] = 5
    let mut outer: List[List[int]] = []
    let mut inner: List[int] = []
    inner.push(7)
    outer.push(inner)
    println(outer[0][0])
}