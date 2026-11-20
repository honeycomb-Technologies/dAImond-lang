module test

fn main() {
    let mut outer: List[List[int]] = []
    let mut inner: List[int] = []
    inner.push(7)
    outer.push(inner)
    let mut b: List[int] = []
    b.push(1)
    b[0] = 9
    println(outer[0][0])
}