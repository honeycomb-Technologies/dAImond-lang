fn main() {
    let mut outer: List[List[int]] = []
    let mut inner1: List[int] = []
    inner1.push(1)
    inner1.push(2)
    let mut inner2: List[int] = []
    inner2.push(3)
    inner2.push(4)
    outer.push(inner1)
    outer.push(inner2)
    println(int_to_string(outer[0][0]))
    println(int_to_string(outer[1][1]))
}