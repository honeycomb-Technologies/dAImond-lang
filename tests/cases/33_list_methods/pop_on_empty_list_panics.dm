module test
struct P {
    x: int
}
fn main() {
    let mut ss: List[string] = []
    ss.push("a")
    println(ss.pop())
    let mut ps: List[P] = []
    ps.push(P { x: 4 })
    let p = ps.pop()
    println(int_to_string(p.x))
    println(int_to_string(ss.len() + ps.len()))
    let q = ps.pop()
    println("not reached")
}