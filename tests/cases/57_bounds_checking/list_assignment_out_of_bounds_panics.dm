module test

struct P {
    x: int
}

fn main() {
    let mut ss: List[string] = []
    ss.push("a")
    ss[0] = "b"
    println(ss[0])
    let mut ps: List[P] = []
    ps.push(P { x: 1 })
    ps[0] = P { x: 2 }
    println(int_to_string(ps[0].x))
    ps[1] = P { x: 3 }
    println("not reached")
}