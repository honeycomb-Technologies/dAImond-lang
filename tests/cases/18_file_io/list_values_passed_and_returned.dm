module test
fn first_two(xs: List[string]) -> string {
    return xs[0] + xs[1]
}
fn make(n: int) -> List[int] {
    let mut out: List[int] = []
    let mut i = 0
    while i < n {
        out.push(i * i)
        i = i + 1
    }
    return out
}
fn main() {
    let mut ws: List[string] = []
    ws.push("a")
    ws.push("b")
    let sq = make(4)
    println(first_two(ws) + " " + int_to_string(sq[3]) + " " + int_to_string(sq.len()))
}