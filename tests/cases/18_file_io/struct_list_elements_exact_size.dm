module test
struct Rec { name: string, a: List[int], b: List[string], n: int }
fn main() {
    let before = 11
    let mut rs: List[Rec] = []
    let mut r = Rec { name: "r0", a: [], b: [], n: 1 }
    r.a.push(10)
    r.b.push("x")
    rs.push(r)
    rs.push(Rec { name: "r1", a: [], b: [], n: 2 })
    let after = 22
    let first = rs[0]
    rs[1] = Rec { name: "r2", a: [], b: [], n: 3 }
    let mut total = 0
    for e in rs {
        total = total + e.n
    }
    println(first.name + " " + int_to_string(first.a[0]) + " " + first.b[0])
    println(rs[1].name)
    println(int_to_string(total))
    println(int_to_string(before) + " " + int_to_string(after))
}