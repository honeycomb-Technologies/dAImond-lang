module test

struct P {
    x: int,
    name: string
}

fn rename(ss: List[string]) -> string {
    let mut q = ss
    q[0] = "renamed"
    return "!"
}

fn bump(ps: List[P]) -> int {
    let mut q = ps
    q[0] = P { x: 99, name: "bumped" }
    return 1
}

fn main() {
    let mut ss: List[string] = []
    ss.push("a")
    ss.push("b")
    let t = ss[1]
    ss[1] = "z"
    println(t + " " + ss[1])
    println(ss[0] + rename(ss) + ss[0])

    let mut ps: List[P] = []
    ps.push(P { x: 2, name: "two" })
    let p = ps[0]
    println(int_to_string(ps[0].x + bump(ps) + ps[0].x))
    println(p.name + " " + ps[0].name)

    let mut grid: List[List[int]] = []
    let mut row: List[int] = []
    row.push(1)
    grid.push(row)
    grid.push(row)
    let r0 = grid[0]
    grid[0].push(2)
    grid[1] = grid[0]
    grid[1][0] = 8
    println(int_to_string(r0.len()) + " " + int_to_string(grid[0].len()) + " " + int_to_string(grid[1].len()) + " " + int_to_string(grid[0][0]))

    let mut ns: List[i32] = []
    let a: i32 = 7
    ns.push(a)
    ns.push(a)
    ns.push(a)
    ns[1] = 5
    ns[0] = a + a
    println(int_to_string(ns[0] as int) + " " + int_to_string(ns[1] as int) + " " + int_to_string(ns[2] as int))
}