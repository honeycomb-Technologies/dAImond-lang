module test
struct P {
    x: int,
    name: string
}
fn sum(xs: List[int]) -> int {
    let mut total = 0
    for x in xs {
        total = total + x
    }
    return total
}
fn main() {
    let mut xs: List[int] = []
    let mut i = 0
    while i < 100 {
        xs.push(i * i)
        i = i + 1
    }
    xs[3] = 0 - 7
    let top = xs.pop()
    println(int_to_string(top) + " " + int_to_string(xs.len()) + " " + int_to_string(xs[3]) + " " + int_to_string(xs[98]) + " " + int_to_string(sum(xs)) + " " + bool_to_string(xs.contains(64)) + " " + bool_to_string(xs.contains(65)))
    let mut fs: List[float] = []
    fs.push(1.5)
    fs.push(2.5)
    fs.push(0.25)
    fs[1] = fs[0] + fs[2]
    let f = fs.pop()
    println(float_to_string(fs[1]) + " " + float_to_string(f) + " " + int_to_string(fs.len()) + " " + bool_to_string(fs.contains(1.75)) + " " + bool_to_string(fs.contains(0.25)))
    let mut ss: List[string] = []
    i = 0
    while i < 20 {
        ss.push("s" + int_to_string(i))
        i = i + 1
    }
    ss[0] = "first"
    let last = ss.pop()
    println(ss[0] + " " + ss[18] + " " + last + " " + int_to_string(ss.len()) + " " + bool_to_string(ss.contains("s7")) + " " + bool_to_string(ss.contains("s19")))
    let mut joined = ""
    for s in ss {
        joined = joined + s
    }
    println(joined)
    let mut ps: List[P] = []
    i = 0
    while i < 10 {
        ps.push(P { x: i, name: "p" + int_to_string(i) })
        i = i + 1
    }
    ps[2] = P { x: 22, name: "two" }
    let p = ps.pop()
    println(p.name + int_to_string(p.x) + " " + ps[2].name + int_to_string(ps[2].x) + " " + ps[8].name + " " + int_to_string(ps.len()))
    let mut grid: List[List[int]] = []
    i = 0
    while i < 3 {
        let mut row: List[int] = []
        row.push(i)
        row.push(i * 10)
        grid.push(row)
        i = i + 1
    }
    println(int_to_string(grid[2][1]) + " " + int_to_string(grid.len()) + " " + int_to_string(grid[1].len()))
}