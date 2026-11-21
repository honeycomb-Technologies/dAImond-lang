module list_literals_of_lists

-- A list literal whose elements are lists stores each element's
-- header whole, whether the element is a literal, a variable or a
-- call's result

struct P { x: int, y: int }

fn mk() -> List[int] { return [5, 6] }

fn total(xs: List[int]) -> int {
    let mut t = 0
    for x in xs { t = t + x }
    return t
}

fn grid_total(g: List[List[int]]) -> int {
    let mut t = 0
    for row in g { t = t + total(row) }
    return t
}

fn make_grid() -> List[List[int]] {
    return [[1, 2], [3, 4]]
}

fn main() -> int {
    let nums = [3, 4, 5]
    let more = [10]

    -- literals of literals, without and with the type
    let a = [[1, 2], [3]]
    println(a.len())
    println(a[0].len())
    println(a[1][0])
    let b: List[List[int]] = [[1, 2, 3], [4], []]
    println(b[0][2] + b[1][0] + b[2].len())
    let z: List[List[int]] = [[], [1]]
    println(z[0].len() + z[1].len())

    -- literals of variables and of calls
    let c = [nums, more]
    println(c[0][2] + c[1][0])
    let d = [mk(), mk(), nums]
    println(d.len() + d[1][1] + d[2][0])

    -- strings, floats and structs inside
    let w = [["a", "b"], ["c"]]
    println(w[0][1] + w[1][0])
    let fl = [[1.5, 2.5], [3.0]]
    println(fl[0][1] + fl[1][0])
    let ps = [[P { x: 1, y: 2 }], [P { x: 3, y: 4 }, P { x: 5, y: 6 }]]
    println(ps[1][1].y + ps[0][0].x)
    let sp = [string_split("a,b,c", ","), string_split("d", ",")]
    println(sp[0].len() + sp[1].len())
    println(sp[0][2])

    -- three levels
    let deep = [[[1], [2, 3]], [[4, 5, 6]]]
    println(deep[0][1][1] + deep[1][0][2])
    println(deep[0].len() + deep[1].len())

    -- loops, arguments and return values
    let mut sum = 0
    for row in a {
        for v in row { sum = sum + v }
    }
    println(sum)
    let mut rows = 0
    for row in [[1, 2], [3]] {
        rows = rows + row.len()
    }
    println(rows)
    println(grid_total(a))
    println(grid_total([[1], [2, 3]]))
    println(grid_total(make_grid()))

    -- an element taken out
    let row = a[0]
    println(row.len() + row[1])
    return 0
}
