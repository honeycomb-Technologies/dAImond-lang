module push_and_assign_lists

-- A list pushed onto a list of lists, assigned to one of its
-- elements or held in a field, a map or an Option is stored whole

struct Board { rows: List[List[int]], name: string }

fn mk() -> List[int] { return [5, 6] }

fn fill(mut g: List[List[int]]) {
    g.push([1, 2, 3])
    g.push([])
}

fn first_row(g: List[List[int]]) -> List[int] {
    let row: List[int] = g[0]
    return row
}

fn main() -> int {
    let nums = [3, 4, 5]
    let more = [10]

    -- push: a literal, a variable, a call's result and the empty list
    let mut e: List[List[int]] = []
    e.push([7, 8])
    e.push(nums)
    e.push(mk())
    e.push([])
    println(e.len())
    println(e[0][1] + e[1][0] + e[2][1] + e[3].len())

    -- an element assigned, and an element of an element
    e[3] = [100, 200]
    e[0] = more
    println(e[3][1] + e[0][0])
    e[3][1] = 50
    println(e[3][1])

    -- pop
    let last = e.pop()
    println(last.len() + last[0])
    println(e.len())

    -- through a mut parameter
    let mut g: List[List[int]] = []
    fill(g)
    println(g.len())
    println(g[0][2])
    println(first_row(g).len())

    -- in a loop
    let mut i = 0
    let mut acc: List[List[int]] = []
    while i < 1000 {
        acc.push([i, i + 1])
        i = i + 1
    }
    println(acc[999][1] + acc[0][0] + acc.len())

    -- a field
    let mut bd = Board { rows: [[1], [2, 3]], name: "b" }
    println(bd.rows[1][1])
    bd.rows.push([4, 5, 6])
    println(bd.rows.len())
    bd.rows[0].push(9)
    println(bd.rows[0].len())
    bd.rows = [[7]]
    println(bd.rows[0][0])

    -- a map and an Option
    let mut m: Map[string, List[List[int]]] = Map_new()
    m.insert("a", [[1, 2], [3]])
    m["b"] = [nums, more]
    println(m.get("a")[0][1] + m["b"][1][0])
    let o: Option[List[List[int]]] = Some([[1], [2, 3]])
    match o {
        Some(xs) => println(xs[1].len()),
        None => println("none")
    }
    return 0
}
