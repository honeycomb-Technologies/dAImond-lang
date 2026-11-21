module list_from_another_value

-- A list variable, field or branch that takes its value from
-- another list, a call or a literal holds that list

struct Row { cells: List[int], name: string }

fn mk() -> List[int] { return [5, 6] }

fn copy_len(xs: List[int]) -> int {
    let ys: List[int] = xs
    return ys.len()
}

fn main() -> int {
    let nums = [3, 4, 5]
    let more = [10]

    -- a typed let
    let f: List[int] = mk()
    println(f.len())
    let g: List[int] = nums
    println(g[2])
    let parts: List[string] = string_split("x y z", " ")
    println(parts[1])
    println(copy_len(nums))

    -- an assignment
    let mut h: List[int] = []
    h = [9, 8, 7]
    println(h.len())
    h = mk()
    println(h[0])
    h = nums
    println(h[1])
    let mut u = [1]
    u = [4, 5, 6]
    println(u.len())

    -- a field
    let mut r = Row { cells: [1, 2], name: "r" }
    r.cells = [7, 8, 9]
    println(r.cells.len())
    r.cells = nums
    println(r.cells[0])
    let cells: List[int] = r.cells
    println(cells.len())

    -- the value of an if and of a match
    let pick = if nums.len() > 1 { [1, 2] } else { [3] }
    println(pick.len())
    let other: List[int] = if nums.len() > 5 { nums } else { more }
    println(other[0])
    let v = match nums.len() { 0 => [1], _ => [2, 3] }
    println(v[1])

    -- the empty list still starts empty
    let mut n: List[int] = List_new()
    n.push(1)
    let mut n2: List[int] = []
    n2.push(2)
    n2.push(3)
    println(n.len() + n2.len())
    return 0
}
