module containers_in_a_box

-- A list, a map or a string in a box is used as itself: b.len() is
-- (*b).len() and b[i] is (*b)[i], and a change is made in the box

struct P { x: int, y: int }
struct W { items: Box[List[string]], n: int }
struct Node { vals: List[int], next: Box[Node] }

fn sum(b: Box[List[int]]) -> int {
    let mut t = 0
    let mut i = 0
    while i < b.len() {
        t = t + b[i]
        i = i + 1
    }
    return t
}

fn main() -> int {
    -- methods
    let b = Box_new([1, 2])
    println(b.len())
    println((*b).len())
    let mut c: Box[List[int]] = Box_new([4, 5, 6])
    c.push(7)
    println(c.len())
    println(c.contains(7))
    println(c.contains(9))
    println(c.pop())
    println(c.len())

    -- indexing
    println(c[0] + c[2])
    c[1] = 50
    println(c[1])
    println(sum(c))

    -- the box is shared, so its list is too
    let d = c
    d.push(100)
    println(c.len())
    println(d[3])

    -- strings, structs and lists inside
    let mut names: Box[List[string]] = Box_new(["a", "b"])
    names.push("c")
    println(names[2] + names[0])
    let ps = Box_new([P { x: 1, y: 2 }, P { x: 3, y: 4 }])
    println(ps[1].y + ps.len())
    let mut grid: Box[List[List[int]]] = Box_new([[1], [2, 3]])
    grid.push([4, 5, 6])
    println(grid[2][1] + grid[1].len() + grid.len())

    -- in a field
    let mut w = W { items: Box_new(["x", "y"]), n: 0 }
    w.items.push("z")
    println(w.items.len())
    println(w.items[2])
    w.items[0] = "q"
    println(w.items[0])

    -- through another box
    let tail = Box_new(Node { vals: [7, 8, 9], next: Box_null() })
    let head = Node { vals: [1], next: tail }
    println(head.next.vals.len() + head.next.vals[2])

    -- a string and a map in a box
    let s = Box_new("hello")
    println(s.len())
    println(s[1])
    let mut m: Map[string, int] = Map_new()
    m.insert("a", 1)
    let mut bm = Box_new(m)
    bm.insert("b", 2)
    bm["c"] = 3
    println(bm["b"] + bm.get("c") + bm.len())

    -- the list taken out of the box
    let ys: List[int] = *c
    println(ys.len())
    let mut total = 0
    for v in *c { total = total + v }
    println(total)
    return 0
}
