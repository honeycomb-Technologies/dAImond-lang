module list_copies

-- A copy of a list uses the elements of the original, so a change to
-- an element both have is seen through both. A push never changes
-- another list: the list that pushes where a copy already has an
-- element moves to elements of its own first.

struct Point {
    x: int,
    y: int
}

fn grown(xs: List[int]) -> int {
    let mut mine = xs
    mine.push(99)
    return mine.len()
}

fn show(xs: List[int]) -> string {
    let mut text = ""
    for x in xs {
        text = text + int_to_string(x) + " "
    }
    return text
}

fn main() {
    -- An element both have
    let mut a = [1, 2, 3]
    let mut b = a
    b[0] = 9
    println(int_to_string(a[0]))

    -- Each has its own pushes
    b.push(4)
    a.push(5)
    println(show(a) + "| " + show(b))

    -- They are apart now
    a[1] = 70
    b[2] = 30
    println(show(a) + "| " + show(b))

    -- Two lists that begin as one
    let base = [1, 2]
    let mut with_ten = base
    with_ten.push(10)
    let mut with_twenty = base
    with_twenty.push(20)
    println(show(base) + "| " + show(with_ten) + "| " + show(with_twenty))

    -- A function that pushes onto its parameter's copy
    let mut c: List[int] = []
    let mut i = 0
    while i < 5 {
        c.push(i)
        i = i + 1
    }
    println(int_to_string(grown(c)) + " " + int_to_string(c.len()))
    c.push(300)
    println(show(c))

    -- A copy that is as long as the room the list has
    let mut full: List[int] = []
    i = 0
    while i < 8 {
        full.push(i)
        i = i + 1
    }
    let mut other = full
    other.push(80)
    full.push(81)
    println(int_to_string(full[8]) + " " + int_to_string(other[8]) + " " + int_to_string(full.len()))

    -- A pop, then pushes
    let mut stack = [1, 2, 3]
    let kept = stack
    stack.pop()
    stack.pop()
    stack.push(7)
    stack.push(8)
    stack.push(9)
    println(show(stack) + "| " + int_to_string(kept.len()) + " " + int_to_string(kept[0]))

    -- Strings
    let words = ["a", "b"]
    let mut more = words
    more.push("c")
    let mut others = words
    others.push("d")
    println(more[2] + others[2] + int_to_string(words.len()))

    -- Structs
    let points = [Point { x: 1, y: 2 }]
    let mut left = points
    left.push(Point { x: 3, y: 4 })
    let mut right = points
    right.push(Point { x: 5, y: 6 })
    println(int_to_string(left[1].x) + " " + int_to_string(right[1].y) + " " + int_to_string(points.len()))

    -- Floats
    let halves = [0.5]
    let mut up = halves
    up.push(1.5)
    let mut down = halves
    down.push(2.5)
    println(float_to_string(up[1]) + " " + float_to_string(down[1]))

    -- A list in a struct's field, and lists in a list
    let rows = [[1, 2], [3]]
    let mut first = rows[0]
    first.push(4)
    let mut again = rows[0]
    again.push(5)
    println(show(first) + "| " + show(again) + "| " + int_to_string(rows[0].len()))
}
