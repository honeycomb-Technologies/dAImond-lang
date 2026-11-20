module test_for_types

-- Test for-in with different list element types (Phase 1.2)

struct Point {
    x: int,
    y: int
}

fn main() {
    -- Test List[string] iteration
    let mut names: List[string] = []
    names.push("hello")
    names.push("world")
    let mut joined = ""
    for name in names {
        joined = joined + name + " "
    }
    println("strings=" + string_trim(joined))

    -- Test List[struct] iteration
    let mut points: List[Point] = []
    points.push(Point { x: 1, y: 2 })
    points.push(Point { x: 3, y: 4 })
    let mut total = 0
    for p in points {
        total = total + p.x + p.y
    }
    println("struct_sum=" + int_to_string(total))
}
