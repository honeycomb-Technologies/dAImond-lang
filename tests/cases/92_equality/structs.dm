module structs

-- Two structs are equal when their fields are

struct Point {
    x: int,
    y: int
}

struct Named {
    name: string,
    at: Point,
    tags: List[string],
    weight: float,
    on: bool
}

struct Empty {}

fn origin() -> Point {
    return Point { x: 0, y: 0 }
}

fn show(what: string, b: bool) {
    println(what + ": " + bool_to_string(b))
}

fn main() {
    let a = Point { x: 1, y: 2 }
    let b = Point { x: 1, y: 2 }
    let c = Point { x: 1, y: 3 }
    show("a == b", a == b)
    show("a == c", a == c)
    show("a != b", a != b)
    show("a != c", a != c)
    show("a call and a literal", origin() == Point { x: 0, y: 0 })
    show("a literal and a variable", Point { x: 1, y: 3 } == c)

    -- The names are two strings with the same text
    let n1 = Named { name: "n" + int_to_string(1), at: a, tags: ["x", "y"], weight: 1.5, on: true }
    let n2 = Named { name: "n1", at: b, tags: ["x", "y"], weight: 1.5, on: true }
    let n3 = Named { name: "n1", at: b, tags: ["x"], weight: 1.5, on: true }
    let n4 = Named { name: "n1", at: c, tags: ["x", "y"], weight: 1.5, on: true }
    let n5 = Named { name: "n1", at: b, tags: ["x", "y"], weight: 2.5, on: true }
    let n6 = Named { name: "n1", at: b, tags: ["x", "y"], weight: 1.5, on: false }
    show("n1 == n2", n1 == n2)
    show("another list", n1 == n3)
    show("another point", n1 == n4)
    show("another float", n1 == n5)
    show("another bool", n1 == n6)
    show("fields", n1.at == n2.at)
    show("fields that differ", n1.at != n4.at)

    show("no fields", Empty {} == Empty {})

    -- In a condition, with other operators
    if a == b and n1 != n3 {
        println("both")
    }
    show("a == b == true", a == b == true)
    assert_eq(a, b)
    assert_eq(n1, n2)
    println("done")
}
