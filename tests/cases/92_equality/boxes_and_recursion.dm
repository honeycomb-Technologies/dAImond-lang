module boxes_and_recursion

-- A type that contains itself is compared all the way down. A Box is
-- compared by where it points, not by what is there.

struct Tree {
    value: int,
    kids: List[Tree]
}

struct Cell {
    value: int,
    next: Box[Cell]
}

fn leaf(v: int) -> Tree {
    let none: List[Tree] = []
    return Tree { value: v, kids: none }
}

fn tree(v: int, a: Tree, b: Tree) -> Tree {
    return Tree { value: v, kids: [a, b] }
}

fn show(what: string, b: bool) {
    println(what + ": " + bool_to_string(b))
}

fn main() {
    let t = tree(1, leaf(2), tree(3, leaf(4), leaf(5)))
    let u = tree(1, leaf(2), tree(3, leaf(4), leaf(5)))
    let v = tree(1, leaf(2), tree(3, leaf(4), leaf(6)))
    show("trees", t == u)
    show("a deep difference", t == v)
    show("trees differ", t != v)

    let end = Cell { value: 0, next: Box_null() }
    let shared = Box_new(end)
    let a = Cell { value: 1, next: shared }
    let b = Cell { value: 1, next: shared }
    let c = Cell { value: 1, next: Box_new(end) }
    show("the same box", a == b)
    show("another box of an equal value", a == c)
    show("what the boxes hold", *a.next == *c.next)
    show("boxes", a.next == b.next)
    show("null boxes", end.next == Box_null())
}
