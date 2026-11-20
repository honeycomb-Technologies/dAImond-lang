module main

import shapes

struct Node {
    name: string,
    weight: float,
    at: Point,
    tags: List[string],
    next: Box[Node],
    small: i32,
    on: bool
}

fn scale(mut p: Point, k: int) {
    p.x = p.x * k
    p.y = p.y * k
}

fn describe(n: Node, depth: int) -> string {
    let label = n.name + "!"
    let total = n.at.x + n.at.y + depth
    let ratio = n.weight * 2.0
    let ok = total > 3
    let mut counts: List[int] = []
    counts.push(total)
    for tag in n.tags {
        counts.push(len(tag))
    }
    for i in 0..2 {
        counts.push(i)
    }
    let narrow: u8 = 200
    if ok {
        return label + int_to_string(counts.len()) + float_to_string(ratio) + int_to_string(narrow as int)
    }
    return label
}

fn main() -> int {
    let mut p = Point { x: 3, y: 4 }
    scale(p, 2)
    let mut tags: List[string] = []
    tags.push("red")
    tags.push("green")
    let leaf = Node { name: "leaf", weight: 0.5, at: p, tags: tags, next: Box_null(), small: 7, on: true }
    let root = Node { name: "root", weight: 1.25, at: p, tags: tags, next: Box_new(leaf), small: -3, on: false }
    let add = |a: int, b: int| a + b
    println(describe(root, add(1, 2)))
    println(int_to_string(area(p)))
    return 0
}
