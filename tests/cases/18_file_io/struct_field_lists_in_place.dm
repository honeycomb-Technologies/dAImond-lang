module test
struct Point { x: int, y: int }
struct Inner { items: List[int], names: List[string], pts: List[Point], p: Point }
struct Outer { inner: Inner, name: string, count: int }
struct Node { val: int, next: Box[Node], has_next: bool }
fn add_item(mut o: Outer, v: int) {
    o.inner.items.push(v)
    o.count += 1
}
fn total(xs: List[int]) -> int {
    let mut t = 0
    for x in xs { t = t + x }
    return t
}
fn main() {
    let mut o = Outer { inner: Inner { items: [], names: [], pts: [], p: Point { x: 1, y: 2 } }, name: "o", count: 0 }
    o.inner.items.push(7)
    o.inner.items.push(9)
    o.inner.names.push("a")
    o.inner.names.push("bb")
    o.inner.pts.push(Point { x: 3, y: 4 })
    add_item(o, 20)
    println(int_to_string(o.inner.items.len()))
    println(int_to_string(o.inner.items[1]))
    println(int_to_string(o.count))
    println(o.inner.names[1])
    println(int_to_string(o.inner.pts[0].y))
    o.inner.items[0] = 100
    o.inner.p.x = 50
    o.inner.p.y += 5
    o.count = o.count * 10
    println(int_to_string(total(o.inner.items)))
    println(int_to_string(o.inner.p.x + o.inner.p.y))
    println(int_to_string(o.count))
    let mut sum = 0
    for p in o.inner.pts { sum = sum + p.x + p.y }
    for n in o.inner.names { println(n) }
    println(int_to_string(sum))
    let popped = o.inner.items.pop()
    println(int_to_string(popped))
    let mut tail = Node { val: 2, next: Box_null(), has_next: false }
    let mut head = Node { val: 1, next: Box_new(tail), has_next: true }
    println(int_to_string((*head.next).val))
    println(o.name)
}