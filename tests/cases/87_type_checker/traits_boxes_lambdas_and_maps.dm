module test

trait Shape {
    fn area(self) -> float
    fn name(self) -> string
}
struct Sq { s: float }
struct Circ { r: float }
impl Shape for Sq {
    fn area(self) -> float { return self.s * self.s }
    fn name(self) -> string { return "sq" }
}
impl Shape for Circ {
    fn area(self) -> float { return 3.0 * self.r * self.r }
    fn name(self) -> string { return "circ" }
}
struct V { x: int, y: int }
impl V {
    fn new(x: int, y: int) -> V { return V { x: x, y: y } }
    fn add(self, o: V) -> V { return V { x: self.x + o.x, y: self.y + o.y } }
    fn eq(self, o: V) -> bool { return self.x == o.x and self.y == o.y }
    fn dot(self, o: V) -> int { return self.x * o.x + self.y * o.y }
}
enum Tree { Leaf(int), Node(Box[Tree], Box[Tree]), Empty }
fn sum(t: Tree) -> int {
    match t {
        Tree.Leaf(v) => { return v }
        Tree.Node(l, r) => { return sum(*l) + sum(*r) }
        Tree.Empty => { return 0 }
    }
}
struct Node { value: int, next: Box[Node] }
fn apply(f: fn(int) -> int, v: int) -> int { return f(v) }
fn double(x: int) -> int { return x * 2 }
fn inc(x: int) -> int = x + 1
fn main() -> int {
    let sq = Sq { s: 2.0 }
    let d1: dyn Shape = sq
    let ci = Circ { r: 1.0 }
    let d2: dyn Shape = ci
    println(d1.name() + d2.name())
    println(d1.area() + d2.area())
    let a = V.new(1, 2)
    let b = V { x: 3, y: 4 }
    let c = a + b
    if c == V.new(4, 6) { println(c.dot(a)) }
    let t = Tree.Node(Box_new(Tree.Leaf(3)), Box_new(Tree.Node(Box_new(Tree.Leaf(4)), Box_new(Tree.Empty))))
    println(sum(t))
    let tail = Node { value: 2, next: Box_null() }
    let head = Node { value: 1, next: Box_new(tail) }
    println(head.value + head.next.value)
    let k = 10
    let addk = |x: int| -> int x + k
    println(addk(5) + apply(double, 4) + apply(|x: int| -> int x + 1, 1))
    println(5 |> double |> inc)
    let mut m: Map[string, int] = Map_new()
    m.insert("a", 1)
    m["b"] = 2
    let mut tot = 0
    for key in m { tot = tot + m.get(key) + len(key) }
    let ks = m.keys()
    println(tot + m["a"] + ks.len() + m.values()[1])
    let mut grid: List[List[int]] = []
    let row = [1, 2, 3]
    grid.push(row)
    grid[0][1] = 9
    println(grid[0][1] + grid[0].len())
    let s = "hello"
    println(s.len() + len(s))
    println(f"v={c.x} {s}")
    return 0
}
