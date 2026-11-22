module test
enum Opt[T] {
    Has(T),
    Empty
}
impl Opt[T] {
    fn is_has(self) -> bool {
        match self {
            Opt.Has(v) => { return true }
            Opt.Empty => { return false }
        }
    }
    fn get_or(self, fallback: T) -> T {
        match self {
            Opt.Has(v) => { return v }
            Opt.Empty => { return fallback }
        }
    }
}
enum Tree[T] {
    Leaf(T),
    Node(Box[Tree[T]], Box[Tree[T]])
}
fn sum(t: Tree[int]) -> int {
    match t {
        Tree.Leaf(v) => { return v }
        Tree.Node(l, r) => { return sum(*l) + sum(*r) }
    }
}
struct Holder {
    slot: Opt[string]
}
fn main() {
    let a = Opt.Has(3)
    println(a.is_has())
    println(a.get_or(9))
    let b: Opt[string] = Opt.Empty
    println(b.is_has())
    println(b.get_or("fallback"))
    let t = Tree.Node(Box_new(Tree.Leaf(1)), Box_new(Tree.Node(Box_new(Tree.Leaf(2)), Box_new(Tree.Leaf(3)))))
    println(sum(t))
    let mut h = Holder { slot: Opt.Empty }
    println(h.slot.get_or("none"))
    h.slot = Opt.Has("set")
    println(h.slot.get_or("none"))
}
