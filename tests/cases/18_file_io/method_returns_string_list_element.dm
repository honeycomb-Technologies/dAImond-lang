module test
struct Names { items: List[string] }
impl Names {
    fn last(self) -> string {
        return self.items[self.items.len() - 1]
    }
}
fn first_of(xs: List[string]) -> string {
    return xs[0]
}
fn main() {
    let mut ns = Names { items: [] }
    ns.items.push("a")
    ns.items.push("b")
    println(ns.last() + first_of(ns.items))
}