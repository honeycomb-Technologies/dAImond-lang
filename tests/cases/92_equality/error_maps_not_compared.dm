module error_maps_not_compared

struct Index {
    name: string,
    places: Map[string, int]
}

fn same[T](a: T, b: T) -> bool {
    return a == b
}

fn main() {
    let mut m: Map[string, int] = Map_new()
    let mut n: Map[string, int] = Map_new()
    if m == n { println("maps") }
    let a = Index { name: "a", places: m }
    let b = Index { name: "a", places: n }
    if a != b { println("indexes") }
    if [a].contains(b) { println("contains") }
    if same(m, n) { println("generic") }
}
