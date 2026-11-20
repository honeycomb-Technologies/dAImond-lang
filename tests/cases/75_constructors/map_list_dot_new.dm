module test

fn main() {
    let mut m: Map[string, int] = Map.new()
    m.insert("a", 1)
    println(m.get("a"))
    let mut l: List[int] = List.new()
    l.push(7)
    println(l[0])
}