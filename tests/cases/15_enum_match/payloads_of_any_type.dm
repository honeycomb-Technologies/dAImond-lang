module main

struct Point {
    x: int,
    y: int
}

enum Item {
    Name(string),
    Weight(float),
    At(Point),
    Many(List[int]),
    Flag(bool),
    Count(int),
    Nothing
}

fn show(it: Item) -> string {
    match it {
        Item.Name(s) => { return "name " + s }
        Item.Weight(w) => { return "weight " + float_to_string(w * 2.0) }
        Item.At(p) => { return "at " + int_to_string(p.x + p.y) }
        Item.Many(xs) => { return "many " + int_to_string(xs.len()) }
        Item.Flag(b) => { if b { return "yes" } return "no" }
        Item.Count(n) => { return "count " + int_to_string(n + 1) }
        Item.Nothing => { return "nothing" }
    }
    return "?"
}

fn main() -> int {
    println(show(Item.Name("ann")))
    println(show(Item.Weight(1.5)))
    println(show(Item.At(Point { x: 1, y: 2 })))
    println(show(Item.Many([1, 2, 3])))
    println(show(Item.Flag(true)))
    println(show(Item.Count(41)))
    println(show(Item.Nothing))
    return 0
}
