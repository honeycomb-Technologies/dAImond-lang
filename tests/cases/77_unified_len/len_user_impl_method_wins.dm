module test

struct Bag {
    n: int,
}

impl Bag {
    fn len(self: &Self) -> int {
        return self.n
    }
}

fn main() {
    let bag = Bag { n: 7 }
    println(bag.len())
}