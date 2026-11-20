module test

struct Counter {
    value: int,
}

impl Counter {
    fn add(self: &Self, n: int) -> int {
        return self.value + n
    }

    fn multiply(self: &Self, n: int) -> int {
        return self.value * n
    }
}

fn main() {
    let c = Counter { value: 7 }
    println(c.add(3))
    println(c.multiply(4))
}