module test

struct Counter {
    value: int
}

fn main() {
    let mut c = Counter { value: 0 }
    c.value = 5
    println(c.value)
}