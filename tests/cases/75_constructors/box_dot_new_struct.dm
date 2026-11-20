module test

struct Wrapper {
    value: int,
}

fn main() {
    let b: Box[Wrapper] = Box.new(Wrapper { value: 42 })
    println(b.value)
}