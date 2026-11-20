module test

struct Inner {
    value: int,
}

struct Outer {
    inner: Box[Inner],
}

fn main() {
    let i: Box[Inner] = Box_new(Inner { value: 99 })
    let o = Outer { inner: i }
    println(o.inner.value)
}