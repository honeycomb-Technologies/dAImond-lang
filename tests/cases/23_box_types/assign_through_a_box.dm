module test

struct Counter {
    n: int
}

fn bump(b: Box[int]) {
    *b = *b + 1
}

fn main() {
    -- `*b = v` stores v in the box, which every copy of the box shares
    let b = Box_new(1)
    *b = 5
    println(*b)
    *b += 4
    println(*b)
    *b *= 2
    println(*b)
    bump(b)
    println(*b)
    let c = b
    *c = 7
    println(*b)

    let s = Box_new("a")
    *s = *s + "b"
    println(*s)

    let p = Box_new(Counter { n: 1 })
    *p = Counter { n: 40 }
    p.n += 2
    println(p.n)

    let small = Box_new(3 as u8)
    *small = 200
    println(*small)
    (*b) = 11
    println(*b)
}
