module test

-- A lambda whose body branches: a remainder (which checks for zero),
-- `and`, `or`, an `if` and a `match`. Each has its own blocks, apart
-- from the function it is written in.

fn keep(xs: List[int], wanted: fn(int) -> bool) -> List[int] {
    let mut out: List[int] = []
    for x in xs {
        if wanted(x) {
            out.push(x)
        }
    }
    return out
}

fn each(xs: List[int], f: fn(int) -> int) -> string {
    let mut text = ""
    for x in xs {
        text = text + int_to_string(f(x)) + " "
    }
    return text
}

fn main() -> int {
    let mut xs: List[int] = []
    let mut i = 1
    while i <= 9 {
        xs.push(i)
        i = i + 1
    }
    let label = "kept"

    println(each(keep(xs, |x: int| -> bool x % 2 == 0), |x: int| -> int x))
    println(each(keep(xs, |x: int| -> bool x > 2 and x < 6), |x: int| -> int x / 2))
    println(each(keep(xs, |x: int| -> bool x < 2 or x > 8), |x: int| -> int x))
    println(each(xs, |x: int| -> int if x % 3 == 0 { 0 } else { x }))

    -- The same in a lambda that captures: limit and label are main's
    let limit = 4
    let small = |x: int| -> bool x % 2 == 1 and x < limit
    let mut count = 0
    for x in xs {
        if small(x) {
            count = count + 1
        }
    }
    println(label + ": " + int_to_string(count))

    -- main goes on in its own blocks after each lambda
    if count == 2 {
        println("done")
    }
    return 0
}
