module test

enum Shape {
    Circle(int),
    Square(int),
    Dot
}

struct P {
    n: int
}

fn pick(o: Option[int], x: int) -> int {
    -- The x that Some binds is the arm's own
    return match o {
        Some(x) => x,
        None => x
    }
}

fn twice(x: int) -> int {
    let f = |x: int| -> int x * 2
    return f(x + 1) + x
}

fn main() {
    -- A variable declared in a block is gone at the block's end, and
    -- the variable it shadowed is the one it was
    let x = 10
    if x > 5 {
        let x = 99
        println(x)
    }
    println(x)

    let mut i = 0
    while i < 2 {
        let x = "s"
        println(x)
        i = i + 1
    }
    println(x + 1)

    for x in [7, 8] {
        println(x)
    }
    println(x)

    {
        let x = P { n: 3 }
        println(x.n)
    }
    println(x * 2)

    -- Names that a match arm binds
    let none: Option[int] = None
    println(pick(none, 4))
    println(pick(Some(5), 4))
    let s = Shape.Dot
    let q = match s {
        Shape.Circle(x) => x,
        Shape.Square(y) => y,
        Shape.Dot => x
    }
    println(q)
    let r: Result[string, int] = Err(3)
    match r {
        Ok(x) => println(x),
        Err(i) => println(i + x)
    }
    println(i)
    let n = 5
    let named = match n {
        1 => 0,
        x => x + 1
    }
    println(named)
    println(x)

    -- A lambda's parameter
    println(twice(3))
    let g = |x: string| -> string x + "!"
    println(g("hi"))
    println(x)

    -- A variable declared again in its own block takes the name from
    -- there on, and its value may read the one before it
    let v = 1
    let v = v + 1
    let v = "v" + int_to_string(v)
    println(v)
    let mut xs: List[int] = []
    xs.push(x)
    if true {
        let xs = ["a", "b"]
        println(xs[1])
        let xs = 2.5
        println(xs)
    }
    println(xs[0])

    -- Shadowing inside shadowing, and a change to the inner one
    let mut total = 0
    if true {
        let mut total = 100
        if true {
            let mut total = 1000
            total += 1
            println(total)
        }
        total += 1
        println(total)
    }
    total += 1
    println(total)
}
