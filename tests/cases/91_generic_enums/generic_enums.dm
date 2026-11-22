module test

enum Opt[T] {
    Has(T),
    Empty
}

enum Either[L, R] {
    Left(L),
    Right(R)
}

struct Point {
    x: int,
    y: int
}

fn show(o: Opt[int]) -> int {
    return match o {
        Opt.Has(v) => v,
        Opt.Empty => -1
    }
}

fn name_of(o: Opt[string]) -> string {
    match o {
        Opt.Has(s) if len(s) == 0 => { return "(blank)" }
        Opt.Has(s) => { return s }
        Opt.Empty => { return "(none)" }
    }
}

fn find(xs: List[int], wanted: int) -> Opt[int] {
    let mut i = 0
    while i < xs.len() {
        if xs[i] == wanted { return Opt.Has(i) }
        i = i + 1
    }
    return Opt.Empty
}

fn halve(n: int) -> Either[string, int] {
    if n % 2 != 0 { return Either.Left("odd") }
    return Either.Right(n / 2)
}

fn describe(e: Either[string, int]) -> string {
    return match e {
        Either.Left(why) => "left " + why,
        Either.Right(n) => "right " + int_to_string(n)
    }
}

-- A generic function over a generic enum
fn or_else[T](o: Opt[T], fallback: T) -> T {
    return match o {
        Opt.Has(v) => v,
        Opt.Empty => fallback
    }
}

fn wrap[T](x: T) -> Opt[T] {
    return Opt.Has(x)
}

fn nothing[T](like: T) -> Opt[T] {
    return Opt.Empty
}

fn main() {
    -- The type arguments come from the values, or from the place
    let a = Opt.Has(3)
    let b: Opt[int] = Opt.Empty
    println(show(a))
    println(show(b))
    println(show(Opt.Has(40) ))
    println(show(Opt.Empty))

    let c = Opt.Has("s")
    println(name_of(c))
    println(name_of(Opt.Has("")))
    println(name_of(Opt.Empty))

    println(show(find([5, 6, 7], 7)))
    println(show(find([5, 6, 7], 9)))

    println(describe(halve(7)))
    println(describe(halve(8)))

    -- Instances of one enum are types of their own
    let p = Opt.Has(Point { x: 1, y: 2 })
    match p {
        Opt.Has(pt) => println(pt.x + pt.y),
        Opt.Empty => println("no point")
    }
    let f: Opt[float] = Opt.Has(2.5)
    match f {
        Opt.Has(v) => println(v),
        Opt.Empty => println("no float")
    }

    -- In lists and in other enums
    let mut found: List[Opt[int]] = []
    found.push(find([1, 2], 2))
    found.push(Opt.Empty)
    for o in found {
        println(show(o))
    }
    let nested: Opt[Opt[int]] = Opt.Has(Opt.Has(9))
    match nested {
        Opt.Has(inner) => println(show(inner)),
        Opt.Empty => println("nothing")
    }

    println(or_else(a, 0))
    println(or_else(b, 77))
    println(or_else(c, "d"))
    println(show(wrap(12)))
    println(name_of(wrap("w")))
    println(show(nothing(1)))

    -- A narrow integer takes the type of its place
    let small: Opt[u8] = Opt.Has(200)
    match small {
        Opt.Has(v) => println(v),
        Opt.Empty => println("none")
    }
}
