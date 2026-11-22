module closure_values

-- A lambda that uses variables of the function around it is a value
-- like any other: it is passed, returned, kept in a list, a field or
-- a map, and called wherever it ends up. It has the variables' values
-- as they were when it was made.

struct Point {
    x: int,
    y: int
}

struct Button {
    label: string,
    on_press: fn(int) -> string
}

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn through(x: int, f: fn(int) -> int) -> int {
    return f(x)
}

fn twice(f: fn(int) -> int, x: int) -> int {
    return f(f(x))
}

fn make_adder(n: int) -> fn(int) -> int {
    return |x: int| -> int x + n
}

fn make_greeter(greeting: string) -> fn(string) -> string {
    let mark = "!"
    return |name: string| -> string greeting + ", " + name + mark
}

fn compose(f: fn(int) -> int, g: fn(int) -> int) -> fn(int) -> int {
    return |x: int| -> int g(f(x))
}

fn keep(xs: List[int], wanted: fn(int) -> bool) -> List[int] {
    let mut out: List[int] = []
    for x in xs {
        if wanted(x) { out.push(x) }
    }
    return out
}

fn show(xs: List[int]) -> string {
    let mut text = ""
    for x in xs {
        text = text + int_to_string(x) + " "
    }
    return text
}

fn square(x: int) -> int {
    return x * x
}

fn half(x: float) -> float {
    return x / 2.0
}

fn main() {
    -- Passed, by its name and where it is written
    let scale = 10
    let scaled = |x: int| -> int x * scale
    println(int_to_string(scaled(4)))
    println(int_to_string(apply(scaled, 4)))
    println(int_to_string(apply(|x: int| -> int x * scale + 1, 4)))
    println(int_to_string(twice(scaled, 2)))

    -- Returned
    let add5 = make_adder(5)
    let add7 = make_adder(7)
    println(int_to_string(add5(1)) + " " + int_to_string(add7(1)))
    println(int_to_string(make_adder(100)(1)))
    let hello = make_greeter("hello")
    println(hello("Ada"))
    println(int_to_string(compose(add5, square)(1)))
    println(int_to_string(compose(square, add5)(3)))

    -- It has the values as they were when it was made
    let mut base = 1
    let from_one = |x: int| -> int x + base
    base = 100
    let from_hundred = |x: int| -> int x + base
    println(int_to_string(from_one(1)) + " " + int_to_string(from_hundred(1)))

    -- A string, a list and a struct in it
    let words = ["a", "b", "c"]
    let origin = Point { x: 3, y: 4 }
    let prefix = "n="
    let describe = |i: int| -> string prefix + int_to_string(i + origin.x + origin.y) + words[i]
    println(describe(0) + " " + describe(2))

    -- In a list, one for each turn of a loop
    let mut adders: List[fn(int) -> int] = []
    let mut i = 0
    while i < 3 {
        adders.push(|x: int| -> int x + i * 10)
        i = i + 1
    }
    adders.push(square)
    println(int_to_string(adders[0](1)) + " " + int_to_string(adders[1](1)) + " " + int_to_string(adders[2](1)) + " " + int_to_string(adders[3](5)))
    let mut total = 0
    for f in adders {
        total = total + f(2)
    }
    println(int_to_string(total))

    -- In a field, called as a method is and as a value
    let count = 3
    let button = Button { label: "ok", on_press: |times: int| -> string "pressed " + int_to_string(times * count) }
    println(button.label + ": " + button.on_press(2))
    let handler = button.on_press
    println(handler(1))

    -- In a map
    let mut ops: Map[string, fn(int) -> int] = Map_new()
    ops.insert("scale", scaled)
    ops.insert("square", square)
    println(int_to_string(ops.get("scale")(3)) + " " + int_to_string(ops.get("square")(3)))

    -- A lambda in a lambda, and one that takes a function
    let outer = 2
    let make = |a: int| -> fn(int) -> int {
        return |b: int| -> int a * b + outer
    }
    println(int_to_string(make(3)(4)))
    let limit = 4
    println(show(keep([1, 5, 2, 8, 3], |x: int| -> bool x < limit)))

    -- Other parameter and result types
    let ratio = 3.0
    let third = |x: float| -> float half(x) / ratio
    println(float_to_string(third(12.0)))
    let found = |n: int| -> Option[int] if n > scale { Some(n - scale) } else { None }
    match found(12) {
        Some(over) => println("over by " + int_to_string(over)),
        None => println("not over")
    }
    match found(2) {
        Some(over) => println("over by " + int_to_string(over)),
        None => println("not over")
    }
    let shout = |text: string| { println(text + mark_of(scale)) }
    shout("done")

    -- The pipeline calls a function value too
    println(int_to_string(3 |> scaled |> add5 |> through(4 |> make_adder)))
}

fn mark_of(n: int) -> string {
    if n > 5 { return "!" }
    return "."
}
