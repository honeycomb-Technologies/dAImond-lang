module test

-- A lambda takes from its surroundings only the variables its body
-- names. One that names none is a plain function, whatever variables
-- are in scope where it is written, so it can be passed as a fn(...).

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn twice(f: fn(int) -> int, x: int) -> int {
    return f(f(x))
}

fn main() -> int {
    let name = "result"
    let scale = 10
    let mut items: List[string] = []
    items.push("a")

    -- Names nothing of main's
    println(apply(|x: int| -> int x + 1, 1))
    let add_two = |x: int| -> int x + 2
    println(twice(add_two, 1))

    -- A parameter with the name of one of main's variables is the parameter
    println(apply(|scale: int| -> int scale * 2, 4))

    -- Names scale, in a nested expression and in a block
    let scaled = |x: int| -> int (x + 1) * scale
    println(scaled(2))
    let described = |x: int| -> string {
        let n = x * scale
        return name + " " + int_to_string(n) + " " + int_to_string(items.len())
    }
    println(described(3))
    return 0
}
