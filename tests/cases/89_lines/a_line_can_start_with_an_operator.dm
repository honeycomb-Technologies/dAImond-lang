module test

fn main() {
    -- A token that can begin an expression begins one at the start of a
    -- line: these are statements of their own, not the rest of the
    -- line before
    let mut b = Box_new(1)
    let y = 2
    *b = 5
    println(*b)
    let z = 3
    -y
    println(z)
    let xs = [1, 2]
    let w = xs
    [3, 4, 5].len()
    println(w.len())

    -- An operator that cannot begin an expression continues the line
    -- before it
    let total = 1
        + 2
        + 3
    println(total)
    let ok = total == 6
        and y == 2
        or false
    println(ok)
    let shifted = 1
        << 4
    println(shifted)
    let length = "abc"
        .len()
    println(length)

    -- And an operator at the end of a line takes the next line
    let diff = 10 -
        4
    println(diff)
    let product = 3 *
        4
    println(product)
}
