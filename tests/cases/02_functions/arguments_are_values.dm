module arguments_are_values

-- A string or a list that is read is the value it has then. What is
-- evaluated after it, in the same call or expression, may change the
-- variable it was read from, and does not change the value read.
-- (The first argument of join(s, append(s, "b"), s) was `s` as
-- append left it.)

struct Pair { first: string, second: string }

fn join(a: string, b: string, c: string) -> string {
    return a + "|" + b + "|" + c
}

fn append(mut s: string, t: string) -> string {
    s = s + t
    return s
}

fn grow(mut xs: List[int]) -> int {
    xs.push(99)
    return xs.len()
}

fn sizes(a: List[int], n: int, b: List[int]) -> string {
    return int_to_string(a.len()) + "/" + int_to_string(n) + "/" + int_to_string(b.len())
}

fn main() {
    let mut s = "a"
    println(join(s, append(s, "b"), s))
    println(s + append(s, "c") + s)
    let parts = [s, append(s, "d"), s]
    println(parts[0] + "," + parts[1] + "," + parts[2])
    let p = Pair { first: s, second: append(s, "e") }
    println(p.first + "," + p.second)
    println(f"{s}{append(s, \"f\")}{s}")
    println(join(s, { s = "z"; s }, s))

    let mut xs = [1, 2]
    println(sizes(xs, grow(xs), xs))
    let before = xs
    xs.push(5)
    println(sizes(before, 0, xs))
    let lists = [xs, { xs.push(6); xs }]
    println(sizes(lists[0], 0, lists[1]))

    -- a mut parameter still reaches the caller's variable
    let mut t = "x"
    append(t, "y")
    append(t, append(t, "z"))
    println(t)
    let mut ys: List[int] = []
    grow(ys)
    println(int_to_string(grow(ys) + ys.len()))
}
