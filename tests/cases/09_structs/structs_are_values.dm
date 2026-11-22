module structs_are_values

-- A struct is a value: a copy does not change when the original does,
-- whatever way the two are passed, returned, kept and read.

struct Inner {
    v: int,
    name: string
}

struct Outer {
    id: int,
    inner: Inner,
    items: List[int],
    ratio: float
}

struct Wide {
    a: int, b: int, c: int, d: int, e: int, f: int, g: int, h: int,
    s: string, t: string, u: float, w: bool
}

enum Shape {
    Dot,
    Box(Inner, int),
    Pair(Inner, Inner)
}

fn make(v: int) -> Inner {
    return Inner { v: v, name: "n" + int_to_string(v) }
}

fn outer(id: int) -> Outer {
    return Outer { id: id, inner: make(id * 10), items: [id], ratio: 0.5 }
}

fn show(o: Outer) -> string {
    return int_to_string(o.id) + ":" + int_to_string(o.inner.v) + ":" + o.inner.name
}

fn inner_of(o: Outer) -> Inner {
    return o.inner
}

fn bumped(i: Inner) -> Inner {
    let mut out = i
    out.v = out.v + 1
    return out
}

impl Inner {
    fn plus(self, n: int) -> Inner {
        return Inner { v: self.v + n, name: self.name }
    }
    fn grow(mut self, n: int) {
        self.v = self.v + n
    }
}

-- The first parameter is the caller's variable, the second a copy of it
fn change_and_read(mut a: Outer, b: Outer) -> string {
    a.id = 100
    a.inner.v = 200
    a.inner.name = "changed"
    return show(a) + " " + show(b)
}

fn both(a: Outer, b: Outer) -> int {
    return a.id + b.inner.v
}

fn step(o: Outer, by: int) -> Outer {
    let mut next = o
    next.id = next.id + by
    next.inner = bumped(next.inner)
    return next
}

fn pick(c: bool, a: Inner, b: Inner) -> Inner {
    if c { return a }
    return b
}

fn wide(n: int) -> Wide {
    return Wide { a: n, b: n + 1, c: n + 2, d: n + 3, e: n + 4, f: n + 5, g: n + 6, h: n + 7, s: "s", t: "t" + int_to_string(n), u: 1.5, w: n > 0 }
}

fn sum(w: Wide) -> int {
    return w.a + w.b + w.c + w.d + w.e + w.f + w.g + w.h
}

fn depth(n: int, acc: Outer) -> Outer {
    if n == 0 { return acc }
    return depth(n - 1, step(acc, n))
}

fn maybe(v: int) -> Option[Inner] {
    if v > 0 { return Some(make(v)) }
    return None
}

fn checked(v: int) -> Result[Outer, string] {
    if v < 0 { return Err("negative") }
    return Ok(outer(v))
}

fn twice(v: int) -> Result[Inner, string] {
    let o = checked(v)?
    return Ok(bumped(bumped(o.inner)))
}

fn area(s: Shape) -> int {
    match s {
        Shape.Dot => { return 0 }
        Shape.Box(i, n) => { return i.v * n }
        Shape.Pair(a, b) => { return a.v + b.v }
    }
}

fn main() {
    -- A copy, then a change to the original
    let mut x = outer(1)
    let kept = x
    x.id = 2
    x.inner.v = 99
    x.inner.name = "new"
    println(show(kept) + " " + show(x))

    -- A field that is a struct, read before the original changes
    let p = x.inner
    x.inner.v = 5
    x.inner = make(7)
    println(int_to_string(p.v) + " " + p.name + " " + int_to_string(x.inner.v))

    -- The caller's variable and a copy of it, in one call
    println(change_and_read(x, x))
    println(show(x))
    println(int_to_string(both(x, x)))

    -- A variable that is given the result of a call on itself
    let mut o = outer(3)
    o = step(o, 1)
    o = step(step(o, 1), 1)
    println(show(o))
    o.inner = o.inner.plus(100)
    o.inner = bumped(o.inner)
    o.inner.grow(1000)
    println(show(o))
    println(show(depth(50, outer(0))))

    -- An argument that is read before a later argument changes it
    let mut q = make(1)
    let r = pick(true, q, { q.v = 50; q })
    println(int_to_string(r.v) + " " + int_to_string(q.v))
    let s = pick(false, inner_of(o), bumped(q))
    println(int_to_string(s.v))

    -- Elements of a list
    let mut ps = [make(1), make(2), make(3)]
    let first = ps[0]
    ps[0] = ps[2]
    ps[2].v = 30
    println(int_to_string(first.v) + " " + int_to_string(ps[0].v) + " " + int_to_string(ps[2].v))
    let mut total = 0
    for e in ps {
        ps[1] = make(100)
        total = total + e.v
    }
    println(int_to_string(total))
    ps[1] = bumped(ps[1])
    ps[1].grow(5)
    println(int_to_string(ps[1].v) + " " + ps[1].name)

    -- A struct with many fields
    let w = wide(1)
    let mut w2 = w
    w2.h = 1000
    w2.t = "other"
    println(int_to_string(sum(w)) + " " + int_to_string(sum(w2)) + " " + w.t + " " + w2.t + " " + bool_to_string(w.w))

    -- Options and Results of structs
    match maybe(4) {
        Some(i) => println(i.name),
        None => println("none")
    }
    match maybe(0) {
        Some(i) => println(i.name),
        None => println("none")
    }
    match twice(6) {
        Ok(i) => println(int_to_string(i.v)),
        Err(e) => println(e)
    }
    match twice(-1) {
        Ok(i) => println(int_to_string(i.v)),
        Err(e) => println(e)
    }
    let found = maybe(9)
    let mut again = found
    again = maybe(1)
    match found {
        Some(i) => println(int_to_string(i.v)),
        None => println("none")
    }

    -- Enum values that carry structs
    let mut inside = make(6)
    let shape = Shape.Box(inside, 7)
    inside.v = 0
    println(int_to_string(area(shape)) + " " + int_to_string(area(Shape.Pair(inside, make(4)))) + " " + int_to_string(area(Shape.Dot)))
    let shapes = [shape, Shape.Dot, Shape.Pair(make(1), make(2))]
    let mut all = 0
    for sh in shapes {
        all = all + area(sh)
    }
    println(int_to_string(all))

    -- Function values and closures with structs
    let by = make(10)
    let add = |i: Inner| -> Inner Inner { v: i.v + by.v, name: i.name + by.name }
    let step_fn = bumped
    let fs = [add, step_fn]
    let mut cur = make(1)
    for f in fs {
        cur = f(cur)
    }
    println(int_to_string(cur.v) + " " + cur.name)
    let build = |n: int| -> Outer step(outer(n), n)
    println(show(build(4)))

    -- An if and a match that give structs
    let chosen = if cur.v > 5 { make(1) } else { make(2) }
    let other = match chosen.v {
        1 => outer(8),
        _ => outer(9)
    }
    println(int_to_string(chosen.v) + " " + show(other))

    -- A box of a struct
    let boxed = Box_new(outer(5))
    let mut from_box = *boxed
    from_box.id = 6
    let b2 = *boxed
    println(show(b2) + " " + show(from_box) + " " + bool_to_string(b2 == *boxed) + " " + bool_to_string(from_box.inner == b2.inner))
}
