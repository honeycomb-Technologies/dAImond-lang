module test
-- A list literal stores its elements by the type the checker found for
-- it, from all its elements and from the place the list goes to,
-- wherever it is written: in a `let` with or without a type, as an
-- argument, in a Some
struct P { x: int }
fn sum(xs: List[i32]) -> int {
    let mut t = 0
    for x in xs { t += x }
    return t
}
fn count(bs: List[bool]) -> int {
    let mut t = 0
    for b in bs { if b { t += 1 } }
    return t
}
fn bsum(xs: List[u8]) -> int {
    let mut t = 0
    for x in xs { t += x as int }
    return t
}
fn boxes(xs: List[Box[int]]) -> int {
    let mut t = 0
    for x in xs { t += *x }
    return t
}
fn calls(fs: List[fn(int) -> int]) -> int {
    let mut t = 0
    for f in fs { t += f(1) }
    return t
}
fn fsum(xs: List[f32]) -> float {
    let mut t = 0.0
    for x in xs { t += x }
    return t
}
fn inc(x: int) -> int { return x + 1 }
fn size(m: Map[string, int]) -> int { return m.len() }
fn first(o: Option[Map[int, string]]) -> int {
    match o { Some(m) => { return m.len() }, None => { return -1 } }
}
fn fresh() -> List[Map[string, int]] { return [Map_new(), Map.new()] }
struct Reg { tables: List[Map[string, int]] }

fn main() {
    -- bools
    println(int_to_string(count([true, false, true])))
    let bs = [true, false, true]
    println(int_to_string(count(bs)) + bool_to_string(bs[2]))

    -- narrow integers: constants take the type of the place, and of
    -- the other elements
    let a: i32 = 7
    let xs = [2, a, -3]
    println(int_to_string(sum(xs)) + " " + int_to_string(sum([a, 2, -3])) + " " + int_to_string(sum([1, 2, -3])) + " " + int_to_string(xs[1]))
    let o = Some([a, 2])
    match o { Some(ys) => println(int_to_string(sum(ys))), None => println("none") }
    let u: u8 = 200
    println(int_to_string(bsum([u, 1, u])))
    let us = [u, u]
    let mut ut = 0
    for x in us { ut += x as int }
    println(int_to_string(ut + us[1] as int))

    -- a wider element widens the list
    let big = 5000000000
    let wide = [a, big]
    let wide2 = [big, a]
    println(int_to_string(wide[1] + wide[0]) + " " + int_to_string(wide2[0] + wide2[1]))

    -- floats
    let h: f32 = 0.5
    let fs = [h, 0.25]
    println(float_to_string(fsum(fs) + fsum([0.25, h]) + fsum([1.0, 2.0]) + fs[0]))
    let mixed = [h, 2.5 * 2.0, 1.0]
    let d: float = 0.125
    let floats = [h, d]
    println(float_to_string(mixed[1] + floats[0] + floats[1]))

    -- boxes, functions, structs, strings, lists, options
    let bx = [Box_new(1), Box_new(2)]
    println(int_to_string(boxes(bx) + boxes([Box_new(5)]) + *bx[1]))
    let fns = [inc, |x: int| -> int x * 10]
    println(int_to_string(calls(fns) + calls([inc]) + fns[1](2)))
    let nested = [[1, 2], [3]]
    let ps = [P { x: 1 }, P { x: 2 }]
    let ss = ["a", "b"]
    let os = [None, Some(3)]
    let mut t = 0
    match os[1] { Some(v) => { t = v }, None => {} }
    match os[0] { Some(v) => { t = 100 }, None => { t += 10 } }
    println(int_to_string(nested[0][1] + ps[1].x + len(ss[1]) + t))

    -- the variable of a loop over a list of lists is a list of the
    -- inner type
    let s: i16 = -5
    let rows = [[s, 1], [2]]
    let mut rt = 0
    for row in rows {
        for v in row { rt += v }
        rt += row[0] * 100 + row.len() * 1000
    }
    let typed: List[List[i16]] = [[s, 1], [2]]
    for row in typed { rt += row[0] }
    let flags = [[true, false], [true]]
    for row in flags { for v in row { if v { rt += 10000 } } }
    println(int_to_string(rt))
    let boxed = [Some(Box_new(4)), None]
    let mut bt = 0
    for b in boxed { match b { Some(v) => { bt += *v }, None => { bt += 100 } } }
    println(int_to_string(bt))

    -- Map_new() is an empty map of the type of its place, whatever
    -- the place is
    let ms: List[Map[string, int]] = [Map_new()]
    let mut mt = 0
    for m in ms { mt += m.len() }
    println(int_to_string(mt + size(Map_new()) + first(Some(Map_new())) + fresh().len()))
    let mut reg = Reg { tables: [] }
    reg.tables.push(Map_new())
    reg.tables[0].insert("k", 3)
    let mut ms2 = fresh()
    ms2.push(Map.new())
    ms2[2].insert("z", 1)
    let pick = if mt == 0 { ms2[2] } else { Map_new() }
    println(int_to_string(reg.tables[0].get("k") + ms2.len() + pick.len()))
    let mb: Box[Map[string, bool]] = Box_new(Map_new())
    println(int_to_string((*mb).len()))
}
