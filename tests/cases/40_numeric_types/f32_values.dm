module test
-- An f32 beside float constants and floats: a constant takes the type
-- of its place and of the other operand, and an f32 widens to a float
struct Plain { a: i32, b: f32 }
struct Pair[A, B] { first: A, second: B }
enum E { V(f32), W(float) }
fn half(x: f32) -> f32 { return x / 2.0 }
fn lit() -> f32 { return 2.5 }
fn wide(x: f32) -> float { return x }
fn pick(c: bool) -> f32 { return if c { 1.5 } else { -0.5 } }
fn sum(xs: List[f32]) -> f32 {
    let mut t: f32 = 0.0
    for x in xs { t += x }
    return t
}
fn show(x: float) -> string { return float_to_string(x) }
fn main() {
    let x: f32 = 2.5
    println(show(x))
    let mut y: f32 = 1.0
    y = 2.5
    y = y * 2.0 + 0.5
    y -= 0.5
    println(show(y))
    println(show(half(2.5)))
    println(show(lit()))
    println(show(wide(x) + 1.0))
    println(show(pick(true)) + " " + show(pick(false)))
    let xs: List[f32] = [1.5, 2.5]
    let mut ys: List[f32] = []
    ys.push(2.5)
    ys.push(x)
    ys[0] = 0.25
    ys[1] += 1.0
    println(show(xs[1]) + " " + show(sum(ys)))
    let e = E.V(2.5)
    match e { E.V(f) => println(show(f)), E.W(d) => println(show(d)) }
    let e2 = E.W(x)
    match e2 { E.V(f) => println(show(f)), E.W(d) => println(show(d)) }
    let mut m: Map[string, f32] = Map_new()
    m.insert("a", 2.5)
    println(show(m.get("a")))
    let o: Option[f32] = Some(2.5)
    match o { Some(f) => println(show(f)), None => println("n") }
    let mut p = Plain { a: 1, b: 1.0 }
    p.b = 2.5
    p.b *= 2.0
    println(show(p.b))
    let d: float = 2.5
    let q = Plain { a: 1, b: d as f32 }
    let s: f32 = q.b + 1.0
    let mixed: float = q.b + d
    println(show(s) + " " + show(mixed))
    let b = Box_new(2.5 as f32)
    let c: Box[f32] = Box_new(2.5)
    println(show(*b as float) + show(*c as float))
    let pi: Pair[i32, f32] = Pair { first: 3, second: 2.5 }
    println(show(pi.second))
    let neg: f32 = -x
    println(show(neg) + " " + show(-x as float))
    if x > 2.0 and 3.0 > x and x == 2.5 { println("cmp") }
    let third: f32 = 1.0 / 3.0
    println(show(third))
    let n = -3
    println(int_to_string(-n as i32 as int))
    let r: Result[f32, string] = Ok(0.5)
    match r { Ok(v) => println(show(v)), Err(msg) => println(msg) }
    let fl: List[float] = [x, 1.0]
    println(show(fl[0]))
    -- An f32 and a float in the branches of an `if` meet at a float; an
    -- f32 and a constant at an f32
    let c = len("ab") > 1
    let fa = if c { x } else { d }
    let fb = if c { d } else { x }
    let fe: f32 = if c { x } else { 0.5 }
    let fm = match c { true => x, false => 4.0 }
    println(show(fa) + " " + show(fb) + " " + show(fe) + " " + show(fm))
    let ft: float = fa + fb
    println(show(ft))
    -- An f32 is printed as a float is
    println(x)
    println(float_to_string(x))
    println(f"x is {x} and twice is {x * 2.0}")
    print(x + 0.25)
    println("")
}
