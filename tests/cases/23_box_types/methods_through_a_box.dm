module methods_through_a_box

-- A method called on a box is called on what is in the box, and a
-- `mut self` method changes the value in the box

struct Counter { n: int, name: string }

impl Counter {
    fn inc(mut self) { self.n = self.n + 1 }
    fn add(mut self, k: int) -> int {
        self.n = self.n + k
        return self.n
    }
    fn get(self) -> int { return self.n }
}

-- Another struct with methods of the same names
struct Gauge { level: float }

impl Gauge {
    fn inc(mut self) { self.level = self.level + 0.5 }
    fn reading(self) -> float { return self.level }
}

struct Pair[A, B] { a: A, b: B }

impl Pair[A, B] {
    fn set_a(mut self, v: A) { self.a = v }
    fn first(self) -> A { return self.a }
}

struct Holder { c: Box[Counter] }

fn bump(b: Box[Counter]) {
    b.inc()
}

fn main() -> int {
    let bc = Box_new(Counter { n: 3, name: "c" })
    bc.inc()
    bc.inc()
    println(bc.get())
    println(bc.n)
    println(bc.add(10))
    println(bc.name)

    -- the same box under another name, and as an argument
    let same = bc
    same.inc()
    bump(bc)
    println(bc.n)

    let bg = Box_new(Gauge { level: 1.0 })
    bg.inc()
    println(bg.reading())
    bc.inc()
    println(bc.get())

    -- a generic struct
    let bp = Box_new(Pair[int, string] { a: 1, b: "x" })
    bp.set_a(5)
    println(bp.first())
    println(bp.a)
    println(bp.b)

    -- a box in a field
    let h = Holder { c: Box_new(Counter { n: 100, name: "h" }) }
    h.c.inc()
    println(h.c.get())

    -- a map
    let mut m: Map[string, int] = Map_new()
    m.insert("a", 1)
    let bm = Box_new(m)
    bm.insert("b", 2)
    bm.insert("c", 3)
    println(bm.len())
    println(bm.get("b") + bm.get("c"))
    println(bm.contains("a"))
    bm.remove("a")
    println(bm.contains("a"))
    return 0
}
