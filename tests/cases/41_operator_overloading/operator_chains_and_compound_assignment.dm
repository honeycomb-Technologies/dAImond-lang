module main

struct V { x: int, y: int }

impl V {
    fn add(self, o: V) -> V { return V { x: self.x + o.x, y: self.y + o.y } }
    fn sub(self, o: V) -> V { return V { x: self.x - o.x, y: self.y - o.y } }
    fn eq(self, o: V) -> bool { return self.x == o.x and self.y == o.y }
}

struct Box2 { lo: V, hi: V }

fn mk(n: int) -> V { return V { x: n, y: n * 2 } }

fn show(v: V) { println(int_to_string(v.x) + " " + int_to_string(v.y)) }

fn main() {
    let a = V { x: 1, y: 2 }
    let b = V { x: 10, y: 20 }
    let c = V { x: 100, y: 200 }
    show(a + b + c)
    show(c - b - a)
    let bx = Box2 { lo: a, hi: b }
    show(bx.lo + bx.hi)
    show(mk(3) + mk(4))
    if bx.lo + bx.hi == mk(11) { println("eq") }
    if a + b == V { x: 11, y: 22 } { println("eq2") }
    let mut vs: List[V] = [a, b]
    vs[0] += c
    show(vs[0])
    let mut m = Box2 { lo: a, hi: b }
    m.hi -= a
    show(m.hi)
    let mut z = a
    z += b
    z += b + c
    show(z)
}
