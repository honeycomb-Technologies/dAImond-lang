module test

struct Counter { count: int, seen: List[int] }

impl Counter {
    fn bump(mut self) {
        self.count = self.count + 1
    }
    fn value(self) -> int {
        return self.count
    }
    fn note(self, v: int) {
        self.seen.push(v)
    }
}

fn grow(mut xs: List[int]) {
    xs.push(9)
}

fn total(xs: List[int]) -> int {
    xs.pop()
    return xs.len()
}

fn main() -> int {
    let xs: List[int] = [1]
    xs.push(2)
    let n = xs.pop()
    grow(xs)
    let c = Counter { count: 0, seen: [] }
    c.bump()
    c.seen.push(1)
    let ages: Map[string, int] = Map_new()
    ages.insert("a", 1)

    -- A Box is a pointer: what it points to may be changed through a
    -- variable that is not `mut`
    let b = Box_new(Counter { count: 0, seen: [] })
    b.bump()
    b.count = 3
    (*b).count = 4
    b.seen.push(1)
    let mut ys: List[int] = [1]
    ys.push(2)
    grow(ys)
    grow([1, 2])
    let mut d = Counter { count: 0, seen: [] }
    d.bump()
    d.seen.push(1)
    let mut years: Map[string, int] = Map_new()
    years.insert("a", 1)
    for item in ys {
        println(item)
    }
    return c.value() + ages.len() + years.len() + total(ys) + n
}
