module test

struct Pair[A, B] {
    first: A,
    second: B
}

struct Bag[T] {
    items: List[T],
    pairs: List[Pair[T, int]]
}

impl Bag[T] {
    fn add(mut self, x: T) {
        self.items.push(x)
        let p = Pair[T, int] { first: x, second: self.items.len() }
        self.pairs.push(p)
    }
    fn last(self) -> T {
        return self.items[self.items.len() - 1]
    }
    fn count(self) -> int {
        return self.items.len()
    }
    fn twice(mut self, x: T) -> int {
        self.add(x)
        self.add(x)
        return self.count()
    }
    fn copy_of(self) -> Self {
        let mut out = Bag[T] { items: [], pairs: [] }
        for it in self.items {
            out.add(it)
        }
        return out
    }
}

struct Holder {
    names: Bag[string],
    nums: Bag[int]
}

fn make_bag() -> Bag[string] {
    let mut b = Bag[string] { items: [], pairs: [] }
    b.add("x")
    return b
}

fn total(b: Bag[int]) -> int {
    let mut s = 0
    for v in b.items { s = s + v }
    return s
}

fn grow(mut b: Bag[int], n: int) {
    b.add(n)
}

fn main() {
    let mut h = Holder { names: make_bag(), nums: Bag[int] { items: [], pairs: [] } }
    h.names.add("y")
    h.nums.add(10)
    h.nums.add(32)
    grow(h.nums, 100)
    println(h.names.last())
    println(int_to_string(h.names.count()))
    println(int_to_string(total(h.nums)))
    println(int_to_string(h.nums.pairs[2].second))
    println(h.names.pairs[1].first)
    let n = h.names.twice("z")
    println(int_to_string(n))
    let c = h.names.copy_of()
    println(int_to_string(c.count()) + " " + c.last())
    let mut bags: List[Bag[int]] = []
    bags.push(h.nums)
    bags[0].add(7)
    println(int_to_string(bags[0].count()) + " " + int_to_string(h.nums.count()))
    let p: Pair[string, Bag[int]] = Pair[string, Bag[int]] { first: "k", second: bags[0] }
    println(p.first + int_to_string(p.second.last()))
}