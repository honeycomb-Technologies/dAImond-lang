module test
struct Counter { n: int, log: List[int] }
impl Counter {
    fn bump(mut self) {
        self.n = self.n + 1
        self.log.push(self.n)
    }
}
struct Holder { c: Counter }
fn grow(mut c: Counter) {
    c.bump()
}
fn main() {
    let mut h = Holder { c: Counter { n: 0, log: [] } }
    h.c.bump()
    grow(h.c)
    let mut cs: List[Counter] = []
    cs.push(Counter { n: 10, log: [] })
    cs[0].bump()
    grow(cs[0])
    println(int_to_string(h.c.n) + " " + int_to_string(h.c.log.len()))
    println(int_to_string(cs[0].n) + " " + int_to_string(cs[0].log[1]))
}