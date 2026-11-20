module test
struct Counter { n: int, }
impl Counter {
    fn inc(mut self) {
        self.n = self.n + 1
    }
    fn get(self) -> int {
        return self.n
    }
}
fn main() {
    let mut c = Counter { n: 0 }
    c.inc()
    c.inc()
    c.inc()
    println(int_to_string(c.get()))
}