module test
struct Point { x: int, y: int, }
impl Point {
    fn sum(self) -> int {
        return self.x + self.y
    }
}
fn main() {
    let p = Point { x: 3, y: 7 }
    println(int_to_string(p.sum()))
}