module test
struct Foo { x: int, }
impl Foo {
    fn bar(self) -> int {
        return self.x
    }
}
fn main() {
    let f = Foo { x: 1 }
    println(int_to_string(f.bar()))
}