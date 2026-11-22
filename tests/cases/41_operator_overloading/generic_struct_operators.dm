module test
-- The operator methods of a generic struct, for each of its instances
struct Vec2[T] { x: T, y: T }
impl Vec2[T] {
    fn of(x: T, y: T) -> Vec2[T] { return Vec2 { x, y } }
    fn add(self, o: Vec2[T]) -> Vec2[T] { return Vec2 { x: self.x + o.x, y: self.y + o.y } }
    fn sub(self, o: Vec2[T]) -> Vec2[T] { return Vec2 { x: self.x - o.x, y: self.y - o.y } }
    fn swap(self) -> Vec2[T] { return Vec2 { x: self.y, y: self.x } }
    fn eq(self, o: Vec2[T]) -> bool { return self.x == o.x and self.y == o.y }
    fn lt(self, o: Vec2[T]) -> bool { return self.x < o.x }
}
struct Wrap[T] { inner: Vec2[T], label: string }
fn sum[T](vs: List[Vec2[T]], zero: Vec2[T]) -> Vec2[T] {
    let mut t = zero
    for v in vs { t = t + v }
    return t
}
fn main() {
    let v = Vec2.of(1, 2) + Vec2.of(10, 20)
    println(int_to_string(v.x * 100 + v.y))
    let f = Vec2.of(0.5, 1.5) + Vec2 { x: 1.0, y: 1.0 }
    println(float_to_string(f.swap().x))
    println(bool_to_string(v == Vec2.of(11, 22)) + bool_to_string(v == v.swap()) + bool_to_string(v != v.swap()))
    let s = Vec2.of("a", "b") + Vec2.of("c", "d")
    println(s.x + s.y)
    let mut acc = Vec2.of(0, 0)
    acc += v
    acc += v - Vec2.of(1, 1)
    acc -= Vec2.of(1, 0)
    println(int_to_string(acc.x) + " " + int_to_string(acc.y))
    println(bool_to_string(v < acc) + bool_to_string(acc < v))
    let w = Wrap { inner: v + v + v, label: "w" }
    println(w.label + int_to_string(w.inner.x) + " " + int_to_string((w.inner - v).swap().x))
    let total = sum([v, acc, Vec2.of(1, 1)], Vec2.of(0, 0))
    println(int_to_string(total.x) + " " + int_to_string(total.y))
    let ft = sum([f, f], Vec2.of(0.0, 0.0))
    println(float_to_string(ft.x + ft.y))
}
