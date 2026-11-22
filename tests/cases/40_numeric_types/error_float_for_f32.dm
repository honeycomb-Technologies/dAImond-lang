module test
-- A float that is no constant does not go where an f32 is expected
-- without `as f32`
struct Plain { a: i32, b: f32 }
fn half(x: f32) -> float { return x / 2.0 }
fn narrow(d: float) -> f32 { return d }
fn main() {
    let d: float = 2.5
    let x: f32 = d
    let mut y: f32 = 1.0
    y = d
    y += d
    println(float_to_string(half(d)))
    let p = Plain { a: 1, b: d }
    let xs: List[f32] = [d]
    let o: Option[f32] = Some(d)
    let z: f32 = if d > 1.0 { d } else { 0.5 }
}
