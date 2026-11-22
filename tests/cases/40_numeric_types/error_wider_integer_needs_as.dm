module error_wider_integer_needs_as

-- A wider integer does not go to a narrower place without `as`, and a
-- constant must fit the place

struct S { n: i8, m: i32 }
enum E { A(i8), B }

fn take(v: i8) -> int { return v as int }
fn wide() -> int { return 5000000000 }

impl S {
    fn set(mut self, v: i8) { self.n = v }
}

fn main() -> int {
    let big: int = 5000000000
    let mid: i32 = 70000
    let a: i8 = big
    let b: i8 = mid
    let c: i32 = big + 1
    let d: i8 = 300
    let e: u8 = -1
    let f: i32 = wide()
    let g: i8 = if mid > 0 { 1 } else { 300 }
    let h: i16 = if mid > 0 { mid } else { 0 }
    println(take(mid))
    println(take(300))
    let mut s = S { n: mid, m: big }
    s.set(mid)
    let s2 = S { n: 200, m: 1 }
    let en = E.A(mid)
    let o: Option[i8] = Some(mid)
    let xs: List[i8] = [mid, 300]
    let mut ys: List[i32] = []
    ys.push(big)
    let mut m: Map[string, i32] = Map_new()
    m.insert("a", big)
    return 0
}
