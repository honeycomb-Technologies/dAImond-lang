module main

fn twice(x: i32) -> i32 {
    return x + x
}

fn fails(v: int) -> Result[int, i32] {
    return Err(v)
}

fn main() {
    let big: int = 4294967301
    let f = |x: i32| -> i32 x + 1
    let r = f(big)
    let g: fn(i32) -> i32 = twice
    let r2 = g(big)
    let b: Box[i32] = Box_new(big)
    let mut k: i8 = 1 as i8
    k += big
    let mut xs: List[i32] = [1 as i32]
    xs[0] += big
    let ok = f(7)
    let ok2 = g(7)
    let ok3: Box[i32] = Box_new(7)
}
