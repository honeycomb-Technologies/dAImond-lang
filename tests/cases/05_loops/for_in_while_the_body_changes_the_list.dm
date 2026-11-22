module main

fn main() {
    let mut xs: List[int] = [1, 2, 3, 4, 5, 6]
    let mut seen = 0
    for x in xs {
        seen = seen + x
        if xs.len() > 3 { xs.pop() }
    }
    println(int_to_string(seen) + " " + int_to_string(xs.len()))
    let mut ys: List[int] = [1, 2, 3]
    let mut n = 0
    for y in ys {
        n = n + 1
        if n < 4 { ys.push(y * 10) }
    }
    println(int_to_string(n) + " " + int_to_string(ys.len()))
}
