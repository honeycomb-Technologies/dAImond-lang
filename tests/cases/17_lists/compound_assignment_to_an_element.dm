module main

struct S { xs: List[int] }

fn main() {
    let mut xs: List[int] = [10, 20, 30]
    xs[1] += 5
    xs[2] -= 1
    xs[0] *= 3
    println(int_to_string(xs[0]) + " " + int_to_string(xs[1]) + " " + int_to_string(xs[2]))
    let mut fs: List[float] = [1.5, 2.5]
    fs[0] += 1.0
    println(float_to_string(fs[0]))
    let mut ss: List[string] = ["a", "b"]
    ss[1] += "c"
    println(ss[1])
    let mut s = S { xs: [1, 2] }
    s.xs[1] += 40
    println(int_to_string(s.xs[1]))
    let mut ns: List[i32] = [1 as i32, 2 as i32]
    ns[0] += 7
    println(int_to_string(ns[0] as int))
}
