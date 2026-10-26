module r2

fn main() {
    let mut s: string = "outer"
    region r {
        let t = "a" + "b"
        let u = t
        s = u
    }
    println(s)
}
