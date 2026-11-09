module r1

fn main() {
    let mut n: int = 0
    let mut s: string = "outer"
    let plain = "p"
    region r {
        let t = "hel" + "lo"
        println(t)
        println(s)
        n = 42
        s = plain
    }
    println(int_to_string(n))
    println(s)
}
