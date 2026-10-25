module probe

fn main() {
    let mut s: string = "outer"
    region r {
        s = "hel" + "lo"
    }
    println(s)
}
