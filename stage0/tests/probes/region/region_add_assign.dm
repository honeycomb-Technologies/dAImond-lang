module r7

fn main() {
    let mut s: string = "outer"
    region r {
        s += "more"
    }
    println(s)
}
