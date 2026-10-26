module r8

fn main() {
    let mut s: string = "outer"
    let n = 3
    region r {
        s = f"n is {int_to_string(n)}"
    }
    println(s)
}
