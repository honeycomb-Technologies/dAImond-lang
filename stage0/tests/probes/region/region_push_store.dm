module r4

fn main() {
    let mut xs: List[string] = []
    region r {
        xs.push("a" + "b")
    }
    println(int_to_string(len(xs)))
}
