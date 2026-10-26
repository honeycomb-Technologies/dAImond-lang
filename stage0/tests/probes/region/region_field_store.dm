module r5

struct Holder {
    text: string
}

fn main() {
    let mut h = Holder { text: "outer" }
    region r {
        h.text = "a" + "b"
    }
    println(h.text)
}
