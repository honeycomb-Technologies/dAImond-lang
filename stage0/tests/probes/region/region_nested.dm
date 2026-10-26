module r6

fn main() {
    region outer {
        let mut s: string = "mid"
        region inner {
            s = "a" + "b"
        }
        println(s)
    }
}
