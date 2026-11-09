module probe

fn main() {
    let mark = mem_mark()
    let s = "hello" + " world"
    mem_sweep(mark)
    println(s)
}
