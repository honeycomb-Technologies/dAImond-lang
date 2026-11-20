module test

fn main() {
    let ext = path_extension("hello.dm")
    println(ext)
    let stem = path_stem("hello.dm")
    println(stem)
}