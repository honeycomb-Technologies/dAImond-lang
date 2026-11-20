module test

fn main() {
    let dir = path_dirname("/foo/bar/baz.dm")
    println(dir)
    let base = path_basename("/foo/bar/baz.dm")
    println(base)
}