module test

fn p(path: string) {
    println("[" + path + "] dir=[" + path_dirname(path) + "] base=[" + path_basename(path) + "] ext=[" + path_extension(path) + "] stem=[" + path_stem(path) + "]")
}

fn main() {
    p("")
    p("/")
    p("a")
    p("/a")
    p("a/")
    p("a/b")
    p("/foo/bar/baz.dm")
    p("a//b")
    p("/foo/")
    p(".bashrc")
    p("a.tar.gz")
    p("dir.d/file")
    p("dir.d/")
    p("a.")
    p("..")
    p(".")
    p("a/.b")
    p("noext")
    p("/x.y/z")
    p("//")
    println("join [" + path_join("", "") + "][" + path_join("a", "") + "][" + path_join("", "b") + "][" + path_join("a", "b") + "][" + path_join("a/", "b") + "][" + path_join("a", "/b") + "][" + path_join("/", "b") + "][" + path_join("src", "main.zig") + "]")
}