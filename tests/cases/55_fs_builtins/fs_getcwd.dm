module test

fn main() {
    let cwd = fs_getcwd()
    if len(cwd) > 0 {
        println("got cwd")
    }
}