module test

fn report(path: string, times: int) -> int with [Console, FileSystem] {
    let mut i = 0
    while i < times {
        if file_exists(path) == false {
            println("no file " + int_to_string(i))
        }
        i = i + 1
    }
    return i
}

fn main() -> int {
    println(int_to_string(report("/nonexistent/file", 2)))
    return 0
}
