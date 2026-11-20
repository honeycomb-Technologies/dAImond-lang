module main

fn log(message: string) with [IO] {
    println(message)
}

fn save(path: string) with [IO] {
    log(path)
    file_write(path, "saved")
}

fn main() -> int with [IO, FileSystem] {
    save("out.txt")
    return 0
}
