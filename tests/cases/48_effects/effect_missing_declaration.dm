module test
fn read_data() -> string with [IO] {
    return file_read("nonexistent.txt")
}
fn main() {
    let s = read_data()
}