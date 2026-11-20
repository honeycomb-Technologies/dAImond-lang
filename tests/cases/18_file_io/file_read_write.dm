module test

fn main() {
    file_write("/tmp/daimond_test_io.txt", "hello from daimond")
    let content = file_read("/tmp/daimond_test_io.txt")
    println(content)
}