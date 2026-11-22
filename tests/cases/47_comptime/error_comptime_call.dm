module error_comptime_call

const TEXT = comptime file_read("/etc/hostname")

fn main() {
    println(TEXT)
}
