module test

fn main() {
    -- A file in /proc reports a size of 0
    let status = file_read("/proc/self/status")
    println(bool_to_string(string_contains(status, "Pid:")))
}