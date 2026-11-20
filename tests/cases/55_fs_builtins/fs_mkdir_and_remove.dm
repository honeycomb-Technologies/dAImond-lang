module test

fn main() {
    let r1 = fs_mkdir("/tmp/dm_test_fs_mkdir")
    println(int_to_string(r1))
    let r2 = fs_remove("/tmp/dm_test_fs_mkdir")
    println(int_to_string(r2))
}