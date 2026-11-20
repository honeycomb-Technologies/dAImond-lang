module test

fn main() {
    let r1 = fs_mkdir("/tmp/dm_test_fs_readdir")
    file_write("/tmp/dm_test_fs_readdir/a.txt", "aaa")
    let entries = fs_readdir("/tmp/dm_test_fs_readdir")
    println(entries)
    let r2 = fs_remove("/tmp/dm_test_fs_readdir/a.txt")
    let r3 = fs_remove("/tmp/dm_test_fs_readdir")
}