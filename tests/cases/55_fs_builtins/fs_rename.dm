module test

fn main() {
    let r0 = fs_mkdir("/tmp/dm_test_fs_rename")
    file_write("/tmp/dm_test_fs_rename/old.txt", "data")
    let r = fs_rename("/tmp/dm_test_fs_rename/old.txt", "/tmp/dm_test_fs_rename/new.txt")
    println(int_to_string(r))
    let exists = file_exists("/tmp/dm_test_fs_rename/new.txt")
    println(bool_to_string(exists))
    let r2 = fs_remove("/tmp/dm_test_fs_rename/new.txt")
    let r3 = fs_remove("/tmp/dm_test_fs_rename")
}