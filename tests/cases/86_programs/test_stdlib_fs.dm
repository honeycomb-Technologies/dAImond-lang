-- Integration test for stdlib filesystem builtins

module test_stdlib_fs

fn main() {
    -- Test fs_mkdir
    let r1 = fs_mkdir("/tmp/dm_test_stdlib_dir")
    assert(r1 == 0)
    println("mkdir: PASS")

    -- Test file_write and file_read
    file_write("/tmp/dm_test_stdlib_dir/test.txt", "hello daimond")
    let content = file_read("/tmp/dm_test_stdlib_dir/test.txt")
    assert(content == "hello daimond")
    println("write+read: PASS")

    -- Test file_exists
    assert(file_exists("/tmp/dm_test_stdlib_dir/test.txt"))
    println("exists: PASS")

    -- Test fs_readdir
    let entries = fs_readdir("/tmp/dm_test_stdlib_dir")
    assert(entries == "test.txt")
    println("readdir: PASS")

    -- Test fs_rename
    let r2 = fs_rename("/tmp/dm_test_stdlib_dir/test.txt", "/tmp/dm_test_stdlib_dir/renamed.txt")
    assert(r2 == 0)
    assert(file_exists("/tmp/dm_test_stdlib_dir/renamed.txt"))
    println("rename: PASS")

    -- Test fs_getcwd
    let cwd = fs_getcwd()
    assert(len(cwd) > 0)
    println("getcwd: PASS")

    -- Test env_get
    let home = env_get("HOME")
    assert(len(home) > 0)
    println("env_get: PASS")

    -- Cleanup
    let r3 = fs_remove("/tmp/dm_test_stdlib_dir/renamed.txt")
    assert(r3 == 0)
    let r4 = fs_remove("/tmp/dm_test_stdlib_dir")
    assert(r4 == 0)
    println("cleanup: PASS")

    println("All stdlib fs tests passed!")
}
