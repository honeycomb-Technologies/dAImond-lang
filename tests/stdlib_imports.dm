module stdlib_imports

import std.os
import std.fs
import std.collections

fn main() {
    println("os args count: " + int_to_string(os_args_len()))
    println("first arg: " + os_args_get(0))

    -- Test fs operations with builtins
    let tmp = "/tmp/_dm_test_stdlib"
    fs_write(tmp, "hello from dAImond")
    let content = fs_read(tmp)
    println(content)
    println("file exists: " + bool_to_string(fs_exists(tmp)))
    let _ = fs_remove(tmp)
    println("empty check: " + bool_to_string(is_empty_count(0)))
    println("done")
}
