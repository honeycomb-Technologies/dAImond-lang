module stdlib_imports

import std.os
import std.fs
import std.collections

fn main() {
    println("os args count: " + int_to_string(os_args_len()))
    println("first arg: " + os_args_get(0))

    -- Test fs operations through the stdlib wrappers
    let tmp = "/tmp/_dm_test_stdlib"
    write(tmp, "hello from dAImond")
    let content = read(tmp)
    println(content)
    println("file exists: " + bool_to_string(exists(tmp)))
    let _ = remove_file(tmp)
    println("empty check: " + bool_to_string(is_empty_count(0)))
    println("done")
}
