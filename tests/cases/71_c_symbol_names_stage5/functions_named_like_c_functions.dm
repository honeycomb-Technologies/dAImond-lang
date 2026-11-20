module test

-- Functions that have the names of C functions the runtime calls

fn rename(from: string, to: string) -> int {
    return 7
}

fn remove(path: string) -> int {
    return 8
}

fn strlen(s: string) -> int {
    return 100
}

fn getenv(name: string) -> string {
    return "mine"
}

fn memcmp(a: string, b: string) -> int {
    return 5
}

-- Box_new allocates with C's malloc, not with this one
fn malloc(n: int) -> int {
    return 0
}

fn free(p: int) -> int {
    return p + 1
}

struct Node {
    v: int
}

fn main() {
    let p = args_get(0) + ".data"
    file_write(p, "x")
    let r = fs_rename(p, p + "2")
    println(int_to_string(rename("a", "b")) + " " + int_to_string(r) + " " + bool_to_string(file_exists(p + "2")))
    let d = fs_remove(p + "2")
    println(int_to_string(remove("a")) + " " + int_to_string(d) + " " + bool_to_string(file_exists(p + "2")))
    println(int_to_string(strlen("abc")) + " " + bool_to_string(len(args_get(0)) < 100))
    println(getenv("PATH") + " " + bool_to_string(env_get("PATH") != "mine"))
    let b = Box_new(Node { v: 41 })
    println(int_to_string(b.v + malloc(8)) + " " + int_to_string(free(4)))
    println(int_to_string(memcmp("a", "b")) + " " + bool_to_string("abc" == "abd") + " " + bool_to_string("abc" == "abc"))
}