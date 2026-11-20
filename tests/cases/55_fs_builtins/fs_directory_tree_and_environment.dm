module test

fn sorted(text: string) -> string {
    if len(text) == 0 { return "(none)" }
    let mut names = string_split(text, "\n")
    let n = names.len()
    let mut i = 1
    while i < n {
        let mut j = i
        while j > 0 and names[j - 1] > names[j] {
            let t = names[j]
            names[j] = names[j - 1]
            names[j - 1] = t
            j = j - 1
        }
        i = i + 1
    }
    let mut out = int_to_string(n) + ":"
    i = 0
    while i < n {
        if i < 5 or i > n - 3 { out = out + names[i] + "," }
        i = i + 1
    }
    return out
}

fn main() {
    -- The program's own path names a place no other test writes to
    let d = args_get(0) + ".d"
    let m1 = fs_mkdir(d + "/a/b/c")
    let m2 = fs_mkdir(d + "/a/b/c")
    let m3 = fs_mkdir(d + "/one/")
    println("mkdir " + int_to_string(m1) + " " + int_to_string(m2) + " " + int_to_string(m3))
    file_write(d + "/a/file.txt", "x")
    let m4 = fs_mkdir(d + "/a/file.txt/sub")
    let m5 = fs_mkdir("")
    println("mkdir under a file " + int_to_string(m4) + ", of no name " + int_to_string(m5))
    println("readdir " + sorted(fs_readdir(d)) + " " + sorted(fs_readdir(d + "/a")) + " " + sorted(fs_readdir(d + "/a/b/c")) + " " + sorted(fs_readdir(d + "/missing")) + " " + sorted(fs_readdir(d + "/a/file.txt")) + " " + sorted(fs_readdir("")))
    -- More names than the first buffer holds
    let mut i = 0
    while i < 300 {
        file_write(d + "/one/entry_with_a_long_name_" + int_to_string(i) + ".txt", "")
        i = i + 1
    }
    let many = fs_readdir(d + "/one")
    println("many " + sorted(many) + " " + int_to_string(len(many)))
    let r1 = fs_rename(d + "/a/file.txt", d + "/a/moved.txt")
    let r2 = fs_rename(d + "/a/file.txt", d + "/a/again.txt")
    println("rename " + int_to_string(r1) + " " + int_to_string(r2) + " " + bool_to_string(file_exists(d + "/a/moved.txt")) + " " + bool_to_string(file_exists(d + "/a/file.txt")))
    let d1 = fs_remove(d + "/a/moved.txt")
    let d2 = fs_remove(d + "/a/moved.txt")
    let d3 = fs_remove(d + "/a")
    let d4 = fs_remove(d + "/a/b/c")
    let d5 = fs_remove("")
    println("remove " + int_to_string(d1) + " " + int_to_string(d2) + " " + int_to_string(d3) + " " + int_to_string(d4) + " " + int_to_string(d5))
    println("env [" + env_get("DM_TEST_SURELY_NOT_SET") + "][" + env_get("") + "] " + bool_to_string(len(env_get("PATH")) > 0))
    println(bool_to_string(len(fs_getcwd()) > 0) + " " + bool_to_string(starts_with(fs_getcwd(), "/")))
}