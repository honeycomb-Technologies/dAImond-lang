module test

fn main() {
    -- The program's own path names a place no other test writes to
    let p = args_get(0) + ".data"
    println(int_to_string(args_len()) + " [" + args_get(1) + "][" + args_get(0 - 1) + "]")
    println(bool_to_string(file_exists(p)))
    file_write(p, "hello\n")
    println(bool_to_string(file_exists(p)))
    print(file_read(p))
    file_append(p, "world\n")
    file_append(p, "")
    let both = file_read(p)
    println(int_to_string(len(both)) + ":" + string_replace(both, "\n", "|"))
    file_write(p, "short")
    println(file_read(p))
    file_write(p, "")
    println("[" + file_read(p) + "] " + bool_to_string(file_exists(p)))
    file_append(p + "2", "made by append")
    println(file_read(p + "2"))
    -- A file larger than one read buffer
    let mut big = ""
    let mut i = 0
    while i < 3000 {
        big = big + "line " + int_to_string(i) + "\t\r\n"
        i = i + 1
    }
    file_write(p, big)
    let back = file_read(p)
    println(int_to_string(len(back)) + " " + bool_to_string(back == big))
    println(bool_to_string(file_exists(p + "/missing")) + " " + bool_to_string(file_exists("")))
    println(int_to_string(system("exit 3")) + " " + int_to_string(system("true")))
}