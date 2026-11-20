module test

fn main() {
    if args_len() > 1 {
        -- The child: standard input is a file
        let a = read_key()
        let b = read_key_nb()
        println(int_to_string(a) + " " + int_to_string(b))
        let line = read_line()
        println("[" + line + "]")
        let c = read_key()
        let d = read_key()
        println(int_to_string(c) + " " + int_to_string(d))
        let mut count = 0
        let mut total = 0
        let mut k = read_key()
        while k != 0 - 1 {
            count = count + 1
            total = total + k
            k = read_key()
        }
        println(int_to_string(count) + " " + int_to_string(total))
        -- At the end of input both give -1, again and again
        let e1 = read_key()
        let e2 = read_key_nb()
        let e3 = read_key()
        println(int_to_string(e1) + " " + int_to_string(e2) + " " + int_to_string(e3))
        return
    }
    let input = args_get(0) + ".in"
    file_write(input, "AB rest of line\n\n" + chr(200) + "xyz")
    let status = system(args_get(0) + " child < " + input)
    let removed = fs_remove(input)
    println(int_to_string(status) + " " + int_to_string(removed))
}