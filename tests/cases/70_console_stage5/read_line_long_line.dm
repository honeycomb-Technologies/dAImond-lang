module test

fn main() {
    if args_len() > 1 {
        let line = read_line()
        println(int_to_string(len(line)) + " " + substr(line, 4990, 20) + " [" + read_line() + "]")
        return
    }
    -- A line of 5000 bytes, then a short one
    let r = system("{ head -c 5000 /dev/zero | tr '\\0' x; printf '\\nnext\\n'; } | " + args_get(0) + " child")
    println(int_to_string(r))
}