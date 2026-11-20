module test

fn main() {
    if args_len() > 1 {
        -- The child: echo the lines of standard input
        let mut i = 0
        while i < 6 {
            let line = read_line()
            println(int_to_string(len(line)) + "[" + line + "]")
            i = i + 1
        }
        return
    }
    -- The parent runs this program again with a pipe as its input
    let child = " | " + args_get(0) + " child"
    let r1 = system("printf 'first\\n\\nthird line\\r\\nlast, without a newline'" + child)
    let r2 = system("printf ''" + child)
    println(int_to_string(r1) + " " + int_to_string(r2))
}