module test

fn main() {
    if args_len() > 1 {
        -- The child: standard output is a pipe, so it holds what is
        -- printed until a flush or the end of the program
        print("one ")
        flush()
        eprint("two ")
        print("three ")
        flush()
        eprint("four ")
        print("five")
        flush()
        flush()
        eprintln("")
        return
    }
    -- The parent runs this program again with both streams on its own
    -- standard output
    let status = system(args_get(0) + " child 2>&1")
    println(int_to_string(status))
}