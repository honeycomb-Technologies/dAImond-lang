module main

-- Writes a program whose one string literal has 6,000 escapes, or,
-- given the argument "lines", a file of 20,000 lines to format. A
-- compiler that builds a text by adding to it piece by piece copies
-- the text so far each time and frees none of it: it took 1.7 GB for
-- this literal, and 2 GB to format these lines.
fn main() {
    println("module main")
    println("fn main() {")
    let mut i = 0
    if args_len() > 1 {
        while i < 20000 {
            println("let x = 1")
            i = i + 1
        }
        println("}")
        return
    }
    print("    let s = \"")
    while i < 6000 {
        print("line " + int_to_string(i) + "\\n")
        i = i + 1
    }
    println("\"")
    println("    println(int_to_string(len(s)))")
    println("}")
}
