module test

fn main() {
    print("")
    print("a")
    print("b\n")
    println("")
    println("line")
    eprint("")
    eprint("e1")
    eprintln("")
    eprintln("e2")
    -- More than stdio buffers at once
    let mut i = 0
    while i < 3000 {
        print("0123456789")
        i = i + 1
    }
    println("")
    print("no newline at the end")
}