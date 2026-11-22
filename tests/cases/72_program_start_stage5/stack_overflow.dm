module stack_overflow

-- A function that calls itself until the stack is full ends the
-- program with a message and exit code 1, and what the program
-- printed before is written. The system's answer to a full stack is
-- to kill the program with a signal, and no word of why.

fn down(n: int) -> int {
    return down(n + 1) + 1
}

fn main() {
    println("before")
    println(int_to_string(down(0)))
    println("never")
}
