module test

const TOO_FAR = comptime { 1 << 64 }

fn main() {
    println(TOO_FAR)
}
