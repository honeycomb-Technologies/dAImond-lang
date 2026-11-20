module test

const LIMIT: int = 10
const NAME: string = "n"
const RATIO: float = 2.5
const PLAIN = 7

fn main() -> int {
    println(int_to_string(LIMIT + PLAIN) + " " + NAME + " " + float_to_string(RATIO))
    return 0
}
