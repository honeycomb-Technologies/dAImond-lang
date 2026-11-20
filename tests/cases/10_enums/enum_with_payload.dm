module test

enum Maybe {
    Nothing,
    Just(int)
}

fn get_value(opt: Maybe) -> int {
    return 42
}

fn main() {
    let x = Maybe::Just(42)
    println(get_value(x))
}