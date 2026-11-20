module test

enum Color {
    Red,
    Green,
    Blue
}

fn color_value(c: Color) -> int {
    if c == Color::Red {
        return 1
    }
    if c == Color::Green {
        return 2
    }
    return 3
}

fn main() {
    let c = Color::Red
    println(color_value(c))
}