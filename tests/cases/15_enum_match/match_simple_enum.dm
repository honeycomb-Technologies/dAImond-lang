module test

enum Color {
    Red,
    Green,
    Blue
}

fn color_name(c: Color) -> int {
    match c {
        Color::Red => { return 1 }
        Color::Green => { return 2 }
        Color::Blue => { return 3 }
    }
    return 0
}

fn main() {
    println(color_name(Color::Red))
    println(color_name(Color::Green))
    println(color_name(Color::Blue))
}