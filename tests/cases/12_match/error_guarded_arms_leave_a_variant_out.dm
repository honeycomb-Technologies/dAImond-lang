module test

enum Color {
    Red,
    Blue(int)
}

fn shade(c: Color) -> string {
    return match c {
        Color.Blue(n) if n > 5 => "deep blue",
        Color.Red => "red"
    }
}

fn sign(o: Option[int]) -> string {
    return match o {
        Some(v) if v > 0 => "positive",
        Some(v) if "v" => "odd",
        None => "nothing"
    }
}

fn main() {
    println(shade(Color.Red) + sign(None))
}
