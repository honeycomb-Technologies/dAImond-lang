module main

enum Shade {
    Dark,
    Tint(int, int)
}

fn first(xs: List[int]) -> Option[int] {
    if xs.len() == 0 { return None }
    return Some(xs[0])
}

fn main() -> int {
    let shade = Shade.Tint(3, 4)
    let mut total = 0
    if let Some(head) = first([7, 8]) {
        total = total + head
    }
    if let Shade.Tint(level, depth) = shade {
        total = total + level * depth
    } else if let Some(other) = first([total, 2]) {
        total = other
    }
    println(int_to_string(total))
    return 0
}
