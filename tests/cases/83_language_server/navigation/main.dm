module main

import shapes

const COATS: int = 2

fn describe(shade: Shade) -> string {
    match shade {
        Shade.Dark => { return "dark" }
        Shade.Tint(level) => { return "tint " + int_to_string(level) }
    }
    return ""
}

fn main() -> int {
    let room = Rect.square(4)
    let mut total = 0
    let coats: List[int] = [1, COATS]
    for coat in coats {
        total = total + paint_needed(room, coat)
    }
    -- total is not a name here, nor in "total"
    println(int_to_string(total) + " " + describe(Shade.Tint(room.width)))
    return 0
}
