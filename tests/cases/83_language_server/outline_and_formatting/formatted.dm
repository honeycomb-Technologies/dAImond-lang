module main

import nothing_here_matters

const LIMIT: int = 3

struct Pair[T] {
    first: T,
    second: T
}

enum Light { Red, Green }

trait Named {
    fn name(self) -> string
}

impl Named for Light {
    fn name(self) -> string {
        match self {
            Light.Red => { return "red" }
            Light.Green => { return "green" }
        }
        return ""
    }
}

fn main() -> int {
    let name = "x"
    println("né 😀" + name)
    let last = name
    return 0
}
