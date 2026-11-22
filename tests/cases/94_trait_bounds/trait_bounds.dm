module trait_bounds

-- fn f[T: Show + Named](x: T): the type a call gives for T must
-- implement the traits

trait Show {
    fn show(self) -> string
}

trait Named {
    fn name(self) -> string
}

struct Point {
    x: int,
    y: int
}

struct Tag {
    text: string
}

enum Light {
    On,
    Off
}

impl Show for Point {
    fn show(self) -> string {
        return "(" + int_to_string(self.x) + ", " + int_to_string(self.y) + ")"
    }
}

impl Named for Point {
    fn name(self) -> string {
        return "point"
    }
}

impl Show for Tag {
    fn show(self) -> string {
        return "#" + self.text
    }
}

impl Show for Light {
    fn show(self) -> string {
        match self {
            Light.On => { return "on" }
            Light.Off => { return "off" }
        }
        return ""
    }
}

fn shown[T: Show](x: T) -> string {
    return "<" + x.show() + ">"
}

fn described[T: Show + Named](x: T) -> string {
    return x.name() + " " + x.show()
}

fn both[A: Show, B: Show](a: A, b: B) -> string {
    return a.show() + " and " + b.show()
}

fn all[T: Show](xs: List[T]) -> string {
    let mut out = ""
    for x in xs {
        out = out + x.show() + ";"
    }
    return out
}

-- A parameter without a bound takes any type
fn pair[T: Show, U](x: T, other: U) -> string {
    return x.show()
}

fn main() {
    let p = Point { x: 1, y: 2 }
    println(shown(p))
    println(shown(Tag { text: "t" }))
    println(shown(Light.On))
    println(described(p))
    println(both(p, Tag { text: "u" }))
    println(all([Tag { text: "a" }, Tag { text: "b" }]))
    println(pair(Light.Off, 5))
}
