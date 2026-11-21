module test

struct A { x: int }
struct B { x: int }

const LIMIT: int = "ten"

fn nothing() {
    println("nothing")
}

fn id[T](x: T) -> T {
    return x
}

fn main() -> int {
    let a: A = B { x: 1 }
    let half: float = 3
    let whole: int = 2.5
    let truth: int = 1 < 2
    let maybe: Option[int] = Some("one")
    let names: List[int] = ["a"]
    let same: string = id(3)
    let length: string = len("abc")
    let v = nothing()
    let mut n = 3
    n = "three"
    n = nothing()
    let mut p = A { x: 1 }
    p.x = "one"
    let mut text = "a"
    text += 1
    return a.x
}
