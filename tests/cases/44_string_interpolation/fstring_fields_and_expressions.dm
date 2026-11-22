module test

struct P {
    name: string,
    age: int,
    tall: bool,
    h: float
}

fn main() {
    -- A value in an f-string is printed as its own type, whatever
    -- expression gives it
    let p = P { name: "ann", age: 30, tall: true, h: 1.5 }
    println(f"{p.name} is {p.age}, tall: {p.tall}, {p.h}")
    let xs = ["a", "b"]
    println(f"{xs[1]}{xs.len()} {p.age + 1} {string_to_upper(p.name)}")
    let small: i16 = -300
    println(f"{small} {small * 2} {p.age > 18}")
}
