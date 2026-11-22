module dynamic_dispatch

-- A `dyn Shape` holds a value of any type that implements Shape, and
-- a method call on it reaches the method of the type it holds

trait Shape {
    fn area(self) -> float
    fn name(self) -> string

    fn describe(self) -> string {
        return self.name() + " of area " + float_to_string(self.area())
    }
}

trait Counter {
    fn bump(mut self, by: int)
    fn count(self) -> int
}

struct Circle {
    radius: float
}

struct Rect {
    w: float,
    h: float
}

enum Blob {
    Small,
    Sized(float)
}

struct Tally {
    n: int
}

struct Scene {
    main: dyn Shape,
    title: string
}

impl Shape for Circle {
    fn area(self) -> float {
        return 3.0 * self.radius * self.radius
    }
    fn name(self) -> string {
        return "circle"
    }
}

impl Shape for Rect {
    fn area(self) -> float {
        return self.w * self.h
    }
    fn name(self) -> string {
        return "rect"
    }
    fn describe(self) -> string {
        return "a " + float_to_string(self.w) + " by " + float_to_string(self.h) + " rect"
    }
}

impl Shape for Blob {
    fn area(self) -> float {
        match self {
            Blob.Small => { return 0.5 }
            Blob.Sized(a) => { return a }
        }
    }
    fn name(self) -> string {
        return "blob"
    }
}

impl Counter for Tally {
    fn bump(mut self, by: int) {
        self.n = self.n + by
    }
    fn count(self) -> int {
        return self.n
    }
}

-- A parameter takes every type that implements the trait
fn report(s: dyn Shape) -> string {
    return s.name() + ": " + float_to_string(s.area())
}

-- A function returns one or another
fn make(kind: int) -> dyn Shape {
    if kind == 0 { return Circle { radius: 1.0 } }
    if kind == 1 { return Rect { w: 2.0, h: 3.0 } }
    return Blob.Sized(9.0)
}

-- A value that is made where a `dyn Shape` is expected inside it
fn find(kind: int) -> Option[dyn Shape] {
    if kind < 0 { return None }
    if kind == 0 { return Some(Circle { radius: 1.0 }) }
    return Some(make(kind))
}

fn checked(kind: int) -> Result[dyn Shape, string] {
    if kind > 2 { return Err("no shape " + int_to_string(kind)) }
    return Ok(Rect { w: 1.0, h: 4.0 })
}

fn total(shapes: List[dyn Shape]) -> float {
    let mut sum = 0.0
    for s in shapes {
        sum = sum + s.area()
    }
    return sum
}

fn bump_twice(c: dyn Counter) {
    c.bump(1)
    c.bump(1)
}

fn main() {
    let c = Circle { radius: 2.0 }
    let r = Rect { w: 2.0, h: 5.0 }
    println(report(c))
    println(report(r))
    println(report(Blob.Small))
    println(report(make(2)))

    -- A variable holds one value and then another
    let mut s: dyn Shape = c
    println(s.describe())
    s = r
    println(s.describe())
    s = make(2)
    println(s.describe())

    -- In a list
    let mut shapes: List[dyn Shape] = []
    shapes.push(c)
    shapes.push(r)
    shapes.push(make(0))
    shapes.push(Blob.Sized(1.5))
    println(float_to_string(total(shapes)))
    for one in shapes {
        println(one.name())
    }
    println(shapes[1].describe())
    let cast = [c as dyn Shape, r as dyn Shape]
    println(float_to_string(total(cast)))

    -- In a field and in an Option
    let scene = Scene { main: r, title: "scene" }
    println(scene.title + ": " + scene.main.name())
    let maybe: Option[dyn Shape] = Some(c as dyn Shape)
    match maybe {
        Some(found) => println(found.name()),
        None => println("nothing")
    }

    let both: List[dyn Shape] = [c, c]
    println(float_to_string(total(both)))
    println(float_to_string(total([c as dyn Shape, r, Blob.Small])))
    let picked: dyn Shape = if total(both) > 20.0 { r as dyn Shape } else { c }
    println(picked.name())
    let boxed: Box[dyn Shape] = Box_new(r)
    let unboxed = *boxed
    println(unboxed.name())
    match find(0) {
        Some(found) => println(found.describe()),
        None => println("nothing")
    }
    match find(1) {
        Some(found) => println(found.describe()),
        None => println("nothing")
    }
    match find(-1) {
        Some(found) => println(found.describe()),
        None => println("nothing")
    }
    match checked(1) {
        Ok(shape) => println(shape.describe()),
        Err(why) => println(why)
    }
    match checked(3) {
        Ok(shape) => println(shape.describe()),
        Err(why) => println(why)
    }

    -- A method that changes the value changes the one the dyn holds
    let counter: dyn Counter = Tally { n: 10 }
    counter.bump(5)
    bump_twice(counter)
    println(int_to_string(counter.count()))
}
