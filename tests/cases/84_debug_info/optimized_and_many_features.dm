module main

-- A program of many features, built with -g and -O2: debug info must
-- not change what a program does or keep it from being optimized

enum Shape {
    Circle(int),
    Square(int),
    Empty
}

trait Named {
    fn name(self) -> string
}

struct Counter {
    label: string,
    hits: int
}

impl Named for Counter {
    fn name(self) -> string {
        return "counter " + self.label
    }
}

impl Counter {
    fn bump(mut self, by: int) {
        self.hits = self.hits + by
    }
}

struct Pair[A, B] {
    first: A,
    second: B
}

fn area(s: Shape) -> int {
    match s {
        Shape.Circle(r) => { return 3 * r * r }
        Shape.Square(w) => { return w * w }
        Shape.Empty => { return 0 }
    }
    return 0
}

fn largest[T](a: T, b: T) -> T {
    if a > b {
        return a
    }
    return b
}

fn parse(text: string) -> Result[int, string] {
    if len(text) == 0 {
        return Err("empty")
    }
    return Ok(parse_int(text))
}

fn twice(text: string) -> Result[int, string] {
    let n = parse(text)?
    return Ok(n * 2)
}

fn find(xs: List[int], wanted: int) -> Option[int] {
    let mut i = 0
    while i < xs.len() {
        if xs[i] == wanted {
            return Some(i)
        }
        i = i + 1
    }
    return None
}

fn main() -> int {
    let mut shapes: List[Shape] = []
    shapes.push(Shape.Circle(1))
    shapes.push(Shape.Square(2))
    shapes.push(Shape.Empty)
    let mut total = 0
    let half = 0.5
    for s in shapes {
        total = total + area(s)
    }
    println(int_to_string(total) + " " + float_to_string(half * 3.0))

    let mut c = Counter { label: "a", hits: 0 }
    c.bump(2)
    c.bump(3)
    println(c.name() + " " + int_to_string(c.hits))

    let pair = Pair[string, int] { first: "one", second: 1 }
    println(pair.first + int_to_string(pair.second))
    println(int_to_string(largest(3, 9)) + largest("pear", "apple"))

    let mut ages: Map[string, int] = Map_new()
    ages.insert("ann", 31)
    ages.insert("bob", 27)
    for who in ages {
        println(who + " " + int_to_string(ages[who]))
    }

    let offset = 10
    let shift = |n: int| n + offset
    println(int_to_string(shift(5)))

    let doubled: Result[int, string] = twice("21")
    match doubled {
        Ok(v) => println("ok " + int_to_string(v)),
        Err(e) => println("err " + e)
    }
    let failed: Result[int, string] = twice("")
    match failed {
        Ok(v) => println("ok " + int_to_string(v)),
        Err(e) => println("err " + e)
    }
    let numbers = [4, 5, 6]
    match find(numbers, 6) {
        Some(at) => println("at " + int_to_string(at)),
        None => println("missing")
    }
    let boxed = Box_new(Counter { label: "b", hits: 7 })
    println(int_to_string(boxed.hits))
    region scratch {
        let piece = "in a " + "region"
        println(piece)
    }
    return 0
}
