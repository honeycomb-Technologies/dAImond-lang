module values_in_generic_bodies

-- The body of a generic function is checked for each set of type
-- arguments, so a `match`, an `if`, a None and a list literal in it
-- have the types the arguments give them

struct Point {
    x: int,
    y: int
}

struct Slot[T] {
    value: Option[T],
    name: string
}

impl Slot[T] {
    fn get_or(self, fallback: T) -> T {
        return match self.value {
            Some(v) => v,
            None => fallback
        }
    }

    fn cleared(self) -> Slot[T] {
        return Slot[T] { value: None, name: self.name + " (cleared)" }
    }

    fn both(self, other: T) -> List[T] {
        let first = self.get_or(other)
        return [first, other]
    }
}

fn or_else[T](o: Option[T], fallback: T) -> T {
    return match o {
        Some(v) => v,
        None => fallback
    }
}

fn pick[T](first: bool, a: T, b: T) -> T {
    let chosen = if first { a } else { b }
    return chosen
}

fn first_or[T](xs: List[T], fallback: T) -> T {
    let found: Option[T] = if xs.len() > 0 { Some(xs[0]) } else { None }
    return or_else(found, fallback)
}

fn twice[T](x: T) -> List[T] {
    return [x, x]
}

fn last[T](xs: List[T]) -> Option[T] {
    if xs.len() == 0 { return None }
    return Some(xs[xs.len() - 1])
}

fn show_point(p: Point) -> string {
    return "(" + int_to_string(p.x) + ", " + int_to_string(p.y) + ")"
}

fn main() {
    -- A string is wider than the 8 bytes an int takes
    let some_text: Option[string] = Some("text")
    let no_text: Option[string] = None
    println(or_else(some_text, "fallback"))
    println(or_else(no_text, "fallback"))
    let some_int: Option[int] = Some(7)
    println(int_to_string(or_else(some_int, 0)))
    let no_point: Option[Point] = None
    println(show_point(or_else(no_point, Point { x: 1, y: 2 })))
    println(show_point(or_else(Some(Point { x: 3, y: 4 }), Point { x: 1, y: 2 })))
    println(float_to_string(or_else(Some(1.5), 0.0)))

    println(pick(true, "a", "b"))
    println(pick(false, "a", "b"))
    println(show_point(pick(false, Point { x: 0, y: 0 }, Point { x: 5, y: 6 })))
    println(int_to_string(pick(true, 1, 2)))

    println(first_or(["x", "y"], "none"))
    let no_words: List[string] = []
    println(first_or(no_words, "none"))
    println(show_point(first_or([Point { x: 7, y: 8 }], Point { x: 0, y: 0 })))

    let words = twice("w")
    println(words[0] + words[1])
    let points = twice(Point { x: 9, y: 9 })
    println(show_point(points[1]))

    match last(["a", "b", "c"]) {
        Some(w) => println(w),
        None => println("empty")
    }
    match last(no_words) {
        Some(w) => println(w),
        None => println("empty")
    }

    -- Methods of a generic struct
    let s = Slot[string] { value: Some("held"), name: "s" }
    println(s.get_or("nothing"))
    println(s.cleared().get_or("nothing"))
    println(s.cleared().name)
    let pair = s.both("other")
    println(pair[0] + " " + pair[1])
    let n = Slot[int] { value: None, name: "n" }
    println(int_to_string(n.get_or(42)))
    let p = Slot[Point] { value: Some(Point { x: 2, y: 3 }), name: "p" }
    println(show_point(p.get_or(Point { x: 0, y: 0 })))
    println(show_point(p.cleared().get_or(Point { x: 0, y: 0 })))
}
