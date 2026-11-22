module overview

-- The examples of README.md's language overview, as one program.

-- Functions

fn add(a: int, b: int) -> int {
    return a + b
}

-- Single expression (implicit return)
fn double(x: int) -> int = x * 2

-- Generic function
fn max_of[T](a: T, b: T) -> T {
    if a > b { return a }
    return b
}

-- Structs and enums

struct Point {
    x: int,
    y: int,
}

impl Point {
    fn manhattan(self) -> int {             -- bare self (type inferred from impl block)
        return self.x + self.y
    }
    fn translate(self: Point, dx: int, dy: int) -> Point {  -- explicit self type also works
        return Point { x: self.x + dx, y: self.y + dy }
    }
}

enum Shape {
    Circle(float),
    Rect(float, float),
    Dot,
}

-- Traits

trait Display {
    fn show(self: &Self) -> string
}

impl Display for Point {
    fn show(self: &Self) -> string {
        return f"({self.x}, {self.y})"
    }
}

-- Pattern matching

fn describe(shape: Shape) -> string {
    match shape {
        Shape.Circle(r) => { return f"circle with radius {r}" }
        Shape.Rect(w, h) => { return f"rectangle {w} by {h}" }
        Shape.Dot => { return "just a dot" }
    }
    return ""
}

fn first_even(numbers: List[int]) -> Option[int] {
    for n in numbers {
        if n % 2 == 0 { return Some(n) }
    }
    return None
}

-- Error handling

struct Config {
    value: int
}

fn read_number(text: string) -> Result[int, string] {
    if len(text) == 0 {
        return Err("nothing to read")
    }
    return Ok(parse_int(text))
}

fn parse_config(text: string) -> Result[Config, string] {
    let val = read_number(text)?   -- propagate the error with ?
    return Ok(Config { value: val })
}

-- Operator overloading

struct Vec2 { x: int, y: int }

impl Vec2 {
    fn add(self, other: Vec2) -> Vec2 {
        return Vec2 { x: self.x + other.x, y: self.y + other.y }
    }
}

-- Async/await

async fn compute(x: int, y: int) -> int {
    return x + y
}

-- Effects

fn read_file(path: string) -> string with [FileSystem] {
    return file_read(path)
}

fn pure_add(a: int, b: int) -> int {
    -- no effects needed for pure computation
    return a + b
}

-- FFI

extern fn abs(n: i32) -> i32
extern fn puts(s: string) -> i32

fn main() {
    -- Variables: immutable by default
    let x = 42
    let y: int = 42          -- explicit type
    let mut counter = 0      -- mutable requires the keyword
    counter = counter + 1
    println(int_to_string(add(x, y) + counter + double(4)))
    println(int_to_string(max_of(3, 7)))
    println(max_of("apple", "pear"))

    -- String interpolation
    let name = "world"
    let age = 42
    println(f"Hello {name}, you are {age} years old")

    -- Structs, methods and traits
    let p = Point { x: 3, y: 4 }
    let q = p.translate(1, 1)
    println(int_to_string(q.manhattan()))
    println(q.show())
    let d: dyn Display = p   -- dynamic dispatch
    println(d.show())

    -- Pattern matching
    println(describe(Shape.Circle(1.5)))
    println(describe(Shape.Rect(2.0, 3.0)))
    println(describe(Shape.Dot))
    let numbers = [3, 5, 8, 9]
    match first_even(numbers) {
        Some(n) => println(f"first even: {n}"),
        None => println("no even number"),
    }

    -- One pattern, and what to do without it
    if let Some(n) = first_even([1, 7]) {
        println(f"first even: {n}")
    } else {
        println("only odd numbers")
    }

    -- Error handling
    match parse_config("17") {
        Ok(config) => println(f"config value {config.value}"),
        Err(message) => println("error: " + message),
    }
    match parse_config("") {
        Ok(config) => println(f"config value {config.value}"),
        Err(message) => println("error: " + message),
    }

    -- Closures
    let base = 10
    let add_base = |n: int| -> int base + n   -- captures base
    println(int_to_string(add_base(5)))

    -- Operator overloading
    let v1 = Vec2 { x: 1, y: 2 }
    let v2 = Vec2 { x: 10, y: 20 }
    let v = v1 + v2          -- calls Vec2.add
    println(f"{v.x}, {v.y}")

    -- Regions
    region scratch {
        let label = "built in a region: " + int_to_string(x * 2)
        println(label)
    }                        -- label cannot be used after the region

    -- Async/await
    let sum = await compute(10, 20)
    println(int_to_string(sum))

    -- Effects
    file_write("/tmp/daimond_overview.txt", "written and read back")
    println(read_file("/tmp/daimond_overview.txt"))
    println(int_to_string(pure_add(2, 3)))

    -- FFI: calling C needs an unsafe block
    unsafe {
        println(int_to_string(abs(-7) as int))
        puts("printed by C's puts")
    }
}
