-- Struct and enum test file
-- Tests struct definitions, enums, and pattern matching
--
-- Aspirational material formerly in this file (user-defined generic
-- enums, struct enum variants, if-let) lives in proposals/; it is not
-- implemented. This file covers the real APIs only.

module tests.structs

import std.math

-- A simple 2D point
struct Point {
    x: float,
    y: float,
}

impl Point {
    fn new(x: float, y: float) -> Point {
        return Point { x, y }
    }

    fn distance(self, other: Point) -> float {
        let dx = self.x - other.x
        let dy = self.y - other.y
        let mut r: float = 0.0
        unsafe { r = sqrt(dx * dx + dy * dy) }
        return r
    }

    fn origin() -> Point {
        return Point { x: 0.0, y: 0.0 }
    }
}

-- Option and Result are builtin generic types; they are used below
-- without redefinition (user-defined generic enums are a proposal).

-- A color enum
enum Color {
    Red,
    Green,
    Blue,
    Rgb(int, int, int),
}

fn main() {
    -- Create points
    let p1 = Point.new(0.0, 0.0)
    let p2 = Point { x: 3.0, y: 4.0 }

    -- Use methods
    let dist = p1.distance(p2)
    if dist > 4.9 and dist < 5.1 { println("dist ok") }
    let origin = Point.origin()
    println(origin.x + origin.y)

    -- Pattern matching on Option
    let maybe_value: Option[int] = Some(42)

    match maybe_value {
        Some(x) => println(f"Got value: {x}"),
        _ => println("No value"),
    }

    -- Pattern matching on Color
    let color = Color.Rgb(255, 128, 0)

    match color {
        Color::Red => println("Red"),
        Color::Green => println("Green"),
        Color::Blue => println("Blue"),
        Color::Rgb(r, g, b) => println(f"RGB({r}, {g}, {b})"),
        _ => println("other"),
    }

    -- Option access via has_value/value (if-let is a proposal)
    if maybe_value.has_value {
        println(f"Value is: {maybe_value.value}")
    }
}
