-- Struct and enum test file
-- Tests struct definitions, enums, and pattern matching

module tests.structs

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
        return sqrt(dx * dx + dy * dy)
    }

    fn origin() -> Point {
        return Point { x: 0.0, y: 0.0 }
    }
}

-- An optional value
enum Option[T] {
    Some(T),
    None,
}

-- A result type for error handling
enum Result[T, E] {
    Ok(T),
    Err(E),
}

-- A color enum
enum Color {
    Red,
    Green,
    Blue,
    Rgb { r: u8, g: u8, b: u8 },
}

fn main() {
    -- Create points
    let p1 = Point.new(0.0, 0.0)
    let p2 = Point { x: 3.0, y: 4.0 }

    -- Use methods
    let dist = p1.distance(p2)
    let origin = Point.origin()

    -- Pattern matching on Option
    let maybe_value: Option[int] = Some(42)

    match maybe_value {
        Some(x) => print("Got value: {x}"),
        None => print("No value"),
    }

    -- Pattern matching on Color
    let color = Color.Rgb { r: 255, g: 128, b: 0 }

    match color {
        Color.Red => print("Red"),
        Color.Green => print("Green"),
        Color.Blue => print("Blue"),
        Color.Rgb { r, g, b } => print("RGB({r}, {g}, {b})"),
    }

    -- If-let pattern
    if let Some(value) = maybe_value {
        print("Value is: {value}")
    }
}
