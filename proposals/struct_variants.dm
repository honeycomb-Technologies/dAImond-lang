-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- Enum variants with named fields: `Rgb { r: u8, g: u8, b: u8 }` in
-- the declaration, `Color.Rgb { r: 255, ... }` to make one, and
-- `Color.Rgb { r, g, b } =>` as a pattern.
-- Variants whose values are in order work: `Rgb(int, int, int)`,
-- `Color.Rgb(255, 128, 0)` and the pattern `Color.Rgb(r, g, b)`, as do
-- `Color.Red` as a value and as a pattern (tests/cases/10_enums,
-- 15_enum_match, 42_enum_dot_notation).
--
-- What the compiler says today: the declaration is a syntax error
-- ("expected ',' or '}' but got '{'").

module proposals.struct_variants

-- A color enum
enum Color {
    Red,
    Green,
    Blue,
    Rgb { r: u8, g: u8, b: u8 },
}

fn main() {
    -- Pattern matching on Color
    let color = Color.Rgb { r: 255, g: 128, b: 0 }

    match color {
        Color.Red => print("Red"),
        Color.Green => print("Green"),
        Color.Blue => print("Blue"),
        Color.Rgb { r, g, b } => print("RGB"),
    }
}
