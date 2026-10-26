-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- Struct-style enum variants: Rgb { r: u8, g: u8, b: u8 } in the
-- declaration, Color.Rgb { r: 255, ... } construction, qualified
-- match patterns (Color.Red), and field shorthand inside patterns.
-- Moved from tests/structs.dm in Stage 5 Phase 3: this file does not
-- compile and is not run by any test suite. Tuple variants such as
-- Rgb(int, int, int) with Color::Rgb(r, g, b) patterns work and are
-- tested in tests/structs.dm.
--
-- What breaks today (Stage 0):
-- - `Color.Rgb { r: 255, g: 128, b: 0 }` is a parse error
--   ("Expected expression, found: :").
-- - The declaration `Rgb { r: u8, g: u8, b: u8 }` parses, but a bare
--   `Rgb { ... }` construction is treated as a struct literal of an
--   unknown type Rgb ("error: unknown type name 'dm_Rgb'").
-- - `Color.Red =>` as a match pattern is a parse error
--   ('=>' after match pattern, found '.'); use Color::Red today.
-- - `Color.Red` as a value fails the checker ("field access
--   requires struct type"); use Color::Red today. Dot construction
--   works for payload variants only: Color.Rgb(255, 128, 0).

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
