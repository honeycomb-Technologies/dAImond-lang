-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- `if let` pattern conditions.
-- Today the same is written with `match`.
--
-- What the compiler says today: `if let` is a syntax error ("expected
-- an expression but got 'let'"). It would be a new statement form in
-- the parser, the checker and code generation, or a rewrite to `match`.

module proposals.if_let

fn main() {
    let maybe_value: Option[int] = Some(42)

    -- If-let pattern
    if let Some(value) = maybe_value {
        print("Value is set")
    }
}
