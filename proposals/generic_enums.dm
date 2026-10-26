-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- User-defined generic enums, including redefining Option/Result.
-- Moved from tests/structs.dm in Stage 5 Phase 3: this file does not
-- compile and is not run by any test suite.
--
-- What breaks today (Stage 0): the checker reports
-- "Error[E0202]: undefined type" for the enum's type parameter, for
-- any generic enum name (verified with both Option[T] and a fresh
-- name, MyOpt[T]). Option[T] and Result[T, E] themselves exist as
-- builtins and work when used without redefinition.

module proposals.generic_enums

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

fn main() {
    let maybe_value: Option[int] = Some(42)

    match maybe_value {
        Some(x) => print("Got value"),
        None => print("No value"),
    }
}
