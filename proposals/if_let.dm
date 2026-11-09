-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- `if let` pattern-condition statements.
-- Moved from tests/structs.dm in Stage 5 Phase 3: this file does not
-- compile and is not run by any test suite. Today the same logic is
-- written with the Option fields has_value/value, or with match.
--
-- What breaks today (Stage 0): `if let` is a parse error
-- ("Expected expression, found: let"). The parser has no if-let
-- form; implementing it means a new statement form across parser,
-- checker, and codegen (or a desugar to match).

module proposals.if_let

fn main() {
    let maybe_value: Option[int] = Some(42)

    -- If-let pattern
    if let Some(value) = maybe_value {
        print("Value is set")
    }
}
