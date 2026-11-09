-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- Generic struct definitions (Pair[A, B]) with a generic impl block.
-- Moved from tests/generics.dm in Stage 5 Phase 3: this file does not
-- compile and is not run by any test suite.
--
-- What breaks today (Stage 0): the checker does not register a struct's
-- generic parameters, so A/B in field types, the impl target, and the
-- return type all report "Error[E0202]: undefined type", and codegen
-- has no monomorphization for generic struct layouts.

module proposals.generic_structs

-- Generic pair struct
struct Pair[A, B] {
    first: A,
    second: B,
}

impl Pair[A, B] {
    fn new(first: A, second: B) -> Pair[A, B] {
        return Pair { first, second }
    }

    fn swap(self) -> Pair[B, A] {
        return Pair { first: self.second, second: self.first }
    }
}

fn main() {
    let pair = Pair.new(1, "one")
    let swapped = pair.swap()
}
