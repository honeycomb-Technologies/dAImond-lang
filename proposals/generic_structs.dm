-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- Type arguments of a generic struct found from its values.
-- Generic structs work when the type arguments are written out:
-- `Pair[int, string] { first: 1, second: "one" }`, with a generic impl
-- block (tests/cases/65_generic_structs). What this file proposes is
-- leaving them out: `Pair.new(1, "one")` and `Pair { first, second }`
-- (which also uses the field shorthand, a proposal of its own).
--
-- What the compiler says today: "cannot determine the type of the
-- value '.new()' is called on" and "undefined variable 'Pair'".

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
