-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- The `fn(x) => expr` lambda spelling.
-- Moved from tests/generics.dm in Stage 5 Phase 3: this file does not
-- compile and is not run by any test suite. The implemented lambda
-- spelling is `|x: int| -> int expr` (typed parameters and return),
-- used in tests/generics.dm. One obvious spelling should win; this
-- file only records the alternative that used to appear in tests.
--
-- What breaks today (Stage 0): `fn(x) => x * 2` is a parse error
-- ("Expected identifier for function name, found: (").

module proposals.fn_arrow_lambdas

fn double_each(list: List[int]) -> List[int] {
    let mut result: List[int] = []
    for item in list {
        result.push(item * 2)
    }
    return result
}

fn main() {
    let mut numbers: List[int] = []
    numbers.push(1)
    let doubled = double_each(numbers)
    let f = fn(x) => x * 2
    println(f(21))
}
