-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- The `fn(x) => expr` lambda spelling.
-- The lambda spelling the compiler has is `|x: int| -> int expr`. One
-- spelling should do; this file records the other one, which early
-- test files used.
--
-- What the compiler says today: `let f = fn(x) => x * 2` is a syntax
-- error ("expected an expression but got 'fn'").

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
