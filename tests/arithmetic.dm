-- Arithmetic test file
-- Tests basic arithmetic operations in dAImond

module tests.arithmetic

fn main() {
    -- Integer arithmetic
    let a = 10
    let b = 3
    let sum = a + b
    let diff = a - b
    let prod = a * b
    let quot = a / b
    let rem = a % b

    -- Floating point arithmetic
    let x = 3.14
    let y = 2.0
    let z = x * y

    -- Compound expressions
    let result = (a + b) * (a - b)
    let complex = 1 + 2 * 3 - 4 / 2

    -- Unary operations
    let neg = -42
    let double_neg = --42

    -- Comparisons
    let eq = a == b
    let ne = a != b
    let lt = a < b
    let gt = a > b
    let le = a <= b
    let ge = a >= b

    -- Logical operations
    let and_result = true and false
    let or_result = true or false
    let not_result = not true
}
