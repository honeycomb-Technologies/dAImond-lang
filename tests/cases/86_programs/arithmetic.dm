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

    println("sum=" + int_to_string(sum))
    println("diff=" + int_to_string(diff))
    println("prod=" + int_to_string(prod))
    println("quot=" + int_to_string(quot))
    println("rem=" + int_to_string(rem))

    -- Compound expressions
    let result = (a + b) * (a - b)
    let complex = 1 + 2 * 3 - 4 / 2
    println("compound=" + int_to_string(result))
    println("precedence=" + int_to_string(complex))

    -- Unary operations
    let neg = -42
    let double_neg = -(-42)
    println("neg=" + int_to_string(neg))
    println("double_neg=" + int_to_string(double_neg))
}
