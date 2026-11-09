-- Generics test file
-- Tests generic functions and list operations with lambdas/pipelines
--
-- Aspirational material formerly in this file (generic structs,
-- generic list higher-order functions, trait bounds, @derive,
-- fn(x) => lambdas) lives in proposals/; it is not implemented.
-- The list operations below use concrete List[int] signatures,
-- which is the supported form today.

module tests.generics

-- Generic identity function
fn identity[T](x: T) -> T {
    return x
}

-- List operations (concrete over int; see proposals/generic_list_hofs.dm)
fn first_int(list: List[int]) -> Option[int] {
    if list.len() == 0 {
        return None
    }
    return Some(list[0])
}

fn last_int(list: List[int]) -> Option[int] {
    if list.len() == 0 {
        return None
    }
    return Some(list[list.len() - 1])
}

fn map_int(list: List[int], f: fn(int) -> int) -> List[int] {
    let mut result: List[int] = []
    for item in list {
        result.push(f(item))
    }
    return result
}

fn filter_int(list: List[int], predicate: fn(int) -> bool) -> List[int] {
    let mut result: List[int] = []
    for item in list {
        if predicate(item) {
            result.push(item)
        }
    }
    return result
}

fn fold_int(list: List[int], initial: int, f: fn(int, int) -> int) -> int {
    let mut acc = initial
    for item in list {
        acc = f(acc, item)
    }
    return acc
}

fn main() {
    -- Use generic functions
    let x = identity(42)
    let y = identity("hello")
    let z = identity(3.14)
    println(x)
    println(y)
    if z > 3.0 { println("float ok") }

    -- Use list functions
    let mut numbers: List[int] = []
    numbers.push(1)
    numbers.push(2)
    numbers.push(3)
    numbers.push(4)
    numbers.push(5)

    let doubled = map_int(numbers, |x: int| -> int x * 2)
    let evens = filter_int(numbers, |x: int| -> bool x % 2 == 0)
    let sum = fold_int(numbers, 0, |acc: int, x: int| -> int acc + x)
    println(fold_int(doubled, 0, |a: int, b: int| -> int a + b))
    println(len(evens))
    println(sum)

    let f = first_int(numbers)
    if f.has_value { println(f.value) }
    let l = last_int(numbers)
    if l.has_value { println(l.value) }

    -- Pipeline syntax
    let result = numbers
        |> filter_int(|x: int| -> bool x > 2)
        |> map_int(|x: int| -> int x * x)
        |> fold_int(0, |a: int, b: int| -> int a + b)
    println(result)
}
