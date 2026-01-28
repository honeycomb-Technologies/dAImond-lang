-- Generics test file
-- Tests generic functions, structs, and traits

module tests.generics

-- Generic identity function
fn identity[T](x: T) -> T {
    return x
}

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

-- Generic list operations
fn first[T](list: List[T]) -> Option[T] {
    if list.len() == 0 {
        return None
    }
    return Some(list[0])
}

fn last[T](list: List[T]) -> Option[T] {
    if list.len() == 0 {
        return None
    }
    return Some(list[list.len() - 1])
}

fn map[T, U](list: List[T], f: fn(T) -> U) -> List[U] {
    let mut result: List[U] = []
    for item in list {
        result.push(f(item))
    }
    return result
}

fn filter[T](list: List[T], predicate: fn(T) -> bool) -> List[T] {
    let mut result: List[T] = []
    for item in list {
        if predicate(item) {
            result.push(item)
        }
    }
    return result
}

fn fold[T, U](list: List[T], initial: U, f: fn(U, T) -> U) -> U {
    let mut acc = initial
    for item in list {
        acc = f(acc, item)
    }
    return acc
}

-- Trait example
trait Eq {
    fn eq(self, other: Self) -> bool
}

trait Ord: Eq {
    fn cmp(self, other: Self) -> Ordering
}

enum Ordering {
    Less,
    Equal,
    Greater,
}

-- Implement traits for Point
@derive(Eq)
struct Point3D {
    x: float,
    y: float,
    z: float,
}

-- Generic function with trait bounds
fn max[T: Ord](a: T, b: T) -> T {
    match a.cmp(b) {
        Ordering.Greater => a,
        _ => b,
    }
}

fn min[T: Ord](a: T, b: T) -> T {
    match a.cmp(b) {
        Ordering.Less => a,
        _ => b,
    }
}

fn main() {
    -- Use generic functions
    let x = identity(42)
    let y = identity("hello")
    let z = identity(3.14)

    -- Use generic structs
    let pair = Pair.new(1, "one")
    let swapped = pair.swap()

    -- Use list functions
    let numbers = [1, 2, 3, 4, 5]
    let doubled = map(numbers, fn(x) => x * 2)
    let evens = filter(numbers, fn(x) => x % 2 == 0)
    let sum = fold(numbers, 0, fn(acc, x) => acc + x)

    -- Pipeline syntax
    let result = numbers
        |> filter(fn(x) => x > 2)
        |> map(fn(x) => x * x)
        |> fold(0, fn(a, b) => a + b)
}
