-- PROPOSAL -- NOT IMPLEMENTED, NOT A TEST
--
-- Generic higher-order list functions (map/filter/fold/first/last
-- over List[T]) called with inferred type arguments, and non-empty
-- list literals passed where List[T] is expected.
-- Moved from tests/generics.dm in Stage 5 Phase 3: this file does not
-- compile and is not run by any test suite. Concrete List[int]
-- versions of these functions work and are tested in
-- tests/generics.dm.
--
-- What breaks today (Stage 0): monomorphization of a generic
-- function whose signature mentions List[T]/Option[T] emits the
-- unsubstituted C type name `dm_T` ("error: unknown type name
-- 'dm_T'"), with implicit or explicit type arguments alike.
-- A non-empty literal such as [1, 2, 3] annotated as List[int]
-- emits a bare C brace initializer `{ 1, 2, 3 }` against the list
-- struct, which also fails the C compile.

module proposals.generic_list_hofs

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

fn main() {
    let numbers = [1, 2, 3, 4, 5]
    let doubled = map(numbers, |x: int| -> int x * 2)
    let evens = filter(numbers, |x: int| -> bool x % 2 == 0)
    let sum = fold(numbers, 0, |acc: int, x: int| -> int acc + x)
}
