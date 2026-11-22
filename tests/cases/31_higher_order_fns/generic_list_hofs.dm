module generic_list_hofs

-- Generic functions that take functions: map, filter and fold over a
-- List[T], with the type arguments found at each call

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

fn join(words: List[string]) -> string {
    return fold(words, "", |acc: string, w: string| -> string acc + w + " ")
}

fn show(o: Option[int]) -> string {
    match o {
        Some(n) => { return "Some " + int_to_string(n) }
        None => { return "None" }
    }
}

fn is_long(s: string) -> bool {
    return len(s) > 2
}

fn main() {
    let numbers = [1, 2, 3, 4, 5]
    let doubled = map(numbers, |x: int| -> int x * 2)
    let evens = filter(numbers, |x: int| -> bool x % 2 == 0)
    let sum = fold(numbers, 0, |acc: int, x: int| -> int acc + x)
    println(join(map(doubled, |x: int| -> string int_to_string(x))))
    println(join(map(evens, |x: int| -> string int_to_string(x))))
    println(int_to_string(sum))
    println(show(first(numbers)) + ", " + show(last(numbers)))
    let nothing: List[int] = []
    println(show(first(nothing)))

    -- The functions use variables of main
    let factor = 3
    let limit = 10
    let scaled = map(numbers, |x: int| -> int x * factor)
    let big = filter(scaled, |x: int| -> bool x >= limit)
    println(join(map(big, |x: int| -> string int_to_string(x))))
    let label = "#"
    println(join(map(numbers, |x: int| -> string label + int_to_string(x * factor))))
    println(fold(numbers, 0.5, |acc: float, x: int| -> float acc + 1.0) |> float_to_string)

    -- Functions by their names
    let words = ["to", "be", "or", "not"]
    println(join(filter(words, is_long)))
    println(join(map(words, string_upper)))
}

fn string_upper(s: string) -> string {
    return string_to_upper(s)
}
