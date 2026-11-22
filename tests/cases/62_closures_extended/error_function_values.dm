module error_function_values

extern fn qsort(base: int, count: int, size: int, compare: fn(int, int) -> i32)

fn first[T](xs: List[T]) -> T {
    return xs[0]
}

fn grow(mut xs: List[int], n: int) {
    xs.push(n)
}

fn apply(f: fn(int) -> int, x: int) -> int {
    return f(x)
}

fn main() {
    let mut total = 0
    let items = [1, 2]
    let add = |x: int| -> int {
        total = total + x
        return total
    }
    let grows = |x: int| -> int {
        items.push(x)
        return x
    }
    let f = first
    let g = grow
    let n = 5
    println(int_to_string(n(1)))
    println(int_to_string(apply(add, 1, 2)))
    println(int_to_string(apply(|s: string| -> int len(s), 1)))
}
