module error_c_function_argument

-- A C function cannot call a closure: it takes a function by its name

extern fn qsort(base: int, count: int, size: int, compare: fn(int, int) -> i32)

fn main() {
    let descending = true
    unsafe {
        qsort(0, 0, 8, |a: int, b: int| -> i32 if descending { 1 as i32 } else { 0 as i32 })
    }
}
