module test

fn label(i: int) -> string {
    return "item " + int_to_string(i)
}

fn main() {
    let before = "made before the mark " + int_to_string(1)
    let mark = mem_mark()
    let mut items: List[string] = []
    let mut i = 0
    while i < 5 {
        items.push(label(i))
        i = i + 1
    }
    let after = "made after the mark " + int_to_string(2)
    mem_sweep(mark)
    -- A sweep frees nothing: strings made after the mark are still whole
    println(before)
    println(after)
    println(items[0] + ", " + items[4] + ", " + int_to_string(items.len()))
    let inner = mem_mark()
    mem_sweep(inner)
    mem_sweep(mark)
    println(label(9) + " " + bool_to_string(inner >= mark))
}