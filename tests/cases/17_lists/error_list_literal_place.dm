module test
-- An element that does not fit the type the list has in its place,
-- and a Map_new() whose place gives it no type
fn sum(xs: List[i32]) -> int {
    let mut t = 0
    for x in xs { t += x }
    return t
}
fn main() {
    let big = 5000000000
    println(int_to_string(sum([1, big])))
    println(int_to_string(sum([1, 3000000000])))
    let m = [Map_new()]
}
