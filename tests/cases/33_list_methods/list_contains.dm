module test

fn main() {
    let mut items: List[int] = List_new()
    items.push(10)
    items.push(20)
    items.push(30)
    println(bool_to_string(items.contains(20)))
    println(bool_to_string(items.contains(42)))
}