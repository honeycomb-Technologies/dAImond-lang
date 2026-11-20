module test

fn main() {
    let mut items: List[int] = List_new()
    println(len(items))
    items.push(1)
    items.push(2)
    items.push(3)
    println(len(items))
}