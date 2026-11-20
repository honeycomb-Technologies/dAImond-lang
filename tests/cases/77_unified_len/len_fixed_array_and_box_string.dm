module test

fn main() {
    let arr: [int; 3] = [1, 2, 3]
    println(len(arr))
    println(arr.len())
    let bs: Box[string] = Box.new("world!")
    println(bs.len())
}