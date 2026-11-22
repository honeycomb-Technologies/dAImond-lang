module test
-- Nothing says what the type parameters stand for: not the values,
-- not the place the struct goes to
struct Pair[A, B] { first: A, second: B }
struct Stack[T] { items: List[T] }
impl Stack[T] {
    fn new() -> Stack[T] { return Stack { items: [] } }
    fn zero() -> int { return 0 }
}
fn main() {
    let p = Pair { first: 1, second: None }
    let s = Stack.new()
    println(int_to_string(Stack.zero()))
    println(int_to_string(Stack[string].zero()))
}
