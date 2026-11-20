module test

fn main() {
    print("printed before the panic")
    panic("boom: " + int_to_string(42))
}