module test
-- A program's own function of a string method's name is what the
-- method call calls
fn trim(s: string) -> string { return "mine:" + s }
fn main() {
    println(" a ".trim())
    println(" a ".to_upper().trim())
    println("[" + string_trim(" a ") + "]")
}
