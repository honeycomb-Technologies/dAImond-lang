module test
fn main() {
    let a = "hello"
    let b = a + ", " + "world"
    println(b + " " + int_to_string(len(b)) + " " + int_to_string(len("lit")) + " " + int_to_string(len("")))
    println(bool_to_string(a == "hello") + " " + bool_to_string(a == "hellp") + " " + bool_to_string(a != b) + " " + bool_to_string("" == "") + " " + bool_to_string("a" == ""))
    println(bool_to_string("abc" < "abd") + " " + bool_to_string("abc" < "ab") + " " + bool_to_string("ab" < "abc") + " " + bool_to_string("b" >= "abc") + " " + bool_to_string("" < "a") + " " + bool_to_string("x" <= "x"))
    println(char_at(b, 0) + char_at(b, 4) + "|" + substr(b, 7, 5) + "|" + substr(b, 7, 500) + "|" + substr(b, 50, 2) + "|" + substr(b, 3, 0))
    let mut acc = ""
    let mut i = 0
    while i < 1000 {
        acc = acc + char_at("abcdefghij", i % 10)
        i = i + 1
    }
    println(int_to_string(len(acc)) + " " + substr(acc, 995, 10) + " " + bool_to_string(substr(acc, 0, 10) == "abcdefghij"))
    let e = "" + ""
    println("[" + e + "]" + int_to_string(len(e)))
    println("tab\there \"q\" \\ end" + int_to_string(len("tab\there \"q\" \\ end")))
}