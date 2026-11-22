module error_fstring_braces

fn open() {
    println(f"open {1")
}

fn empty() {
    println(f"empty {}")
}

fn string() {
    println(f"string {len(\"abc)}")
}

fn main() {
    open()
}
