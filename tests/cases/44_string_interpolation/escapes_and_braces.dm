module escapes_and_braces

-- Escapes in the text of an f-string, {{ and }} for braces, and
-- strings in its expressions (written with \", since a plain " ends
-- the f-string)

fn main() {
    let n = 3
    let name = "a"
    let mut m: Map[string, int] = Map_new()
    m.insert("k", 7)
    println(f"tab:\t|{n}|")
    println(f"line one\nline two {n}")
    println(f"a quote \" and a backslash \\ and {name}")
    println(f"braces {{{n}}} and {{}}")
    println(f"strings in expressions: {m.get(\"k\")} {name + \"}\"}")
    println(f"{if n > 2 { \"big\" } else { \"small\" }}")
    println(f"\\{n}\\")
}
