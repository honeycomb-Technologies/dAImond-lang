module test_builtins

-- Test all builtins (Phase 3)

fn main() {
    -- eprint (writes to stderr, can't easily verify in stdout, just test it doesn't crash)
    eprint("stderr_test")
    eprintln("")

    -- parse_float
    let f = parse_float("3.14")
    println("parse_float=" + float_to_string(f))

    -- string_to_upper / string_to_lower
    let upper = string_to_upper("hello")
    println("to_upper=" + upper)

    let lower = string_to_lower("WORLD")
    println("to_lower=" + lower)

    -- modulo operator
    let m = 17 % 5
    println("modulo=" + int_to_string(m))

    -- string builtins that already existed
    println("len=" + int_to_string(len("hello")))
    println("contains=" + bool_to_string(string_contains("hello world", "world")))
    println("find=" + int_to_string(string_find("hello", "llo")))
    println("starts=" + bool_to_string(starts_with("hello", "hel")))
    println("ends=" + bool_to_string(ends_with("hello", "llo")))
    println("replace=" + string_replace("hello", "l", "r"))
    println("trim=" + string_trim("  hi  "))
    println("substr=" + substr("hello", 1, 3))
    println("char_at=" + char_at("hello", 0))
}
