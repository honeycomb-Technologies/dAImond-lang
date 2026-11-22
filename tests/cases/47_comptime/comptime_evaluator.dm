module comptime_evaluator

-- What the compile-time evaluator takes: loops of any length,
-- functions with variables of their own, lists, strings, casts

fn sum_to(n: int) -> int {
    let mut total = 0
    let mut i = 1
    while i <= n {
        total += i
        i = i + 1
    }
    return total
}

-- Its `total` is not the caller's
fn twice_sum(n: int) -> int {
    let total = 1000000
    let inner = sum_to(n)
    return inner * 2 + total - total
}

fn count_odd(from: int, to: int) -> int {
    let mut n = 0
    for i in from..to {
        if i % 2 == 0 { continue }
        n = n + 1
    }
    return n
}

fn first_power_over(limit: int) -> int {
    let mut p = 1
    loop {
        if p > limit { break }
        p = p * 2
    }
    return p
}

fn squares(n: int) -> int {
    let mut xs = [0]
    for i in 1..=n {
        xs.push(i * i)
    }
    xs[0] = 100
    xs[1] += 5
    let mut total = 0
    for x in xs {
        total = total + x
    }
    return total + xs.len()
}

fn shout(s: string) -> string {
    let upper = string_to_upper(string_trim(s))
    if starts_with(upper, "HE") and string_contains(upper, "LL") {
        return f"{upper}! ({len(upper)} letters, {char_at(upper, 1)} second)"
    }
    return upper
}

fn grade(score: int) -> string {
    return match score / 10 {
        10 => "top",
        9 => "top",
        n if n >= 5 => "pass " + int_to_string(n),
        _ => "fail"
    }
}

fn pick(word: string) -> int {
    match word {
        "one" => { return 1 }
        "two" => { return 2 }
        _ => { return 0 }
    }
}

const LATER = comptime EARLIER + 1
const EARLIER = 41

const BIG_LOOP = comptime sum_to(20000)
const SCOPES = comptime twice_sum(10)
const ODD = comptime count_odd(0, 9)
const POWER = comptime first_power_over(1000)
const SQUARES = comptime squares(3)
const SHOUT = comptime shout("  hello ")
const GRADES = comptime grade(100) + " " + grade(73) + " " + grade(12)
const PICKED = comptime pick("two") * 10 + pick("three")
const SHORT = comptime false and (1 / 0 == 0)
const MIXED = comptime 1 + 2.5
const CAST = comptime (7.9 as int) + (200 as u8)
const HALF = comptime (3 as float) / 2.0
const WORDS = comptime ["a", "b"] == ["a", "b"]
const ORDER = comptime "apple" < "banana"

fn main() {
    println(int_to_string(LATER))
    println(int_to_string(BIG_LOOP))
    println(int_to_string(SCOPES))
    println(int_to_string(ODD))
    println(int_to_string(POWER))
    println(int_to_string(SQUARES))
    println(SHOUT)
    println(GRADES)
    println(int_to_string(PICKED))
    println(bool_to_string(SHORT))
    println(float_to_string(MIXED))
    println(int_to_string(CAST))
    println(float_to_string(HALF))
    println(bool_to_string(WORDS))
    println(bool_to_string(ORDER))
    let local = comptime {
        let mut text = ""
        for i in 0..3 {
            text = text + int_to_string(i)
        }
        text
    }
    println(local)
}
