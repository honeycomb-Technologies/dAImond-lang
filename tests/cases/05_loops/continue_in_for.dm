module continue_in_for

-- `continue` in a for loop goes on with the next element. It went to
-- the loop's test without stepping, and the loop never ended.

fn main() {
    -- over a range
    let mut odd = 0
    for i in 0..10 {
        if i % 2 == 0 { continue }
        odd += i
    }
    println(int_to_string(odd))

    -- over an inclusive range, with break
    let mut seen = ""
    for i in 1..=9 {
        if i == 3 or i == 5 { continue }
        if i == 8 { break }
        seen = seen + int_to_string(i)
    }
    println(seen)

    -- over a list
    let words = ["a", "", "b", "", "", "c"]
    let mut joined = ""
    for w in words {
        if len(w) == 0 { continue }
        joined = joined + w
    }
    println(joined)

    -- over a string
    let mut letters = ""
    for ch in "a-b--c" {
        if ch == "-" { continue }
        letters = letters + ch
    }
    println(letters)

    -- over a map's keys
    let mut ages: Map[string, int] = Map_new()
    ages.insert("ann", 30)
    ages.insert("bob", 0)
    ages.insert("cy", 41)
    let mut total = 0
    for name in ages {
        if ages[name] == 0 { continue }
        total += ages[name]
    }
    println(int_to_string(total))

    -- nested, where the inner loop's continue and break leave the outer one alone
    let mut pairs = 0
    for x in 0..4 {
        for y in 0..4 {
            if y > x { continue }
            if x == 3 { break }
            pairs += 1
        }
        if x == 1 { continue }
        pairs += 100
    }
    println(int_to_string(pairs))

    -- in a match and a nested if inside the loop
    let mut kept: List[int] = []
    for n in [1, 2, 3, 4, 5, 6] {
        match n % 3 {
            0 => { continue }
            _ => {}
        }
        if n > 1 {
            if n == 4 { continue }
        }
        kept.push(n)
    }
    println(int_to_string(kept.len()) + " " + int_to_string(kept[0]) + int_to_string(kept[1]) + int_to_string(kept[2]))

    -- a body that always continues
    let mut count = 0
    for i in 0..5 {
        count += 1
        continue
    }
    println(int_to_string(count))
}
