module main

-- An arm that is a name binds the matched value, with its type

enum Light {
    Red,
    Green(int)
}

fn describe(word: string) -> string {
    match word {
        "apple" => { return "an apple" }
        w => { return "a " + w + " of " + int_to_string(len(w)) }
    }
    return "?"
}

-- Strings are matched by their text
fn color(word: string) -> int {
    match word {
        "red" => { return 1 }
        "green" => { return 2 }
        _ => { return 0 }
    }
    return -1
}

fn seconds(light: Light) -> int {
    match light {
        Light.Red => { return 0 }
        other => {
            match other {
                Light.Green(n) => { return n }
                _ => { return -1 }
            }
        }
    }
    return -2
}

fn main() -> int {
    println(describe("apple"))
    println(describe("pear"))
    let ratio = 2.5
    match ratio {
        r => println(float_to_string(r * 2.0))
    }
    match 7 {
        1 => println("one"),
        n => println(int_to_string(n + 1))
    }
    println(int_to_string(color("red")) + int_to_string(color("gre" + "en")) + int_to_string(color("blue")))
    println(int_to_string(seconds(Light.Green(30))))
    println(int_to_string(seconds(Light.Red)))
    return 0
}
