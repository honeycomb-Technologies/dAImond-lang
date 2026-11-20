module test

fn main() {
    let start = time_ms()
    sleep_ms(150)
    let waited = time_ms() - start
    println(bool_to_string(waited >= 150) + " " + bool_to_string(waited < 20000))
    -- The clock never goes back
    let mut last = time_ms()
    let mut ordered = true
    let mut i = 0
    while i < 1000 {
        let now = time_ms()
        if now < last { ordered = false }
        last = now
        i = i + 1
    }
    println(bool_to_string(ordered))
    -- Zero and negative times do not wait
    let before = time_ms()
    sleep_ms(0)
    sleep_ms(0 - 1)
    sleep_ms(0 - 60000)
    println(bool_to_string(time_ms() - before < 20000))
}