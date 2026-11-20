module test

fn main() {
    println(chr(72) + chr(105) + chr(33))
    println(int_to_string(len(chr(65))) + " " + int_to_string(len(chr(0))) + " " + int_to_string(len(chr(10))))
    -- Three bytes that are one character in UTF-8
    let block = chr(226) + chr(150) + chr(160)
    println(block + " " + int_to_string(len(block)))
    -- Every byte from 32 to 126 is itself
    let mut text = ""
    let mut c = 32
    while c < 127 {
        text = text + chr(c)
        c = c + 1
    }
    println(text)
    println(bool_to_string(chr(97) == "a") + " " + bool_to_string(chr(97) < chr(98)))
    -- Only the low 8 bits count
    println(chr(256 + 66) + chr(300) + chr(1024 + 48))
}