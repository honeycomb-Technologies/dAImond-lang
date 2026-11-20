module test

fn main() {
    let f = parse_float("3.14")
    -- Just verify it parses without error by printing > 3
    if f > 3.0 {
        println("ok")
    }
}