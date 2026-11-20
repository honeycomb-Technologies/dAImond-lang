module test

fn main() {
    if args_len() > 1 {
        -- The child: standard input is a pipe that stays open and
        -- never has a byte in it
        let start = time_ms()
        let a = read_key_nb()
        let b = read_key_nb()
        println(int_to_string(a) + " " + int_to_string(b) + " " + bool_to_string(time_ms() - start < 900))
        return
    }
    let status = system("sleep 1 | " + args_get(0) + " child")
    println(int_to_string(status))
}