module main

-- "localhost" is this machine, and an address with no host listens on
-- every address of this machine.

fn try_port(tries: int) -> string {
    return int_to_string(40000 + (time_ms() + tries * 7919) % 20000)
}

fn main() -> int {
    let mut listener = -1
    let mut port = ""
    let mut tries = 0
    while listener < 0 and tries < 50 {
        port = try_port(tries)
        listener = tcp_listen(":" + port)
        tries = tries + 1
    }
    if listener < 0 {
        println("cannot listen")
        return 1
    }
    -- A connection waits in the listener's queue until it is accepted
    let client = tcp_connect("localhost:" + port)
    let conn = tcp_accept(listener)
    println(int_to_string(tcp_write(client, "over localhost")))
    println(tcp_read(conn, 4))
    println(tcp_read(conn, 100))
    tcp_close(client)
    tcp_close(conn)
    tcp_close(listener)
    return 0
}
