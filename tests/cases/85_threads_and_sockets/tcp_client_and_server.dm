module main

-- A server and a client in one program: the client runs on a thread
-- and connects to the port the server listens on.

-- The port, shared with the client's thread
fn port_cell() -> int {
    unsafe {
        return static_addr("port", 8)
    }
}

fn address() -> string {
    unsafe {
        return "127.0.0.1:" + int_to_string(load_i64(port_cell()))
    }
}

fn client() {
    let fd = tcp_connect(address())
    if fd < 0 {
        println("client: cannot connect")
        return
    }
    let sent = tcp_write(fd, "ping from the client")
    let reply = tcp_read(fd, 100)
    let at_end = tcp_read(fd, 100)
    tcp_close(fd)
    println("client sent " + int_to_string(sent) + " bytes")
    println("client got: " + reply)
    println("client after the server closed: [" + at_end + "]")
}

fn main() -> int {
    -- Listen on a port that is free
    let mut listener = -1
    let mut tries = 0
    while listener < 0 and tries < 50 {
        unsafe {
            store_i64(port_cell(), 20000 + (time_ms() + tries * 7919) % 20000)
        }
        listener = tcp_listen(address())
        tries = tries + 1
    }
    if listener < 0 {
        println("cannot listen")
        return 1
    }
    println("a second listener on the port: " + int_to_string(tcp_listen(address())))

    let thread = thread_spawn(client)
    let conn = tcp_accept(listener)
    println("server got: " + tcp_read(conn, 0))
    println("server sent " + int_to_string(tcp_write(conn, "pong")) + " bytes")
    tcp_close(conn)
    thread_join(thread)
    tcp_close(listener)

    println("connect when nothing listens: " + int_to_string(tcp_connect(address())))
    println("no port: " + int_to_string(tcp_connect("127.0.0.1")))
    println("a name that is not an address: " + int_to_string(tcp_connect("example.invalid:80")))
    println("a port that is too large: " + int_to_string(tcp_listen("127.0.0.1:70000")))
    println("accept on no socket: " + int_to_string(tcp_accept(-1)))
    println("read from no socket: [" + tcp_read(-1, 10) + "]")
    println("write to no socket: " + int_to_string(tcp_write(-1, "x")))
    tcp_close(-1)
    tcp_close(99999999999)
    println("done")
    return 0
}
