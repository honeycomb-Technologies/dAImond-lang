module net

-- ============================================================
-- TCP sockets
-- tcp_listen, tcp_accept, tcp_connect, tcp_read, tcp_write and
-- tcp_close, over libc's sockets. IPv4 only, and no name lookup: an
-- address is "HOST:PORT" with HOST a dotted address, "localhost" or
-- nothing. See runtime/string.dm for how rt_X functions replace the C
-- runtime.
--
-- A socket is its file descriptor. The functions that open one give
-- -1 when they fail.
-- ============================================================

extern fn libc_socket(domain: i32, kind: i32, protocol: i32) -> i32 = "socket"
extern fn libc_setsockopt(fd: i32, level: i32, option: i32, value: int, size: u32) -> i32 = "setsockopt"
extern fn libc_bind(fd: i32, address: int, size: u32) -> i32 = "bind"
extern fn libc_listen(fd: i32, backlog: i32) -> i32 = "listen"
extern fn libc_accept(fd: i32, address: int, size: int) -> i32 = "accept"
extern fn libc_connect(fd: i32, address: int, size: u32) -> i32 = "connect"
extern fn libc_inet_pton(family: i32, text: int, out: int) -> i32 = "inet_pton"
extern fn libc_read(fd: i32, buf: int, n: int) -> int = "read"
extern fn libc_write(fd: i32, buf: int, n: int) -> int = "write"
extern fn libc_close(fd: i32) -> i32 = "close"

-- libc's struct sockaddr_in on x86-64 Linux, 16 bytes: the family
-- (2 bytes), the port (2 bytes, high byte first), the address (4
-- bytes) and 8 bytes of padding
struct RtSockaddr {
    head: int,
    padding: int
}

-- Whether fd can be a file descriptor, which is a C int
fn rt_fd_ok(fd: int) -> bool {
    return fd >= 0 and fd <= 2147483647
}

-- Write the address "HOST:PORT" into the struct sockaddr_in at
-- sockaddr. No HOST means 0.0.0.0, which is every address of this
-- machine to listen on, and this machine to connect to. False when
-- the text is not an address.
fn rt_tcp_address(text: string, sockaddr: int) -> bool {
    -- 58 is ':'
    let mut colon = len(text) - 1
    while colon >= 0 and byte_at(text, colon) != 58 {
        colon = colon - 1
    }
    if colon < 0 {
        return false
    }
    -- The port is the digits after the colon, as C's atoi reads them
    let mut port = 0
    let mut i = colon + 1
    while i < len(text) and byte_at(text, i) >= 48 and byte_at(text, i) <= 57 and port <= 65535 {
        port = port * 10 + byte_at(text, i) - 48
        i = i + 1
    }
    if port > 65535 {
        return false
    }
    let mut host = substr(text, 0, colon)
    if rt_string_eq(host, "") {
        host = "0.0.0.0"
    }
    if rt_string_eq(host, "localhost") {
        host = "127.0.0.1"
    }
    unsafe {
        -- 2 is AF_INET
        store_u16(sockaddr, 2)
        store_u8(sockaddr + 2, port / 256)
        store_u8(sockaddr + 3, port % 256)
        return libc_inet_pton(2, string_data(host), sockaddr + 4) == 1
    }
}

-- tcp_listen(address): a socket that listens on "HOST:PORT", or -1
fn rt_tcp_listen(address: string) -> int {
    unsafe {
        let mut sockaddr = RtSockaddr { head: 0, padding: 0 }
        if not rt_tcp_address(address, addr_of(sockaddr)) {
            return -1
        }
        -- AF_INET, SOCK_STREAM
        let fd = libc_socket(2, 1, 0)
        if fd < 0 {
            return -1
        }
        -- SOL_SOCKET, SO_REUSEADDR: a server that is started again can
        -- take its port back at once
        let mut on = 1
        libc_setsockopt(fd, 1, 2, addr_of(on), 4)
        if libc_bind(fd, addr_of(sockaddr), 16) < 0 {
            libc_close(fd)
            return -1
        }
        if libc_listen(fd, 128) < 0 {
            libc_close(fd)
            return -1
        }
        return fd as int
    }
}

-- tcp_accept(listener): wait for a connection to the listening
-- socket and give its socket, or -1
fn rt_tcp_accept(listener: int) -> int {
    if not rt_fd_ok(listener) {
        return -1
    }
    unsafe {
        return libc_accept(listener as i32, 0, 0) as int
    }
}

-- tcp_connect(address): a socket connected to "HOST:PORT", or -1
fn rt_tcp_connect(address: string) -> int {
    unsafe {
        let mut sockaddr = RtSockaddr { head: 0, padding: 0 }
        if not rt_tcp_address(address, addr_of(sockaddr)) {
            return -1
        }
        let fd = libc_socket(2, 1, 0)
        if fd < 0 {
            return -1
        }
        if libc_connect(fd, addr_of(sockaddr), 16) < 0 {
            libc_close(fd)
            return -1
        }
        return fd as int
    }
}

-- tcp_read(fd, max_bytes): what the socket has, up to max_bytes
-- bytes, waiting for the first of them. "" when the other side has
-- closed the connection, or on an error. A max_bytes of 0 or less
-- means 4096.
fn rt_tcp_read(fd: int, max_bytes: int) -> string {
    if not rt_fd_ok(fd) {
        return ""
    }
    let mut room = max_bytes
    if room <= 0 {
        room = 4096
    }
    unsafe {
        let buf = rt_string_buffer(room)
        let n = libc_read(fd as i32, buf, room)
        if n <= 0 {
            libc_free(buf)
            return ""
        }
        store_u8(buf + n, 0)
        return string_from_raw(buf, n, room + 1)
    }
}

-- tcp_write(fd, data): send data, and give the number of bytes the
-- socket took, 0 on an error
fn rt_tcp_write(fd: int, data: string) -> int {
    if not rt_fd_ok(fd) {
        return 0
    }
    unsafe {
        let n = libc_write(fd as i32, string_data(data), len(data))
        if n < 0 {
            return 0
        }
        return n
    }
}

-- tcp_close(fd): close the socket
fn rt_tcp_close(fd: int) {
    if not rt_fd_ok(fd) {
        return
    }
    unsafe {
        libc_close(fd as i32)
    }
}
