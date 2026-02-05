module net

-- Standard Networking Module
-- Provides TCP socket primitives backed by BSD sockets via the C runtime.
-- All operations use integer file descriptors as handles.
--
-- Example:
--   import std.net
--
--   fn handle_client(fd: int) {
--       let data = tcp_read(fd, 4096)
--       println("Received: " + data)
--       tcp_write(fd, "Hello from dAImond!")
--       tcp_close(fd)
--   }
--
--   fn main() {
--       let listener = tcp_listen("0.0.0.0:8080")
--       println("Listening on :8080")
--       let client = tcp_accept(listener)
--       handle_client(client)
--       tcp_close(listener)
--   }

-- Bind and listen on a TCP address (e.g., "0.0.0.0:8080")
-- Returns a listener file descriptor, or -1 on error
fn net_listen(addr: string) -> int {
    return tcp_listen(addr)
}

-- Accept a connection from a listener
-- Blocks until a client connects
-- Returns a client file descriptor, or -1 on error
fn net_accept(listener_fd: int) -> int {
    return tcp_accept(listener_fd)
}

-- Connect to a remote TCP address (e.g., "127.0.0.1:8080")
-- Returns a socket file descriptor, or -1 on error
fn net_connect(addr: string) -> int {
    return tcp_connect(addr)
}

-- Read up to max_bytes from a socket
-- Returns the data read as a string (empty on error or EOF)
fn net_read(fd: int, max_bytes: int) -> string {
    return tcp_read(fd, max_bytes)
}

-- Write data to a socket
-- Returns the number of bytes written
fn net_write(fd: int, data: string) -> int {
    return tcp_write(fd, data)
}

-- Close a socket (listener or stream)
fn net_close(fd: int) {
    tcp_close(fd)
}
