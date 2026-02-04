module net

-- Standard Networking Module
-- Provides TCP socket primitives backed by BSD sockets.
-- All functions are available in the C runtime.

-- TCP Listener: bind and listen for incoming connections
-- extern fn dm_tcp_listen(addr: string) -> TcpListener
-- extern fn dm_tcp_accept(listener: &TcpListener) -> TcpStream
-- extern fn dm_tcp_listener_close(listener: &TcpListener) -> void

-- TCP Stream: connected socket for reading/writing
-- extern fn dm_tcp_connect(addr: string) -> TcpStream
-- extern fn dm_tcp_read(stream: &TcpStream, max_bytes: int) -> string
-- extern fn dm_tcp_write(stream: &TcpStream, data: string) -> int
-- extern fn dm_tcp_close(stream: &TcpStream) -> void

-- Note: Direct use of the runtime networking API requires extern fn declarations.
-- The networking primitives (dm_tcp_*) are available in the C runtime and can be
-- called via extern fn from dAImond code.
--
-- Example:
--   extern fn dm_tcp_listen(addr: string) -> int
--   extern fn dm_tcp_accept(listener: int) -> int
--   extern fn dm_tcp_read(stream: int, max_bytes: int) -> string
--   extern fn dm_tcp_write(stream: int, data: string) -> int
--   extern fn dm_tcp_close(stream: int) -> void
--
--   fn main() {
--       let listener = dm_tcp_listen("0.0.0.0:8080")
--       let client = dm_tcp_accept(listener)
--       let data = dm_tcp_read(client, 4096)
--       println("Received: " + data)
--       dm_tcp_write(client, "Hello from dAImond!")
--       dm_tcp_close(client)
--   }
