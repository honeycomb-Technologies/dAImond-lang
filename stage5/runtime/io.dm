module io

-- ============================================================
-- The console
-- print, println, eprint, eprintln, flush, read_line, read_key,
-- read_key_nb, term_width, term_height and panic. See
-- runtime/string.dm for how rt_X functions replace the C runtime.
--
-- They use libc's stdio streams, so what a program prints keeps its
-- order with what C code in the same program prints, and libc writes
-- out what is still buffered when the program exits. Standard output
-- is buffered and standard error is not.
-- ============================================================

extern fn libc_fputc(c: i32, stream: int) -> i32 = "fputc"
extern fn libc_getline(line: int, room: int, stream: int) -> int = "getline"
extern fn libc_fflush(stream: int) -> i32 = "fflush"
extern fn libc_fgetc(stream: int) -> i32 = "fgetc"
extern fn libc_poll(fds: int, count: int, timeout: i32) -> i32 = "poll"
extern fn libc_ioctl(fd: i32, request: int, ...) -> i32 = "ioctl"

-- libc's stdin, stdout and stderr are variables that hold a FILE*

fn rt_stdin() -> int {
    unsafe {
        return load_i64(symbol_addr("stdin"))
    }
}

fn rt_stdout() -> int {
    unsafe {
        return load_i64(symbol_addr("stdout"))
    }
}

fn rt_stderr() -> int {
    unsafe {
        return load_i64(symbol_addr("stderr"))
    }
}

-- Write s to a stdio stream
fn rt_stream_write(stream: int, s: string) {
    let n = len(s)
    if n > 0 {
        unsafe {
            libc_fwrite(string_data(s), 1, n, stream)
        }
    }
}

-- Write s and a newline to a stdio stream
fn rt_stream_write_line(stream: int, s: string) {
    rt_stream_write(stream, s)
    unsafe {
        libc_fputc(10, stream)
    }
}

fn rt_print(s: string) {
    rt_stream_write(rt_stdout(), s)
}

fn rt_println(s: string) {
    rt_stream_write_line(rt_stdout(), s)
}

fn rt_eprint(s: string) {
    rt_stream_write(rt_stderr(), s)
}

fn rt_eprintln(s: string) {
    rt_stream_write_line(rt_stderr(), s)
}

-- panic(message): write "PANIC: <message>" to standard error and exit
-- with code 1. Every failed check ends here, the compiler's and the
-- runtime's own, so nothing here may panic or allocate.
fn rt_panic(message: string) {
    let err = rt_stderr()
    rt_stream_write(err, "PANIC: ")
    rt_stream_write_line(err, message)
    exit(1)
}

-- assert(cond): panic unless the condition holds
fn rt_assert(condition: bool) {
    if condition == false {
        panic("assertion failed")
    }
}

-- assert_eq(a, b): the compiler passes whether a == b
fn rt_assert_eq_values(equal: bool) {
    if equal == false {
        panic("assert_eq failed: values not equal")
    }
}

fn rt_assert_eq_strings(equal: bool) {
    if equal == false {
        panic("assert_eq failed: strings not equal")
    }
}

-- read_line(): the next line of standard input without its newline,
-- or "" at the end of input
fn rt_read_line() -> string {
    unsafe {
        -- getline allocates the line and stores its address and the
        -- allocation's size in these two
        let mut line = 0
        let mut room = 0
        let mut n = libc_getline(addr_of(line), addr_of(room), rt_stdin())
        if n > 0 and load_u8(line + n - 1) == 10 {
            n = n - 1
        }
        if n <= 0 {
            libc_free(line)
            return ""
        }
        store_u8(line + n, 0)
        return string_from_raw(line, n, room)
    }
}

-- flush(): write out what standard output still holds
fn rt_flush() {
    unsafe {
        libc_fflush(rt_stdout())
    }
}

-- read_key(): the next byte of standard input, or -1 at its end. It
-- waits for a byte.
fn rt_read_key() -> int {
    unsafe {
        return libc_fgetc(rt_stdin()) as int
    }
}

-- read_key_nb(): the next byte of standard input, or -1 when none is
-- waiting or the input has ended. It does not wait.
fn rt_read_key_nb() -> int {
    unsafe {
        -- A struct pollfd is fd (4 bytes), events (2) and revents (2).
        -- This one is fd 0, standard input, and event 1, POLLIN.
        let mut pollfd = 0
        store_u16(addr_of(pollfd) + 4, 1)
        if libc_poll(addr_of(pollfd), 1, 0) > 0 {
            return libc_fgetc(rt_stdin()) as int
        }
        return -1
    }
}

-- The size of the terminal on standard output as the kernel's struct
-- winsize: rows, columns and two sizes in pixels, 2 bytes each. All
-- are 0 when standard output is not a terminal.
fn rt_term_size() -> int {
    unsafe {
        let mut size = 0
        -- 21523 is TIOCGWINSZ on Linux
        if libc_ioctl(1, 21523, addr_of(size)) != 0 {
            return 0
        }
        return size
    }
}

-- term_width(): the terminal's width in columns, or 80 when standard
-- output is not a terminal
fn rt_term_width() -> int {
    unsafe {
        let mut size = rt_term_size()
        let columns = load_u16(addr_of(size) + 2)
        if columns > 0 {
            return columns
        }
        return 80
    }
}

-- term_height(): the terminal's height in rows, or 24 when standard
-- output is not a terminal
fn rt_term_height() -> int {
    unsafe {
        let mut size = rt_term_size()
        let rows = load_u16(addr_of(size))
        if rows > 0 {
            return rows
        }
        return 24
    }
}
