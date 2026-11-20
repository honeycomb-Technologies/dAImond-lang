module io

-- ============================================================
-- The console
-- print, println, eprint, eprintln, read_line and panic. See
-- runtime/string.dm for how rt_X functions replace the C runtime.
--
-- They use libc's stdio streams, so what a program prints keeps its
-- order with what C code in the same program prints, and libc writes
-- out what is still buffered when the program exits. Standard output
-- is buffered and standard error is not.
-- ============================================================

extern fn libc_fputc(c: i32, stream: int) -> i32 = "fputc"
extern fn libc_getline(line: int, room: int, stream: int) -> int = "getline"

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
