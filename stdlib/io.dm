module io

-- Standard I/O Module
-- Wraps dAImond I/O built-in functions for organized imports.
-- All functions in this module are available as builtins without import,
-- but importing std.io makes the intent explicit.

-- Output functions
fn io_print(s: string) {
    print(s)
}

fn io_println(s: string) {
    println(s)
}

fn io_eprint(s: string) {
    eprint(s)
}

fn io_eprintln(s: string) {
    eprintln(s)
}

-- File I/O functions
fn io_file_read(path: string) -> string {
    return file_read(path)
}

fn io_file_write(path: string, content: string) {
    file_write(path, content)
}

-- Read a line from stdin
fn io_read_line() -> string {
    return read_line()
}
