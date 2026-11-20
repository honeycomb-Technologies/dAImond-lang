module os

-- ============================================================
-- Files, the command line and processes
-- file_read, file_write, file_append, file_exists, args_len,
-- args_get and system. See runtime/string.dm for how rt_X functions
-- replace the C runtime.
--
-- Files go through libc's stdio. A path or a command goes to C as
-- the address of the string's bytes: every literal and every string
-- the runtime builds is followed by a NUL byte.
-- ============================================================

extern fn libc_fopen(path: int, mode: int) -> int = "fopen"
extern fn libc_fclose(file: int) -> i32 = "fclose"
extern fn libc_fseek(file: int, offset: int, whence: i32) -> i32 = "fseek"
extern fn libc_ftell(file: int) -> int = "ftell"
extern fn libc_fread(buf: int, size: int, count: int, file: int) -> int = "fread"
extern fn libc_fwrite(buf: int, size: int, count: int, file: int) -> int = "fwrite"
extern fn libc_strlen(s: int) -> int = "strlen"
extern fn libc_system(command: int) -> i32 = "system"

-- ---- Files ----

-- Open the file at path in a stdio mode such as "rb". Gives the
-- FILE*, or 0 when the file cannot be opened.
fn rt_file_open(path: string, mode: string) -> int {
    if len(path) == 0 {
        return 0
    }
    unsafe {
        return libc_fopen(string_data(path), string_data(mode))
    }
}

-- file_read(path): the whole file as a string. Panics when the file
-- cannot be opened.
fn rt_file_read(path: string) -> string {
    let file = rt_file_open(path, "rb")
    if file == 0 {
        panic("file_read: cannot open file")
    }
    unsafe {
        let mut size = 0
        if libc_fseek(file, 0, 2) == 0 {
            size = libc_ftell(file)
            libc_fseek(file, 0, 0)
        }
        -- A file that reports its size is read in one piece. One that
        -- reports none is read piece by piece to its end: a file in
        -- /proc reports 0, and a pipe has no size.
        let mut room = size
        if size <= 0 {
            room = 4096
        }
        let mut buf = rt_string_buffer(room)
        let mut count = libc_fread(buf, 1, room, file)
        if size <= 0 {
            let mut got = count
            while got > 0 {
                if count == room {
                    let bigger = rt_string_buffer(room * 2)
                    libc_memcpy(bigger, buf, count)
                    libc_free(buf)
                    buf = bigger
                    room = room * 2
                }
                got = libc_fread(buf + count, 1, room - count, file)
                count = count + got
            }
        }
        libc_fclose(file)
        if count == 0 {
            libc_free(buf)
            return ""
        }
        store_u8(buf + count, 0)
        return string_from_raw(buf, count, room + 1)
    }
}

-- Open the file at path in stdio mode `mode`, write content to it and
-- close it. Panics with `failure` when the file cannot be opened.
fn rt_file_put(path: string, content: string, mode: string, failure: string) {
    let file = rt_file_open(path, mode)
    if file == 0 {
        panic(failure)
    }
    unsafe {
        if len(content) > 0 {
            libc_fwrite(string_data(content), 1, len(content), file)
        }
        libc_fclose(file)
    }
}

-- file_write(path, content): make content the whole of the file
fn rt_file_write(path: string, content: string) {
    rt_file_put(path, content, "wb", "file_write: cannot open file")
}

-- file_append(path, content): add content to the end of the file,
-- creating the file if there is none
fn rt_file_append(path: string, content: string) {
    rt_file_put(path, content, "ab", "file_append: cannot open file")
}

-- file_exists(path): whether the file can be opened for reading
fn rt_file_exists(path: string) -> bool {
    let file = rt_file_open(path, "r")
    if file == 0 {
        return false
    }
    unsafe {
        libc_fclose(file)
    }
    return true
}

-- ---- Command line ----
-- main stores its argument count in the symbol dm_argc, a C int, and
-- its argument vector in dm_argv. The compiler defines both in every
-- program.

-- args_len(): the number of arguments, the program's name included
fn rt_args_len() -> int {
    unsafe {
        return load_i32(symbol_addr("dm_argc"))
    }
}

-- args_get(i): argument i, or "" when there is no such argument.
-- Argument 0 is the program's name.
fn rt_args_get(i: int) -> string {
    unsafe {
        if i < 0 or i >= load_i32(symbol_addr("dm_argc")) {
            return ""
        }
        -- The argument's bytes stay where the OS put them, which is
        -- theirs for as long as the program runs
        let arg = load_i64(load_i64(symbol_addr("dm_argv")) + i * 8)
        return string_from_raw(arg, libc_strlen(arg), 0)
    }
}

-- ---- Processes ----

-- system(command): run a shell command and give its wait status, as
-- libc's system() does
fn rt_system(command: string) -> int {
    if len(command) == 0 {
        return 0
    }
    unsafe {
        return libc_system(string_data(command)) as int
    }
}
