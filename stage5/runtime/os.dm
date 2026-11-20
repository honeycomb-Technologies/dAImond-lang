module os

-- ============================================================
-- Files, directories, the environment, the command line and processes
-- file_read, file_write, file_append, file_exists, fs_mkdir,
-- fs_readdir, fs_remove, fs_rename, fs_getcwd, env_get, args_len,
-- args_get and system. See runtime/string.dm for how rt_X functions
-- replace the C runtime.
--
-- Files go through libc's stdio. A path, a name or a command goes to
-- C as the address of the string's bytes: every literal and every
-- string the runtime builds is followed by a NUL byte.
-- ============================================================

extern fn libc_fopen(path: int, mode: int) -> int = "fopen"
extern fn libc_fclose(file: int) -> i32 = "fclose"
extern fn libc_fseek(file: int, offset: int, whence: i32) -> i32 = "fseek"
extern fn libc_ftell(file: int) -> int = "ftell"
extern fn libc_fread(buf: int, size: int, count: int, file: int) -> int = "fread"
extern fn libc_fwrite(buf: int, size: int, count: int, file: int) -> int = "fwrite"
extern fn libc_strlen(s: int) -> int = "strlen"
extern fn libc_system(command: int) -> i32 = "system"
extern fn libc_realloc(p: int, n: int) -> int = "realloc"
extern fn libc_mkdir(path: int, mode: u32) -> i32 = "mkdir"
extern fn libc_opendir(path: int) -> int = "opendir"
extern fn libc_readdir(dir: int) -> int = "readdir"
extern fn libc_closedir(dir: int) -> i32 = "closedir"
extern fn libc_remove(path: int) -> i32 = "remove"
extern fn libc_rename(from: int, to: int) -> i32 = "rename"
extern fn libc_getcwd(buf: int, size: int) -> int = "getcwd"
extern fn libc_getenv(name: int) -> int = "getenv"
extern fn libc_errno_location() -> int = "__errno_location"

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

-- ---- Directories ----

-- Where the name starts in the struct dirent that readdir gives, on
-- x86-64 Linux: after d_ino (8 bytes), d_off (8), d_reclen (2) and
-- d_type (1)
fn rt_dirent_name_offset() -> int {
    return 19
}

-- fs_mkdir(path): create the directory and the directories above it
-- that are missing. Gives 0 when the path exists afterwards and -1
-- when it could not be created.
fn rt_fs_mkdir(path: string) -> int {
    let n = len(path)
    unsafe {
        -- A copy of the path, to end early at each "/" in turn
        let buf = rt_string_buffer(n)
        if n > 0 {
            libc_memcpy(buf, string_data(path), n)
        }
        let mut i = 1
        while i < n {
            if load_u8(buf + i) == 47 {
                store_u8(buf + i, 0)
                -- 493 is mode 0755
                libc_mkdir(buf, 493)
                store_u8(buf + i, 47)
            }
            i = i + 1
        }
        let mut result = libc_mkdir(buf, 493) as int
        -- 17 is EEXIST
        if result != 0 and load_i32(libc_errno_location()) == 17 {
            result = 0
        }
        libc_free(buf)
        return result
    }
}

-- fs_readdir(path): the names in the directory, one per line, in the
-- order the OS gives them and without "." and "..". Gives "" when the
-- directory cannot be read.
fn rt_fs_readdir(path: string) -> string {
    if len(path) == 0 {
        return ""
    }
    unsafe {
        let dir = libc_opendir(string_data(path))
        if dir == 0 {
            return ""
        }
        let mut room = 1024
        let mut buf = rt_string_buffer(room)
        let mut count = 0
        let mut entry = libc_readdir(dir)
        while entry != 0 {
            let name = entry + rt_dirent_name_offset()
            let name_len = libc_strlen(name)
            -- 46 is "."
            let is_self = name_len == 1 and load_u8(name) == 46
            let is_parent = name_len == 2 and load_u8(name) == 46 and load_u8(name + 1) == 46
            if is_self == false and is_parent == false {
                -- Room for a newline and the name
                let mut needed = room
                while count + 1 + name_len > needed {
                    needed = needed * 2
                }
                if needed != room {
                    buf = libc_realloc(buf, needed + 1)
                    if buf == 0 {
                        panic("out of memory")
                    }
                    room = needed
                }
                if count > 0 {
                    store_u8(buf + count, 10)
                    count = count + 1
                }
                libc_memcpy(buf + count, name, name_len)
                count = count + name_len
            }
            entry = libc_readdir(dir)
        }
        libc_closedir(dir)
        if count == 0 {
            libc_free(buf)
            return ""
        }
        store_u8(buf + count, 0)
        return string_from_raw(buf, count, room + 1)
    }
}

-- fs_remove(path): delete the file or the empty directory. Gives 0,
-- or -1 when it could not be deleted.
fn rt_fs_remove(path: string) -> int {
    if len(path) == 0 {
        return -1
    }
    unsafe {
        return libc_remove(string_data(path)) as int
    }
}

-- fs_rename(from, to): move a file or directory. Gives 0, or -1 when
-- it could not be moved.
fn rt_fs_rename(from: string, to: string) -> int {
    if len(from) == 0 or len(to) == 0 {
        return -1
    }
    unsafe {
        return libc_rename(string_data(from), string_data(to)) as int
    }
}

-- fs_getcwd(): the current directory, or "" when it cannot be found
fn rt_fs_getcwd() -> string {
    unsafe {
        -- Given no buffer, getcwd allocates one of the size it needs
        let cwd = libc_getcwd(0, 0)
        if cwd == 0 {
            return ""
        }
        let n = libc_strlen(cwd)
        return string_from_raw(cwd, n, n + 1)
    }
}

-- ---- Environment ----

-- env_get(name): the value of the environment variable, or "" when
-- it is not set
fn rt_os_getenv(name: string) -> string {
    if len(name) == 0 {
        return ""
    }
    unsafe {
        let value = libc_getenv(string_data(name))
        if value == 0 {
            return ""
        }
        -- A copy: the environment's own bytes move when a variable is set
        return rt_string_copy(value, libc_strlen(value))
    }
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
