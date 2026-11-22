module os

-- ============================================================
-- Files, directories, the environment, the command line, processes
-- and time
-- file_read, file_write, file_append, file_exists, fs_mkdir,
-- fs_readdir, fs_remove, fs_rename, fs_getcwd, env_get, args_len,
-- args_get, system, time_ms and sleep_ms, and what a program does
-- when it starts. See runtime/string.dm for how rt_X functions replace
-- the C runtime.
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
extern fn libc_clock_gettime(clock: i32, time: int) -> i32 = "clock_gettime"
extern fn libc_nanosleep(time: int, remaining: int) -> i32 = "nanosleep"
extern fn libc_getrlimit(resource: i32, limit: int) -> i32 = "getrlimit"
extern fn libc_setrlimit(resource: i32, limit: int) -> i32 = "setrlimit"

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

-- libc's struct rlimit: the limit in force, and the highest value it
-- may be raised to. Both are unsigned, and all bits set means no limit,
-- which reads here as -1.
struct RtRlimit {
    current: int,
    highest: int
}

-- The compiler calls this first thing in main
fn rt_start() {
    rt_raise_stack_limit()
    rt_catch_stack_overflow()
}

-- Raise the limit on the stack's size to 64 MB where the limit is
-- lower: deep recursion needs more than the usual 8 MB. The stack of
-- a running program may grow up to the limit in force at the time, so
-- this takes effect at once.
fn rt_raise_stack_limit() {
    unsafe {
        let wanted = 64 * 1024 * 1024
        let mut limit = RtRlimit { current: 0, highest: 0 }
        -- 3 is RLIMIT_STACK on Linux
        if libc_getrlimit(3, addr_of(limit)) != 0 {
            return
        }
        if limit.current < 0 or limit.current >= wanted {
            return
        }
        limit.current = wanted
        if limit.highest >= 0 and limit.highest < wanted {
            limit.current = limit.highest
        }
        libc_setrlimit(3, addr_of(limit))
    }
}

-- ============================================================
-- STACK OVERFLOW
-- A program whose calls are nested until the stack is full reads or
-- writes below the stack, which the system answers with the signal
-- SIGSEGV. rt_on_fault takes that signal: for a fault at the stack's
-- end it writes `PANIC: stack overflow` and ends the program with
-- exit code 1, where the program was otherwise killed by the signal
-- with no word of why. It runs on a stack of its own, the full one
-- having no room for it.
--
-- It writes out what standard output still holds, unless the stack
-- filled up inside a stdio call of the runtime's (rt_in_stdio in
-- runtime/io.dm): the stream is then half changed, and what it holds
-- is lost. A program that fills its stack inside a C function it
-- calls itself, which is writing to standard output, is not known to
-- be there. The program then ends with _exit, which runs nothing more.
--
-- Only the main thread has that second stack. A thread made by
-- thread_spawn that fills its stack is killed by the signal, as
-- before.
-- ============================================================

extern fn libc_signal_function(signal: i32, handler: fn(i32, int, int) -> void) -> int = "signal"
extern fn libc_sigaction(signal: i32, action: int, old: int) -> i32 = "sigaction"
extern fn libc_sigaltstack(stack: int, old: int) -> i32 = "sigaltstack"
extern fn libc_write(fd: i32, data: int, count: int) -> int = "write"
extern fn libc_exit_now(code: i32) = "_exit"

-- The signal SIGSEGV on Linux
fn RT_SIGSEGV() -> i32 { return 11 }

-- What is written when the stack is full: the address and the length
-- of the text at static_addr("dm_stack_overflow_note", 16), where a
-- program has put one (the compiler does: its stack is full when the
-- program it compiles is nested too deeply), or else
-- `PANIC: stack overflow`.
fn rt_on_fault(signal: i32, info: int, context: int) {
    unsafe {
        -- Where the program read or wrote (siginfo_t.si_addr), and its
        -- stack pointer at the time (ucontext_t.uc_mcontext.gregs[REG_RSP]),
        -- at their places on x86-64 Linux
        let at = load_i64(info + 16)
        let stack = load_i64(context + 160)
        -- A function that needs stack moves the stack pointer down by
        -- what it needs and then uses that memory, and a call writes
        -- just below the stack pointer: a fault from a full stack is
        -- near the stack pointer, on either side. An address from
        -- anywhere else is not.
        if at > stack - 1048576 and at < stack + 67108864 {
            let note = static_addr("dm_stack_overflow_note", 16)
            if load_i64(note) != 0 {
                libc_write(2, load_i64(note), load_i64(note + 8))
            } else {
                let message = "PANIC: stack overflow\n"
                libc_write(2, string_data(message), len(message))
            }
            if load_i64(rt_in_stdio()) == 0 {
                libc_fflush(rt_stdout())
            }
            libc_exit_now(1)
        }
        -- Any other fault ends the program as it did before: with this
        -- function no longer in the way, the instruction faults again
        libc_signal(RT_SIGSEGV(), 0)
    }
}

-- Have rt_on_fault called for SIGSEGV, on a stack of its own
fn rt_catch_stack_overflow() {
    unsafe {
        -- stack_t: the address, flags (an int) and the size
        let described = static_addr("rt_fault_stack_t", 24)
        store_i64(described, static_addr("rt_fault_stack", 65536))
        store_i64(described + 8, 0)
        store_i64(described + 16, 65536)
        if libc_sigaltstack(described, 0) != 0 { return }
        -- signal() takes the function; sigaction() then gives the
        -- action back as a struct sigaction, in which the flags (an
        -- int at byte 136, after the handler and the 128 bytes of the
        -- signal mask) get SA_SIGINFO (4: the function takes the
        -- fault's details) and SA_ONSTACK (0x08000000: it runs on the
        -- stack given above)
        libc_signal_function(RT_SIGSEGV(), rt_on_fault)
        let action = static_addr("rt_fault_action", 152)
        if libc_sigaction(RT_SIGSEGV(), 0, action) != 0 { return }
        store_i32(action + 136, load_i32(action + 136) | 134217732)
        libc_sigaction(RT_SIGSEGV(), action, 0)
    }
}

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

-- ---- Programs ----

extern fn libc_posix_spawnp(pid: int, file: int, file_actions: int, attributes: int, argv: int, envp: int) -> i32 = "posix_spawnp"
extern fn libc_posix_spawn_file_actions_init(actions: int) -> i32 = "posix_spawn_file_actions_init"
extern fn libc_posix_spawn_file_actions_addopen(actions: int, fd: i32, path: int, flags: i32, mode: u32) -> i32 = "posix_spawn_file_actions_addopen"
extern fn libc_posix_spawn_file_actions_destroy(actions: int) -> i32 = "posix_spawn_file_actions_destroy"
extern fn libc_posix_spawnattr_init(attributes: int) -> i32 = "posix_spawnattr_init"
extern fn libc_posix_spawnattr_setsigdefault(attributes: int, signals: int) -> i32 = "posix_spawnattr_setsigdefault"
extern fn libc_posix_spawnattr_setflags(attributes: int, flags: i16) -> i32 = "posix_spawnattr_setflags"
extern fn libc_posix_spawnattr_destroy(attributes: int) -> i32 = "posix_spawnattr_destroy"
extern fn libc_sigemptyset(signals: int) -> i32 = "sigemptyset"
extern fn libc_sigaddset(signals: int, signal: i32) -> i32 = "sigaddset"
extern fn libc_signal(signal: i32, handler: int) -> int = "signal"
extern fn libc_waitpid(pid: i32, status: int, options: i32) -> i32 = "waitpid"
extern fn libc_calloc(count: int, size: int) -> int = "calloc"

-- run_command(args) and run_command_io(args, input, output, errors):
-- run the program args[0] with the arguments args[1..] and wait for
-- it. No shell takes part: each argument reaches the program as it is.
-- A name without a slash is looked for on PATH. input, output and
-- errors are files that stand for the program's standard input, output
-- and error; "" leaves the stream as this program's own. Gives the
-- program's exit code, 128 plus the signal's number when a signal
-- ended it, or 127 when it could not be started.
--
-- `list` is the address of the List[string]'s header. As libc's
-- system() does, this program ignores an interrupt (SIGINT, SIGQUIT)
-- while it waits, so that Ctrl-C ends the program that runs and leaves
-- this one to see its exit code.
fn rt_run_command(list: int, input: string, output: string, errors: string) -> int {
    unsafe {
        let count = load_i64(list + 8)
        if count < 1 {
            return 127
        }
        let data = load_i64(list)
        if load_i64(data + 8) == 0 {
            return 127
        }
        -- argv: the address of each argument's bytes, then a null
        let argv = libc_malloc((count + 1) * 8)
        if argv == 0 {
            panic("out of memory")
        }
        let mut i = 0
        while i < count {
            store_i64(argv + i * 8, load_i64(data + i * 24))
            i = i + 1
        }
        store_i64(argv + count * 8, 0)

        -- What this program has written so far comes before the
        -- other program's output
        libc_fflush(0)

        -- Room for glibc's posix_spawn_file_actions_t (80 bytes),
        -- posix_spawnattr_t (336 bytes) and sigset_t (128 bytes) on
        -- x86-64 Linux, each with room to spare
        let room = libc_calloc(1, 1024)
        if room == 0 {
            panic("out of memory")
        }
        let actions = room
        let attributes = room + 256
        let defaults = room + 768
        libc_posix_spawn_file_actions_init(actions)
        -- 0 is O_RDONLY; 577 is O_WRONLY | O_CREAT | O_TRUNC; 420 is mode 0644
        if len(input) > 0 { libc_posix_spawn_file_actions_addopen(actions, 0, string_data(input), 0, 0) }
        if len(output) > 0 { libc_posix_spawn_file_actions_addopen(actions, 1, string_data(output), 577, 420) }
        if len(errors) > 0 { libc_posix_spawn_file_actions_addopen(actions, 2, string_data(errors), 577, 420) }

        -- 2 is SIGINT and 3 is SIGQUIT; the handler 1 is SIG_IGN. The
        -- program that runs gets back the default for each signal that
        -- this program did not already ignore.
        libc_posix_spawnattr_init(attributes)
        libc_sigemptyset(defaults)
        let old_int = libc_signal(2, 1)
        let old_quit = libc_signal(3, 1)
        if old_int != 1 { libc_sigaddset(defaults, 2) }
        if old_quit != 1 { libc_sigaddset(defaults, 3) }
        libc_posix_spawnattr_setsigdefault(attributes, defaults)
        -- 4 is POSIX_SPAWN_SETSIGDEF
        libc_posix_spawnattr_setflags(attributes, 4)

        let mut pid = 0
        let started = libc_posix_spawnp(addr_of(pid), load_i64(argv), actions, attributes, argv, load_i64(symbol_addr("environ")))
        libc_posix_spawn_file_actions_destroy(actions)
        libc_posix_spawnattr_destroy(attributes)
        libc_free(argv)
        libc_free(room)

        let mut code = 127
        if started == 0 {
            let mut status = 0
            let mut waited = libc_waitpid(load_i32(addr_of(pid)) as i32, addr_of(status), 0)
            -- 4 is EINTR: a signal arrived while waiting
            while waited < 0 and load_i32(libc_errno_location()) == 4 {
                waited = libc_waitpid(load_i32(addr_of(pid)) as i32, addr_of(status), 0)
            }
            if waited >= 0 {
                let st = load_i32(addr_of(status))
                let low = st % 128
                if low == 0 {
                    code = (st / 256) % 256
                } else {
                    code = 128 + low
                }
            }
        }
        libc_signal(2, old_int)
        libc_signal(3, old_quit)
        return code
    }
}

-- ---- Time ----

-- libc's struct timespec on x86-64 Linux
struct RtTimespec {
    seconds: int,
    nanoseconds: int
}

-- time_ms(): milliseconds on a clock that only moves forward. Its
-- zero is not a date; only the difference of two readings means
-- something.
fn rt_time_ms() -> int {
    unsafe {
        let mut now = RtTimespec { seconds: 0, nanoseconds: 0 }
        -- 1 is CLOCK_MONOTONIC
        libc_clock_gettime(1, addr_of(now))
        return now.seconds * 1000 + now.nanoseconds / 1000000
    }
}

-- sleep_ms(ms): wait for ms milliseconds. A negative ms does not wait.
fn rt_sleep_ms(ms: int) {
    if ms <= 0 {
        return
    }
    unsafe {
        let mut time = RtTimespec { seconds: ms / 1000, nanoseconds: (ms % 1000) * 1000000 }
        libc_nanosleep(addr_of(time), 0)
    }
}
