module os

-- Standard OS Module
-- Operating system interaction via extern fn wrapping C stdlib.

extern fn getenv(name: string) -> string
extern fn setenv(name: string, value: string, overwrite: int) -> int
extern fn getcwd(buf: string, size: int) -> string
extern fn chdir(path: string) -> int

-- Get an environment variable (returns empty string if not set)
fn env_get(key: string) -> string {
    return getenv(key)
}

-- Get the current working directory
fn os_cwd() -> string {
    return getcwd("", 4096)
}

-- Exit with a status code
fn os_exit(code: int) {
    exit(code)
}

-- Get command line arguments count
fn os_args_len() -> int {
    return args_len()
}

-- Get command line argument at index
fn os_args_get(i: int) -> string {
    return args_get(i)
}

-- Run a shell command and return exit code
fn os_system(cmd: string) -> int {
    return system(cmd)
}
