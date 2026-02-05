module os

-- Standard OS Module
-- Operating system interaction via builtins.

-- Get command line arguments count
fn os_args_len() -> int {
    return args_len()
}

-- Get command line argument at index
fn os_args_get(i: int) -> string {
    return args_get(i)
}

-- Exit with a status code
fn os_exit(code: int) {
    exit(code)
}

-- Run a shell command and return exit code
fn os_system(cmd: string) -> int {
    return system(cmd)
}

-- Get an environment variable value
-- Returns empty string if the variable is not set
fn os_env_get(key: string) -> string {
    return env_get(key)
}

-- Get the current working directory
fn os_getcwd() -> string {
    return fs_getcwd()
}
