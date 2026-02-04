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

-- Get an environment variable by reading from a temp file
-- (extern fn getenv conflicts with C stdlib, so we use a shell workaround)
fn env_get(key: string) -> string {
    let tmp = "/tmp/_dm_env_" + key
    let rc = system("printenv " + key + " > " + tmp + " 2>/dev/null")
    if rc == 0 {
        let val = file_read(tmp)
        let _ = system("rm -f " + tmp)
        return string_trim(val)
    }
    let _ = system("rm -f " + tmp)
    return ""
}
