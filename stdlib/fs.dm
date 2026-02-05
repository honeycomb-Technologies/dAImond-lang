module fs

-- Standard Filesystem Module
-- File system operations via builtins.

-- Read entire file contents
fn fs_read(path: string) -> string {
    return file_read(path)
}

-- Write content to file
fn fs_write(path: string, content: string) {
    file_write(path, content)
}

-- Append content to file
fn fs_append(path: string, content: string) {
    file_append(path, content)
}

-- Check if a file exists
fn fs_exists(path: string) -> bool {
    return file_exists(path)
}

-- Create a directory (and parents if needed)
-- Returns 0 on success, -1 on error
fn fs_mkdir(path: string) -> int {
    return fs_mkdir(path)
}

-- Read directory entries as newline-separated string
fn fs_readdir(path: string) -> string {
    return fs_readdir(path)
}

-- Remove a file or empty directory
-- Returns 0 on success, -1 on error
fn fs_remove(path: string) -> int {
    return fs_remove(path)
}

-- Rename/move a file or directory
-- Returns 0 on success, -1 on error
fn fs_rename(old_path: string, new_path: string) -> int {
    return fs_rename(old_path, new_path)
}

-- Get the current working directory
fn fs_getcwd() -> string {
    return fs_getcwd()
}
