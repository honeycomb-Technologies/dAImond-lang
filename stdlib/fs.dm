module fs

-- Standard Filesystem Module
-- Convenience wrappers around filesystem builtins.
--
-- The builtins fs_mkdir, fs_readdir, fs_remove, fs_rename, fs_getcwd
-- are recognized by the compiler and emitted as direct C calls.
-- These wrapper functions use different names to avoid shadowing.

-- Read entire file contents
fn read(path: string) -> string {
    return file_read(path)
}

-- Write content to file
fn write(path: string, content: string) {
    file_write(path, content)
}

-- Append content to file
fn append(path: string, content: string) {
    file_append(path, content)
}

-- Check if a file exists
fn exists(path: string) -> bool {
    return file_exists(path)
}

-- Create a directory (and parents if needed)
-- Returns 0 on success, -1 on error
fn mkdir(path: string) -> int {
    return fs_mkdir(path)
}

-- Read directory entries as newline-separated string
fn readdir(path: string) -> string {
    return fs_readdir(path)
}

-- Remove a file or empty directory
-- Returns 0 on success, -1 on error
fn remove_file(path: string) -> int {
    return fs_remove(path)
}

-- Rename/move a file or directory
-- Returns 0 on success, -1 on error
fn rename_file(old_path: string, new_path: string) -> int {
    return fs_rename(old_path, new_path)
}

-- Get the current working directory
fn getcwd() -> string {
    return fs_getcwd()
}
