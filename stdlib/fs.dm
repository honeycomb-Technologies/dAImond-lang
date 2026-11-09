module fs

-- Standard Filesystem Module
-- Convenience wrappers around filesystem builtins.
--
-- The builtins fs_mkdir, fs_readdir, fs_remove, fs_rename, fs_getcwd
-- are recognized by the compiler and emitted as direct C calls; call
-- them directly. Wrapper names must not match C runtime symbols
-- (dm_mkdir, dm_readdir, dm_getcwd): a wrapper named mkdir/readdir/
-- getcwd mangles to the same symbol and fails to link, so this
-- module intentionally has no wrappers for the directory builtins.

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
