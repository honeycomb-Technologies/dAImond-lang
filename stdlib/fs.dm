module fs

-- Standard Filesystem Module
-- File system operations via extern fn and builtins.

extern fn mkdir(path: string, mode: int) -> int
extern fn rmdir(path: string) -> int
extern fn remove(path: string) -> int
extern fn rename(old_path: string, new_path: string) -> int

-- Read entire file contents
fn fs_read(path: string) -> string {
    return file_read(path)
}

-- Write content to file
fn fs_write(path: string, content: string) {
    file_write(path, content)
}

-- Create a directory (mode 0755)
fn fs_mkdir(path: string) -> int {
    return mkdir(path, 493)
}

-- Remove a directory
fn fs_rmdir(path: string) -> int {
    return rmdir(path)
}

-- Remove a file
fn fs_remove(path: string) -> int {
    return remove(path)
}

-- Rename/move a file
fn fs_rename(old_path: string, new_path: string) -> int {
    return rename(old_path, new_path)
}
