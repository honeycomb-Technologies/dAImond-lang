module fs

-- Standard Filesystem Module
-- File system operations via builtins.
-- Note: mkdir/rmdir/remove/rename use system() calls since
-- extern fn with string types conflicts with C stdlib declarations.

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

-- Create a directory (uses shell command)
fn fs_mkdir(path: string) -> int {
    return system("mkdir -p " + path)
}

-- Remove a directory (uses shell command)
fn fs_rmdir(path: string) -> int {
    return system("rmdir " + path)
}

-- Remove a file (uses shell command)
fn fs_remove(path: string) -> int {
    return system("rm -f " + path)
}

-- Rename/move a file (uses shell command)
fn fs_rename(old_path: string, new_path: string) -> int {
    return system("mv " + old_path + " " + new_path)
}
