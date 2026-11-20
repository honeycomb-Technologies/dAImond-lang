// LLVM Backend ABI Wrappers
//
// On x86-64, dm_string (24 bytes) is passed on the stack and returned via sret.
// Rather than implementing sret/byval in the LLVM IR generator, these thin
// wrappers expose a pointer-based API that avoids struct-by-value ABI issues.
//
// Functions that exist in the C runtime are called via pointer-based wrapper.
// Functions that Stage 0 codegen emits inline are implemented here directly.

#include "../../stage0/runtime/daimond_runtime.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/resource.h>

// Forward declaration: string pool tracking for mem_mark/mem_sweep
static void _dm_str_track(char* p);

// Increase default stack size to 64MB for LLVM-compiled binaries.
// LLVM passes large structs by value through recursive calls, consuming
// more stack than equivalent C code (e.g., the Stage 1 compiler's ~922-byte
// Compiler struct through recursive descent parsing).
__attribute__((constructor))
static void dm_increase_stack_size(void) {
    struct rlimit rl;
    if (getrlimit(RLIMIT_STACK, &rl) == 0 && rl.rlim_cur < 64 * 1024 * 1024) {
        rl.rlim_cur = 64 * 1024 * 1024;
        setrlimit(RLIMIT_STACK, &rl);
    }
}

// --- Argc/Argv globals (set by main wrapper or linker) ---
int dm_argc = 0;
char** dm_argv = NULL;

// =====================================================================
// String creation (return via output pointer)
// =====================================================================

void llvm_dm_read_line(dm_string* out) {
    *out = dm_read_line();
    if (out->data && out->cap > 0) _dm_str_track((char*)out->data);
}

// =====================================================================
// I/O (take string via pointer)
// =====================================================================

void llvm_dm_print(const dm_string* s) {
    dm_print(*s);
}

void llvm_dm_println(const dm_string* s) {
    dm_println(*s);
}

void llvm_dm_eprint(const dm_string* s) {
    fwrite(s->data, 1, s->len, stderr);
}

void llvm_dm_eprintln(const dm_string* s) {
    fwrite(s->data, 1, s->len, stderr);
    fputc('\n', stderr);
}

// A panic writes "PANIC: <message>" to stderr and exits with code 1, as
// in Stage 0. Callers pass the message without the prefix.
void llvm_dm_panic(const dm_string* s) {
    fputs("PANIC: ", stderr);
    fwrite(s->data, 1, s->len, stderr);
    fputc('\n', stderr);
    exit(1);
}

static void panic_cstr(const char* message) {
    fprintf(stderr, "PANIC: %s\n", message);
    exit(1);
}

// =====================================================================
// File I/O (not in runtime with matching names — implemented inline)
// =====================================================================

void llvm_dm_file_read(dm_string* out, const dm_string* path) {
    char* cpath = (char*)malloc(path->len + 1);
    memcpy(cpath, path->data, path->len);
    cpath[path->len] = '\0';
    FILE* f = fopen(cpath, "rb");
    free(cpath);
    if (!f) {
        panic_cstr("file_read: cannot open file");
    }
    fseek(f, 0, SEEK_END);
    long file_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    char* buf = (char*)malloc((size_t)file_size + 1);
    size_t read_size = fread(buf, 1, (size_t)file_size, f);
    fclose(f);
    buf[read_size] = '\0';
    *out = (dm_string){ .data = buf, .len = read_size, .cap = (size_t)file_size };
}

void llvm_dm_file_write(const dm_string* path, const dm_string* content) {
    char* cpath = (char*)malloc(path->len + 1);
    memcpy(cpath, path->data, path->len);
    cpath[path->len] = '\0';
    FILE* f = fopen(cpath, "wb");
    free(cpath);
    if (!f) {
        panic_cstr("file_write: cannot open file");
    }
    fwrite(content->data, 1, content->len, f);
    fclose(f);
}

void llvm_dm_file_append(const dm_string* path, const dm_string* content) {
    char* cpath = (char*)malloc(path->len + 1);
    memcpy(cpath, path->data, path->len);
    cpath[path->len] = '\0';
    FILE* f = fopen(cpath, "ab");
    free(cpath);
    if (!f) {
        panic_cstr("file_append: cannot open file");
    }
    fwrite(content->data, 1, content->len, f);
    fclose(f);
}

int llvm_dm_file_exists(const dm_string* path) {
    char* cpath = (char*)malloc(path->len + 1);
    memcpy(cpath, path->data, path->len);
    cpath[path->len] = '\0';
    FILE* f = fopen(cpath, "r");
    free(cpath);
    if (f) { fclose(f); return 1; }
    return 0;
}

// =====================================================================
// System/process
// =====================================================================

int64_t llvm_dm_system(const dm_string* cmd) {
    char* c = (char*)malloc(cmd->len + 1);
    memcpy(c, cmd->data, cmd->len);
    c[cmd->len] = '\0';
    int result = system(c);
    free(c);
    return (int64_t)result;
}

int64_t dm_args_len(void) {
    return (int64_t)dm_argc;
}

// Keep the llvm_ prefix version for backward compat
int64_t llvm_dm_args_len(void) {
    return dm_args_len();
}

void llvm_dm_args_get(dm_string* out, int64_t idx) {
    if (idx < 0 || idx >= (int64_t)dm_argc) {
        *out = (dm_string){ .data = "", .len = 0, .cap = 0 };
        return;
    }
    *out = dm_string_new(dm_argv[idx]);
}

// =====================================================================
// Filesystem / OS Builtins
// =====================================================================
// These are implemented inline in Stage 0's C codegen. For Stage 3,
// we implement them as real C functions.

#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <errno.h>

// Helper: convert dm_string to null-terminated C string (caller must free)
static char* dm_fs_to_cstr(dm_string s) {
    char* buf = (char*)malloc(s.len + 1);
    if (!buf) return NULL;
    memcpy(buf, s.data, s.len);
    buf[s.len] = '\0';
    return buf;
}

int64_t dm_fs_mkdir(dm_string path) {
    char* cpath = dm_fs_to_cstr(path);
    if (!cpath) return -1;
    // Create parent directories
    char* p = cpath;
    while (*p) {
        if (*p == '/' && p != cpath) {
            *p = '\0'; mkdir(cpath, 0755); *p = '/';
        }
        p++;
    }
    int result = mkdir(cpath, 0755);
    if (result != 0 && errno == EEXIST) result = 0;
    free(cpath);
    return (int64_t)result;
}

dm_string dm_fs_readdir(dm_string path) {
    char* cpath = dm_fs_to_cstr(path);
    if (!cpath) return (dm_string){ .data = "", .len = 0, .cap = 0 };
    DIR* dir = opendir(cpath);
    free(cpath);
    if (!dir) return (dm_string){ .data = "", .len = 0, .cap = 0 };
    size_t buf_cap = 1024;
    char* buf = (char*)malloc(buf_cap);
    size_t buf_len = 0;
    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) continue;
        size_t name_len = strlen(entry->d_name);
        while (buf_len + name_len + 1 >= buf_cap) { buf_cap *= 2; buf = (char*)realloc(buf, buf_cap); }
        if (buf_len > 0) buf[buf_len++] = '\n';
        memcpy(buf + buf_len, entry->d_name, name_len);
        buf_len += name_len;
    }
    closedir(dir);
    if (buf_len == 0) { free(buf); return (dm_string){ .data = "", .len = 0, .cap = 0 }; }
    buf[buf_len] = '\0';
    return (dm_string){ .data = buf, .len = buf_len, .cap = buf_cap };
}

int64_t dm_fs_remove(dm_string path) {
    char* cpath = dm_fs_to_cstr(path);
    if (!cpath) return -1;
    int result = remove(cpath);
    free(cpath);
    return (int64_t)result;
}

int64_t dm_fs_rename(dm_string old_path, dm_string new_path) {
    char* cold = dm_fs_to_cstr(old_path);
    char* cnew = dm_fs_to_cstr(new_path);
    if (!cold || !cnew) { free(cold); free(cnew); return -1; }
    int result = rename(cold, cnew);
    free(cold); free(cnew);
    return (int64_t)result;
}

dm_string dm_fs_getcwd(void) {
    char buf[4096];
    if (getcwd(buf, sizeof(buf)) == NULL) return (dm_string){ .data = "", .len = 0, .cap = 0 };
    size_t len = strlen(buf);
    char* result = (char*)malloc(len + 1);
    memcpy(result, buf, len + 1);
    return (dm_string){ .data = result, .len = len, .cap = len };
}

dm_string dm_os_getenv(dm_string name) {
    char* cname = dm_fs_to_cstr(name);
    if (!cname) return (dm_string){ .data = "", .len = 0, .cap = 0 };
    const char* val = getenv(cname);
    free(cname);
    if (!val) return (dm_string){ .data = "", .len = 0, .cap = 0 };
    size_t len = strlen(val);
    char* result = (char*)malloc(len + 1);
    memcpy(result, val, len + 1);
    return (dm_string){ .data = result, .len = len, .cap = len };
}

// LLVM ABI wrappers for string-returning fs/os functions
void llvm_dm_fs_readdir(dm_string* out, const dm_string* path) {
    *out = dm_fs_readdir(*path);
}

void llvm_dm_fs_getcwd(dm_string* out) {
    *out = dm_fs_getcwd();
}

void llvm_dm_os_getenv(dm_string* out, const dm_string* name) {
    *out = dm_os_getenv(*name);
}

// LLVM ABI wrappers for string-param fs functions (int return)
int64_t llvm_dm_fs_mkdir(const dm_string* path) {
    return dm_fs_mkdir(*path);
}

int64_t llvm_dm_fs_remove(const dm_string* path) {
    return dm_fs_remove(*path);
}

int64_t llvm_dm_fs_rename(const dm_string* old_path, const dm_string* new_path) {
    return dm_fs_rename(*old_path, *new_path);
}

// =====================================================================
// Path Utilities (inline in Stage 0, implemented here for Stage 3)
// =====================================================================

static dm_string dm_path_dirname_impl(dm_string path) {
    if (path.len == 0) return dm_string_new(".");
    size_t i = path.len;
    while (i > 0 && path.data[i-1] != '/') i--;
    if (i == 0) return dm_string_new(".");
    size_t dlen = i - 1;
    if (dlen == 0) dlen = 1;  // root "/"
    char* buf = (char*)malloc(dlen + 1);
    memcpy(buf, path.data, dlen);
    buf[dlen] = '\0';
    return (dm_string){ .data = buf, .len = dlen, .cap = dlen };
}

static dm_string dm_path_basename_impl(dm_string path) {
    if (path.len == 0) return dm_string_new("");
    size_t i = path.len;
    while (i > 0 && path.data[i-1] != '/') i--;
    size_t blen = path.len - i;
    char* buf = (char*)malloc(blen + 1);
    memcpy(buf, path.data + i, blen);
    buf[blen] = '\0';
    return (dm_string){ .data = buf, .len = blen, .cap = blen };
}

static dm_string dm_path_extension_impl(dm_string path) {
    size_t i = path.len;
    while (i > 0 && path.data[i-1] != '.' && path.data[i-1] != '/') i--;
    if (i == 0 || path.data[i-1] == '/') return dm_string_new("");
    i--;  /* point to the dot */
    size_t elen = path.len - i;
    char* buf = (char*)malloc(elen + 1);
    memcpy(buf, path.data + i, elen);
    buf[elen] = '\0';
    return (dm_string){ .data = buf, .len = elen, .cap = elen };
}

static dm_string dm_path_stem_impl(dm_string path) {
    dm_string base = dm_path_basename_impl(path);
    size_t i = base.len;
    while (i > 0 && base.data[i-1] != '.') i--;
    if (i <= 1) return base;
    size_t slen = i - 1;
    char* buf = (char*)malloc(slen + 1);
    memcpy(buf, base.data, slen);
    buf[slen] = '\0';
    return (dm_string){ .data = buf, .len = slen, .cap = slen };
}

static dm_string dm_path_join_impl(dm_string a, dm_string b) {
    if (a.len == 0) return b;
    if (b.len == 0) return a;
    int need_sep = (a.data[a.len-1] != '/');
    size_t new_len = a.len + (need_sep ? 1 : 0) + b.len;
    char* buf = (char*)malloc(new_len + 1);
    memcpy(buf, a.data, a.len);
    if (need_sep) buf[a.len] = '/';
    memcpy(buf + a.len + (need_sep ? 1 : 0), b.data, b.len);
    buf[new_len] = '\0';
    return (dm_string){ .data = buf, .len = new_len, .cap = new_len };
}

// LLVM ABI wrappers for path functions (string params/returns)
void llvm_dm_path_dirname(dm_string* out, const dm_string* path) {
    *out = dm_path_dirname_impl(*path);
}

void llvm_dm_path_basename(dm_string* out, const dm_string* path) {
    *out = dm_path_basename_impl(*path);
}

void llvm_dm_path_extension(dm_string* out, const dm_string* path) {
    *out = dm_path_extension_impl(*path);
}

void llvm_dm_path_stem(dm_string* out, const dm_string* path) {
    *out = dm_path_stem_impl(*path);
}

void llvm_dm_path_join(dm_string* out, const dm_string* a, const dm_string* b) {
    *out = dm_path_join_impl(*a, *b);
}

// =====================================================================
// Game/Terminal Builtins
// =====================================================================

#include <time.h>
#include <poll.h>
#include <termios.h>
#include <sys/ioctl.h>

// chr(n) -> string: convert codepoint to single-byte string (sret wrapper)
void llvm_dm_chr(dm_string* out, int64_t n) {
    char* buf = (char*)malloc(2);
    buf[0] = (char)n;
    buf[1] = '\0';
    _dm_str_track(buf);
    *out = (dm_string){ .data = buf, .len = 1, .cap = 1 };
}

// flush() -> void: flush stdout
void dm_flush(void) {
    fflush(stdout);
}

// read_key() -> int: blocking single-key read
int64_t dm_read_key(void) {
    return (int64_t)getchar();
}

// read_key_nb() -> int: non-blocking key read (-1 if nothing)
int64_t dm_read_key_nb(void) {
    struct pollfd pfd = { .fd = 0, .events = POLLIN, .revents = 0 };
    if (poll(&pfd, 1, 0) > 0) return (int64_t)getchar();
    return -1;
}

// time_ms() -> int: current monotonic time in milliseconds
int64_t dm_time_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000 + (int64_t)ts.tv_nsec / 1000000;
}

// sleep_ms(ms) -> void: sleep for ms milliseconds
void dm_sleep_ms(int64_t ms) {
    struct timespec ts;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (ms % 1000) * 1000000;
    nanosleep(&ts, NULL);
}

// String allocation pool for mem_mark/mem_sweep
static char** _dm_str_pool = NULL;
static int64_t _dm_str_pool_len = 0;
static int64_t _dm_str_pool_cap = 0;

static void _dm_str_track(char* p) {
    if (_dm_str_pool_len >= _dm_str_pool_cap) {
        _dm_str_pool_cap = _dm_str_pool_cap ? _dm_str_pool_cap * 2 : 4096;
        _dm_str_pool = (char**)realloc(_dm_str_pool, (size_t)_dm_str_pool_cap * sizeof(char*));
    }
    _dm_str_pool[_dm_str_pool_len++] = p;
}

// mem_mark() -> int: return current pool position
int64_t dm_mem_mark(void) {
    return _dm_str_pool_len;
}

// mem_sweep(mark) -> void: documented no-op (Phase 2 semantics, as in
// Stage 0). Strings are never freed here, so values created after the
// mark stay valid after the sweep; the pool is bookkeeping only.
void dm_mem_sweep(int64_t mark) {
    (void)mark;
}

// term_width() -> int: terminal column count (default 80)
int64_t dm_term_width(void) {
    struct winsize ws;
    if (ioctl(1, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0) return (int64_t)ws.ws_col;
    return 80;
}

// term_height() -> int: terminal row count (default 24)
int64_t dm_term_height(void) {
    struct winsize ws;
    if (ioctl(1, TIOCGWINSZ, &ws) == 0 && ws.ws_row > 0) return (int64_t)ws.ws_row;
    return 24;
}
