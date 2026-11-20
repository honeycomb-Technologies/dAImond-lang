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
