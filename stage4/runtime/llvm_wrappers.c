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

void llvm_dm_string_new(dm_string* out, const char* s) {
    *out = dm_string_new(s);
}

void llvm_dm_string_concat(dm_string* out, const dm_string* a, const dm_string* b) {
    *out = dm_string_concat(*a, *b);
}

void llvm_dm_int_to_string(dm_string* out, int64_t n) {
    *out = dm_int_to_string(n);
}

void llvm_dm_float_to_string(dm_string* out, double n) {
    *out = dm_float_to_string(n);
}

void llvm_dm_bool_to_string(dm_string* out, int b) {
    *out = b ? dm_string_new("true") : dm_string_new("false");
}

void llvm_dm_read_line(dm_string* out) {
    *out = dm_read_line();
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

void llvm_dm_panic(const dm_string* s) {
    fwrite(s->data, 1, s->len, stderr);
    fputc('\n', stderr);
    exit(1);
}

// =====================================================================
// String queries (take string via pointer, return scalar)
// =====================================================================

int64_t llvm_dm_parse_int(const dm_string* s) {
    char buf[32];
    size_t copy_len = s->len < 31 ? s->len : 31;
    memcpy(buf, s->data, copy_len);
    buf[copy_len] = '\0';
    return (int64_t)atoll(buf);
}

double llvm_dm_parse_float(const dm_string* s) {
    char buf[64];
    size_t copy_len = s->len < 63 ? s->len : 63;
    memcpy(buf, s->data, copy_len);
    buf[copy_len] = '\0';
    return atof(buf);
}

int64_t llvm_dm_string_len(const dm_string* s) {
    return (int64_t)s->len;
}

int llvm_dm_string_eq(const dm_string* a, const dm_string* b) {
    return dm_string_eq(*a, *b);
}

int llvm_dm_string_contains(const dm_string* a, const dm_string* b) {
    if (b->len == 0) return 1;
    if (b->len > a->len) return 0;
    for (size_t i = 0; i <= a->len - b->len; i++) {
        if (memcmp(a->data + i, b->data, b->len) == 0) return 1;
    }
    return 0;
}

// =====================================================================
// String comparison (ordering) — calls runtime dm_string_cmp
// =====================================================================

int64_t llvm_dm_string_cmp(const dm_string* a, const dm_string* b) {
    return (int64_t)dm_string_cmp(*a, *b);
}

// =====================================================================
// Character/substring operations
// =====================================================================

// char_at(s, i) -> single-character string
void llvm_dm_char_at(dm_string* out, const dm_string* s, int64_t idx) {
    if (idx < 0 || (size_t)idx >= s->len) {
        *out = (dm_string){ .data = "", .len = 0, .cap = 0 };
        return;
    }
    char* buf = (char*)malloc(2);
    buf[0] = s->data[idx];
    buf[1] = '\0';
    *out = (dm_string){ .data = buf, .len = 1, .cap = 1 };
}

// char_to_string(s) -> identity for string (Stage 3 char_at already returns string)
void llvm_dm_char_to_string(dm_string* out, const dm_string* s) {
    *out = *s;
}

// is_alpha(s) -> bool (checks first char)
int64_t llvm_dm_is_alpha(const dm_string* s) {
    if (s->len == 0) return 0;
    char c = s->data[0];
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ? 1 : 0;
}

// is_digit(s) -> bool (checks first char)
int64_t llvm_dm_is_digit(const dm_string* s) {
    if (s->len == 0) return 0;
    char c = s->data[0];
    return (c >= '0' && c <= '9') ? 1 : 0;
}

// is_whitespace(s) -> bool (checks first char)
int64_t llvm_dm_is_whitespace(const dm_string* s) {
    if (s->len == 0) return 0;
    char c = s->data[0];
    return (c == ' ' || c == '\t' || c == '\n' || c == '\r') ? 1 : 0;
}

// is_alnum(s) -> bool (checks first char)
int64_t llvm_dm_is_alnum(const dm_string* s) {
    if (s->len == 0) return 0;
    char c = s->data[0];
    return ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) ? 1 : 0;
}

// substr(s, start, length) -> substring
void llvm_dm_substr(dm_string* out, const dm_string* s, int64_t start, int64_t length) {
    if (start < 0 || (size_t)start >= s->len) {
        *out = (dm_string){ .data = "", .len = 0, .cap = 0 };
        return;
    }
    size_t actual_len = (size_t)length;
    if ((size_t)start + actual_len > s->len) actual_len = s->len - (size_t)start;
    char* buf = (char*)malloc(actual_len + 1);
    memcpy(buf, s->data + start, actual_len);
    buf[actual_len] = '\0';
    *out = (dm_string){ .data = buf, .len = actual_len, .cap = actual_len };
}

// =====================================================================
// String search — calls runtime where available
// =====================================================================

int64_t llvm_dm_string_find(const dm_string* a, const dm_string* b) {
    if (b->len == 0) return 0;
    if (b->len > a->len) return -1;
    for (size_t i = 0; i <= a->len - b->len; i++) {
        if (memcmp(a->data + i, b->data, b->len) == 0) return (int64_t)i;
    }
    return -1;
}

int llvm_dm_starts_with(const dm_string* s, const dm_string* prefix) {
    return dm_string_starts_with(*s, *prefix);
}

int llvm_dm_ends_with(const dm_string* s, const dm_string* suffix) {
    return dm_string_ends_with(*s, *suffix);
}

// =====================================================================
// String transformation (not in runtime — implemented inline)
// =====================================================================

void llvm_dm_string_trim(dm_string* out, const dm_string* s) {
    size_t start = 0;
    while (start < s->len && (s->data[start] == ' ' || s->data[start] == '\t' ||
           s->data[start] == '\n' || s->data[start] == '\r')) start++;
    size_t end = s->len;
    while (end > start && (s->data[end-1] == ' ' || s->data[end-1] == '\t' ||
           s->data[end-1] == '\n' || s->data[end-1] == '\r')) end--;
    size_t new_len = end - start;
    if (new_len == 0) {
        *out = (dm_string){ .data = "", .len = 0, .cap = 0 };
        return;
    }
    char* buf = (char*)malloc(new_len + 1);
    memcpy(buf, s->data + start, new_len);
    buf[new_len] = '\0';
    *out = (dm_string){ .data = buf, .len = new_len, .cap = new_len };
}

void llvm_dm_string_replace(dm_string* out, const dm_string* s, const dm_string* old_s, const dm_string* new_s) {
    if (old_s->len == 0) {
        char* buf = (char*)malloc(s->len + 1);
        memcpy(buf, s->data, s->len);
        buf[s->len] = '\0';
        *out = (dm_string){ .data = buf, .len = s->len, .cap = s->len };
        return;
    }
    // Count occurrences
    size_t count = 0;
    for (size_t i = 0; i + old_s->len <= s->len; i++) {
        if (memcmp(s->data + i, old_s->data, old_s->len) == 0) { count++; i += old_s->len - 1; }
    }
    if (count == 0) {
        char* buf = (char*)malloc(s->len + 1);
        memcpy(buf, s->data, s->len);
        buf[s->len] = '\0';
        *out = (dm_string){ .data = buf, .len = s->len, .cap = s->len };
        return;
    }
    size_t new_len = s->len + count * (new_s->len - old_s->len);
    char* buf = (char*)malloc(new_len + 1);
    size_t pos = 0;
    for (size_t i = 0; i < s->len; ) {
        if (i + old_s->len <= s->len && memcmp(s->data + i, old_s->data, old_s->len) == 0) {
            memcpy(buf + pos, new_s->data, new_s->len); pos += new_s->len; i += old_s->len;
        } else { buf[pos++] = s->data[i++]; }
    }
    buf[new_len] = '\0';
    *out = (dm_string){ .data = buf, .len = new_len, .cap = new_len };
}

void llvm_dm_string_to_upper(dm_string* out, const dm_string* s) {
    char* buf = (char*)malloc(s->len + 1);
    for (size_t i = 0; i < s->len; i++)
        buf[i] = (s->data[i] >= 'a' && s->data[i] <= 'z') ? s->data[i] - 32 : s->data[i];
    buf[s->len] = '\0';
    *out = (dm_string){ .data = buf, .len = s->len, .cap = s->len };
}

void llvm_dm_string_to_lower(dm_string* out, const dm_string* s) {
    char* buf = (char*)malloc(s->len + 1);
    for (size_t i = 0; i < s->len; i++)
        buf[i] = (s->data[i] >= 'A' && s->data[i] <= 'Z') ? s->data[i] + 32 : s->data[i];
    buf[s->len] = '\0';
    *out = (dm_string){ .data = buf, .len = s->len, .cap = s->len };
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
        fprintf(stderr, "file_read: cannot open file\n");
        exit(1);
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
        fprintf(stderr, "file_write: cannot open file\n");
        exit(1);
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
        fprintf(stderr, "file_append: cannot open file\n");
        exit(1);
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
// Typed List Operations (int64_t)
// =====================================================================

typedef struct {
    int64_t* data;
    int64_t len;
    int64_t capacity;
} dm_list_int64;

void dm_list_int64_new(dm_list_int64* out) {
    out->data = NULL;
    out->len = 0;
    out->capacity = 0;
}

void dm_list_int64_push(dm_list_int64* list, int64_t value) {
    if (list->len >= list->capacity) {
        int64_t new_cap = list->capacity == 0 ? 8 : list->capacity * 2;
        int64_t* new_data = (int64_t*)realloc(list->data, (size_t)new_cap * sizeof(int64_t));
        if (!new_data) { fprintf(stderr, "list push: out of memory\n"); exit(1); }
        list->data = new_data;
        list->capacity = new_cap;
    }
    list->data[list->len] = value;
    list->len++;
}

int64_t dm_list_int64_get(dm_list_int64* list, int64_t index) {
    if (index < 0 || index >= list->len) {
        fprintf(stderr, "list index out of bounds: %lld (len=%lld)\n", (long long)index, (long long)list->len);
        exit(1);
    }
    return list->data[index];
}

int64_t dm_list_int64_len(dm_list_int64* list) {
    return list->len;
}

int64_t dm_list_int64_pop(dm_list_int64* list) {
    if (list->len == 0) { fprintf(stderr, "list pop: empty list\n"); exit(1); }
    list->len--;
    return list->data[list->len];
}

int64_t dm_list_int64_contains(dm_list_int64* list, int64_t value) {
    for (size_t i = 0; i < list->len; i++) {
        if (list->data[i] == value) return 1;
    }
    return 0;
}

// =====================================================================
// Typed List Operations (dm_string)
// =====================================================================

typedef struct {
    dm_string* data;
    int64_t len;
    int64_t capacity;
} dm_list_dm_string;

void dm_list_string_new(dm_list_dm_string* out) {
    out->data = NULL;
    out->len = 0;
    out->capacity = 0;
}

void dm_list_string_push(dm_list_dm_string* list, const dm_string* value) {
    if (list->len >= list->capacity) {
        int64_t new_cap = list->capacity == 0 ? 8 : list->capacity * 2;
        dm_string* new_data = (dm_string*)realloc(list->data, (size_t)new_cap * sizeof(dm_string));
        if (!new_data) { fprintf(stderr, "list push: out of memory\n"); exit(1); }
        list->data = new_data;
        list->capacity = new_cap;
    }
    list->data[list->len] = *value;
    list->len++;
}

void dm_list_string_get(dm_string* out, dm_list_dm_string* list, int64_t index) {
    if (index < 0 || index >= list->len) {
        fprintf(stderr, "list index out of bounds: %lld (len=%lld)\n", (long long)index, (long long)list->len);
        exit(1);
    }
    *out = list->data[index];
}

int64_t dm_list_string_len(dm_list_dm_string* list) {
    return list->len;
}

// LLVM ABI wrappers for string list (llvm_ prefix for string-param externs)
void llvm_dm_list_string_push(dm_list_dm_string* list, const dm_string* value) {
    dm_list_string_push(list, value);
}

void llvm_dm_list_string_get(dm_string* out, dm_list_dm_string* list, int64_t index) {
    dm_list_string_get(out, list, index);
}

int64_t dm_list_string_contains(dm_list_dm_string* list, const dm_string* value) {
    for (size_t i = 0; i < list->len; i++) {
        if (list->data[i].len == value->len &&
            memcmp(list->data[i].data, value->data, value->len) == 0) return 1;
    }
    return 0;
}

// LLVM ABI wrapper for string list contains
int64_t llvm_dm_list_string_contains(dm_list_dm_string* list, const dm_string* value) {
    return dm_list_string_contains(list, value);
}

// =====================================================================
// Typed List Operations (double / float)
// =====================================================================

typedef struct {
    double* data;
    int64_t len;
    int64_t capacity;
} dm_list_double;

void dm_list_double_new(dm_list_double* out) {
    out->data = NULL;
    out->len = 0;
    out->capacity = 0;
}

void dm_list_double_push(dm_list_double* list, double value) {
    if (list->len >= list->capacity) {
        int64_t new_cap = list->capacity == 0 ? 8 : list->capacity * 2;
        double* new_data = (double*)realloc(list->data, (size_t)new_cap * sizeof(double));
        if (!new_data) { fprintf(stderr, "list push: out of memory\n"); exit(1); }
        list->data = new_data;
        list->capacity = new_cap;
    }
    list->data[list->len] = value;
    list->len++;
}

double dm_list_double_get(dm_list_double* list, int64_t index) {
    if (index < 0 || index >= list->len) {
        fprintf(stderr, "list index out of bounds: %lld (len=%lld)\n", (long long)index, (long long)list->len);
        exit(1);
    }
    return list->data[index];
}

int64_t dm_list_double_len(dm_list_double* list) {
    return list->len;
}

// =====================================================================
// Generic List Operations (for struct types — uses void* + elem_size)
// =====================================================================

typedef struct {
    void* data;
    int64_t len;
    int64_t capacity;
} dm_list_generic;

void dm_list_generic_new(dm_list_generic* out) {
    out->data = NULL;
    out->len = 0;
    out->capacity = 0;
}

void dm_list_generic_push(dm_list_generic* list, const void* value, int64_t elem_size) {
    if (list->len >= list->capacity) {
        int64_t new_cap = list->capacity == 0 ? 8 : list->capacity * 2;
        void* new_data = realloc(list->data, (size_t)(new_cap * elem_size));
        if (!new_data) { fprintf(stderr, "list push: out of memory\n"); exit(1); }
        list->data = new_data;
        list->capacity = new_cap;
    }
    memcpy((char*)list->data + list->len * elem_size, value, (size_t)elem_size);
    list->len++;
}

void dm_list_generic_get(void* out, dm_list_generic* list, int64_t index, int64_t elem_size) {
    if (index < 0 || index >= list->len) {
        fprintf(stderr, "list index out of bounds: %lld (len=%lld)\n", (long long)index, (long long)list->len);
        exit(1);
    }
    memcpy(out, (char*)list->data + index * elem_size, (size_t)elem_size);
}

int64_t dm_list_generic_len(dm_list_generic* list) {
    return list->len;
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
// String split — returns a List[string] (dm_list_dm_string struct)
// =====================================================================

void dm_string_split(dm_list_dm_string* out, const dm_string* s, const dm_string* delim) {
    dm_list_string_new(out);
    if (s->len == 0) return;
    if (delim->len == 0) {
        // Split into individual characters
        for (size_t i = 0; i < s->len; i++) {
            char* ch = (char*)malloc(2);
            ch[0] = s->data[i];
            ch[1] = '\0';
            dm_string part = { .data = ch, .len = 1, .cap = 1 };
            dm_list_string_push(out, &part);
        }
        return;
    }
    size_t start = 0;
    for (size_t i = 0; i + delim->len <= s->len; i++) {
        if (memcmp(s->data + i, delim->data, delim->len) == 0) {
            size_t part_len = i - start;
            char* buf = (char*)malloc(part_len + 1);
            memcpy(buf, s->data + start, part_len);
            buf[part_len] = '\0';
            dm_string part = { .data = buf, .len = part_len, .cap = part_len };
            dm_list_string_push(out, &part);
            start = i + delim->len;
            i += delim->len - 1;
        }
    }
    // Last part
    size_t part_len = s->len - start;
    char* buf = (char*)malloc(part_len + 1);
    memcpy(buf, s->data + start, part_len);
    buf[part_len] = '\0';
    dm_string part = { .data = buf, .len = part_len, .cap = part_len };
    dm_list_string_push(out, &part);
}

// =====================================================================
// Map Types — Monomorphized hash map implementations for LLVM backend
// =====================================================================

// --- Map[string, int] ---
typedef struct {
    dm_string key;
    int64_t value;
    int state;  /* 0=empty, 1=occupied, 2=tombstone */
} dm_map_string_int_entry;

typedef struct {
    dm_map_string_int_entry* entries;
    int64_t len;
    int64_t capacity;
} dm_map_string_int;

static size_t dm_map_string_int_hash(const dm_string* key) {
    size_t h = 14695981039346656037ULL;
    for (size_t i = 0; i < key->len; i++) {
        h ^= (unsigned char)key->data[i];
        h *= 1099511628211ULL;
    }
    return h;
}

static size_t dm_map_string_int_find_slot(dm_map_string_int* map, const dm_string* key) {
    size_t idx = dm_map_string_int_hash(key) & ((size_t)map->capacity - 1);
    size_t first_tombstone = (size_t)-1;
    for (size_t i = 0; i < (size_t)map->capacity; i++) {
        if (map->entries[idx].state == 0)
            return (first_tombstone != (size_t)-1) ? first_tombstone : idx;
        if (map->entries[idx].state == 1 && dm_string_eq(map->entries[idx].key, *key))
            return idx;
        if (map->entries[idx].state == 2 && first_tombstone == (size_t)-1)
            first_tombstone = idx;
        idx = (idx + 1) & ((size_t)map->capacity - 1);
    }
    return (first_tombstone != (size_t)-1) ? first_tombstone : idx;
}

static void dm_map_string_int_resize(dm_map_string_int* map) {
    int64_t new_cap = map->capacity == 0 ? 16 : map->capacity * 2;
    dm_map_string_int_entry* new_entries = (dm_map_string_int_entry*)calloc((size_t)new_cap, sizeof(dm_map_string_int_entry));
    if (!new_entries) { fprintf(stderr, "map resize: out of memory\n"); exit(1); }
    dm_map_string_int new_map = { .entries = new_entries, .len = 0, .capacity = new_cap };
    for (int64_t i = 0; i < map->capacity; i++) {
        if (map->entries[i].state == 1) {
            size_t slot = dm_map_string_int_find_slot(&new_map, &map->entries[i].key);
            new_map.entries[slot] = map->entries[i];
            new_map.entries[slot].state = 1;
            new_map.len++;
        }
    }
    free(map->entries);
    map->entries = new_entries;
    map->capacity = new_cap;
}

void dm_map_string_int_new(dm_map_string_int* out) {
    out->entries = NULL;
    out->len = 0;
    out->capacity = 0;
}

void dm_map_string_int_insert(dm_map_string_int* map, const dm_string* key, int64_t value) {
    if (map->capacity == 0 || (map->len + 1) * 4 > map->capacity * 3)
        dm_map_string_int_resize(map);
    size_t slot = dm_map_string_int_find_slot(map, key);
    if (map->entries[slot].state != 1) map->len++;
    map->entries[slot].key = *key;
    map->entries[slot].value = value;
    map->entries[slot].state = 1;
}

int64_t dm_map_string_int_get(dm_map_string_int* map, const dm_string* key) {
    if (map->capacity == 0) { fprintf(stderr, "map get: key not found\n"); exit(1); }
    size_t slot = dm_map_string_int_find_slot(map, key);
    if (map->entries[slot].state != 1) { fprintf(stderr, "map get: key not found\n"); exit(1); }
    return map->entries[slot].value;
}

int dm_map_string_int_contains(dm_map_string_int* map, const dm_string* key) {
    if (map->capacity == 0) return 0;
    size_t slot = dm_map_string_int_find_slot(map, key);
    return map->entries[slot].state == 1;
}

void dm_map_string_int_remove(dm_map_string_int* map, const dm_string* key) {
    if (map->capacity == 0) return;
    size_t slot = dm_map_string_int_find_slot(map, key);
    if (map->entries[slot].state == 1) {
        map->entries[slot].state = 2;
        map->len--;
    }
}

int64_t dm_map_string_int_len(dm_map_string_int* map) {
    return map->len;
}

void dm_map_string_int_keys(dm_list_dm_string* out, dm_map_string_int* map) {
    dm_list_string_new(out);
    for (int64_t i = 0; i < map->capacity; i++) {
        if (map->entries[i].state == 1) {
            dm_list_string_push(out, &map->entries[i].key);
        }
    }
}

void dm_map_string_int_values(dm_list_int64* out, dm_map_string_int* map) {
    dm_list_int64_new(out);
    for (int64_t i = 0; i < map->capacity; i++) {
        if (map->entries[i].state == 1) {
            dm_list_int64_push(out, map->entries[i].value);
        }
    }
}

// --- Map[int, string] ---
typedef struct {
    int64_t key;
    dm_string value;
    int state;
} dm_map_int_string_entry;

typedef struct {
    dm_map_int_string_entry* entries;
    int64_t len;
    int64_t capacity;
} dm_map_int_string;

static size_t dm_map_int_string_hash(int64_t key) {
    uint64_t x = (uint64_t)key;
    x ^= x >> 30; x *= 0xbf58476d1ce4e5b9ULL;
    x ^= x >> 27; x *= 0x94d049bb133111ebULL;
    x ^= x >> 31;
    return (size_t)x;
}

static size_t dm_map_int_string_find_slot(dm_map_int_string* map, int64_t key) {
    size_t idx = dm_map_int_string_hash(key) & ((size_t)map->capacity - 1);
    size_t first_tombstone = (size_t)-1;
    for (size_t i = 0; i < (size_t)map->capacity; i++) {
        if (map->entries[idx].state == 0)
            return (first_tombstone != (size_t)-1) ? first_tombstone : idx;
        if (map->entries[idx].state == 1 && map->entries[idx].key == key)
            return idx;
        if (map->entries[idx].state == 2 && first_tombstone == (size_t)-1)
            first_tombstone = idx;
        idx = (idx + 1) & ((size_t)map->capacity - 1);
    }
    return (first_tombstone != (size_t)-1) ? first_tombstone : idx;
}

static void dm_map_int_string_resize(dm_map_int_string* map) {
    int64_t new_cap = map->capacity == 0 ? 16 : map->capacity * 2;
    dm_map_int_string_entry* new_entries = (dm_map_int_string_entry*)calloc((size_t)new_cap, sizeof(dm_map_int_string_entry));
    if (!new_entries) { fprintf(stderr, "map resize: out of memory\n"); exit(1); }
    dm_map_int_string new_map = { .entries = new_entries, .len = 0, .capacity = new_cap };
    for (int64_t i = 0; i < map->capacity; i++) {
        if (map->entries[i].state == 1) {
            size_t slot = dm_map_int_string_find_slot(&new_map, map->entries[i].key);
            new_map.entries[slot] = map->entries[i];
            new_map.entries[slot].state = 1;
            new_map.len++;
        }
    }
    free(map->entries);
    map->entries = new_entries;
    map->capacity = new_cap;
}

void dm_map_int_string_new(dm_map_int_string* out) {
    out->entries = NULL; out->len = 0; out->capacity = 0;
}

void dm_map_int_string_insert(dm_map_int_string* map, int64_t key, const dm_string* value) {
    if (map->capacity == 0 || (map->len + 1) * 4 > map->capacity * 3)
        dm_map_int_string_resize(map);
    size_t slot = dm_map_int_string_find_slot(map, key);
    if (map->entries[slot].state != 1) map->len++;
    map->entries[slot].key = key;
    map->entries[slot].value = *value;
    map->entries[slot].state = 1;
}

void dm_map_int_string_get(dm_string* out, dm_map_int_string* map, int64_t key) {
    if (map->capacity == 0) { fprintf(stderr, "map get: key not found\n"); exit(1); }
    size_t slot = dm_map_int_string_find_slot(map, key);
    if (map->entries[slot].state != 1) { fprintf(stderr, "map get: key not found\n"); exit(1); }
    *out = map->entries[slot].value;
}

int dm_map_int_string_contains(dm_map_int_string* map, int64_t key) {
    if (map->capacity == 0) return 0;
    size_t slot = dm_map_int_string_find_slot(map, key);
    return map->entries[slot].state == 1;
}

void dm_map_int_string_remove(dm_map_int_string* map, int64_t key) {
    if (map->capacity == 0) return;
    size_t slot = dm_map_int_string_find_slot(map, key);
    if (map->entries[slot].state == 1) { map->entries[slot].state = 2; map->len--; }
}

int64_t dm_map_int_string_len(dm_map_int_string* map) { return map->len; }

// =====================================================================
// LLVM ABI wrappers for map functions (llvm_ prefix for string params)
// =====================================================================

// Map[string, int] wrappers
void llvm_dm_map_string_int_insert(dm_map_string_int* map, const dm_string* key, int64_t value) {
    dm_map_string_int_insert(map, key, value);
}

int64_t llvm_dm_map_string_int_get(dm_map_string_int* map, const dm_string* key) {
    return dm_map_string_int_get(map, key);
}

int llvm_dm_map_string_int_contains(dm_map_string_int* map, const dm_string* key) {
    return dm_map_string_int_contains(map, key);
}

void llvm_dm_map_string_int_remove(dm_map_string_int* map, const dm_string* key) {
    dm_map_string_int_remove(map, key);
}

// Map[int, string] wrappers
void llvm_dm_map_int_string_insert(dm_map_int_string* map, int64_t key, const dm_string* value) {
    dm_map_int_string_insert(map, key, value);
}

void llvm_dm_map_int_string_get(dm_string* out, dm_map_int_string* map, int64_t key) {
    dm_map_int_string_get(out, map, key);
}

// string_split wrapper (has string params so needs llvm_ prefix)
void llvm_dm_string_split(dm_list_dm_string* out, const dm_string* s, const dm_string* delim) {
    dm_string_split(out, s, delim);
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
