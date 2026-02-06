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

int64_t llvm_dm_args_len(void) {
    return (int64_t)dm_argc;
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
