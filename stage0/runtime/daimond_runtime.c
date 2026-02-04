/**
 * dAImond Runtime Library - Implementation
 *
 * This file provides the implementations for the dAImond runtime functions.
 */

#include "daimond_runtime.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>

/* ============================================================================
 * Internal Helpers
 * ============================================================================ */

/* Align a size up to the given alignment (must be power of 2) */
static inline size_t align_up(size_t size, size_t alignment) {
    return (size + alignment - 1) & ~(alignment - 1);
}

/* Default alignment for allocations */
#define DEFAULT_ALIGNMENT (sizeof(void*))

/* Initial buffer size for dynamic strings */
#define INITIAL_STRING_BUFFER_SIZE 64

/* Growth factor for dynamic arrays */
#define GROWTH_FACTOR 2

/* ============================================================================
 * Arena Allocator Implementation
 * ============================================================================ */

dm_arena* dm_arena_create(size_t initial_size) {
    dm_arena* arena = (dm_arena*)malloc(sizeof(dm_arena));
    if (!arena) {
        return NULL;
    }

    arena->data = (char*)malloc(initial_size);
    if (!arena->data) {
        free(arena);
        return NULL;
    }

    arena->size = initial_size;
    arena->used = 0;
    arena->next = NULL;

    return arena;
}

void* dm_arena_alloc(dm_arena* arena, size_t size) {
    return dm_arena_alloc_aligned(arena, size, DEFAULT_ALIGNMENT);
}

void* dm_arena_alloc_aligned(dm_arena* arena, size_t size, size_t alignment) {
    if (!arena || size == 0) {
        return NULL;
    }

    /* Find the last arena in the chain */
    dm_arena* current = arena;
    while (current->next) {
        current = current->next;
    }

    /* Align the current position */
    size_t aligned_used = align_up(current->used, alignment);

    /* Check if we have enough space */
    if (aligned_used + size <= current->size) {
        void* ptr = current->data + aligned_used;
        current->used = aligned_used + size;
        return ptr;
    }

    /* Need to allocate a new arena block */
    size_t new_size = current->size * GROWTH_FACTOR;
    if (new_size < size + alignment) {
        new_size = size + alignment;
    }

    dm_arena* new_arena = dm_arena_create(new_size);
    if (!new_arena) {
        return NULL;
    }

    current->next = new_arena;

    /* Allocate from the new arena */
    size_t new_aligned = align_up(new_arena->used, alignment);
    void* ptr = new_arena->data + new_aligned;
    new_arena->used = new_aligned + size;
    return ptr;
}

void dm_arena_reset(dm_arena* arena) {
    if (!arena) {
        return;
    }

    /* Reset all arenas in the chain */
    dm_arena* current = arena;
    while (current) {
        current->used = 0;
        current = current->next;
    }
}

void dm_arena_destroy(dm_arena* arena) {
    if (!arena) {
        return;
    }

    /* Free all arenas in the chain */
    dm_arena* current = arena;
    while (current) {
        dm_arena* next = current->next;
        free(current->data);
        free(current);
        current = next;
    }
}

/* ============================================================================
 * String Operations Implementation
 * ============================================================================ */

dm_string dm_string_new(const char* s) {
    if (!s) {
        return DM_STRING_EMPTY;
    }
    return (dm_string){
        .data = s,
        .len = strlen(s),
        .cap = 0
    };
}

dm_string dm_string_from_len(const char* s, size_t len) {
    if (!s) {
        return DM_STRING_EMPTY;
    }
    return (dm_string){
        .data = s,
        .len = len,
        .cap = 0
    };
}

dm_string dm_string_clone(dm_string s) {
    if (s.len == 0) {
        return DM_STRING_EMPTY;
    }

    char* new_data = (char*)malloc(s.len + 1);
    if (!new_data) {
        return DM_STRING_EMPTY;
    }

    memcpy(new_data, s.data, s.len);
    new_data[s.len] = '\0';

    return (dm_string){
        .data = new_data,
        .len = s.len,
        .cap = s.len + 1
    };
}

void dm_string_free(dm_string s) {
    if (s.cap > 0 && s.data) {
        free((void*)s.data);
    }
}

dm_string dm_string_concat(dm_string a, dm_string b) {
    size_t total_len = a.len + b.len;
    if (total_len == 0) {
        return DM_STRING_EMPTY;
    }

    char* new_data = (char*)malloc(total_len + 1);
    if (!new_data) {
        return DM_STRING_EMPTY;
    }

    if (a.len > 0) {
        memcpy(new_data, a.data, a.len);
    }
    if (b.len > 0) {
        memcpy(new_data + a.len, b.data, b.len);
    }
    new_data[total_len] = '\0';

    return (dm_string){
        .data = new_data,
        .len = total_len,
        .cap = total_len + 1
    };
}

bool dm_string_eq(dm_string a, dm_string b) {
    if (a.len != b.len) {
        return false;
    }
    if (a.len == 0) {
        return true;
    }
    return memcmp(a.data, b.data, a.len) == 0;
}

int dm_string_cmp(dm_string a, dm_string b) {
    size_t min_len = a.len < b.len ? a.len : b.len;
    int cmp = 0;

    if (min_len > 0) {
        cmp = memcmp(a.data, b.data, min_len);
    }

    if (cmp != 0) {
        return cmp;
    }

    /* Strings are equal up to min_len, compare by length */
    if (a.len < b.len) return -1;
    if (a.len > b.len) return 1;
    return 0;
}

dm_string dm_string_slice(dm_string s, size_t start, size_t end) {
    if (start >= s.len || start >= end) {
        return DM_STRING_EMPTY;
    }

    if (end > s.len) {
        end = s.len;
    }

    return (dm_string){
        .data = s.data + start,
        .len = end - start,
        .cap = 0  /* Slice does not own data */
    };
}

size_t dm_string_len(dm_string s) {
    return s.len;
}

bool dm_string_is_empty(dm_string s) {
    return s.len == 0;
}

bool dm_string_starts_with(dm_string s, dm_string prefix) {
    if (prefix.len > s.len) {
        return false;
    }
    if (prefix.len == 0) {
        return true;
    }
    return memcmp(s.data, prefix.data, prefix.len) == 0;
}

bool dm_string_ends_with(dm_string s, dm_string suffix) {
    if (suffix.len > s.len) {
        return false;
    }
    if (suffix.len == 0) {
        return true;
    }
    return memcmp(s.data + s.len - suffix.len, suffix.data, suffix.len) == 0;
}

size_t dm_string_find(dm_string haystack, dm_string needle) {
    if (needle.len == 0) {
        return 0;
    }
    if (needle.len > haystack.len) {
        return (size_t)-1;
    }

    for (size_t i = 0; i <= haystack.len - needle.len; i++) {
        if (memcmp(haystack.data + i, needle.data, needle.len) == 0) {
            return i;
        }
    }
    return (size_t)-1;
}

char dm_string_char_at(dm_string s, size_t index) {
    if (index >= s.len) {
        return '\0';
    }
    return s.data[index];
}

/* ============================================================================
 * List Operations Implementation
 * ============================================================================ */

dm_list dm_list_new(size_t elem_size) {
    return (dm_list){
        .data = NULL,
        .len = 0,
        .cap = 0,
        .elem_size = elem_size
    };
}

dm_list dm_list_with_capacity(size_t elem_size, size_t capacity) {
    dm_list list = dm_list_new(elem_size);

    if (capacity > 0) {
        list.data = malloc(elem_size * capacity);
        if (list.data) {
            list.cap = capacity;
        }
    }

    return list;
}

void dm_list_free(dm_list* list) {
    if (list && list->data) {
        free(list->data);
        list->data = NULL;
        list->len = 0;
        list->cap = 0;
    }
}

static bool dm_list_ensure_capacity(dm_list* list, size_t needed) {
    if (needed <= list->cap) {
        return true;
    }

    size_t new_cap = list->cap == 0 ? 4 : list->cap * GROWTH_FACTOR;
    while (new_cap < needed) {
        new_cap *= GROWTH_FACTOR;
    }

    void* new_data = realloc(list->data, new_cap * list->elem_size);
    if (!new_data) {
        return false;
    }

    list->data = new_data;
    list->cap = new_cap;
    return true;
}

void dm_list_push(dm_list* list, const void* elem) {
    if (!list || !elem) {
        return;
    }

    if (!dm_list_ensure_capacity(list, list->len + 1)) {
        return;
    }

    memcpy((char*)list->data + list->len * list->elem_size, elem, list->elem_size);
    list->len++;
}

bool dm_list_pop(dm_list* list, void* out) {
    if (!list || list->len == 0) {
        return false;
    }

    list->len--;

    if (out) {
        memcpy(out, (char*)list->data + list->len * list->elem_size, list->elem_size);
    }

    return true;
}

void* dm_list_get(dm_list* list, size_t index) {
    if (!list || index >= list->len) {
        return NULL;
    }
    return (char*)list->data + index * list->elem_size;
}

bool dm_list_set(dm_list* list, size_t index, const void* elem) {
    if (!list || !elem || index >= list->len) {
        return false;
    }
    memcpy((char*)list->data + index * list->elem_size, elem, list->elem_size);
    return true;
}

size_t dm_list_len(dm_list* list) {
    return list ? list->len : 0;
}

bool dm_list_is_empty(dm_list* list) {
    return !list || list->len == 0;
}

void dm_list_clear(dm_list* list) {
    if (list) {
        list->len = 0;
    }
}

/* ============================================================================
 * I/O Operations Implementation
 * ============================================================================ */

void dm_print(dm_string s) {
    if (s.len > 0 && s.data) {
        fwrite(s.data, 1, s.len, stdout);
    }
}

void dm_println(dm_string s) {
    dm_print(s);
    putchar('\n');
}

void dm_printf(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
}

dm_string dm_read_line(void) {
    char* buffer = NULL;
    size_t buffer_size = 0;
    size_t len = 0;

    int c;
    while ((c = getchar()) != EOF && c != '\n') {
        if (len + 1 >= buffer_size) {
            size_t new_size = buffer_size == 0 ? INITIAL_STRING_BUFFER_SIZE : buffer_size * GROWTH_FACTOR;
            char* new_buffer = (char*)realloc(buffer, new_size);
            if (!new_buffer) {
                free(buffer);
                return DM_STRING_EMPTY;
            }
            buffer = new_buffer;
            buffer_size = new_size;
        }
        buffer[len++] = (char)c;
    }

    if (len == 0 && c == EOF) {
        free(buffer);
        return DM_STRING_EMPTY;
    }

    if (buffer) {
        buffer[len] = '\0';
    }

    return (dm_string){
        .data = buffer ? buffer : "",
        .len = len,
        .cap = buffer_size
    };
}

dm_string dm_read_file(dm_string path) {
    /* Create null-terminated path */
    char* path_cstr = (char*)malloc(path.len + 1);
    if (!path_cstr) {
        return DM_STRING_EMPTY;
    }
    memcpy(path_cstr, path.data, path.len);
    path_cstr[path.len] = '\0';

    FILE* file = fopen(path_cstr, "rb");
    free(path_cstr);

    if (!file) {
        return DM_STRING_EMPTY;
    }

    /* Get file size */
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (file_size < 0) {
        fclose(file);
        return DM_STRING_EMPTY;
    }

    /* Allocate buffer */
    char* buffer = (char*)malloc((size_t)file_size + 1);
    if (!buffer) {
        fclose(file);
        return DM_STRING_EMPTY;
    }

    /* Read file */
    size_t bytes_read = fread(buffer, 1, (size_t)file_size, file);
    fclose(file);

    buffer[bytes_read] = '\0';

    return (dm_string){
        .data = buffer,
        .len = bytes_read,
        .cap = (size_t)file_size + 1
    };
}

bool dm_write_file(dm_string path, dm_string content) {
    /* Create null-terminated path */
    char* path_cstr = (char*)malloc(path.len + 1);
    if (!path_cstr) {
        return false;
    }
    memcpy(path_cstr, path.data, path.len);
    path_cstr[path.len] = '\0';

    FILE* file = fopen(path_cstr, "wb");
    free(path_cstr);

    if (!file) {
        return false;
    }

    size_t written = 0;
    if (content.len > 0) {
        written = fwrite(content.data, 1, content.len, file);
    }

    fclose(file);
    return written == content.len;
}

bool dm_append_file(dm_string path, dm_string content) {
    /* Create null-terminated path */
    char* path_cstr = (char*)malloc(path.len + 1);
    if (!path_cstr) {
        return false;
    }
    memcpy(path_cstr, path.data, path.len);
    path_cstr[path.len] = '\0';

    FILE* file = fopen(path_cstr, "ab");
    free(path_cstr);

    if (!file) {
        return false;
    }

    size_t written = 0;
    if (content.len > 0) {
        written = fwrite(content.data, 1, content.len, file);
    }

    fclose(file);
    return written == content.len;
}

bool dm_file_exists(dm_string path) {
    /* Create null-terminated path */
    char* path_cstr = (char*)malloc(path.len + 1);
    if (!path_cstr) {
        return false;
    }
    memcpy(path_cstr, path.data, path.len);
    path_cstr[path.len] = '\0';

    FILE* file = fopen(path_cstr, "r");
    free(path_cstr);

    if (file) {
        fclose(file);
        return true;
    }
    return false;
}

/* ============================================================================
 * Panic and Assertions Implementation
 * ============================================================================ */

_Noreturn void dm_panic(const char* msg, const char* file, int line) {
    fprintf(stderr, "PANIC at %s:%d: %s\n", file, line, msg);
    fflush(stderr);
    abort();
}

/* ============================================================================
 * Numeric Conversions Implementation
 * ============================================================================ */

dm_string dm_int_to_string(int64_t n) {
    char buffer[32];
    int len = snprintf(buffer, sizeof(buffer), "%lld", (long long)n);

    if (len < 0 || len >= (int)sizeof(buffer)) {
        return DM_STRING_EMPTY;
    }

    char* result = (char*)malloc((size_t)len + 1);
    if (!result) {
        return DM_STRING_EMPTY;
    }

    memcpy(result, buffer, (size_t)len + 1);

    return (dm_string){
        .data = result,
        .len = (size_t)len,
        .cap = (size_t)len + 1
    };
}

dm_string dm_float_to_string(double f) {
    char buffer[64];
    int len = snprintf(buffer, sizeof(buffer), "%g", f);

    if (len < 0 || len >= (int)sizeof(buffer)) {
        return DM_STRING_EMPTY;
    }

    char* result = (char*)malloc((size_t)len + 1);
    if (!result) {
        return DM_STRING_EMPTY;
    }

    memcpy(result, buffer, (size_t)len + 1);

    return (dm_string){
        .data = result,
        .len = (size_t)len,
        .cap = (size_t)len + 1
    };
}

bool dm_string_to_int(dm_string s, int64_t* out) {
    if (!out || s.len == 0) {
        return false;
    }

    /* Create null-terminated copy */
    char* buffer = (char*)malloc(s.len + 1);
    if (!buffer) {
        return false;
    }
    memcpy(buffer, s.data, s.len);
    buffer[s.len] = '\0';

    char* endptr;
    errno = 0;
    long long value = strtoll(buffer, &endptr, 10);

    bool success = (errno == 0 && endptr == buffer + s.len);
    free(buffer);

    if (success) {
        *out = (int64_t)value;
    }
    return success;
}

bool dm_string_to_float(dm_string s, double* out) {
    if (!out || s.len == 0) {
        return false;
    }

    /* Create null-terminated copy */
    char* buffer = (char*)malloc(s.len + 1);
    if (!buffer) {
        return false;
    }
    memcpy(buffer, s.data, s.len);
    buffer[s.len] = '\0';

    char* endptr;
    errno = 0;
    double value = strtod(buffer, &endptr);

    bool success = (errno == 0 && endptr == buffer + s.len);
    free(buffer);

    if (success) {
        *out = value;
    }
    return success;
}

/* ============================================================================
 * Runtime Initialization Implementation
 * ============================================================================ */

void dm_runtime_init(void) {
    /* Currently a no-op, but can be extended for:
     * - Global allocator initialization
     * - Signal handlers
     * - Thread-local storage
     * - Garbage collector initialization
     */
}

void dm_runtime_cleanup(void) {
    /* Currently a no-op, but can be extended for:
     * - Final cleanup of global resources
     * - Memory leak detection in debug mode
     */
}

/* ============================================================================
 * Threading Primitives
 * ============================================================================ */

#ifndef _WIN32

/* Trampoline for thread spawn: pthreads expects void* (*)(void*) but
 * dAImond spawns void (*)(void) functions. */
typedef struct {
    void (*func)(void);
} dm_thread_trampoline_arg;

static void* dm_thread_trampoline(void* arg) {
    dm_thread_trampoline_arg* ta = (dm_thread_trampoline_arg*)arg;
    void (*func)(void) = ta->func;
    free(ta);
    func();
    return NULL;
}

dm_thread dm_thread_spawn(void (*func)(void)) {
    dm_thread t;
    dm_thread_trampoline_arg* arg = (dm_thread_trampoline_arg*)malloc(sizeof(dm_thread_trampoline_arg));
    arg->func = func;
    pthread_create(&t.handle, NULL, dm_thread_trampoline, arg);
    return t;
}

void dm_thread_join(dm_thread thread) {
    pthread_join(thread.handle, NULL);
}

dm_mutex dm_mutex_new(void) {
    dm_mutex m;
    pthread_mutex_init(&m.handle, NULL);
    return m;
}

void dm_mutex_lock(dm_mutex* mutex) {
    pthread_mutex_lock(&mutex->handle);
}

void dm_mutex_unlock(dm_mutex* mutex) {
    pthread_mutex_unlock(&mutex->handle);
}

void dm_mutex_destroy(dm_mutex* mutex) {
    pthread_mutex_destroy(&mutex->handle);
}

/* ============================================================================
 * Networking Primitives (BSD Sockets)
 * ============================================================================ */

/**
 * Parse "host:port" from a dm_string into host/port components.
 * Returns 0 on success, -1 on failure.
 */
static int dm_parse_addr(dm_string addr, char* host_buf, size_t host_size, int* port) {
    /* Find the last ':' separator */
    size_t colon_pos = addr.len;
    for (size_t i = addr.len; i > 0; i--) {
        if (addr.data[i - 1] == ':') {
            colon_pos = i - 1;
            break;
        }
    }
    if (colon_pos >= addr.len) return -1;

    size_t host_len = colon_pos < host_size - 1 ? colon_pos : host_size - 1;
    memcpy(host_buf, addr.data, host_len);
    host_buf[host_len] = '\0';

    char port_buf[16];
    size_t port_len = addr.len - colon_pos - 1;
    if (port_len >= sizeof(port_buf)) port_len = sizeof(port_buf) - 1;
    memcpy(port_buf, addr.data + colon_pos + 1, port_len);
    port_buf[port_len] = '\0';
    *port = atoi(port_buf);
    return 0;
}

dm_tcp_listener dm_tcp_listen(dm_string addr) {
    dm_tcp_listener listener = { .fd = -1 };
    char host[256];
    int port = 0;

    if (dm_parse_addr(addr, host, sizeof(host), &port) < 0) {
        fprintf(stderr, "dm_tcp_listen: invalid address format\n");
        return listener;
    }

    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        perror("dm_tcp_listen: socket");
        return listener;
    }

    int opt = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in sa;
    memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_port = htons((uint16_t)port);
    if (strlen(host) == 0 || strcmp(host, "0.0.0.0") == 0) {
        sa.sin_addr.s_addr = INADDR_ANY;
    } else {
        inet_pton(AF_INET, host, &sa.sin_addr);
    }

    if (bind(fd, (struct sockaddr*)&sa, sizeof(sa)) < 0) {
        perror("dm_tcp_listen: bind");
        close(fd);
        return listener;
    }

    if (listen(fd, 128) < 0) {
        perror("dm_tcp_listen: listen");
        close(fd);
        return listener;
    }

    listener.fd = fd;
    return listener;
}

dm_tcp_stream dm_tcp_accept(dm_tcp_listener* listener) {
    dm_tcp_stream stream = { .fd = -1 };
    struct sockaddr_in client_addr;
    socklen_t addr_len = sizeof(client_addr);

    int client_fd = accept(listener->fd, (struct sockaddr*)&client_addr, &addr_len);
    if (client_fd < 0) {
        perror("dm_tcp_accept: accept");
        return stream;
    }

    stream.fd = client_fd;
    return stream;
}

dm_tcp_stream dm_tcp_connect(dm_string addr) {
    dm_tcp_stream stream = { .fd = -1 };
    char host[256];
    int port = 0;

    if (dm_parse_addr(addr, host, sizeof(host), &port) < 0) {
        fprintf(stderr, "dm_tcp_connect: invalid address format\n");
        return stream;
    }

    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        perror("dm_tcp_connect: socket");
        return stream;
    }

    struct sockaddr_in sa;
    memset(&sa, 0, sizeof(sa));
    sa.sin_family = AF_INET;
    sa.sin_port = htons((uint16_t)port);
    inet_pton(AF_INET, host, &sa.sin_addr);

    if (connect(fd, (struct sockaddr*)&sa, sizeof(sa)) < 0) {
        perror("dm_tcp_connect: connect");
        close(fd);
        return stream;
    }

    stream.fd = fd;
    return stream;
}

dm_string dm_tcp_read(dm_tcp_stream* stream, int64_t max_bytes) {
    if (max_bytes <= 0) max_bytes = 4096;
    char* buf = (char*)malloc((size_t)max_bytes + 1);
    ssize_t n = read(stream->fd, buf, (size_t)max_bytes);
    if (n <= 0) {
        free(buf);
        return (dm_string){ .data = "", .len = 0, .cap = 0 };
    }
    buf[n] = '\0';
    return (dm_string){ .data = buf, .len = (size_t)n, .cap = (size_t)max_bytes };
}

int64_t dm_tcp_write(dm_tcp_stream* stream, dm_string data) {
    ssize_t n = write(stream->fd, data.data, data.len);
    return (int64_t)(n < 0 ? 0 : n);
}

void dm_tcp_close(dm_tcp_stream* stream) {
    if (stream->fd >= 0) {
        close(stream->fd);
        stream->fd = -1;
    }
}

void dm_tcp_listener_close(dm_tcp_listener* listener) {
    if (listener->fd >= 0) {
        close(listener->fd);
        listener->fd = -1;
    }
}

#endif /* _WIN32 */
