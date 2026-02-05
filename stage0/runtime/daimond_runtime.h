/**
 * dAImond Runtime Library - C Runtime Support for Compiled dAImond Programs
 *
 * This header provides the core types and functions needed by dAImond programs
 * when compiled to C. It includes string handling, memory management via arenas,
 * option/result types, and basic I/O operations.
 */

#ifndef DAIMOND_RUNTIME_H
#define DAIMOND_RUNTIME_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Math Constants
 * ============================================================================ */

#define DM_PI 3.14159265358979323846
#define DM_E 2.71828182845904523536

/* ============================================================================
 * Core Types
 * ============================================================================ */

/**
 * String type - immutable string with optional capacity for dynamic strings.
 * Static strings have cap = 0.
 */
typedef struct {
    const char* data;
    size_t len;
    size_t cap;  /* 0 for static/borrowed strings */
} dm_string;

/**
 * Option type macro - declares an optional value container type.
 * Usage: DM_DEFINE_OPTION(int, OptInt);
 *        OptInt maybe_int = DM_SOME_VAL(OptInt, 42);
 */
#define DM_DEFINE_OPTION(T, NAME) \
    typedef struct { bool has_value; T value; } NAME

/**
 * Generic option for inline use (C11 anonymous struct)
 */
#define DM_OPTION(T) struct dm_option_##T { bool has_value; T value; }

/**
 * Create a Some value (has value) - use with defined option types
 * Usage: DM_SOME_VAL(OptInt, 42)
 */
#define DM_SOME_VAL(TYPE, v) ((TYPE){.has_value = true, .value = (v)})

/**
 * Create a None value (no value) - use with defined option types
 * Usage: DM_NONE_VAL(OptInt)
 */
#define DM_NONE_VAL(TYPE) ((TYPE){.has_value = false})

/**
 * Initialize an option variable as Some
 */
#define DM_OPTION_SOME(var, val) do { (var).has_value = true; (var).value = (val); } while(0)

/**
 * Initialize an option variable as None
 */
#define DM_OPTION_NONE(var) do { (var).has_value = false; } while(0)

/**
 * Check if option has a value
 */
#define DM_IS_SOME(opt) ((opt).has_value)
#define DM_IS_NONE(opt) (!(opt).has_value)

/**
 * Unwrap option value (undefined behavior if None)
 */
#define DM_UNWRAP(opt) ((opt).value)

/**
 * Unwrap option value with default
 */
#define DM_UNWRAP_OR(opt, def) ((opt).has_value ? (opt).value : (def))

/* Pre-defined common option types */
DM_DEFINE_OPTION(int, dm_option_int);
DM_DEFINE_OPTION(int64_t, dm_option_i64);
DM_DEFINE_OPTION(double, dm_option_f64);
DM_DEFINE_OPTION(bool, dm_option_bool);
DM_DEFINE_OPTION(size_t, dm_option_size);

/**
 * Result type macro - declares a result container type for success or error.
 * Usage: DM_DEFINE_RESULT(int, dm_string, IntResult);
 *        IntResult res = DM_OK_VAL(IntResult, 42);
 */
#define DM_DEFINE_RESULT(T, E, NAME) \
    typedef struct { bool is_ok; union { T ok; E err; }; } NAME

/**
 * Create an Ok result (success) - use with defined result types
 * Usage: DM_OK_VAL(IntResult, 42)
 */
#define DM_OK_VAL(TYPE, v) ((TYPE){.is_ok = true, .ok = (v)})

/**
 * Create an Err result (failure) - use with defined result types
 * Usage: DM_ERR_VAL(IntResult, error_string)
 */
#define DM_ERR_VAL(TYPE, e) ((TYPE){.is_ok = false, .err = (e)})

/**
 * Initialize a result variable as Ok
 */
#define DM_RESULT_OK(var, val) do { (var).is_ok = true; (var).ok = (val); } while(0)

/**
 * Initialize a result variable as Err
 */
#define DM_RESULT_ERR(var, e) do { (var).is_ok = false; (var).err = (e); } while(0)

/**
 * Check if result is Ok or Err
 */
#define DM_IS_OK(res) ((res).is_ok)
#define DM_IS_ERR(res) (!(res).is_ok)

/**
 * Unwrap result values
 */
#define DM_UNWRAP_OK(res) ((res).ok)
#define DM_UNWRAP_ERR(res) ((res).err)

/**
 * List type - dynamic array with type-erased storage.
 */
typedef struct {
    void* data;
    size_t len;
    size_t cap;
    size_t elem_size;
} dm_list;

/* ============================================================================
 * Memory Management / Arena Allocator
 * ============================================================================ */

/**
 * Arena allocator for region-based memory management.
 * Arenas allow bulk allocation and deallocation of memory.
 */
typedef struct dm_arena {
    char* data;
    size_t size;
    size_t used;
    struct dm_arena* next;  /* Linked list for overflow arenas */
} dm_arena;

/**
 * Create a new arena with the specified initial size.
 * @param initial_size Initial allocation size in bytes
 * @return Pointer to the new arena, or NULL on failure
 */
dm_arena* dm_arena_create(size_t initial_size);

/**
 * Allocate memory from an arena.
 * @param arena The arena to allocate from
 * @param size Number of bytes to allocate
 * @return Pointer to allocated memory, or NULL on failure
 */
void* dm_arena_alloc(dm_arena* arena, size_t size);

/**
 * Allocate aligned memory from an arena.
 * @param arena The arena to allocate from
 * @param size Number of bytes to allocate
 * @param alignment Required alignment (must be power of 2)
 * @return Pointer to allocated memory, or NULL on failure
 */
void* dm_arena_alloc_aligned(dm_arena* arena, size_t size, size_t alignment);

/**
 * Reset an arena for reuse (keeps allocated memory).
 * @param arena The arena to reset
 */
void dm_arena_reset(dm_arena* arena);

/**
 * Destroy an arena and free all associated memory.
 * @param arena The arena to destroy
 */
void dm_arena_destroy(dm_arena* arena);

/**
 * Region macros for scoped memory management.
 * Usage:
 *   DM_REGION_BEGIN(my_region);
 *   // allocate from my_region
 *   DM_REGION_END(my_region);
 */
#define DM_REGION_BEGIN(name) dm_arena* name = dm_arena_create(4096)
#define DM_REGION_END(name) dm_arena_destroy(name)

/* ============================================================================
 * String Operations
 * ============================================================================ */

/**
 * Create a string from a null-terminated C string.
 * The string data is NOT copied - the original must remain valid.
 * @param s Null-terminated C string
 * @return dm_string wrapping the input
 */
dm_string dm_string_new(const char* s);

/**
 * Create a string from a pointer and length.
 * The string data is NOT copied - the original must remain valid.
 * @param s Pointer to string data
 * @param len Length of string
 * @return dm_string wrapping the input
 */
dm_string dm_string_from_len(const char* s, size_t len);

/**
 * Create an owned copy of a string.
 * @param s String to copy
 * @return New string with owned data (must be freed with dm_string_free)
 */
dm_string dm_string_clone(dm_string s);

/**
 * Free a dynamically allocated string.
 * Only call this on strings with cap > 0.
 * @param s String to free
 */
void dm_string_free(dm_string s);

/**
 * Concatenate two strings.
 * @param a First string
 * @param b Second string
 * @return New string containing a followed by b (must be freed)
 */
dm_string dm_string_concat(dm_string a, dm_string b);

/**
 * Compare two strings for equality.
 * @param a First string
 * @param b Second string
 * @return true if strings are equal, false otherwise
 */
bool dm_string_eq(dm_string a, dm_string b);

/**
 * Compare two strings lexicographically.
 * @param a First string
 * @param b Second string
 * @return < 0 if a < b, 0 if equal, > 0 if a > b
 */
int dm_string_cmp(dm_string a, dm_string b);

/**
 * Get a substring (slice).
 * @param s Source string
 * @param start Start index (inclusive)
 * @param end End index (exclusive)
 * @return Slice of the original string (not a copy)
 */
dm_string dm_string_slice(dm_string s, size_t start, size_t end);

/**
 * Get the length of a string.
 * @param s String
 * @return Length in bytes
 */
size_t dm_string_len(dm_string s);

/**
 * Check if a string is empty.
 * @param s String
 * @return true if length is 0
 */
bool dm_string_is_empty(dm_string s);

/**
 * Check if a string starts with a prefix.
 * @param s String to check
 * @param prefix Prefix to look for
 * @return true if s starts with prefix
 */
bool dm_string_starts_with(dm_string s, dm_string prefix);

/**
 * Check if a string ends with a suffix.
 * @param s String to check
 * @param suffix Suffix to look for
 * @return true if s ends with suffix
 */
bool dm_string_ends_with(dm_string s, dm_string suffix);

/**
 * Find the first occurrence of a substring.
 * @param haystack String to search in
 * @param needle Substring to find
 * @return Index of first occurrence, or (size_t)-1 if not found
 */
size_t dm_string_find(dm_string haystack, dm_string needle);

/**
 * Get a character at an index.
 * @param s String
 * @param index Character index
 * @return Character at index, or '\0' if out of bounds
 */
char dm_string_char_at(dm_string s, size_t index);

/**
 * Create a string literal (compile-time).
 */
#define DM_STRING_LIT(s) ((dm_string){.data = (s), .len = sizeof(s) - 1, .cap = 0})

/**
 * Empty string constant.
 */
#define DM_STRING_EMPTY ((dm_string){.data = "", .len = 0, .cap = 0})

/* ============================================================================
 * List Operations
 * ============================================================================ */

/**
 * Create a new empty list.
 * @param elem_size Size of each element in bytes
 * @return New empty list
 */
dm_list dm_list_new(size_t elem_size);

/**
 * Create a list with initial capacity.
 * @param elem_size Size of each element in bytes
 * @param capacity Initial capacity
 * @return New list with reserved capacity
 */
dm_list dm_list_with_capacity(size_t elem_size, size_t capacity);

/**
 * Free a list and its data.
 * @param list List to free
 */
void dm_list_free(dm_list* list);

/**
 * Push an element to the end of a list.
 * @param list List to modify
 * @param elem Pointer to element to copy
 */
void dm_list_push(dm_list* list, const void* elem);

/**
 * Pop an element from the end of a list.
 * @param list List to modify
 * @param out Pointer to store popped element (can be NULL)
 * @return true if an element was popped, false if list was empty
 */
bool dm_list_pop(dm_list* list, void* out);

/**
 * Get a pointer to an element by index.
 * @param list List to access
 * @param index Element index
 * @return Pointer to element, or NULL if out of bounds
 */
void* dm_list_get(dm_list* list, size_t index);

/**
 * Set an element by index.
 * @param list List to modify
 * @param index Element index
 * @param elem Pointer to element to copy
 * @return true if successful, false if out of bounds
 */
bool dm_list_set(dm_list* list, size_t index, const void* elem);

/**
 * Get the length of a list.
 * @param list List
 * @return Number of elements
 */
size_t dm_list_len(dm_list* list);

/**
 * Check if a list is empty.
 * @param list List
 * @return true if length is 0
 */
bool dm_list_is_empty(dm_list* list);

/**
 * Clear a list (remove all elements, keep capacity).
 * @param list List to clear
 */
void dm_list_clear(dm_list* list);

/* ============================================================================
 * I/O Operations
 * ============================================================================ */

/**
 * Print a string to stdout (no newline).
 * @param s String to print
 */
void dm_print(dm_string s);

/**
 * Print a string to stdout followed by a newline.
 * @param s String to print
 */
void dm_println(dm_string s);

/**
 * Print a formatted string to stdout.
 * @param fmt Format string (printf-style)
 * @param ... Format arguments
 */
void dm_printf(const char* fmt, ...);

/**
 * Read a line from stdin.
 * @return Line read (must be freed with dm_string_free)
 */
dm_string dm_read_line(void);

/**
 * Read entire contents of a file.
 * @param path File path
 * @return File contents (must be freed), or empty string on error
 */
dm_string dm_read_file(dm_string path);

/**
 * Write a string to a file (overwrites existing content).
 * @param path File path
 * @param content Content to write
 * @return true on success, false on error
 */
bool dm_write_file(dm_string path, dm_string content);

/**
 * Append a string to a file.
 * @param path File path
 * @param content Content to append
 * @return true on success, false on error
 */
bool dm_append_file(dm_string path, dm_string content);

/**
 * Check if a file exists.
 * @param path File path
 * @return true if file exists
 */
bool dm_file_exists(dm_string path);

/* ============================================================================
 * Panic and Assertions
 * ============================================================================ */

/**
 * Panic with a message and location info.
 * This function never returns.
 * @param msg Error message
 * @param file Source file name
 * @param line Source line number
 */
_Noreturn void dm_panic(const char* msg, const char* file, int line);

/**
 * Panic macro with automatic file/line info.
 */
#define DM_PANIC(msg) dm_panic(msg, __FILE__, __LINE__)

/**
 * Assert macro - panics if condition is false.
 */
#define DM_ASSERT(cond, msg) do { if (!(cond)) DM_PANIC(msg); } while(0)

/**
 * Debug assert - only enabled in debug builds.
 */
#ifdef NDEBUG
#define DM_DEBUG_ASSERT(cond, msg) ((void)0)
#else
#define DM_DEBUG_ASSERT(cond, msg) DM_ASSERT(cond, msg)
#endif

/**
 * Unreachable code marker.
 */
#define DM_UNREACHABLE() DM_PANIC("unreachable code reached")

/* ============================================================================
 * Numeric Conversions
 * ============================================================================ */

/**
 * Convert integer to string.
 * @param n Integer value
 * @return String representation (must be freed)
 */
dm_string dm_int_to_string(int64_t n);

/**
 * Convert float to string.
 * @param f Float value
 * @return String representation (must be freed)
 */
dm_string dm_float_to_string(double f);

/**
 * Parse integer from string.
 * @param s String to parse
 * @param out Output value
 * @return true on success, false on parse error
 */
bool dm_string_to_int(dm_string s, int64_t* out);

/**
 * Parse float from string.
 * @param s String to parse
 * @param out Output value
 * @return true on success, false on parse error
 */
bool dm_string_to_float(dm_string s, double* out);

/* ============================================================================
 * Math Utilities
 * ============================================================================ */

/**
 * Minimum of two values.
 */
#define DM_MIN(a, b) ((a) < (b) ? (a) : (b))

/**
 * Maximum of two values.
 */
#define DM_MAX(a, b) ((a) > (b) ? (a) : (b))

/**
 * Clamp a value between min and max.
 */
#define DM_CLAMP(x, lo, hi) DM_MIN(DM_MAX(x, lo), hi)

/**
 * Absolute value.
 */
#define DM_ABS(x) ((x) < 0 ? -(x) : (x))

/* ============================================================================
 * Runtime Initialization
 * ============================================================================ */

/**
 * Initialize the dAImond runtime.
 * Call this at program start.
 */
void dm_runtime_init(void);

/**
 * Cleanup the dAImond runtime.
 * Call this at program end.
 */
void dm_runtime_cleanup(void);

/* ============================================================================
 * Threading Primitives
 * ============================================================================ */

#ifdef _WIN32
#include <windows.h>

/**
 * Thread handle type (Windows).
 */
typedef struct dm_thread {
    HANDLE handle;
} dm_thread;

/**
 * Mutex type (Windows).
 */
typedef struct dm_mutex {
    HANDLE handle;
} dm_mutex;

#else /* POSIX */
#include <pthread.h>

/**
 * Thread handle type (POSIX).
 */
typedef struct dm_thread {
    pthread_t handle;
} dm_thread;

/**
 * Mutex type (POSIX).
 */
typedef struct dm_mutex {
    pthread_mutex_t handle;
} dm_mutex;

#endif /* _WIN32 */

/**
 * Spawn a new thread running a function.
 * The function takes no arguments and returns void.
 */
dm_thread dm_thread_spawn(void (*func)(void));

/**
 * Wait for a thread to finish.
 */
void dm_thread_join(dm_thread thread);

/**
 * Create a new mutex.
 */
dm_mutex dm_mutex_new(void);

/**
 * Lock a mutex.
 */
void dm_mutex_lock(dm_mutex* mutex);

/**
 * Unlock a mutex.
 */
void dm_mutex_unlock(dm_mutex* mutex);

/**
 * Destroy a mutex.
 */
void dm_mutex_destroy(dm_mutex* mutex);

/* ============================================================================
 * Networking Primitives
 * ============================================================================ */

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>

/**
 * Initialize Winsock (call once at startup).
 */
void dm_winsock_init(void);

/**
 * Cleanup Winsock (call once at shutdown).
 */
void dm_winsock_cleanup(void);

/**
 * TCP listener - listens for incoming connections (Windows: SOCKET).
 */
typedef struct dm_tcp_listener {
    SOCKET fd;
} dm_tcp_listener;

/**
 * TCP stream - a connected socket (Windows: SOCKET).
 */
typedef struct dm_tcp_stream {
    SOCKET fd;
} dm_tcp_stream;

#else /* POSIX */
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>

/**
 * TCP listener - listens for incoming connections (POSIX: int fd).
 */
typedef struct dm_tcp_listener {
    int fd;
} dm_tcp_listener;

/**
 * TCP stream - a connected socket (POSIX: int fd).
 */
typedef struct dm_tcp_stream {
    int fd;
} dm_tcp_stream;

#endif /* _WIN32 */

/**
 * Bind a TCP listener to an address (e.g., "0.0.0.0:8080").
 */
dm_tcp_listener dm_tcp_listen(dm_string addr);

/**
 * Accept a connection on a listener.
 * Blocks until a client connects.
 */
dm_tcp_stream dm_tcp_accept(dm_tcp_listener* listener);

/**
 * Connect to a remote TCP address (e.g., "127.0.0.1:8080").
 */
dm_tcp_stream dm_tcp_connect(dm_string addr);

/**
 * Read up to max_bytes from a TCP stream.
 * Returns the data read as a string.
 */
dm_string dm_tcp_read(dm_tcp_stream* stream, int64_t max_bytes);

/**
 * Write data to a TCP stream.
 * Returns the number of bytes written.
 */
int64_t dm_tcp_write(dm_tcp_stream* stream, dm_string data);

/**
 * Close a TCP stream.
 */
void dm_tcp_close(dm_tcp_stream* stream);

/**
 * Close a TCP listener.
 */
void dm_tcp_listener_close(dm_tcp_listener* listener);

/* ============================================================================
 * Filesystem Operations
 * ============================================================================ */

#ifdef _WIN32
#include <direct.h>   /* _mkdir, _getcwd, _rmdir */
#include <io.h>       /* _access */
#else
#include <sys/stat.h>
#include <dirent.h>
#endif

/**
 * Create a directory (and parents if needed).
 * @param path Directory path
 * @return 0 on success, -1 on error
 */
int dm_mkdir(dm_string path);

/**
 * Read directory entries.
 * Returns a newline-separated string of entry names.
 * @param path Directory path
 * @return Entry names separated by newlines, or empty string on error
 */
dm_string dm_readdir(dm_string path);

/**
 * Remove a file or empty directory.
 * @param path Path to remove
 * @return 0 on success, -1 on error
 */
int dm_remove(dm_string path);

/**
 * Rename/move a file or directory.
 * @param old_path Current path
 * @param new_path New path
 * @return 0 on success, -1 on error
 */
int dm_rename(dm_string old_path, dm_string new_path);

/**
 * Get the current working directory.
 * @return Current directory path (must be freed)
 */
dm_string dm_getcwd(void);

/* ============================================================================
 * OS Functions
 * ============================================================================ */

/**
 * Get an environment variable.
 * @param name Variable name
 * @return Variable value, or empty string if not set
 */
dm_string dm_getenv(dm_string name);

/* ============================================================================
 * SIMD Vector Types
 * ============================================================================ */

/* GCC/Clang vector extensions for SIMD types */
#if defined(__GNUC__) || defined(__clang__)

typedef float    dm_f32x4 __attribute__((vector_size(16)));
typedef float    dm_f32x8 __attribute__((vector_size(32)));
typedef double   dm_f64x2 __attribute__((vector_size(16)));
typedef double   dm_f64x4 __attribute__((vector_size(32)));
typedef int32_t  dm_i32x4 __attribute__((vector_size(16)));
typedef int32_t  dm_i32x8 __attribute__((vector_size(32)));
typedef int64_t  dm_i64x2 __attribute__((vector_size(16)));
typedef int64_t  dm_i64x4 __attribute__((vector_size(32)));

#else
/* Scalar fallback for compilers without vector extensions */

typedef struct { float    v[4]; } dm_f32x4;
typedef struct { float    v[8]; } dm_f32x8;
typedef struct { double   v[2]; } dm_f64x2;
typedef struct { double   v[4]; } dm_f64x4;
typedef struct { int32_t  v[4]; } dm_i32x4;
typedef struct { int32_t  v[8]; } dm_i32x8;
typedef struct { int64_t  v[2]; } dm_i64x2;
typedef struct { int64_t  v[4]; } dm_i64x4;

#endif /* __GNUC__ || __clang__ */

/* SIMD constructors - splat a scalar to all lanes */
static inline dm_f32x4 dm_simd_splat_f32x4(float v) {
    return (dm_f32x4){v, v, v, v};
}
static inline dm_f32x8 dm_simd_splat_f32x8(float v) {
    return (dm_f32x8){v, v, v, v, v, v, v, v};
}
static inline dm_f64x2 dm_simd_splat_f64x2(double v) {
    return (dm_f64x2){v, v};
}
static inline dm_f64x4 dm_simd_splat_f64x4(double v) {
    return (dm_f64x4){v, v, v, v};
}
static inline dm_i32x4 dm_simd_splat_i32x4(int32_t v) {
    return (dm_i32x4){v, v, v, v};
}
static inline dm_i32x8 dm_simd_splat_i32x8(int32_t v) {
    return (dm_i32x8){v, v, v, v, v, v, v, v};
}
static inline dm_i64x2 dm_simd_splat_i64x2(int64_t v) {
    return (dm_i64x2){v, v};
}
static inline dm_i64x4 dm_simd_splat_i64x4(int64_t v) {
    return (dm_i64x4){v, v, v, v};
}

/* SIMD set - construct from individual elements */
static inline dm_f32x4 dm_simd_set_f32x4(float a, float b, float c, float d) {
    return (dm_f32x4){a, b, c, d};
}
static inline dm_f64x2 dm_simd_set_f64x2(double a, double b) {
    return (dm_f64x2){a, b};
}
static inline dm_i32x4 dm_simd_set_i32x4(int32_t a, int32_t b, int32_t c, int32_t d) {
    return (dm_i32x4){a, b, c, d};
}
static inline dm_i64x2 dm_simd_set_i64x2(int64_t a, int64_t b) {
    return (dm_i64x2){a, b};
}

/* SIMD extract - get element at index (compile-time constant index) */
#if defined(__GNUC__) || defined(__clang__)
#define dm_simd_extract(vec, idx) ((vec)[(idx)])
#else
#define dm_simd_extract(vec, idx) ((vec).v[(idx)])
#endif

/* SIMD arithmetic - with vector extensions, +, -, *, / work directly */
#if defined(__GNUC__) || defined(__clang__)

#define dm_simd_add(a, b) ((a) + (b))
#define dm_simd_sub(a, b) ((a) - (b))
#define dm_simd_mul(a, b) ((a) * (b))
#define dm_simd_div(a, b) ((a) / (b))

#else
/* Scalar fallback - only f32x4 shown for brevity, others follow same pattern */

static inline dm_f32x4 dm_simd_add_f32x4(dm_f32x4 a, dm_f32x4 b) {
    dm_f32x4 r; for (int i = 0; i < 4; i++) r.v[i] = a.v[i] + b.v[i]; return r;
}
static inline dm_f32x4 dm_simd_sub_f32x4(dm_f32x4 a, dm_f32x4 b) {
    dm_f32x4 r; for (int i = 0; i < 4; i++) r.v[i] = a.v[i] - b.v[i]; return r;
}
static inline dm_f32x4 dm_simd_mul_f32x4(dm_f32x4 a, dm_f32x4 b) {
    dm_f32x4 r; for (int i = 0; i < 4; i++) r.v[i] = a.v[i] * b.v[i]; return r;
}
static inline dm_f32x4 dm_simd_div_f32x4(dm_f32x4 a, dm_f32x4 b) {
    dm_f32x4 r; for (int i = 0; i < 4; i++) r.v[i] = a.v[i] / b.v[i]; return r;
}
static inline dm_f64x2 dm_simd_add_f64x2(dm_f64x2 a, dm_f64x2 b) {
    dm_f64x2 r; for (int i = 0; i < 2; i++) r.v[i] = a.v[i] + b.v[i]; return r;
}
static inline dm_f64x2 dm_simd_sub_f64x2(dm_f64x2 a, dm_f64x2 b) {
    dm_f64x2 r; for (int i = 0; i < 2; i++) r.v[i] = a.v[i] - b.v[i]; return r;
}
static inline dm_f64x2 dm_simd_mul_f64x2(dm_f64x2 a, dm_f64x2 b) {
    dm_f64x2 r; for (int i = 0; i < 2; i++) r.v[i] = a.v[i] * b.v[i]; return r;
}
static inline dm_f64x2 dm_simd_div_f64x2(dm_f64x2 a, dm_f64x2 b) {
    dm_f64x2 r; for (int i = 0; i < 2; i++) r.v[i] = a.v[i] / b.v[i]; return r;
}
static inline dm_i32x4 dm_simd_add_i32x4(dm_i32x4 a, dm_i32x4 b) {
    dm_i32x4 r; for (int i = 0; i < 4; i++) r.v[i] = a.v[i] + b.v[i]; return r;
}
static inline dm_i32x4 dm_simd_sub_i32x4(dm_i32x4 a, dm_i32x4 b) {
    dm_i32x4 r; for (int i = 0; i < 4; i++) r.v[i] = a.v[i] - b.v[i]; return r;
}
static inline dm_i32x4 dm_simd_mul_i32x4(dm_i32x4 a, dm_i32x4 b) {
    dm_i32x4 r; for (int i = 0; i < 4; i++) r.v[i] = a.v[i] * b.v[i]; return r;
}
static inline dm_i32x4 dm_simd_div_i32x4(dm_i32x4 a, dm_i32x4 b) {
    dm_i32x4 r; for (int i = 0; i < 4; i++) r.v[i] = a.v[i] / b.v[i]; return r;
}
static inline dm_i64x2 dm_simd_add_i64x2(dm_i64x2 a, dm_i64x2 b) {
    dm_i64x2 r; for (int i = 0; i < 2; i++) r.v[i] = a.v[i] + b.v[i]; return r;
}
static inline dm_i64x2 dm_simd_sub_i64x2(dm_i64x2 a, dm_i64x2 b) {
    dm_i64x2 r; for (int i = 0; i < 2; i++) r.v[i] = a.v[i] - b.v[i]; return r;
}
static inline dm_i64x2 dm_simd_mul_i64x2(dm_i64x2 a, dm_i64x2 b) {
    dm_i64x2 r; for (int i = 0; i < 2; i++) r.v[i] = a.v[i] * b.v[i]; return r;
}
static inline dm_i64x2 dm_simd_div_i64x2(dm_i64x2 a, dm_i64x2 b) {
    dm_i64x2 r; for (int i = 0; i < 2; i++) r.v[i] = a.v[i] / b.v[i]; return r;
}

/* Fallback generic macros via _Generic (C11) */
#define dm_simd_add(a, b) _Generic((a), \
    dm_f32x4: dm_simd_add_f32x4, \
    dm_f64x2: dm_simd_add_f64x2, \
    dm_i32x4: dm_simd_add_i32x4, \
    dm_i64x2: dm_simd_add_i64x2)(a, b)
#define dm_simd_sub(a, b) _Generic((a), \
    dm_f32x4: dm_simd_sub_f32x4, \
    dm_f64x2: dm_simd_sub_f64x2, \
    dm_i32x4: dm_simd_sub_i32x4, \
    dm_i64x2: dm_simd_sub_i64x2)(a, b)
#define dm_simd_mul(a, b) _Generic((a), \
    dm_f32x4: dm_simd_mul_f32x4, \
    dm_f64x2: dm_simd_mul_f64x2, \
    dm_i32x4: dm_simd_mul_i32x4, \
    dm_i64x2: dm_simd_mul_i64x2)(a, b)
#define dm_simd_div(a, b) _Generic((a), \
    dm_f32x4: dm_simd_div_f32x4, \
    dm_f64x2: dm_simd_div_f64x2, \
    dm_i32x4: dm_simd_div_i32x4, \
    dm_i64x2: dm_simd_div_i64x2)(a, b)

#endif /* __GNUC__ || __clang__ */

#ifdef __cplusplus
}
#endif

#endif /* DAIMOND_RUNTIME_H */
