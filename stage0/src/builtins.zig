//! dAImond builtin (prelude) registry — the ONE table for free-function
//! builtins. The type checker and the C code generator both consume this
//! table; neither keeps a per-site builtin name list. To add a builtin,
//! add one row here (plus its C runtime helper when it lowers to a call).
//!
//! Semantics of each field, and what is deliberately NOT in this table:
//! - `params` is the documented Diamond signature. The checker records it
//!   but does not yet enforce parameter types or arity for builtins (it
//!   infers each argument, as it always has); `min_args` preserves
//!   codegen's emit guards. Tightening that is a behavior change, not
//!   part of this registry.
//! - `effects` replicates the checker's historical per-builtin effect
//!   additions exactly, including builtins that add none.
//! - Method-call builtins stay method-dispatched on the receiver's C type
//!   in codegen (`generateMethodCall`) and are NOT rows here: List
//!   `push/pop/get/set/insert/contains`, Map
//!   `insert/get/set/remove/contains/keys/values`, `x.len()` (the method
//!   spelling of `len`), associated constructors (`Box.new`, `Box.null`,
//!   `Map.new`, `List.new`, user `Type.new`), and Option/Result field
//!   access (`has_value/value/is_ok/ok/err`). Their free-function
//!   spellings (`Box_new`, ...) ARE rows below.
//! - The LSP server keeps small display-only completion/hover subsets;
//!   those are presentation text, not builtin recognition.
//!
//! Documented builtin list (name, signature, one-line semantics).
//! `T`/`U` are generic (erased in the checker, monomorphized in codegen);
//! `any` means the argument is accepted at any type:
//!
//!   print(v: any) -> unit                 write value to stdout
//!   println(v: any) -> unit               write value + newline to stdout (no-arg form writes just the newline)
//!   eprint(v: any) -> unit                write value to stderr
//!   eprintln(v: any) -> unit              write value + newline to stderr (no-arg form writes just the newline)
//!   panic(msg: string) -> unit            abort with message (never returns)
//!   assert(cond: bool) -> unit            abort if cond is false
//!   assert_eq(a: any, b: any) -> unit     abort unless a == b (string-aware)
//!   exit(code: int) -> unit               terminate process with code (never returns)
//!   flush() -> unit                       flush stdout
//!   read_line() -> string                 read one line from stdin
//!   read_key() -> int                     blocking single-key read (char code)
//!   read_key_nb() -> int                  non-blocking single-key read (-1 when none)
//!   term_width() -> int                   terminal width in columns
//!   term_height() -> int                  terminal height in rows
//!   time_ms() -> int                      wall-clock milliseconds
//!   sleep_ms(ms: int) -> unit             sleep for ms milliseconds
//!   args_len() -> int                     number of process arguments (incl. argv[0])
//!   args_get(i: int) -> string            process argument i
//!   system(cmd: string) -> int            run shell command, return its exit status
//!   env_get(name: string) -> string       environment variable value ("" when unset)
//!   len(x: string|List|Map|array) -> int  element/character count (method spelling: x.len())
//!   chr(code: int) -> string              one-character string for a char code
//!   int_to_string(v: int) -> string       decimal rendering of an int
//!   bool_to_string(v: bool) -> string     "true"/"false"
//!   float_to_string(v: float) -> string   rendering of a float
//!   char_to_string(c: char) -> string     one-character string for a char
//!   parse_int(s: string) -> int           parse decimal int (0 on failure)
//!   string_to_int(s: string) -> int       alias of parse_int
//!   parse_float(s: string) -> float       parse float (0.0 on failure)
//!   substr(s: string, start: int, n: int) -> string   substring of n chars from start
//!   char_at(s: string, i: int) -> string  bounds-checked one-char string at index i
//!   string_contains(s: string, sub: string) -> bool   substring test
//!   string_find(s: string, sub: string) -> int        first index of sub, or -1
//!   starts_with(s: string, p: string) -> bool         prefix test
//!   ends_with(s: string, p: string) -> bool           suffix test
//!   string_trim(s: string) -> string                  strip surrounding whitespace
//!   string_replace(s: string, old: string, new: string) -> string  replace all occurrences
//!   string_to_upper(s: string) -> string   ASCII upper-case
//!   string_to_lower(s: string) -> string   ASCII lower-case
//!   string_split(s: string, sep: string) -> List[string]  split on separator
//!   is_alpha(c: char|string) -> bool       alphabetic test (string: first char, "" is false)
//!   is_digit(c: char|string) -> bool       digit test (same string rule)
//!   is_alnum(c: char|string) -> bool       alphanumeric test (same string rule)
//!   is_whitespace(c: char|string) -> bool  whitespace test (same string rule)
//!   file_read(path: string) -> string      whole file contents ("" on failure)
//!   file_write(path: string, data: string) -> unit    write whole file
//!   file_append(path: string, data: string) -> unit   append to file
//!   file_exists(path: string) -> bool      file existence test
//!   fs_exists(path: string) -> bool        alias of file_exists
//!   fs_mkdir(path: string) -> int          create directory (0 on success)
//!   fs_remove(path: string) -> int         remove file (0 on success)
//!   fs_rmdir(path: string) -> int          remove path (lowers like fs_remove)
//!   fs_rename(old: string, new: string) -> int        rename path (0 on success)
//!   fs_readdir(path: string) -> string     newline-joined directory entries
//!   fs_getcwd() -> string                  current working directory
//!   path_dirname(p: string) -> string      directory part of a path
//!   path_basename(p: string) -> string     final component of a path
//!   path_extension(p: string) -> string    extension of the final component
//!   path_stem(p: string) -> string         final component without extension
//!   path_join(a: string, b: string) -> string        join two path parts
//!   tcp_listen(port: int) -> int           listening socket fd (or -1)
//!   tcp_accept(fd: int) -> int             accepted connection fd (or -1)
//!   tcp_connect(addr: string) -> int       connected socket fd (or -1); addr form is runtime-defined
//!   tcp_read(fd: int, max: int) -> string  read up to max bytes
//!   tcp_write(fd: int, data: string) -> int           write bytes, return count
//!   tcp_close(fd: int) -> unit             close socket fd
//!   thread_spawn(f: callable) -> int       spawn thread running f, return thread id
//!   thread_join(id: int) -> unit           wait for thread id
//!   mutex_new() -> int                     new mutex handle (pointer as int)
//!   mutex_lock(m: int) -> unit             lock mutex handle
//!   mutex_unlock(m: int) -> unit           unlock mutex handle
//!   mutex_destroy(m: int) -> unit          destroy mutex handle
//!   mem_mark() -> int                      arena mark token (documented no-op pair with mem_sweep)
//!   mem_sweep(mark: int) -> unit           sweep to mark (documented no-op)
//!   List_new() -> List[T]                  empty list (T from context; `[]` is the usual spelling)
//!   Map_new() -> Map[K, V]                 empty map (key/value types from context)
//!   Box_new(v: T) -> Box[T]                heap-allocate v
//!   Box_null() -> Box[T]                   null box (T from context)
//!   Some(v: T) -> Option[T]                Option constructor with value
//!   None() -> Option[T]                    Option constructor without value (also usable as bare `None`)
//!   Ok(v: T) -> Result[T, E]               Result success constructor
//!   Err(e: E) -> Result[T, E]              Result error constructor
//!   simd_add(a: V, b: V) -> V              lane-wise vector add (V: any SIMD vector type)
//!   simd_sub(a: V, b: V) -> V              lane-wise vector subtract
//!   simd_mul(a: V, b: V) -> V              lane-wise vector multiply
//!   simd_div(a: V, b: V) -> V              lane-wise vector divide
//!   simd_extract(v: V, i: int) -> scalar   lane i of a vector
//!   simd_splat_<V>(x: scalar) -> V         broadcast scalar to all lanes; V in f32x4 f32x8 f64x2 f64x4 i32x4 i32x8 i64x2 i64x4 (needs a type annotation at the binding)
//!   simd_set_<V>(lanes...) -> V            build vector from lane scalars (same V set and annotation rule)

const std = @import("std");

/// A documented parameter type in a builtin signature.
pub const TypeKind = enum {
    any,
    string,
    int,
    float,
    bool,
    char,
    char_or_string,
    /// A generic value `T` (Box_new/Some/Ok/Err payload).
    generic_value,
    /// string | List | Map | fixed array | slice (the `len` argument).
    collection,
    callable,
    simd_vector,
    simd_scalar,
};

/// How the checker produces the call's result type.
pub const RetKind = enum {
    unit,
    int,
    float,
    string,
    bool,
    /// A fresh type variable: erased generic containers and
    /// Option/Result constructors, resolved from context by codegen.
    fresh,
    /// `Box[typeof(arg0)]` (Box_new); fresh when called with no args.
    box_of_arg0,
    /// The first argument's type (SIMD binary ops); fresh with no args.
    first_arg,
};

/// Checker effect additions for a builtin call.
pub const Effects = packed struct {
    io: bool = false,
    console: bool = false,
    file_system: bool = false,
    diverge: bool = false,
};

/// How codegen emits a builtin call.
pub const Lowering = enum {
    /// `c_name(arg0, ..., argN)` using the first `min_args` arguments.
    call,
    /// `c_name()`; Diamond arguments, if any, are ignored.
    call_no_args,
    print,
    println,
    eprint,
    eprintln,
    panic,
    assert_fn,
    assert_eq_fn,
    len,
    /// `c_name(first-char-or-char)`; see `generateCharPredicateCall`.
    char_predicate,
    char_at,
    box_alloc,
    box_null,
    zero_list,
    zero_map,
    string_split,
    option_some,
    option_none,
    result_ok,
    result_err,
    /// `dm_<name>(arg0, arg1)` (SIMD binary ops; the simd_splat_/set_
    /// prefix families lower the same way via simdPrefix).
    simd_binary,
    flush_stdout,
};

/// Compile-time evaluation class of a builtin.
pub const Comptime = enum {
    other,
    /// Never evaluated at comptime (side effects / environment).
    impure,
    pure_len,
    pure_int_to_string,
};

pub const Builtin = struct {
    name: []const u8,
    params: []const TypeKind,
    ret: RetKind,
    /// Arguments codegen needs before it emits anything.
    min_args: u8,
    effects: Effects = .{},
    lowering: Lowering,
    c_name: ?[]const u8 = null,
    /// Static C return type for `inferCTypeFromExpr`; null keeps the
    /// declaration-lookup / default fallthrough.
    c_ret: ?[]const u8 = null,
    comptime_kind: Comptime = .other,
    /// List_new/Map_new/Box_new: a call produces a region-born value.
    fresh_ctor: bool = false,
    doc: []const u8,
};

const io_console: Effects = .{ .io = true, .console = true };
const io_only: Effects = .{ .io = true };
const io_diverge: Effects = .{ .io = true, .diverge = true };
const fs_only: Effects = .{ .file_system = true };

/// The registry: every free-function builtin, in documentation order.
pub const all = [_]Builtin{
    // Console, process, timing
    .{ .name = "print", .params = &.{.any}, .ret = .unit, .min_args = 0, .effects = io_console, .lowering = .print, .comptime_kind = .impure, .doc = "write value to stdout" },
    .{ .name = "println", .params = &.{.any}, .ret = .unit, .min_args = 0, .effects = io_console, .lowering = .println, .comptime_kind = .impure, .doc = "write value + newline to stdout" },
    .{ .name = "eprint", .params = &.{.any}, .ret = .unit, .min_args = 0, .effects = io_console, .lowering = .eprint, .comptime_kind = .impure, .doc = "write value to stderr" },
    .{ .name = "eprintln", .params = &.{.any}, .ret = .unit, .min_args = 0, .effects = io_console, .lowering = .eprintln, .comptime_kind = .impure, .doc = "write value + newline to stderr" },
    .{ .name = "panic", .params = &.{.string}, .ret = .unit, .min_args = 0, .effects = io_diverge, .lowering = .panic, .comptime_kind = .impure, .doc = "abort with message (never returns)" },
    .{ .name = "assert", .params = &.{.bool}, .ret = .unit, .min_args = 0, .lowering = .assert_fn, .doc = "abort if cond is false" },
    .{ .name = "assert_eq", .params = &.{ .any, .any }, .ret = .unit, .min_args = 2, .lowering = .assert_eq_fn, .doc = "abort unless a == b (string-aware)" },
    .{ .name = "exit", .params = &.{.int}, .ret = .unit, .min_args = 1, .effects = io_diverge, .lowering = .call, .c_name = "dm_exit", .comptime_kind = .impure, .doc = "terminate process with code (never returns)" },
    .{ .name = "flush", .params = &.{}, .ret = .unit, .min_args = 0, .effects = io_only, .lowering = .flush_stdout, .comptime_kind = .impure, .doc = "flush stdout" },
    .{ .name = "read_line", .params = &.{}, .ret = .string, .min_args = 0, .effects = io_console, .lowering = .call_no_args, .c_name = "dm_read_line", .c_ret = "dm_string", .comptime_kind = .impure, .doc = "read one line from stdin" },
    .{ .name = "read_key", .params = &.{}, .ret = .int, .min_args = 0, .effects = io_console, .lowering = .call_no_args, .c_name = "dm_read_key", .c_ret = "int64_t", .comptime_kind = .impure, .doc = "blocking single-key read (char code)" },
    .{ .name = "read_key_nb", .params = &.{}, .ret = .int, .min_args = 0, .effects = io_console, .lowering = .call_no_args, .c_name = "dm_read_key_nb", .c_ret = "int64_t", .comptime_kind = .impure, .doc = "non-blocking single-key read (-1 when none)" },
    .{ .name = "term_width", .params = &.{}, .ret = .int, .min_args = 0, .effects = io_only, .lowering = .call_no_args, .c_name = "dm_term_width", .c_ret = "int64_t", .comptime_kind = .impure, .doc = "terminal width in columns" },
    .{ .name = "term_height", .params = &.{}, .ret = .int, .min_args = 0, .effects = io_only, .lowering = .call_no_args, .c_name = "dm_term_height", .c_ret = "int64_t", .comptime_kind = .impure, .doc = "terminal height in rows" },
    .{ .name = "time_ms", .params = &.{}, .ret = .int, .min_args = 0, .effects = io_only, .lowering = .call_no_args, .c_name = "dm_time_ms", .c_ret = "int64_t", .comptime_kind = .impure, .doc = "wall-clock milliseconds" },
    .{ .name = "sleep_ms", .params = &.{.int}, .ret = .unit, .min_args = 1, .effects = io_only, .lowering = .call, .c_name = "dm_sleep_ms", .comptime_kind = .impure, .doc = "sleep for ms milliseconds" },
    .{ .name = "args_len", .params = &.{}, .ret = .int, .min_args = 0, .effects = io_only, .lowering = .call_no_args, .c_name = "dm_args_len", .c_ret = "int64_t", .doc = "number of process arguments (incl. argv[0])" },
    .{ .name = "args_get", .params = &.{.int}, .ret = .string, .min_args = 1, .effects = io_only, .lowering = .call, .c_name = "dm_args_get", .c_ret = "dm_string", .doc = "process argument i" },
    .{ .name = "system", .params = &.{.string}, .ret = .int, .min_args = 1, .effects = io_only, .lowering = .call, .c_name = "dm_system", .c_ret = "int64_t", .comptime_kind = .impure, .doc = "run shell command, return its exit status" },
    .{ .name = "env_get", .params = &.{.string}, .ret = .string, .min_args = 1, .effects = io_only, .lowering = .call, .c_name = "dm_os_getenv", .c_ret = "dm_string", .doc = "environment variable value (\"\" when unset)" },
    // Length and conversions
    .{ .name = "len", .params = &.{.collection}, .ret = .int, .min_args = 1, .lowering = .len, .c_ret = "int64_t", .comptime_kind = .pure_len, .doc = "element/character count (method spelling: x.len())" },
    .{ .name = "chr", .params = &.{.int}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_chr", .c_ret = "dm_string", .doc = "one-character string for a char code" },
    .{ .name = "int_to_string", .params = &.{.int}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_int_to_string", .c_ret = "dm_string", .comptime_kind = .pure_int_to_string, .doc = "decimal rendering of an int" },
    .{ .name = "bool_to_string", .params = &.{.bool}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_bool_to_string", .c_ret = "dm_string", .doc = "\"true\"/\"false\"" },
    .{ .name = "float_to_string", .params = &.{.float}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_float_to_string", .c_ret = "dm_string", .doc = "rendering of a float" },
    .{ .name = "char_to_string", .params = &.{.char}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_char_to_string", .c_ret = "dm_string", .doc = "one-character string for a char" },
    .{ .name = "parse_int", .params = &.{.string}, .ret = .int, .min_args = 1, .lowering = .call, .c_name = "dm_parse_int", .c_ret = "int64_t", .doc = "parse decimal int (0 on failure)" },
    .{ .name = "string_to_int", .params = &.{.string}, .ret = .int, .min_args = 1, .lowering = .call, .c_name = "dm_parse_int", .c_ret = "int64_t", .doc = "alias of parse_int" },
    .{ .name = "parse_float", .params = &.{.string}, .ret = .float, .min_args = 1, .lowering = .call, .c_name = "dm_parse_float", .c_ret = "double", .doc = "parse float (0.0 on failure)" },
    // Strings
    .{ .name = "substr", .params = &.{ .string, .int, .int }, .ret = .string, .min_args = 3, .lowering = .call, .c_name = "dm_string_substr", .c_ret = "dm_string", .doc = "substring of n chars from start" },
    .{ .name = "char_at", .params = &.{ .string, .int }, .ret = .string, .min_args = 2, .lowering = .char_at, .c_ret = "dm_string", .doc = "bounds-checked one-char string at index i" },
    .{ .name = "string_contains", .params = &.{ .string, .string }, .ret = .bool, .min_args = 2, .lowering = .call, .c_name = "dm_string_contains", .c_ret = "bool", .doc = "substring test" },
    .{ .name = "string_find", .params = &.{ .string, .string }, .ret = .int, .min_args = 2, .lowering = .call, .c_name = "dm_string_find", .c_ret = "int64_t", .doc = "first index of sub, or -1" },
    .{ .name = "starts_with", .params = &.{ .string, .string }, .ret = .bool, .min_args = 2, .lowering = .call, .c_name = "dm_string_starts_with", .c_ret = "bool", .doc = "prefix test" },
    .{ .name = "ends_with", .params = &.{ .string, .string }, .ret = .bool, .min_args = 2, .lowering = .call, .c_name = "dm_string_ends_with", .c_ret = "bool", .doc = "suffix test" },
    .{ .name = "string_trim", .params = &.{.string}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_string_trim", .c_ret = "dm_string", .doc = "strip surrounding whitespace" },
    .{ .name = "string_replace", .params = &.{ .string, .string, .string }, .ret = .string, .min_args = 3, .lowering = .call, .c_name = "dm_string_replace", .c_ret = "dm_string", .doc = "replace all occurrences" },
    .{ .name = "string_to_upper", .params = &.{.string}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_string_to_upper", .c_ret = "dm_string", .doc = "ASCII upper-case" },
    .{ .name = "string_to_lower", .params = &.{.string}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_string_to_lower", .c_ret = "dm_string", .doc = "ASCII lower-case" },
    .{ .name = "string_split", .params = &.{ .string, .string }, .ret = .fresh, .min_args = 2, .lowering = .string_split, .c_ret = "dm_list_dm_string", .doc = "split on separator into List[string]" },
    .{ .name = "is_alpha", .params = &.{.char_or_string}, .ret = .bool, .min_args = 1, .lowering = .char_predicate, .c_name = "dm_is_alpha", .c_ret = "bool", .doc = "alphabetic test (string: first char, \"\" is false)" },
    .{ .name = "is_digit", .params = &.{.char_or_string}, .ret = .bool, .min_args = 1, .lowering = .char_predicate, .c_name = "dm_is_digit", .c_ret = "bool", .doc = "digit test (same string rule)" },
    .{ .name = "is_alnum", .params = &.{.char_or_string}, .ret = .bool, .min_args = 1, .lowering = .char_predicate, .c_name = "dm_is_alnum", .c_ret = "bool", .doc = "alphanumeric test (same string rule)" },
    .{ .name = "is_whitespace", .params = &.{.char_or_string}, .ret = .bool, .min_args = 1, .lowering = .char_predicate, .c_name = "dm_is_whitespace", .c_ret = "bool", .doc = "whitespace test (same string rule)" },
    // Files and paths
    .{ .name = "file_read", .params = &.{.string}, .ret = .string, .min_args = 1, .effects = fs_only, .lowering = .call, .c_name = "dm_file_read", .c_ret = "dm_string", .comptime_kind = .impure, .doc = "whole file contents (\"\" on failure)" },
    .{ .name = "file_write", .params = &.{ .string, .string }, .ret = .unit, .min_args = 2, .effects = fs_only, .lowering = .call, .c_name = "dm_file_write", .comptime_kind = .impure, .doc = "write whole file" },
    .{ .name = "file_append", .params = &.{ .string, .string }, .ret = .unit, .min_args = 2, .effects = fs_only, .lowering = .call, .c_name = "dm_append_file", .comptime_kind = .impure, .doc = "append to file" },
    .{ .name = "file_exists", .params = &.{.string}, .ret = .bool, .min_args = 1, .effects = fs_only, .lowering = .call, .c_name = "dm_file_exists", .c_ret = "bool", .doc = "file existence test" },
    .{ .name = "fs_exists", .params = &.{.string}, .ret = .bool, .min_args = 1, .effects = fs_only, .lowering = .call, .c_name = "dm_file_exists", .c_ret = "bool", .doc = "alias of file_exists" },
    .{ .name = "fs_mkdir", .params = &.{.string}, .ret = .int, .min_args = 1, .effects = fs_only, .lowering = .call, .c_name = "dm_fs_mkdir", .c_ret = "int64_t", .doc = "create directory (0 on success)" },
    .{ .name = "fs_remove", .params = &.{.string}, .ret = .int, .min_args = 1, .effects = fs_only, .lowering = .call, .c_name = "dm_fs_remove", .c_ret = "int64_t", .doc = "remove file (0 on success)" },
    .{ .name = "fs_rmdir", .params = &.{.string}, .ret = .int, .min_args = 1, .effects = fs_only, .lowering = .call, .c_name = "dm_fs_remove", .c_ret = "int64_t", .doc = "remove path (lowers like fs_remove)" },
    .{ .name = "fs_rename", .params = &.{ .string, .string }, .ret = .int, .min_args = 2, .effects = fs_only, .lowering = .call, .c_name = "dm_fs_rename", .c_ret = "int64_t", .doc = "rename path (0 on success)" },
    .{ .name = "fs_readdir", .params = &.{.string}, .ret = .string, .min_args = 1, .effects = fs_only, .lowering = .call, .c_name = "dm_fs_readdir", .c_ret = "dm_string", .doc = "newline-joined directory entries" },
    .{ .name = "fs_getcwd", .params = &.{}, .ret = .string, .min_args = 0, .effects = fs_only, .lowering = .call_no_args, .c_name = "dm_fs_getcwd", .c_ret = "dm_string", .doc = "current working directory" },
    .{ .name = "path_dirname", .params = &.{.string}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_path_dirname", .c_ret = "dm_string", .doc = "directory part of a path" },
    .{ .name = "path_basename", .params = &.{.string}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_path_basename", .c_ret = "dm_string", .doc = "final component of a path" },
    .{ .name = "path_extension", .params = &.{.string}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_path_extension", .c_ret = "dm_string", .doc = "extension of the final component" },
    .{ .name = "path_stem", .params = &.{.string}, .ret = .string, .min_args = 1, .lowering = .call, .c_name = "dm_path_stem", .c_ret = "dm_string", .doc = "final component without extension" },
    .{ .name = "path_join", .params = &.{ .string, .string }, .ret = .string, .min_args = 2, .lowering = .call, .c_name = "dm_path_join", .c_ret = "dm_string", .doc = "join two path parts" },
    // Networking, threads, memory marks
    .{ .name = "tcp_listen", .params = &.{.int}, .ret = .int, .min_args = 1, .lowering = .call, .c_name = "dm_tcp_listen_fd", .c_ret = "int64_t", .doc = "listening socket fd (or -1)" },
    .{ .name = "tcp_accept", .params = &.{.int}, .ret = .int, .min_args = 1, .lowering = .call, .c_name = "dm_tcp_accept_fd", .c_ret = "int64_t", .doc = "accepted connection fd (or -1)" },
    .{ .name = "tcp_connect", .params = &.{.string}, .ret = .int, .min_args = 1, .lowering = .call, .c_name = "dm_tcp_connect_fd", .c_ret = "int64_t", .doc = "connected socket fd (or -1); addr form is runtime-defined" },
    .{ .name = "tcp_read", .params = &.{ .int, .int }, .ret = .string, .min_args = 2, .lowering = .call, .c_name = "dm_tcp_read_fd", .c_ret = "dm_string", .doc = "read up to max bytes" },
    .{ .name = "tcp_write", .params = &.{ .int, .string }, .ret = .int, .min_args = 2, .lowering = .call, .c_name = "dm_tcp_write_fd", .c_ret = "int64_t", .doc = "write bytes, return count" },
    .{ .name = "tcp_close", .params = &.{.int}, .ret = .unit, .min_args = 1, .lowering = .call, .c_name = "dm_tcp_close_fd", .doc = "close socket fd" },
    .{ .name = "thread_spawn", .params = &.{.callable}, .ret = .int, .min_args = 1, .lowering = .call, .c_name = "dm_thread_spawn_fn", .c_ret = "int64_t", .doc = "spawn thread running f, return thread id" },
    .{ .name = "thread_join", .params = &.{.int}, .ret = .unit, .min_args = 1, .lowering = .call, .c_name = "dm_thread_join_id", .doc = "wait for thread id" },
    .{ .name = "mutex_new", .params = &.{}, .ret = .int, .min_args = 0, .lowering = .call_no_args, .c_name = "dm_mutex_new_ptr", .c_ret = "int64_t", .doc = "new mutex handle (pointer as int)" },
    .{ .name = "mutex_lock", .params = &.{.int}, .ret = .unit, .min_args = 1, .lowering = .call, .c_name = "dm_mutex_lock_ptr", .doc = "lock mutex handle" },
    .{ .name = "mutex_unlock", .params = &.{.int}, .ret = .unit, .min_args = 1, .lowering = .call, .c_name = "dm_mutex_unlock_ptr", .doc = "unlock mutex handle" },
    .{ .name = "mutex_destroy", .params = &.{.int}, .ret = .unit, .min_args = 1, .lowering = .call, .c_name = "dm_mutex_destroy_ptr", .doc = "destroy mutex handle" },
    .{ .name = "mem_mark", .params = &.{}, .ret = .int, .min_args = 0, .lowering = .call_no_args, .c_name = "dm_mem_mark", .comptime_kind = .impure, .doc = "arena mark token (documented no-op pair with mem_sweep)" },
    .{ .name = "mem_sweep", .params = &.{.int}, .ret = .unit, .min_args = 1, .lowering = .call, .c_name = "dm_mem_sweep", .comptime_kind = .impure, .doc = "sweep to mark (documented no-op)" },
    // Container constructors and Option/Result constructors
    .{ .name = "List_new", .params = &.{}, .ret = .fresh, .min_args = 0, .lowering = .zero_list, .fresh_ctor = true, .doc = "empty list (element type from context)" },
    .{ .name = "Map_new", .params = &.{}, .ret = .fresh, .min_args = 0, .lowering = .zero_map, .fresh_ctor = true, .doc = "empty map (key/value types from context)" },
    .{ .name = "Box_new", .params = &.{.generic_value}, .ret = .box_of_arg0, .min_args = 1, .lowering = .box_alloc, .fresh_ctor = true, .doc = "heap-allocate a value: (v: T) -> Box[T]" },
    .{ .name = "Box_null", .params = &.{}, .ret = .fresh, .min_args = 0, .lowering = .box_null, .doc = "null box (element type from context)" },
    .{ .name = "Some", .params = &.{.generic_value}, .ret = .fresh, .min_args = 1, .lowering = .option_some, .doc = "Option constructor with value: (v: T) -> Option[T]" },
    .{ .name = "None", .params = &.{}, .ret = .fresh, .min_args = 0, .lowering = .option_none, .doc = "Option constructor without value (also usable as bare None)" },
    .{ .name = "Ok", .params = &.{.generic_value}, .ret = .fresh, .min_args = 1, .lowering = .result_ok, .doc = "Result success constructor: (v: T) -> Result[T, E]" },
    .{ .name = "Err", .params = &.{.generic_value}, .ret = .fresh, .min_args = 1, .lowering = .result_err, .doc = "Result error constructor: (e: E) -> Result[T, E]" },
    // SIMD (prefix families simd_splat_<V>/simd_set_<V>: see simdPrefix)
    .{ .name = "simd_add", .params = &.{ .simd_vector, .simd_vector }, .ret = .first_arg, .min_args = 2, .lowering = .simd_binary, .doc = "lane-wise vector add" },
    .{ .name = "simd_sub", .params = &.{ .simd_vector, .simd_vector }, .ret = .first_arg, .min_args = 2, .lowering = .simd_binary, .doc = "lane-wise vector subtract" },
    .{ .name = "simd_mul", .params = &.{ .simd_vector, .simd_vector }, .ret = .first_arg, .min_args = 2, .lowering = .simd_binary, .doc = "lane-wise vector multiply" },
    .{ .name = "simd_div", .params = &.{ .simd_vector, .simd_vector }, .ret = .first_arg, .min_args = 2, .lowering = .simd_binary, .doc = "lane-wise vector divide" },
    .{ .name = "simd_extract", .params = &.{ .simd_vector, .int }, .ret = .fresh, .min_args = 2, .lowering = .call, .c_name = "dm_simd_extract", .doc = "lane i of a vector" },
};

/// Look up a builtin by its Diamond name.
pub fn lookup(name: []const u8) ?*const Builtin {
    for (&all) |*entry| {
        if (std.mem.eql(u8, entry.name, name)) return entry;
    }
    return null;
}

/// A `simd_splat_<V>` / `simd_set_<V>` family member.
pub const SimdPrefix = struct {
    kind: enum { splat, set },
    /// The vector type suffix, e.g. "f32x4".
    suffix: []const u8,
};

/// Recognize the SIMD prefix families `simd_splat_<V>` and
/// `simd_set_<V>`, where `<V>` is a SIMD vector type name. Splat lowers
/// like a one-argument `dm_<name>(x)` call, set with every argument;
/// both return the vector type named by the suffix.
pub fn simdPrefix(name: []const u8) ?SimdPrefix {
    if (std.mem.startsWith(u8, name, "simd_splat_")) {
        return .{ .kind = .splat, .suffix = name["simd_splat_".len..] };
    }
    if (std.mem.startsWith(u8, name, "simd_set_")) {
        return .{ .kind = .set, .suffix = name["simd_set_".len..] };
    }
    return null;
}

/// True for the fresh-container constructors whose result counts as
/// region-born (List_new/Map_new/Box_new).
pub fn isFreshCtor(name: []const u8) bool {
    const entry = lookup(name) orelse return false;
    return entry.fresh_ctor;
}

test "builtin registry has no duplicate names and covers core builtins" {
    for (all, 0..) |a, i| {
        for (all[i + 1 ..]) |b| {
            try std.testing.expect(!std.mem.eql(u8, a.name, b.name));
        }
    }
    try std.testing.expect(lookup("println") != null);
    try std.testing.expect(lookup("len").?.ret == .int);
    try std.testing.expect(lookup("chr").?.c_ret != null);
    try std.testing.expect(lookup("not_a_builtin") == null);
    try std.testing.expect(simdPrefix("simd_splat_f32x4").?.kind == .splat);
    try std.testing.expectEqualStrings("f32x4", simdPrefix("simd_splat_f32x4").?.suffix);
    try std.testing.expect(simdPrefix("println") == null);
    try std.testing.expect(isFreshCtor("Box_new"));
    try std.testing.expect(!isFreshCtor("println"));
}
