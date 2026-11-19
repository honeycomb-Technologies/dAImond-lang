# CLAUDE.md - AI Assistant Guide for dAImond-lang

## Project Overview

dAImond is a systems programming language designed to be the fastest compiled language, the most AI-generatable language, self-compiling, memory-safe without garbage collection (region-based with full inference), and fully Turing-complete at compile time.

The repository contains the **Stage 0 bootstrap compiler** (Zig → C), the **Stage 1 self-hosting compiler** (dAImond → C), the **Stage 3 LLVM backend** (Zig + LLVM), and the **Stage 5 LLVM backend** (dAImond + LLVM, with memory-safety checks). Stage 5 compiles itself, but its first build still comes from Stage 0 (Zig), and its link step still compiles the C runtime and LLVM bridge with `cc`.

## Repository Structure

```
dAImond-lang/
├── CLAUDE.md              # This file
├── README.md              # Project documentation and language overview
├── LICENSE                # MIT License
├── stage0/                # Stage 0 bootstrap compiler (Zig)
│   ├── build.zig          # Zig build configuration
│   ├── .mise.toml         # Tool version pinning (Zig 0.13.0)
│   ├── .gitignore         # Ignores .zig-cache/, zig-out/, generated *.c
│   ├── src/               # Compiler source code
│   │   ├── main.zig       # CLI entry point, option parsing, pipeline orchestration
│   │   ├── lexer.zig      # Lexical analyzer (source text -> tokens)
│   │   ├── parser.zig     # Recursive descent + Pratt parser (tokens -> AST)
│   │   ├── ast.zig        # AST node type definitions
│   │   ├── types.zig      # Type system, type interning, inference, effects
│   │   ├── checker.zig    # Type checker, symbol tables, scope management, unification
│   │   ├── codegen.zig    # C11 code generation from typed AST
│   │   ├── errors.zig     # Diagnostic reporting, colored output, error codes
│   │   ├── package.zig    # Package manager (TOML manifest, dependency resolution)
│   │   ├── cache.zig      # Compilation cache (SHA-256 content hashing)
│   │   └── lsp.zig        # Language Server Protocol implementation
│   ├── runtime/           # C runtime library linked into compiled programs
│   │   ├── daimond_runtime.h   # Runtime API (strings, arenas, option/result, I/O, sockets, threads)
│   │   ├── daimond_runtime.c   # Runtime implementation
│   │   └── test_runtime.c      # Runtime unit tests
│   └── tests/
│       └── runner.zig     # Integration test harness
├── stage1/                # Stage 1 self-hosting compiler (dAImond)
│   ├── main.dm            # Generated monolithic file (for Stage 0 bootstrap)
│   ├── main_split.dm      # Entry point with imports (for Stage 1 self-compilation)
│   ├── token.dm           # Token kind constants, Token struct, keyword_lookup
│   ├── lexer.dm           # Lexer struct, tokenize, character helpers
│   ├── compiler.dm        # Compiler struct, parser helpers, type mapping
│   ├── compile_expr.dm    # Expression compiler chain, lambda, generics
│   ├── compile_stmt.dm    # Statement compiler, type tracking/inference
│   ├── compile_match.dm   # Match expressions and statements
│   ├── compile_decl.dm    # Function/struct/enum/impl declarations, prescan
│   ├── runtime.dm         # C runtime emission, output assembly
│   ├── imports.dm         # Multi-file import resolver
│   ├── package.dm         # Package manager (TOML manifest, pkg init/add/list)
│   ├── gen_bootstrap.sh   # Concatenates modules into main.dm
│   └── test_lexer.dm      # Lexer standalone test
├── stdlib/                # Standard library modules
│   ├── io.dm              # I/O functions (print, file operations)
│   ├── math.dm            # Math functions (sin, cos, sqrt, etc.)
│   ├── collections.dm     # Set, Queue, Stack wrappers
│   ├── string.dm          # String utilities (split, join, repeat, etc.)
│   ├── os.dm              # OS functions (env, cwd, exit, args)
│   ├── fs.dm              # Filesystem operations (mkdir, readdir, etc.)
│   ├── net.dm             # Networking (TCP sockets)
│   ├── thread.dm          # Concurrency (spawn, join, mutex)
│   └── test.dm            # Testing framework documentation
├── examples/              # Example dAImond programs
│   ├── hello.dm           # Hello World
│   ├── arithmetic.dm      # Basic math operations
│   ├── fibonacci.dm       # Recursive functions
│   └── calculator.dm      # Full scientific calculator demo
├── docs/                  # Design documents
│   ├── ir-spec.md         # dAImond IR specification (SSA-based, typed)
│   ├── llvm-backend.md    # Stage 3 LLVM backend architecture
│   └── stage-history.md   # Archived Stage 1/3 documentation
├── stage3/                # Stage 3 LLVM backend (Zig)
│   ├── build.zig          # Zig build configuration (links LLVM-C)
│   ├── src/               # LLVM backend source
│   │   ├── main.zig       # CLI entry point, pipeline orchestration
│   │   ├── ir.zig         # dAImond IR definitions (SSA types, instructions, builder)
│   │   ├── ir_gen.zig     # IR generation from typed AST
│   │   ├── llvm_gen.zig   # LLVM IR generation from dAImond IR
│   │   └── llvm_bindings.zig  # Safe Zig wrappers for LLVM-C API
│   ├── runtime/
│   │   └── llvm_wrappers.c    # ABI wrappers for string-passing conventions
│   └── tests/
│       └── runner.zig         # Integration test harness (254 tests, mirrors Stage 0)
├── stage5/                # Stage 5 LLVM backend (dAImond; built by Stage 0)
│   ├── main.dm            # Monolithic bootstrap file (generated, ~21.7K lines)
│   ├── main_split.dm      # Entry point with imports (for modular development)
│   ├── token.dm           # Token kinds, Token struct, keyword lookup
│   ├── lexer.dm           # Lexer struct, tokenize, character helpers
│   ├── parser.dm          # Recursive descent + Pratt parser
│   ├── ast.dm             # AST node type definitions (integer kind tags)
│   ├── ir.dm              # dAImond IR definitions (SSA types, instructions)
│   ├── ir_builder.dm      # IR builder (basic blocks, instruction emission)
│   ├── ir_gen.dm          # IR generation entry point, struct/enum registration
│   ├── ir_gen_expr.dm     # Expression IR generation (calls, generics, closures)
│   ├── ir_gen_stmt.dm     # Statement IR generation (let, if, for, while, match)
│   ├── ir_gen_decl.dm     # Declaration IR generation (functions, impls)
│   ├── ir_gen_builtins.dm # Builtin function IR generation
│   ├── ir_gen_comptime.dm # Comptime evaluation
│   ├── llvm.dm            # LLVM type definitions and helpers
│   ├── llvm_gen.dm        # LLVM IR generation from dAImond IR
│   ├── llvm_gen_call.dm   # LLVM call instruction generation
│   ├── llvm_gen_simd.dm   # LLVM SIMD instruction generation
│   ├── llvm_bridge.c      # C bridge to LLVM-C API (extern FFI)
│   ├── prelude.dm         # Generated by gen_bootstrap.sh: the runtime/*.dm files as a string
│   ├── runtime/
│   │   ├── string.dm          # String core (concat, eq, cmp, char_at, substr), written in dAImond
│   │   ├── map.dm             # Map[K, V], written in dAImond, compiled into every program
│   │   └── llvm_wrappers.c    # C wrappers for the runtime functions not yet ported to dAImond
│   ├── build.zig          # Builds the integration test harness
│   ├── tests/
│   │   └── runner.zig     # Runs Stage 0's test cases, plus Stage 5-only ones, against the Stage 5 binary
│   ├── run_parity.sh      # Builds Stage 5 and runs the integration suite
│   └── gen_bootstrap.sh   # Concatenates modules into main.dm
└── tests/                 # Integration test programs (.dm files)
    ├── arithmetic.dm      # Arithmetic validation
    ├── structs.dm         # Struct/enum/pattern matching tests
    ├── generics.dm        # Generic functions and traits tests
    ├── test_nested_for.dm # Nested for-in loop validation (Stage 1)
    ├── test_for_types.dm  # For-in with List[string]/List[struct] (Stage 1)
    ├── test_compound_assign.dm # *=, /= operators (Stage 1)
    ├── test_pipeline.dm   # Pipeline operator |> (Stage 1)
    ├── test_try_operator.dm   # Error propagation ? (Stage 1)
    ├── test_box.dm        # Box[T] support (Stage 1)
    ├── test_builtins.dm   # All builtins (Stage 1)
    └── test_mono_stress.dm # Generic monomorphization stress test (Stage 1)
```

## Compiler Pipeline

Stage 0 (C backend):
```
Source (.dm) -> Lexer -> Tokens -> Parser -> AST -> Checker -> Typed AST -> Codegen -> C Code -> cc -> Binary
```

Stage 3 (LLVM backend, Zig):
```
Source (.dm) -> Lexer -> Tokens -> Parser -> AST -> Checker -> Typed AST -> IR Gen -> dAImond IR -> LLVM Gen -> LLVM IR -> LLVM -> Native Binary
```

Stage 5 (LLVM backend, dAImond):
```
Source (.dm) -> Lexer -> Tokens -> Parser -> AST -> Validation -> Effect Check -> IR Gen -> dAImond IR -> LLVM Gen (via llvm_bridge.c FFI) -> LLVM IR -> LLVM -> Native Binary
```

Each Stage 0 stage maps to a source file: `lexer.zig` -> `parser.zig` -> `ast.zig`/`types.zig` -> `checker.zig` -> `codegen.zig`.

## Build Commands

All build commands run from the `stage0/` directory. **Zig 0.13.0** is required (pinned in `.mise.toml`).

```bash
# Build the compiler (produces zig-out/bin/daimond and zig-out/bin/daimond-lsp)
cd stage0 && zig build

# Run unit tests (all modules: lexer, parser, ast, types, checker, codegen, errors, package, lsp)
cd stage0 && zig build test

# Run integration tests (compiles and executes .dm test programs)
cd stage0 && zig build test-integration

# Run the compiler
cd stage0 && ./zig-out/bin/daimond <file.dm>

# Compile and run in one step
cd stage0 && ./zig-out/bin/daimond run <file.dm>

# Debug: show tokens
cd stage0 && ./zig-out/bin/daimond lex <file.dm>

# Debug: show AST
cd stage0 && ./zig-out/bin/daimond parse <file.dm>

# Type check only
cd stage0 && ./zig-out/bin/daimond check <file.dm>

# Format source code
cd stage0 && ./zig-out/bin/daimond fmt <file.dm>

# Run test functions (discovers fn test_*() in source)
cd stage0 && ./zig-out/bin/daimond test <file.dm>

# Package management
cd stage0 && ./zig-out/bin/daimond pkg init          # Create daimond.toml
cd stage0 && ./zig-out/bin/daimond pkg add <name>     # Add dependency
cd stage0 && ./zig-out/bin/daimond pkg list           # List dependencies

# Start LSP server (for IDE integration, communicates via JSON-RPC/stdio)
cd stage0 && ./zig-out/bin/daimond-lsp
```

### Stage 3 (LLVM Backend — Archived)

Stage 3 is frozen. See `docs/stage-history.md` for build commands.

### Stage 5 (LLVM Backend in dAImond)

Stage 5 is compiled by Stage 0. It requires **LLVM 17+** development libraries and the C runtime.

```bash
# Build Stage 5 compiler (Stage 0 compiles main.dm to C, then gcc produces binary)
cd stage0 && ./zig-out/bin/daimond ../stage5/main.dm -c -o /tmp/stage5.c
gcc -c /tmp/stage5.c -o /tmp/stage5.o -I runtime -Wno-incompatible-pointer-types -Wno-int-conversion
gcc -c ../stage5/llvm_bridge.c -o /tmp/llvm_bridge.o $(llvm-config --cflags)
gcc -c ../stage5/runtime/llvm_wrappers.c -o /tmp/llvm_wrappers.o -I runtime
gcc -c runtime/daimond_runtime.c -o /tmp/daimond_runtime.o
gcc /tmp/stage5.o /tmp/llvm_bridge.o /tmp/llvm_wrappers.o /tmp/daimond_runtime.o -o /tmp/stage5_bin -lm -lpthread $(llvm-config --libs --ldflags)

# Compile a dAImond program to native binary via Stage 5
/tmp/stage5_bin <file.dm> -o output

# Compile with optimization
/tmp/stage5_bin <file.dm> -o output -O2

# Emit dAImond IR (debugging)
/tmp/stage5_bin <file.dm> --emit-ir

# Emit LLVM IR (debugging)
/tmp/stage5_bin <file.dm> --emit-llvm

# Regenerate monolithic main.dm from modules
cd stage5 && bash gen_bootstrap.sh

# Compile Stage 5 with itself (--link-llvm links the LLVM bridge)
cd stage5 && /tmp/stage5_bin main.dm -o /tmp/stage5_self --link-llvm

# Run the integration suite against the Stage 5 binary (290 tests)
bash stage5/run_parity.sh
```

## Testing Strategy

There are two tiers of tests:

1. **Unit tests** (`zig build test`): Each Zig source module contains inline `test` blocks. Nine modules have tests registered in `build.zig`: lexer, errors, ast, types, parser, codegen, checker, package, lsp.

2. **Integration tests** (`zig build test-integration`): The test runner (`stage0/tests/runner.zig`) compiles `.dm` files from the `tests/` directory, executes them, and compares output against expected results. It handles temporary file creation and cleanup. Currently 304 tests pass, 0 fail, 0 skipped.

3. **dAImond test framework** (`daimond test <file.dm>`): Discovers `test_*` functions in source, compiles, and runs them with panic-catching (setjmp/longjmp). Tests use `assert(cond)` and `assert_eq(actual, expected)`.

**Always run `zig build test` after modifying any compiler source file.** Run `zig build test-integration` after changes that affect code generation or runtime behavior.

## Code Conventions

### Zig Style (compiler source)

- **Naming**: `camelCase` for variables and functions, `PascalCase` for types and enums
- **Documentation**: Module-level `//!` doc comments at top of each file; `///` for public declarations
- **Error handling**: Zig error unions (`!`, `try`, `catch`); no panic in library code
- **Memory**: All allocations go through `std.mem.Allocator` interface; arena allocators preferred
- **Section headers**: Use `// ====...` comment bars to separate logical sections within files
- **Imports**: Explicit field imports from modules (e.g., `const Lexer = @import("lexer.zig").Lexer`)

### dAImond Style (language source files)

- **Comments**: `--` for line comments (double dash, not `//`)
- **Module declaration**: Every file starts with `module <name>`
- **Immutability by default**: `let x = 42`, `let mut x = 0` for mutable
- **Functions**: `fn name(params) -> RetType { body }` or `fn name(params) -> RetType = expr`
- **Effects**: Declared with `with [Effect1, Effect2]` on function signatures
- **Regions**: `region name { ... }` for scoped memory management
- **Pattern matching**: `match value { Pattern => expr, ... }`
- **Error propagation**: `?` operator on Result types
- **Pipe operator**: `|>` for function chaining
- **File extension**: `.dm`

### AST Design Patterns

- Tagged unions (`union(enum)`) for all variant node types
- Source locations tracked via `Span` (line, column, byte offset)
- Pointer-based tree structure with allocator ownership
- Type interning via `TypeId` (u32 indices) for efficient comparison

### Type System

- Hindley-Milner style type inference with unification
- Generics via parametric polymorphism with `[T]` syntax
- Traits and `impl` blocks (Rust-style)
- Effect tracking with `EffectSet`
- Built-in types: `int`, `float`, `bool`, `string`, `void`
- Algebraic types: `struct`, `enum` (with variants), `Result[T, E]`, `Option[T]`, `Future[T]`

## C Runtime Library

Generated C code links against the runtime in `stage0/runtime/`. Key components:

- **`dm_string`**: Immutable string type with optional capacity
- **`dm_arena`**: Arena allocator for region-based memory management
- **`DM_DEFINE_OPTION` / `DM_DEFINE_RESULT`**: Macro-based generic option/result types
- **I/O**: `dm_print`, `dm_println`, `dm_read_line`, file operations
- **Math**: `DM_PI`, `DM_E`, `DM_MIN`, `DM_MAX`, `DM_CLAMP`, `DM_ABS`
- **Networking**: `dm_tcp_listen`, `dm_tcp_accept`, `dm_tcp_connect`, `dm_tcp_read`, `dm_tcp_write`, `dm_tcp_close` (BSD sockets)
- **Threading**: `dm_thread_spawn`, `dm_thread_join`, `dm_mutex_new`, `dm_mutex_lock`, `dm_mutex_unlock` (pthreads)

The runtime targets **C11** for portability. Networking requires POSIX sockets. Threading requires pthreads (`-lpthread`).

## Key Architecture Decisions

1. **Bootstrap strategy**: Stage 0 (Zig) -> Stage 1 (dAImond compiled by Stage 0) -> Stage 2 (self-compiled) -> Stage 3 (LLVM backend in Zig) -> Stage 4 (LLVM backend in dAImond, since deleted) -> Stage 5 (Stage 4 plus memory-safety checks). Stage 5 passes 290/290 integration tests and compiles itself, but Stage 0 is still required for its first build.

2. **C as intermediate target**: Rather than emitting native code, the compiler generates portable C11 and delegates optimization to mature C compilers.

3. **No external dependencies**: The Zig compiler uses only the Zig standard library. No package manager or external packages.

4. **Region-based memory**: No garbage collector. Memory safety via region inference and arena allocators.

## CLI Options Reference

| Command / Flag | Description |
|------|-------------|
| `daimond <file.dm>` | Compile to executable |
| `daimond run <file.dm>` | Compile and run |
| `daimond build <file.dm>` | Compile to C only |
| `daimond lex <file.dm>` | Show tokens |
| `daimond parse <file.dm>` | Show AST |
| `daimond check <file.dm>` | Type check only |
| `daimond fmt <file.dm>` | Format source code |
| `daimond test <file.dm>` | Run test_* functions |
| `daimond pkg init` | Create daimond.toml manifest |
| `daimond pkg add <name>` | Add a dependency |
| `daimond pkg list` | List dependencies |
| `daimond clean` | Remove compilation cache |
| `-o <file>` | Output file path |
| `-c` | Compile to C only (no binary) |
| `--emit-c` | Emit C code alongside binary |
| `--no-color` | Disable colored diagnostics |
| `-O0` to `-O3` | Optimization level (passed to C compiler) |
| `-v, --verbose` | Verbose output |
| `-h, --help` | Show help |
| `--version` | Show version (currently 0.1.0) |

## Common Development Tasks

### Adding a new language feature

1. Add token types in `lexer.zig` if new syntax is needed
2. Add AST node types in `ast.zig`
3. Add parsing logic in `parser.zig`
4. Add type-checking rules in `checker.zig` (with `types.zig` for new types)
5. Add C code generation in `codegen.zig`
6. Add unit tests in each modified module
7. Add integration test `.dm` file in `tests/`
8. Run `zig build test && zig build test-integration`

### Adding a new compiler diagnostic

1. Define the error code/message in `errors.zig`
2. Emit the diagnostic from the relevant pipeline stage (parser, checker, etc.)
3. Add a test case that triggers the new diagnostic

### Working on the C runtime

1. Add declarations to `daimond_runtime.h`
2. Implement in `daimond_runtime.c`
3. Add tests in `test_runtime.c`
4. Update `codegen.zig` to emit calls to the new runtime functions

## Current Status

### Implemented

**Language features**: Full type inference (Hindley-Milner), generics (monomorphization), closures with variable capture, traits (static + dynamic dispatch via `dyn Trait`), enums with payloads, Option/Result with `?` propagation, pattern matching, async/await (stackless coroutines), SIMD intrinsics (f32x4/f32x8/f64x2/f64x4/i32x4/i32x8/i64x2/i64x4), effect system (builtin + user-defined), regions (arena-based), comptime Turing-complete evaluation, operator overloading, string interpolation, FFI/extern, all numeric types (i8-i64, u8-u64, f32, f64) with `as` casts, pipeline operator `|>`, concurrency (threads, mutexes)

**Data structures**: List[T], Map[K,V], Box[T] with full method support, for-in iteration

**Tooling**: CLI (compile, run, lex, parse, check, fmt, test, pkg), LSP server, code formatter, test framework, package manager (TOML manifest, registry support)

**Runtime safety**: Division-by-zero protection, list bounds checking, arena OOM panic

**Standard library** (`stdlib/`): io, math, collections, string, os, fs, net, thread, test

**Compiler stages** (all complete):
- Stage 0 (Zig → C): Bootstrap compiler, 304/304 integration tests
- Stage 1 (dAImond → C): Self-hosting with fixed-point bootstrap
- Stage 3 (Zig + LLVM): Native binary output, full test parity
- Stage 5 (dAImond + LLVM): first built by Stage 0, 290/290 tests, 19/19 memory-safety probes. It compiles itself: the Stage 0-built and the self-built compilers emit byte-identical LLVM IR for Stage 5's source, and the self-built compiler also passes 290/290. Its link step still compiles the C runtime, wrappers and LLVM bridge with `cc`. Stage 5 alone supports generic structs (`struct Table[K, V]` with `impl Table[K, V]`), written with explicit type arguments such as `Table[string, int] { ... }`

### Not Yet Implemented
- Dynamic trait dispatch (`dyn Trait`) in Stage 1 (implemented in Stage 0, Stage 3, and Stage 5)
- Debug info (DWARF) in Stage 3/4
- Bootstrapping Stage 5 without Stage 0 (Zig) or C (runtime, wrappers and LLVM bridge in dAImond)

## Documentation Maintenance

**Always keep documentation in sync with code changes.** When making changes to the codebase, update the relevant documentation as part of the same commit or PR:

- **README.md**: Update the "Current Status" checklist when features are completed or new milestones are added. Update "Project Structure" if files or directories are added/removed. Update code examples if syntax changes.
- **CLAUDE.md**: Update the "Repository Structure" tree when files are added/removed. Update "Current Status" sections when features move between implemented and not-yet-implemented. Update "Build Commands", "CLI Options Reference", or "Code Conventions" if those change. Add new entries under "Common Development Tasks" for recurring workflows.
- **Inline docs**: Update module-level `//!` comments and `///` doc comments in Zig source when public APIs change.

Documentation that contradicts the code is worse than no documentation. When in doubt, check the source code -- it is always the ground truth.

## Language Builtins Reference

These built-in functions are available in dAImond programs across all compiler stages. In Stage 0 they are recognized by name in `checker.zig` and emitted in `codegen.zig`.

### I/O Functions
| Function | Signature | Description |
|----------|-----------|-------------|
| `print(s)` | `string -> void` | Print to stdout (no newline) |
| `println(s)` | `string -> void` | Print to stdout with newline |
| `eprint(s)` | `string -> void` | Print to stderr |
| `eprintln(s)` | `string -> void` | Print to stderr with newline |
| `panic(s)` | `string -> void` | Print error and exit |
| `exit(code)` | `int -> void` | Exit with code |

### String Functions
| Function | Signature | Description |
|----------|-----------|-------------|
| `len(s)` | `string -> int` | String length |
| `char_at(s, i)` | `(string, int) -> string` | Get single-char string at index |
| `substr(s, start, length)` | `(string, int, int) -> string` | Substring |
| `int_to_string(n)` | `int -> string` | Convert int to string |
| `float_to_string(f)` | `float -> string` | Convert float to string |
| `bool_to_string(b)` | `bool -> string` | Convert bool to string |
| `parse_int(s)` | `string -> int` | Parse string as int |
| `parse_float(s)` | `string -> float` | Parse string as float |
| `string_contains(s, sub)` | `(string, string) -> bool` | Check if contains substring |
| `string_find(s, sub)` | `(string, string) -> int` | Find index (-1 if not found) |
| `starts_with(s, prefix)` | `(string, string) -> bool` | Prefix check |
| `ends_with(s, suffix)` | `(string, string) -> bool` | Suffix check |
| `string_trim(s)` | `string -> string` | Trim whitespace |
| `string_replace(s, old, new)` | `(string, string, string) -> string` | Replace all occurrences |
| `string_to_upper(s)` | `string -> string` | Uppercase |
| `string_to_lower(s)` | `string -> string` | Lowercase |
| `string_split(s, delim)` | `(string, string) -> List[string]` | Split string by delimiter |

### File I/O
| Function | Signature | Description |
|----------|-----------|-------------|
| `file_read(path)` | `string -> string` | Read entire file |
| `file_write(path, content)` | `(string, string) -> void` | Write file |
| `file_append(path, content)` | `(string, string) -> void` | Append to file |
| `file_exists(path)` | `string -> bool` | Check if file exists |
| `read_line()` | `-> string` | Read line from stdin |

### Command Line
| Function | Signature | Description |
|----------|-----------|-------------|
| `args_len()` | `-> int` | Number of CLI arguments |
| `args_get(i)` | `int -> string` | Get argument at index |
| `system(cmd)` | `string -> int` | Run shell command, return exit code |

### Box (Heap Allocation)
| Function | Signature | Description |
|----------|-----------|-------------|
| `Box_new(value)` | `T -> Box[T]` | Heap-allocate a value |
| `Box_null()` | `-> Box[T]` | Null pointer (type inferred) |

### Testing
| Function | Signature | Description |
|----------|-----------|-------------|
| `assert(cond)` | `bool -> void` | Panic if condition is false |
| `assert_eq(a, b)` | `(T, T) -> void` | Panic if values not equal |

### SIMD Functions
| Function | Signature | Description |
|----------|-----------|-------------|
| `simd_splat_TYPE(val)` | `scalar -> TYPE` | Create vector with all lanes set to val |
| `simd_set_TYPE(a, b, ...)` | `(scalar, ...) -> TYPE` | Create vector from individual values |
| `simd_add(a, b)` | `(TYPE, TYPE) -> TYPE` | Element-wise addition |
| `simd_sub(a, b)` | `(TYPE, TYPE) -> TYPE` | Element-wise subtraction |
| `simd_mul(a, b)` | `(TYPE, TYPE) -> TYPE` | Element-wise multiplication |
| `simd_div(a, b)` | `(TYPE, TYPE) -> TYPE` | Element-wise division |
| `simd_extract(vec, idx)` | `(TYPE, int) -> scalar` | Extract element at index |

Where TYPE is one of: `f32x4`, `f32x8`, `f64x2`, `f64x4`, `i32x4`, `i32x8`, `i64x2`, `i64x4`

### String Operators
- `+` concatenates strings: `"hello" + " " + "world"`
- `==` compares strings by value (uses `dm_string_eq`)
- `>=`, `<=`, `>`, `<` compare strings lexicographically (uses `dm_string_cmp`)

### List Methods
- `let mut l: List[T] = []` — empty list (type annotation required)
- `l.push(item)` — append
- `l.pop()` — remove and return last
- `l.len()` — get length as int
- `l[i]` — index access
- `for item in list { ... }` — iterate

### Map Methods
- `let mut m: Map[K,V] = Map_new()` — empty map (type annotation required)
- `m.insert(key, value)` — insert or update
- `m.get(key)` — get value (panics if missing)
- `m.contains(key)` — check if key exists
- `m.remove(key)` — remove key
- `m.len()` — get number of entries
- `m.keys()` — get List[K] of all keys
- `m.values()` — get List[V] of all values
- `m.set(key, value)` — alias for insert
- `m[key]` — index access (alias for get)
- `m[key] = val` — index assignment (alias for insert)

In Stage 5, `Map[K, V]` is the generic struct in `stage5/runtime/map.dm`. Keys must be strings or integers, `keys()`/`values()` and `for k in m` follow insertion order, and `Map_new()` needs a destination type (a typed `let`, a struct field, an assignment target or a return). Copying a map copies its list headers, so the copy shares storage with the original, as Stage 5 lists do. Stage 5 also has the builtin `byte_at(s, i) -> int` (the byte at index `i`, bounds-checked).

### Stage 5 Runtime Primitives

Stage 5 alone has the low-level features the runtime is being rewritten with (Stage 0 has none of them). The builtins below need an `unsafe` block. Addresses are `int`s.

| Builtin | Description |
|---------|-------------|
| `load_T(addr)` | Read a `T` at `addr`, where `T` is `u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `i64`, `f32` or `f64`. Integers widen to `int` (zero- or sign-extended), floats to `float` |
| `store_T(addr, v)` | Write `v` at `addr` as a `T` (narrowing it) |
| `addr_of(place)` | Address of a variable, field or list element |
| `size_of[T]()` | Size of type `T` in bytes |
| `string_data(s)` | Address of a string's bytes (not NUL-terminated unless `s` is a literal) |
| `string_from_raw(ptr, len, cap)` | The string at `ptr`; `cap` 0 means the string does not own the memory |

- **C-width integers**: integers of different types in one expression meet at one type. A literal takes the other operand's type (`fd < 0` with `fd: i32`), otherwise the narrower operand widens. `u8`/`u16`/`u32`/`u64` compare, divide and take remainders unsigned. Returning or assigning a wider integer to a narrower one is a compile error that asks for `as`, and a literal that doesn't fit its type is an error.
- **Variadic externs**: `extern fn printf(fmt: string, ...) -> i32`. The C function is called directly: string arguments pass as `char*`, and extra arguments get C's default promotions (`f32` to double; `bool`, `i8` and `i16` to `i32`). A variadic extern can't return `string`.
- A source `extern fn` may redeclare a C function the compiler also uses, such as `extern fn malloc(n: int) -> int`; calls convert between pointer and `int` where the two declarations differ.
- **Link names**: `extern fn my_len(s: string) -> u64 = "strlen"` binds the extern to the C symbol `strlen`. Several externs may name one C symbol; each call uses its own declaration's types.

### Stage 5 Runtime in dAImond

The C runtime is being replaced one function at a time by dAImond code in `stage5/runtime/*.dm`, which `gen_bootstrap.sh` embeds in the compiler (`prelude.dm`) and the compiler adds to every program. Only Stage 5 parses these files, so they may use Stage 5-only features.

- A dAImond function `rt_X` replaces the C runtime function `dm_X`: the compiler binds every call to `dm_X` to `rt_X` (see RUNTIME FUNCTIONS in `llvm_gen.dm`). `rt_X` must keep the signature `dm_X` is declared with in `ir_gen_decl.dm`; a mismatch is a compile error.
- To port a function: write `rt_X` in a runtime file, delete `llvm_dm_X` from `runtime/llvm_wrappers.c`, and run `gen_bootstrap.sh`. A new runtime file also needs a line in `gen_bootstrap.sh`.
- Ported so far (`runtime/string.dm`): `string_concat`, `string_eq`, `string_cmp`, `char_at`, `substr`. `len(s)` and string literals are inline and call nothing.
- Runtime code calls libc through link-named externs (`libc_malloc`, `libc_memcpy`, `libc_memcmp`), so a program's own `extern fn malloc` cannot collide with it. It must not use the operators it implements (no `+`, `==` or `<` on strings in `string.dm`).
- Every string the runtime builds is followed by a NUL byte, so its data can go to C as a `char*`.
- `substr` with a negative start or length returns `""` in Stage 5 (the C runtime, and so Stage 0, crashes on a negative length).

## Stage 0 Codegen Quirks and Workarounds

These are remaining known issues in Stage 0's C code generation:

### 1. `Box.new()` / `Box.null()` vs `Box_new()` / `Box_null()`
**Problem**: Method-call syntax `Box.new(val)` generates wrong C code. The codegen expects function-call syntax.

**Workaround**: Always use `Box_new(value)` and `Box_null()` (underscore, not dot).

### 2. `string.len()` Method Call Syntax
**Problem**: The `.len()` method on strings generates `dm_string_len()` which returns `size_t` instead of `int64_t`, causing type mismatches in some contexts.

**Workaround**: Use the free function `len(s)` instead of `s.len()` for consistent `int` return type.

### 3. `is_alpha()` / `is_digit()` Expect `char`, Not `string`
**Problem**: These runtime functions take a C `char`, but dAImond passes `dm_string` from string literals.

**Workaround**: Write your own helpers that compare single-char strings:
```daimond
fn is_alpha_char(ch: string) -> bool {
    if ch >= "a" and ch <= "z" { return true }
    if ch >= "A" and ch <= "Z" { return true }
    if ch == "_" { return true }
    return false
}
```

## Stage 1 & Stage 3 (Archived)

Stage 1 (dAImond → C self-hosting) and Stage 3 (LLVM backend in Zig) are complete and frozen. Stage 1 achieved bootstrap fixed-point. Stage 3 achieved 254/254 test parity. Both are superseded by Stage 5. See `docs/stage-history.md` for detailed architecture, module layout, feature lists, and build commands.

## Stage 5 Compiler Architecture

Stage 5 is the LLVM backend rewritten entirely in dAImond. It is compiled by Stage 0 (to C), then gcc. It produces native binaries by calling LLVM-C API functions via extern FFI through `llvm_bridge.c`.

### Design Decisions
- **Same architecture as Stage 3**: Lexer → Parser → AST → dAImond IR (SSA) → LLVM IR → native binary. The pipeline mirrors Stage 3 but is implemented in dAImond instead of Zig.
- **Integer kind tags**: Like Stage 1, uses `fn EXPR_FUNCTION_CALL() -> int { return 40 }` constants instead of Zig tagged unions.
- **No type checker**: Relies on the C compiler (via Stage 0 compilation) to catch type errors in the compiler itself, and on runtime error detection for compiled programs.
- **LLVM-C FFI bridge**: All LLVM API calls go through `llvm_bridge.c` which is declared as `extern fn` in dAImond. This bridges dAImond's `dm_string` type to C's `const char*` for LLVM API calls.
- **Modular source, monolithic bootstrap**: Same pattern as Stage 1 — split `.dm` module files for development, `gen_bootstrap.sh` concatenates into `main.dm` for compilation by Stage 0.

### Module Layout
```
token.dm           — Token kinds, Token struct, keyword lookup
lexer.dm           — Lexer struct, tokenize, character helpers
parser.dm          — Recursive descent + Pratt parser
ast.dm             — AST node types (expressions, statements, declarations)
ir.dm              — dAImond IR definitions (SSA instructions, basic blocks)
ir_builder.dm      — IR builder (instruction emission, block management)
ir_gen.dm          — IR generation entry point, struct/enum registration
ir_gen_expr.dm     — Expression IR generation (largest module, ~4.7K lines)
ir_gen_stmt.dm     — Statement IR generation (let, if, for, while, match, region)
ir_gen_decl.dm     — Declaration IR generation (functions, impl blocks)
ir_gen_builtins.dm — Builtin function IR generation (println, file_read, etc.)
ir_gen_comptime.dm — Comptime evaluation (arithmetic, variables, functions)
llvm.dm            — LLVM type definitions and helpers
llvm_gen.dm        — LLVM IR generation from dAImond IR (main module)
llvm_gen_call.dm   — LLVM call instruction generation
llvm_gen_simd.dm   — LLVM SIMD instruction generation
llvm_bridge.c      — C bridge: extern fn declarations for LLVM-C API
prelude.dm         — Generated: runtime/string.dm and runtime/map.dm embedded as a string; added to every program
runtime/string.dm  — String core: rt_string_concat, rt_string_eq, rt_string_cmp, rt_char_at, rt_substr
runtime/map.dm     — Map[K, V] as a generic struct (hash table over List fields)
main_split.dm      — Entry point with imports (for modular development)
main.dm            — Monolithic bootstrap file (~21.7K lines, generated)
```

### Error Detection
Stage 5 includes compile-time error detection for:
- **Unterminated strings**: Lexer detects EOF inside string literals and exits with error
- **Syntax errors**: Post-parse AST validation catches malformed function declarations (e.g., missing parameter types)
- **Undefined functions**: IR generation checks a `known_functions` map before generating calls
- **Effect violations**: Builtin-to-effect mapping (e.g., `file_read` → `FileSystem`) with callee effect subset checking ensures functions only call operations permitted by their `with [...]` declarations

### Building Stage 5
```bash
# Stage 0 compiles Stage 5 (from monolithic main.dm)
cd stage0 && ./zig-out/bin/daimond ../stage5/main.dm -c -o /tmp/stage5.c

# Compile C output + LLVM bridge + runtime to binary
gcc -c /tmp/stage5.c -o /tmp/stage5.o -I runtime -Wno-incompatible-pointer-types -Wno-int-conversion
gcc -c ../stage5/llvm_bridge.c -o /tmp/llvm_bridge.o $(llvm-config --cflags)
gcc -c ../stage5/runtime/llvm_wrappers.c -o /tmp/llvm_wrappers.o -I runtime
gcc -c runtime/daimond_runtime.c -o /tmp/daimond_runtime.o
gcc /tmp/stage5.o /tmp/llvm_bridge.o /tmp/llvm_wrappers.o /tmp/daimond_runtime.o \
    -o /tmp/stage5_bin -lm -lpthread $(llvm-config --libs --ldflags)

# Stage 5 compiles a test program
/tmp/stage5_bin test_program.dm -o test_program

# Regenerate monolithic main.dm from modules
cd stage5 && bash gen_bootstrap.sh
```

### Full Bootstrap Chain
```
Stage 0 (Zig) compiles Stage 1 (dAImond→C)     → stage1_compiler
stage1_compiler self-compiles                     → stage1_v2 (fixed point ✓)
Stage 0 (Zig) compiles Stage 5 (dAImond→LLVM)   → stage5_bin
stage5_bin compiles test programs                 → native binaries (290/290 tests pass)
stage5_bin compiles Stage 5                       → stage5_self (same LLVM IR ✓)
```

## Important Notes

- The `.gitignore` in `stage0/` excludes `*.c` files (generated output) but includes `!src/*.c` to allow runtime C source files.
- Zig version is pinned to 0.13.0 via `.mise.toml`. Do not upgrade without testing compatibility.
- The largest source files are `codegen.zig` (~6830 lines), `parser.zig` (~3630 lines), and `checker.zig` (~3116 lines). Changes to these require careful testing.
- Stage 5's `main.dm` is ~21.7K lines (the largest dAImond source file). The monolithic file is generated by `gen_bootstrap.sh` — edit the split module files for development.
