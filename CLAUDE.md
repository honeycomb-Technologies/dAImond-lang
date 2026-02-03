# CLAUDE.md - AI Assistant Guide for dAImond-lang

## Project Overview

dAImond is a systems programming language designed to be the fastest compiled language, the most AI-generatable language, self-compiling, memory-safe without garbage collection (region-based with full inference), and fully Turing-complete at compile time.

The repository contains the **Stage 0 bootstrap compiler**, written in Zig, which compiles dAImond source code to C11. A system C compiler (gcc/clang) then produces the final binary.

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
│   │   └── errors.zig     # Diagnostic reporting, colored output, error codes
│   ├── runtime/           # C runtime library linked into compiled programs
│   │   ├── daimond_runtime.h   # Runtime API (strings, arenas, option/result, I/O)
│   │   ├── daimond_runtime.c   # Runtime implementation
│   │   └── test_runtime.c      # Runtime unit tests
│   └── tests/
│       └── runner.zig     # Integration test harness
├── stage1/                # Stage 1 self-hosting compiler (dAImond)
│   ├── main.dm            # CLI entry, pipeline (single-file compiler)
│   ├── token.dm           # Token kind constants and Token struct
│   ├── lexer.dm           # Tokenizer (reusable module)
│   ├── ast.dm             # AST node structs and constructors
│   └── test_lexer.dm      # Lexer standalone test
├── examples/              # Example dAImond programs
│   ├── hello.dm           # Hello World
│   ├── arithmetic.dm      # Basic math operations
│   ├── fibonacci.dm       # Recursive functions
│   └── calculator.dm      # Full scientific calculator demo
└── tests/                 # Integration test programs (.dm files)
    ├── arithmetic.dm      # Arithmetic validation
    ├── structs.dm         # Struct/enum/pattern matching tests
    └── generics.dm        # Generic functions and traits tests
```

## Compiler Pipeline

```
Source (.dm) -> Lexer -> Tokens -> Parser -> AST -> Checker -> Typed AST -> Codegen -> C Code -> cc -> Binary
```

Each stage maps to a source file: `lexer.zig` -> `parser.zig` -> `ast.zig`/`types.zig` -> `checker.zig` -> `codegen.zig`.

## Build Commands

All build commands run from the `stage0/` directory. **Zig 0.13.0** is required (pinned in `.mise.toml`).

```bash
# Build the compiler
cd stage0 && zig build

# Run unit tests (all modules: lexer, parser, ast, types, checker, codegen, errors)
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
```

## Testing Strategy

There are two tiers of tests:

1. **Unit tests** (`zig build test`): Each Zig source module contains inline `test` blocks. All seven modules have tests registered in `build.zig`: lexer, errors, ast, types, parser, codegen, checker.

2. **Integration tests** (`zig build test-integration`): The test runner (`stage0/tests/runner.zig`) compiles `.dm` files from the `tests/` directory, executes them, and compares output against expected results. It handles temporary file creation and cleanup.

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
- Algebraic types: `struct`, `enum` (with variants), `Result[T, E]`, `Option[T]`

## C Runtime Library

Generated C code links against the runtime in `stage0/runtime/`. Key components:

- **`dm_string`**: Immutable string type with optional capacity
- **`dm_arena`**: Arena allocator for region-based memory management
- **`DM_DEFINE_OPTION` / `DM_DEFINE_RESULT`**: Macro-based generic option/result types
- **I/O**: `dm_print`, `dm_println`, `dm_read_line`, file operations
- **Math**: `DM_PI`, `DM_E`, `DM_MIN`, `DM_MAX`, `DM_CLAMP`, `DM_ABS`

The runtime targets **C11** for portability.

## Key Architecture Decisions

1. **Bootstrap strategy**: Stage 0 (Zig) -> Stage 1 (dAImond compiled by Stage 0) -> Stage 2 (self-compiled) -> Stage 3 (LLVM backend). Stage 0 is complete; Stage 1 is in progress.

2. **C as intermediate target**: Rather than emitting native code, the compiler generates portable C11 and delegates optimization to mature C compilers.

3. **No external dependencies**: The Zig compiler uses only the Zig standard library. No package manager or external packages.

4. **Region-based memory**: No garbage collector. Memory safety via region inference and arena allocators.

## CLI Options Reference

| Flag | Description |
|------|-------------|
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
- Lexer with comprehensive token support
- Recursive descent + Pratt parser
- AST definitions for all language constructs
- Type system with inference and unification
- Type checker with symbol tables and scope management
- C11 code generator
- Error diagnostics with colored output
- C runtime library (strings, arenas, option/result, I/O)
- CLI with multiple commands (compile, run, lex, parse, check)
- Integration test harness

### In Progress
- Stage 1 compiler (dAImond self-hosting) — lexer and AST complete, parser and codegen in development

### Not Yet Implemented
- `fmt` command (code formatter)
- Standard library (`stdlib/`)
- LLVM backend
- Package management
- Module system refinements
- Multi-file imports for user programs

## Documentation Maintenance

**Always keep documentation in sync with code changes.** When making changes to the codebase, update the relevant documentation as part of the same commit or PR:

- **README.md**: Update the "Current Status" checklist when features are completed or new milestones are added. Update "Project Structure" if files or directories are added/removed. Update code examples if syntax changes.
- **CLAUDE.md**: Update the "Repository Structure" tree when files are added/removed. Update "Current Status" sections when features move between implemented and not-yet-implemented. Update "Build Commands", "CLI Options Reference", or "Code Conventions" if those change. Add new entries under "Common Development Tasks" for recurring workflows.
- **Inline docs**: Update module-level `//!` comments and `///` doc comments in Zig source when public APIs change.

Documentation that contradicts the code is worse than no documentation. When in doubt, check the source code -- it is always the ground truth.

## Stage 0 Known Working Builtins

These are the built-in functions available in dAImond programs compiled by Stage 0. They are recognized by name in `checker.zig` and emitted directly in `codegen.zig`.

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

### File I/O
| Function | Signature | Description |
|----------|-----------|-------------|
| `file_read(path)` | `string -> string` | Read entire file |
| `file_write(path, content)` | `(string, string) -> void` | Write file |

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

## Stage 0 Codegen Quirks and Workarounds

These are known issues in Stage 0's C code generation that affect how dAImond programs should be written:

### 1. Enum Payload Construction/Matching is Broken
**Problem**: `Expr.IntLit(42)` generates incorrect C like `dm_int64_t_IntLit(&(Expr), 42)` instead of `dm_Expr_IntLit(42)`. Pattern matching on enum variants with payloads generates wrong tag comparisons.

**Workaround**: Use struct-based nodes with integer kind tags instead of enum variants:
```daimond
-- DO NOT use:
-- enum Expr { IntLit(int), Binary(BinaryExpr) }
-- let e = Expr.IntLit(42)

-- Instead use:
fn EXPR_INT_LIT() -> int { return 1 }
struct Expr { kind: int, int_val: int, ... }
let mut e = null_expr()
e.kind = EXPR_INT_LIT()
e.int_val = 42
```

### 2. Nested `if` Without `else` Returns Wrong Type
**Problem**: An `if` block without `else` that's the last statement in another `if` gets generated as a ternary expression, causing type mismatches.

**Workaround**: Flatten nested conditions into a single `and` chain, or add explicit `else` branches:
```daimond
-- DO NOT use:
-- if condition1 {
--     if condition2 {
--         do_something()
--     }
-- }

-- Instead use:
if condition1 and condition2 {
    do_something()
}
-- Or use a flag:
let mut should_do = false
if condition1 and condition2 {
    should_do = true
}
if should_do {
    do_something()
}
```

### 3. `Box.new()` / `Box.null()` vs `Box_new()` / `Box_null()`
**Problem**: Method-call syntax `Box.new(val)` generates wrong C code. The codegen expects function-call syntax.

**Workaround**: Always use `Box_new(value)` and `Box_null()` (underscore, not dot).

### 4. `string.len()` Not Available
**Problem**: The `.len()` method on strings generates `dm_string_len()` which is not defined in the runtime.

**Workaround**: Use the free function `len(s)` instead of `s.len()`.

### 5. `is_alpha()` / `is_digit()` Expect `char`, Not `string`
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

### 6. Simple Enums (No Payloads) Work Fine
Simple enums without payloads compile correctly to C enums.

### 7. `string_split()` Returns Incompatible Type
**Problem**: `string_split()` returns a `dm_split_result` struct, not a `List[string]`. The `.len()` method and `for` iteration don't work on it.

**Workaround**: Avoid `string_split()`. Parse strings manually with `char_at()` and `substr()`.

## Stage 1 Compiler Architecture

Stage 1 is written in dAImond and compiled by Stage 0. It compiles a subset of dAImond to C11, sufficient to compile itself (bootstrap proof).

### Design Decisions
- **Single-file compiler**: Since Stage 0 doesn't support multi-file imports reliably, Stage 1's `main.dm` contains all code (lexer, parser, codegen) inlined.
- **Struct-based AST**: Uses integer kind tags instead of enum payloads to work around codegen bugs.
- **No type checker**: Generates C directly from AST; the C compiler catches type errors.
- **String-based codegen**: Builds C code via string concatenation.
- **Subset compiler**: Only supports features used within Stage 1 itself.

### Building Stage 1
```bash
# Stage 0 compiles Stage 1
cd stage0 && ./zig-out/bin/daimond compile ../stage1/main.dm -o stage1_compiler

# Stage 1 compiles a test program
./stage1_compiler test_program.dm

# Run the result
./test_program
```

## Important Notes

- The `.gitignore` in `stage0/` excludes `*.c` files (generated output) but includes `!src/*.c` to allow runtime C source files.
- Zig version is pinned to 0.13.0 via `.mise.toml`. Do not upgrade without testing compatibility.
- The largest source files are `codegen.zig` and `parser.zig` (each ~119KB). Changes to these require careful testing.
