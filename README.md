# dAImond Programming Language

**The end-all language that AI builds with.**

dAImond is a systems programming language designed to be:
- **The fastest compiled language** (beating C and Rust)
- **The most AI-generatable language** (unambiguous, verifiable, composable)
- **Self-compiling** (compiler written in dAImond compiles itself)
- **Memory-safe without garbage collection** (region-based with full inference)
- **Fully Turing-complete at compile time**

## Quick Start

### Prerequisites

- [Zig](https://ziglang.org/) 0.13.0 or later

### Building

```bash
cd stage0
zig build
```

### Running

```bash
# Compile a dAImond file
./zig-out/bin/daimond examples/hello.dm

# Compile and run in one step
./zig-out/bin/daimond run examples/hello.dm

# Format source code
./zig-out/bin/daimond fmt myfile.dm

# Run tests (discovers fn test_*() functions)
./zig-out/bin/daimond test myfile.dm

# Package management
./zig-out/bin/daimond pkg init
./zig-out/bin/daimond pkg add mylib 1.0.0
./zig-out/bin/daimond pkg list

# View tokens (for debugging)
./zig-out/bin/daimond lex examples/hello.dm

# View AST (for debugging)
./zig-out/bin/daimond parse examples/hello.dm

# Type check only
./zig-out/bin/daimond check examples/hello.dm

# Clean compilation cache
./zig-out/bin/daimond clean
```

### Running Tests

```bash
cd stage0

# Unit tests (all compiler modules)
zig build test

# Integration tests (compiles and runs .dm test programs)
zig build test-integration
```

### Stage 3 (LLVM Backend)

Requires **LLVM 17+** development libraries.

```bash
cd stage3

# Build the LLVM compiler
zig build

# Compile a dAImond program to native binary
./zig-out/bin/daimond-llvm examples/hello.dm -o hello

# Compile with optimization
./zig-out/bin/daimond-llvm myfile.dm -o output -O2

# Run integration tests (254 tests, same as Stage 0)
zig build test-integration
```

## Project Structure

```
dAImond-lang/
├── stage0/                    # Stage 0 bootstrap compiler (written in Zig)
│   ├── build.zig              # Zig build configuration
│   ├── .mise.toml             # Tool version pinning (Zig 0.13.0)
│   ├── src/
│   │   ├── main.zig           # CLI entry point and pipeline orchestration
│   │   ├── lexer.zig          # Lexical analyzer
│   │   ├── parser.zig         # Recursive descent + Pratt parser
│   │   ├── ast.zig            # AST node type definitions
│   │   ├── types.zig          # Type system, inference, effects
│   │   ├── checker.zig        # Type checker, symbol tables, unification
│   │   ├── codegen.zig        # C11 code generator
│   │   ├── errors.zig         # Diagnostic reporting, colored output
│   │   ├── package.zig        # Package manager (TOML manifest, dependencies)
│   │   ├── cache.zig          # Compilation cache (SHA-256 content hashing)
│   │   └── lsp.zig            # Language Server Protocol implementation
│   ├── runtime/               # C runtime library
│   │   ├── daimond_runtime.h  # Runtime API (strings, arenas, I/O, sockets, threads)
│   │   ├── daimond_runtime.c  # Runtime implementation
│   │   └── test_runtime.c     # Runtime unit tests
│   └── tests/
│       └── runner.zig         # Integration test harness (254 tests)
├── stage1/                    # Stage 1 self-hosting compiler (written in dAImond)
│   ├── main.dm                # Monolithic bootstrap file (generated)
│   ├── main_split.dm          # Entry point with imports (for self-compilation)
│   ├── token.dm               # Token kinds, Token struct, keyword lookup
│   ├── lexer.dm               # Lexer struct, tokenize, character helpers
│   ├── compiler.dm            # Compiler struct, parser helpers, type mapping
│   ├── compile_expr.dm        # Expression compiler, lambdas, generics
│   ├── compile_stmt.dm        # Statement compiler, type tracking/inference
│   ├── compile_match.dm       # Match expressions and statements
│   ├── compile_decl.dm        # Function/struct/enum/impl declarations
│   ├── runtime.dm             # C runtime emission, output assembly
│   ├── imports.dm             # Multi-file import resolver
│   └── gen_bootstrap.sh       # Concatenates modules into main.dm
├── stdlib/                    # Standard library modules
│   ├── io.dm                  # I/O (print, file operations, read_line)
│   ├── math.dm                # Math (sin, cos, sqrt, pow, PI, E)
│   ├── collections.dm         # Set, Queue, Stack wrappers
│   ├── string.dm              # String utilities (split, join, repeat, etc.)
│   ├── os.dm                  # OS functions (env, cwd, exit, args)
│   ├── fs.dm                  # Filesystem (mkdir, readdir, rename, etc.)
│   ├── net.dm                 # Networking (TCP sockets)
│   ├── thread.dm              # Concurrency (spawn, join, mutex)
│   └── test.dm                # Testing framework docs
├── examples/                  # Example programs
│   ├── hello.dm               # Hello World
│   ├── arithmetic.dm          # Basic math operations
│   ├── fibonacci.dm           # Recursive functions
│   └── calculator.dm          # Scientific calculator
├── stage3/                    # Stage 3 LLVM backend (Zig, full test parity with Stage 0)
│   ├── build.zig              # Zig build configuration (links LLVM-C)
│   ├── src/
│   │   ├── main.zig           # CLI entry point, pipeline orchestration
│   │   ├── ir.zig             # dAImond IR definitions (SSA types, instructions)
│   │   ├── ir_gen.zig         # IR generation from typed AST
│   │   ├── llvm_gen.zig       # LLVM IR generation from dAImond IR
│   │   └── llvm_bindings.zig  # Safe Zig wrappers for LLVM-C API
│   ├── runtime/
│   │   └── llvm_wrappers.c    # ABI wrappers for string-passing conventions
│   └── tests/
│       └── runner.zig         # Integration test harness (254 tests)
├── stage4/                    # Stage 4 LLVM backend (written in dAImond, no Zig dependency)
│   ├── main.dm                # Monolithic bootstrap file (18.7K lines, generated)
│   ├── main_split.dm          # Entry point with imports (for modular development)
│   ├── token.dm               # Token kinds, Token struct, keyword lookup
│   ├── lexer.dm               # Lexer struct, tokenize, character helpers
│   ├── parser.dm              # Recursive descent + Pratt parser
│   ├── ast.dm                 # AST node type definitions
│   ├── ir.dm                  # dAImond IR definitions (SSA types, instructions)
│   ├── ir_builder.dm          # IR builder (basic blocks, instructions)
│   ├── ir_gen.dm              # IR generation entry point, module/struct/enum
│   ├── ir_gen_expr.dm         # Expression IR generation
│   ├── ir_gen_stmt.dm         # Statement IR generation
│   ├── ir_gen_decl.dm         # Declaration IR generation (functions, impls)
│   ├── ir_gen_builtins.dm     # Builtin function IR generation
│   ├── ir_gen_comptime.dm     # Comptime evaluation
│   ├── llvm.dm                # LLVM type definitions and helpers
│   ├── llvm_gen.dm            # LLVM IR generation from dAImond IR
│   ├── llvm_gen_call.dm       # LLVM call instruction generation
│   ├── llvm_gen_simd.dm       # LLVM SIMD instruction generation
│   ├── llvm_bridge.c          # C bridge to LLVM-C API
│   ├── runtime/
│   │   └── llvm_wrappers.c    # ABI wrappers for string-passing conventions
│   └── gen_bootstrap.sh       # Concatenates modules into main.dm
├── docs/                      # Design documents
│   ├── ir-spec.md             # dAImond IR specification (SSA-based, typed)
│   └── llvm-backend.md        # Stage 3 LLVM backend architecture
└── tests/                     # Integration test programs (.dm files)
```

## Language Overview

### Hello World

```dm
module hello

fn main() {
    println("Hello, dAImond!")
}
```

### Variables

```dm
-- Immutable by default
let x = 42
let y: int = 42          -- explicit type

-- Mutable requires keyword
let mut counter = 0
counter = counter + 1

-- Constants (must be comptime-known)
const MAX_SIZE = 1024
```

### Functions

```dm
fn add(a: int, b: int) -> int {
    return a + b
}

-- Single expression (implicit return)
fn double(x: int) -> int = x * 2

-- Generic function
fn max[T](a: T, b: T) -> T {
    if a > b { return a }
    return b
}
```

### String Interpolation

```dm
let name = "world"
let age = 42
println(f"Hello {name}, you are {age} years old")
```

### Structs and Enums

```dm
struct Point {
    x: float,
    y: float,
}

impl Point {
    fn magnitude(self) -> float {           -- bare self (type inferred from impl block)
        return sqrt(self.x * self.x + self.y * self.y)
    }
    fn translate(self: Point, dx: float, dy: float) -> Point {  -- explicit self type also works
        return Point { x: self.x + dx, y: self.y + dy }
    }
}

enum Shape {
    Circle(float),
    Rect(float, float),
    Point,
}
```

### Traits

```dm
trait Display {
    fn show(self) -> string
}

impl Display for Point {
    fn show(self) -> string {
        return f"({self.x}, {self.y})"
    }
}

-- Static dispatch with trait bounds
fn print_any[T: Display](x: T) {
    println(x.show())
}

-- Dynamic dispatch
let d: dyn Display = my_point
println(d.show())
```

### Pattern Matching

```dm
match value {
    Some(x) => use(x),
    None => handle_missing(),
}

match shape {
    Shape.Circle(r) => println(f"Circle with radius {r}"),
    Shape.Rect(w, h) => println(f"Rectangle {w}x{h}"),
    Shape.Point => println("Just a point"),
}
```

### Error Handling

```dm
fn parse_config(path: string) -> Result[Config, string] {
    let content = file_read(path)
    let val = parse_int(content)?  -- propagate error with ?
    return Ok(Config { value: val })
}
```

### Closures

```dm
let x = 10
let add_x = |y: int| x + y   -- captures x from outer scope
println(int_to_string(add_x(5)))  -- prints 15
```

### Operator Overloading

```dm
struct Vec2 { x: int, y: int }

impl Vec2 {
    fn add(self, other: Vec2) -> Vec2 {
        return Vec2 { x: self.x + other.x, y: self.y + other.y }
    }
}

let v = v1 + v2  -- calls Vec2.add
```

### Regions (Memory Management)

```dm
region scratch {
    let big_buffer = alloc[byte](1_000_000)
    process_in_buffer(big_buffer)
}   -- big_buffer freed here, all at once, O(1)
```

### Async/Await

```dm
async fn compute(x: int, y: int) -> int {
    return x + y
}

fn main() {
    let result = await compute(10, 20)
    println(int_to_string(result))  -- prints 30
}
```

### Effects

```dm
fn read_file(path: string) -> string with [IO, FileSystem] {
    return file_read(path)
}

fn pure_add(a: int, b: int) -> int {
    -- no effects needed for pure computation
    return a + b
}
```

### Imports

```dm
module myapp

import helpers          -- imports helpers.dm from same directory
import std.math         -- imports stdlib/math.dm
```

### FFI (Foreign Function Interface)

```dm
extern fn sqrt(x: float) -> float
extern fn puts(s: string) -> int

fn main() {
    let val = sqrt(2.0)
    puts(f"sqrt(2) = {val}")
}
```

### Testing

```dm
module my_tests

fn test_addition() {
    assert_eq(2 + 3, 5)
}

fn test_strings() {
    let s = "hello" + " world"
    assert_eq(s, "hello world")
}

fn main() {
    println("Run with: daimond test my_tests.dm")
}
```

```bash
$ daimond test my_tests.dm
Found 2 test(s) in my_tests.dm

  test test_addition ... PASS
  test test_strings ... PASS

2 passed, 0 failed
```

## Bootstrap Path

```
Stage 0 (Zig) -> Stage 1 (dAImond) -> Stage 2 (Self-compiled) -> Stage 3 (LLVM/Zig) -> Stage 4 (LLVM/dAImond)
```

1. **Stage 0** (Complete): Hand-written compiler in Zig, compiles dAImond -> C
2. **Stage 1** (Complete): Compiler rewritten in dAImond, compiled by Stage 0
3. **Stage 2** (Complete): Stage 1 compiles itself -- fixed-point bootstrap verified (Stage 1 output = Stage 2 output)
4. **Stage 3** (Complete): LLVM backend in Zig for optimized native code -- full test parity with Stage 0 (254/254 tests pass), compiles Stage 1 with verified fixed-point bootstrap
5. **Stage 4** (Complete): LLVM backend rewritten entirely in dAImond -- full self-hosting without Zig dependency, 254/254 tests pass

## Current Status

### Stage 0 Compiler -- Complete
- [x] Lexer, parser, AST, type system, type checker, C11 code generator
- [x] Error diagnostics with colored output
- [x] C runtime library (strings, arenas, option/result, I/O, networking, threading)
- [x] CLI: compile, run, lex, parse, check, fmt, test, pkg
- [x] 254 integration tests passing, 0 failing, 0 skipped

### Language Features -- Complete
- [x] Map[K,V] with full method support (insert, get, contains, remove, len, keys, values, indexing)
- [x] Multi-file imports with stdlib path resolution and diamond import deduplication
- [x] String interpolation (`f"Hello {name}"`)
- [x] FFI / extern function declarations
- [x] All numeric types (i8, i16, i32, u8, u16, u32, i64, u64, f32, f64) with `as` casts
- [x] Trait static dispatch with trait bounds on generic functions
- [x] Dynamic trait dispatch (`dyn Trait` with vtable-based fat pointers)
- [x] Closures with variable capture
- [x] Operator overloading via impl methods (+, -, *, /, ==, !=, <, >, <=, >=)
- [x] Bare `self` / `mut self` syntax in impl methods (type inferred from impl block)
- [x] Implicit generic type inference across multiple call sites with different types
- [x] Enum payloads with construction and pattern matching
- [x] Region memory allocation redirection (arena allocator)
- [x] Comptime Turing-complete evaluation (variables, if/else, while/for, match, functions, recursion, arrays, structs, string concatenation)
- [x] Effect system enforcement (opt-in via `with [IO, Console, FileSystem]`)
- [x] Concurrency primitives (thread spawn/join, mutex)
- [x] SIMD intrinsics (f32x4, f32x8, f64x2, f64x4, i32x4, i32x8, i64x2, i64x4)
- [x] Async/await with Future[T] (Phase A synchronous semantics + Phase B stackless coroutines)
- [x] Custom user-defined effects
- [x] Package registry download
- [x] Compile-time array sizes
- [x] Map iteration (for-in on maps)

### Stage 1 Self-Hosting Compiler -- Complete
- [x] Compiler rewritten in dAImond (~10 modules)
- [x] Full feature parity with Stage 0 subset
- [x] Verified fixed-point bootstrap (Stage 1 compiles itself with identical output)
- [x] Multi-file import system with transitive dependency resolution
- [x] Enum payloads, Option[T], Result[T, E], match expressions
- [x] Lambda expressions, generic monomorphization
- [x] Pipeline operator `|>`, error propagation `?`, Box[T]
- [x] Compound assignment (`+=`, `-=`, `*=`, `/=`), modulo `%`

### Standard Library -- Complete
- [x] `std.io` -- I/O functions (print, file read/write/append, read_line)
- [x] `std.math` -- Math functions (sin, cos, sqrt, pow, log, PI, E)
- [x] `std.collections` -- Set, Queue, Stack wrappers
- [x] `std.string` -- String utilities (split, join, repeat, pad, reverse)
- [x] `std.os` -- OS functions (env, cwd, exit, args)
- [x] `std.fs` -- Filesystem (mkdir, readdir, rename, remove, is_dir)
- [x] `std.net` -- TCP networking (listen, accept, connect, read, write)
- [x] `std.thread` -- Concurrency (spawn, join, mutex)

### Tooling -- Complete
- [x] `daimond fmt` -- Code formatter (indent normalization)
- [x] `daimond test` -- Testing framework (test_* discovery, panic catching, assert/assert_eq)
- [x] `daimond pkg` -- Package manager (TOML manifest, version/path/git dependencies)
- [x] `daimond-lsp` -- Language Server Protocol (diagnostics, completion, hover)

### Stage 3 LLVM Backend -- Complete
- [x] LLVM backend with full test parity (254/254 integration tests match Stage 0 output)
- [x] SIMD intrinsics (f32x4, f32x8, f64x2, f64x4, i32x4, i32x8, i64x2, i64x4)
- [x] Dynamic trait dispatch (`dyn Trait`)
- [x] Map[K,V] operations, operator overloading, extern string wrappers
- [x] Async/await (int and string returns), closures with capture, regions
- [x] Optimization passes via LLVM (`-O0` through `-O3`)
- [x] Self-hosting: Stage 3 compiles Stage 1, verified fixed-point bootstrap

### Stage 4 LLVM Backend in dAImond -- Complete
- [x] Entire LLVM backend rewritten in dAImond (~18.7K lines monolithic, ~15 split modules)
- [x] Full test parity with Stage 0 and Stage 3 (254/254 integration tests pass)
- [x] Complete compiler pipeline: lexer, parser, AST, IR gen, LLVM gen -- all in dAImond
- [x] LLVM-C bridge via extern FFI (llvm_bridge.c)
- [x] Error detection: unterminated strings, syntax errors, undefined functions, effect enforcement
- [x] No Zig dependency -- full self-hosting achieved
- [x] Modular source with gen_bootstrap.sh for monolithic generation (same pattern as Stage 1)

### Upcoming
- [ ] Debug info (DWARF) in Stage 3/4
- [ ] Stage 4 self-compilation (Stage 4 compiles itself)

## Design Principles

1. **One obvious way to do anything**
2. **Reading code reveals its cost**
3. **Syntax should be greppable**
4. **No magic, no surprises**
5. **Familiar to C/Rust/Zig programmers**

## Contributing

See `CLAUDE.md` for detailed development guidance.

## License

MIT License - see [LICENSE](LICENSE) for details.
