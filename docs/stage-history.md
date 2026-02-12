# Stage History — Archived Documentation

Archived documentation for completed/frozen compiler stages. For current development, see `CLAUDE.md`.

## Stage 1 Compiler Architecture

Stage 1 is written in dAImond and compiled by Stage 0. It compiles a subset of dAImond to C11, sufficient to compile itself (bootstrap proof).

### Design Decisions
- **Modular source, monolithic bootstrap**: Development happens on split module files (`token.dm`, `lexer.dm`, etc.) using the multi-file import system. For Stage 0 bootstrap (which requires a single file), `gen_bootstrap.sh` concatenates all modules into `main.dm`.
- **Struct-based AST**: Uses integer kind tags instead of enum payloads to work around codegen bugs.
- **No type checker**: Generates C directly from AST; the C compiler catches type errors.
- **String-based codegen**: Builds C code via string concatenation.
- **Subset compiler**: Only supports features used within Stage 1 itself.

### Module Layout
```
token.dm           (no imports)        — Token kinds, Token struct, keyword_lookup
lexer.dm           → token             — Lexer struct, tokenize, character helpers
compiler.dm        → token             — Compiler struct, parser helpers, type mapping
compile_expr.dm    → compiler          — Expression compiler chain, lambda, generics
compile_stmt.dm    → compiler          — Statement compiler, type tracking/inference
compile_match.dm   → compiler          — Match expressions and statements
compile_decl.dm    → compiler          — Function/struct/enum/impl declarations, prescan
runtime.dm         → compiler          — C runtime emission, output assembly
imports.dm         (no imports)        — Multi-file import resolver
package.dm         (no imports)        — Package manager (TOML manifest, pkg commands)
main_split.dm      → all of the above  — Entry point with main()
```

Cross-module function calls (e.g., compile_stmt calls compile_expr) work because:
1. The import system concatenates all source before tokenization
2. `prescan_declarations` registers all function names as forward references
3. The C compiler sees forward declarations for all functions

### Stage 1 Feature Support
- **Core**: structs, simple enums, List[T], Box[T], if/else/while/for/loop/break/continue, functions, string ops, file I/O, method calls (.push/.len/.pop), struct literals, type inference, forward references
- **Enum payloads**: `enum Shape { Circle(float), Rect(float, float), Point }` with constructor syntax `Shape.Circle(5.0)` and `Shape.Point`
- **Option[T]**: `Option[int]` with `Some(val)` / `None` constructors (type annotation required on let bindings)
- **Result[T, E]**: `Result[int, string]` with `Ok(val)` / `Err(msg)` constructors (type annotation required on let bindings)
- **Match expressions**: `match expr { Pattern => body }` with enum variant patterns, payload binding, wildcard `_`, literal patterns, and bare `Ok(v)`/`Err(e)`/`Some(v)`/`None` patterns. Works as both statement and expression.
- **Multi-file imports**: `import module_name` resolves to `module_name.dm` in same directory. `import std.helpers` resolves to `std/helpers.dm`. Transitive imports supported with deduplication (diamond imports handled). Source concatenated before tokenization.
- **Lambda expressions**: `|x: int| x * 2` for expression body, `|a: int, b: int| { ... }` for block body. Lifted to static functions at file scope. Function pointer types generated automatically for variable declarations.
- **Generic monomorphization**: `fn max[T](a: T, b: T) -> T { ... }` with explicit (`max[int](3, 7)`) or implicit (`max(3, 7)`) type arguments. Generates specialized copies at call sites with deduplication.
- **Pipeline operator**: `x |> f` desugars to `f(x)`, `x |> f(y)` desugars to `f(x, y)`. Supports chaining: `x |> f |> g`.
- **Error propagation**: `expr?` on `Result[T, E]` unwraps `Ok` or early-returns `Err`. On `Option[T]` unwraps `Some` or early-returns `None`.
- **Box[T]**: `Box_new(value)` heap-allocates, `Box_null()` returns NULL. Type `Box[T]` maps to `T*` in C.
- **Compound assignment**: `+=`, `-=`, `*=`, `/=` operators
- **Modulo**: `%` operator
- **All builtins**: `print`, `println`, `eprint`, `eprintln`, `panic`, `exit`, `len`, `char_at`, `substr`, `int_to_string`, `float_to_string`, `bool_to_string`, `parse_int`, `parse_float`, `string_contains`, `string_find`, `starts_with`, `ends_with`, `string_replace`, `string_trim`, `string_to_upper`, `string_to_lower`, `file_read`, `file_write`, `args_get`, `args_len`, `system`, `Box_new`, `Box_null`
- **CLI flags**: `-o`, `-c`, `--emit-c`, `-v`/`--verbose`, `--version`, `-h`/`--help`, `run`, `compile` commands
- **Traits**: `trait Name { fn method(self) -> Type }` and `impl TraitName for TypeName { ... }` with static dispatch via mangled C function names (e.g., `dm_TypeName_method`)
- **Effects**: `with [IO, Console, FileSystem, Process]` annotations on function signatures are enforced — builtins are mapped to required effects (`println`→Console, `file_read`→FileSystem, `exit`→Process, etc.) and callers without the required effect in their `with [...]` clause get a compile error
- **Regions**: `region name { ... }` blocks generate `dm_arena_create(4096)` / `dm_arena_destroy()` calls with allocation redirection — string concatenation within a region uses `dm_string_concat_arena` to allocate from the arena. Arena struct and functions emitted inline in Stage 1's runtime. Nested regions supported with unique arena names.
- **Package management**: `pkg init` (create daimond.toml), `pkg add <name>` (add dependency), `pkg list` (list dependencies). TOML manifest parsing with version/path/git dependency support.
- **CLI commands**: `lex`, `parse`, `check`, `fmt`, `test`, `compile`, `run`, `pkg init/add/list`, `clean` — full command parity with Stage 0
- **Not yet supported in Stage 1**: dynamic dispatch (`dyn Trait`)

### Building Stage 1
```bash
# Stage 0 compiles Stage 1 (from monolithic main.dm)
cd stage0 && ./zig-out/bin/daimond compile ../stage1/main.dm -o stage1_compiler

# Stage 1 compiles a test program
./stage1_compiler test_program.dm

# Run the result
./test_program

# Stage 1 self-compiles from split modules
./stage1_compiler ../stage1/main_split.dm

# Regenerate monolithic main.dm from modules
cd stage1 && bash gen_bootstrap.sh
```

### Bootstrap Chain
```
Stage 0 compiles main.dm (monolithic) → stage1_compiler
stage1_compiler compiles main_split.dm (with imports) → stage1_v2
stage1_v2 compiles main_split.dm → stage1_v3
diff stage1_v2.c stage1_v3.c → fixed point ✓
```

## Stage 3 (LLVM Backend) Build Commands

Stage 3 build commands run from the `stage3/` directory. Requires **LLVM 17+** development libraries.

```bash
# Build the LLVM compiler
cd stage3 && zig build

# Run unit tests (IR definitions, LLVM bindings)
cd stage3 && zig build test

# Compile a dAImond program to native binary via LLVM
cd stage3 && ./zig-out/bin/daimond-llvm <file.dm> -o output

# Compile with optimization
cd stage3 && ./zig-out/bin/daimond-llvm <file.dm> -o output -O2

# Emit dAImond IR (debugging)
cd stage3 && ./zig-out/bin/daimond-llvm <file.dm> --emit-ir

# Emit LLVM IR (debugging)
cd stage3 && ./zig-out/bin/daimond-llvm <file.dm> --emit-llvm
```
