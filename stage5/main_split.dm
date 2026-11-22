module main_split

import token
import lexer
import ast
import parser
import ir
import ir_builder
import ir_gen
import ir_gen_decl
import ir_gen_stmt
import ir_gen_expr
import ir_gen_builtins
import ir_gen_comptime
import check
import llvm
import llvm_gen
import llvm_gen_call
import llvm_gen_simd
import prelude

-- ============================================================
-- The dAImond compiler
--
-- Compiles dAImond source to native binaries via LLVM.
--
-- Pipeline:
--   Source (.dm) -> Lexer -> Tokens -> Parser -> AST
--   -> IR Gen -> dAImond IR -> LLVM Gen -> LLVM IR
--   -> Object File -> Linker -> Native Binary
-- ============================================================

fn VERSION() -> string { return "0.1.0" }

-- ============================================================
-- USAGE / HELP
-- ============================================================

fn print_usage() {
    println("dAImond Compiler (LLVM backend)")
    println("")
    println("Usage: daimond [command] <file.dm> [options]")
    println("")
    println("Commands:")
    println("  (none)        Compile the file to a program")
    println("  run           Compile the file and run the program; arguments after -- go to it")
    println("  check         Report the file's errors without building a program")
    println("  fmt           Indent the file by its braces, in place or into -o <file>")
    println("  test          Run each fn test_*() of the file and report which pass")
    println("  symbols       List the file's declarations, for editors")
    println("")
    println("Options:")
    println("  -o <file>     Output binary name (default: input without .dm)")
    println("  -O0           No optimization (default)")
    println("  -O1           Basic optimization")
    println("  -O2           Full optimization")
    println("  -O3           Aggressive optimization")
    println("  -g            Debug info: files, functions, lines and variables, for gdb and lldb")
    println("  --emit-ir     Print dAImond IR and exit")
    println("  --emit-llvm   Emit LLVM IR file (.ll) and exit")
    println("  --emit-bitcode  Emit LLVM bitcode (.bc), optimized as -O says, and exit")
    println("  --stdin       Read the program from standard input; <file.dm> still names it")
    println("                (compiling, check and symbols)")
    println("  --link-llvm   Add the LLVM bridge and link the LLVM libraries (for compiling the compiler itself)")
    println("  -v, --verbose Verbose output")
    println("  -h, --help    Show this help")
    println("  --version     Show version")
    println("")
    println("Environment:")
    println("  DAIMOND_CACHE_DIR  Where the precompiled runtime object is kept")
    println("                     (default: $XDG_CACHE_HOME/daimond or ~/.cache/daimond)")
}

-- ============================================================
-- CLI ARGUMENT PARSING
-- ============================================================

struct CLIArgs {
    -- "" to compile, or "run", "check", "fmt", "test" or "symbols"
    command: string,
    -- --stdin: the program's text comes from standard input, and
    -- input_file is only its name
    from_stdin: bool,
    input_file: string,
    output_file: string,
    -- Whether -o named the output file
    has_output: bool,
    -- run: the program's arguments
    program_args: List[string],
    opt_level: int,
    emit_ir: bool,
    emit_llvm: bool,
    emit_bitcode: bool,
    link_llvm: bool,
    -- -g: the program carries debug info
    debug_info: bool,
    verbose: bool,
    show_help: bool,
    show_version: bool,
    has_error: bool,
    error_msg: string
}

fn parse_cli_args() -> CLIArgs {
    let mut args = CLIArgs {
        command: "",
        from_stdin: false,
        input_file: "",
        output_file: "",
        has_output: false,
        program_args: [],
        opt_level: 0,
        emit_ir: false,
        emit_llvm: false,
        emit_bitcode: false,
        link_llvm: false,
        debug_info: false,
        verbose: false,
        show_help: false,
        show_version: false,
        has_error: false,
        error_msg: ""
    }

    let argc = args_len()
    if argc < 2 {
        args.show_help = true
        return args
    }

    -- First non-flag argument is the input file
    let mut i = 1
    while i < argc {
        let arg = args_get(i)

        if i == 1 and (arg == "run" or arg == "check" or arg == "fmt" or arg == "test" or arg == "symbols") {
            args.command = arg
        } else if arg == "--" and args.command == "run" {
            -- The rest goes to the program
            i = i + 1
            while i < argc {
                args.program_args.push(args_get(i))
                i = i + 1
            }
        } else if arg == "-h" or arg == "--help" {
            args.show_help = true
            return args
        } else if arg == "--version" {
            args.show_version = true
            return args
        } else if arg == "-o" {
            if i + 1 < argc {
                i = i + 1
                args.output_file = args_get(i)
                args.has_output = true
            } else {
                args.has_error = true
                args.error_msg = "Error: -o requires an argument"
                return args
            }
        } else if arg == "-O0" {
            args.opt_level = 0
        } else if arg == "-O1" {
            args.opt_level = 1
        } else if arg == "-O2" {
            args.opt_level = 2
        } else if arg == "-O3" {
            args.opt_level = 3
        } else if arg == "-g" {
            args.debug_info = true
        } else if arg == "--emit-ir" {
            args.emit_ir = true
        } else if arg == "--emit-llvm" {
            args.emit_llvm = true
        } else if arg == "--emit-bitcode" {
            args.emit_bitcode = true
        } else if arg == "--link-llvm" {
            args.link_llvm = true
        } else if arg == "--stdin" {
            args.from_stdin = true
        } else if arg == "-v" or arg == "--verbose" {
            args.verbose = true
        } else if starts_with(arg, "-") {
            args.has_error = true
            args.error_msg = "Error: unknown option: " + arg
            return args
        } else {
            -- Input file
            if args.input_file == "" {
                args.input_file = arg
            } else {
                args.has_error = true
                args.error_msg = "Error: multiple input files specified"
                return args
            }
        }

        i = i + 1
    }

    if args.input_file == "" {
        args.has_error = true
        args.error_msg = "Error: no input file specified"
        return args
    }
    if args.from_stdin and (args.command == "fmt" or args.command == "test") {
        args.has_error = true
        args.error_msg = "Error: --stdin does not go with " + args.command
        return args
    }

    -- Default output file: strip .dm extension from input
    if args.output_file == "" {
        if ends_with(args.input_file, ".dm") {
            let input_len = len(args.input_file)
            args.output_file = substr(args.input_file, 0, input_len - 3)
        } else {
            args.output_file = args.input_file + ".out"
        }
    }

    return args
}

-- ============================================================
-- PATH UTILITIES
-- ============================================================

-- Derive the directory containing a file path.
-- "foo/bar/baz.dm" -> "foo/bar"
-- "baz.dm" -> "."
fn dirname(path: string) -> string {
    let path_len = len(path)
    let mut last_slash = -1
    let mut i = 0
    while i < path_len {
        if char_at(path, i) == "/" {
            last_slash = i
        }
        i = i + 1
    }
    if last_slash < 0 {
        return "."
    }
    return substr(path, 0, last_slash)
}

-- ============================================================
-- LINKING
-- The compiler links a program itself, with LLVM's linker ld.lld. A
-- program is its own object, the runtime's object when that is
-- precompiled, the C library's start-up objects (Scrt1.o, crti.o and
-- crtn.o) and the C library. No C compiler takes part.
-- ============================================================

-- The dynamic loader of a glibc program on x86-64 Linux
fn dynamic_loader() -> string {
    return "/lib64/ld-linux-x86-64.so.2"
}

-- The path of ld.lld, or "" when none is found: $DAIMOND_LD when set,
-- else the first ld.lld on PATH, else that of the newest LLVM in
-- /usr/lib/llvm-<version>/bin, where Debian and Ubuntu keep it
fn find_linker() -> string {
    let chosen = env_get("DAIMOND_LD")
    if chosen != "" { return chosen }
    let dirs = string_split(env_get("PATH"), ":")
    let mut i = 0
    while i < dirs.len() {
        if dirs[i] != "" and file_exists(dirs[i] + "/ld.lld") {
            return dirs[i] + "/ld.lld"
        }
        i = i + 1
    }
    let mut newest = ""
    let mut newest_version = -1
    let names = string_split(fs_readdir("/usr/lib"), "\n")
    i = 0
    while i < names.len() {
        let name = names[i]
        if starts_with(name, "llvm-") {
            let version = parse_int(substr(name, 5, len(name) - 5))
            let candidate = "/usr/lib/" + name + "/bin/ld.lld"
            if version > newest_version and file_exists(candidate) {
                newest = candidate
                newest_version = version
            }
        }
        i = i + 1
    }
    return newest
}

-- The directory that holds the C library's start-up objects and
-- libc.so, or "" when none is found: $DAIMOND_LIBC_DIR when set, else
-- where Debian and Ubuntu, then Fedora, then Arch keep them
fn find_libc_dir() -> string {
    let chosen = env_get("DAIMOND_LIBC_DIR")
    if chosen != "" { return chosen }
    if file_exists("/usr/lib/x86_64-linux-gnu/Scrt1.o") { return "/usr/lib/x86_64-linux-gnu" }
    if file_exists("/usr/lib64/Scrt1.o") { return "/usr/lib64" }
    if file_exists("/usr/lib/Scrt1.o") { return "/usr/lib" }
    return ""
}

-- The path with every symbolic link in it followed, or "" when there is
-- no such file
fn real_path(path: string) -> string {
    unsafe {
        -- Given no buffer, realpath allocates one of the size it needs
        let real = compiler_realpath(path, 0)
        if real == 0 {
            return ""
        }
        let n = compiler_strlen(real)
        return string_from_raw(real, n, n + 1)
    }
}

-- The LLVM library that --link-llvm links, or "" when none is found:
-- $DAIMOND_LLVM_LIB when set, else libLLVM.so of the LLVM that the
-- linker belongs to, which is in lib/ (or lib64/) next to the bin/
-- that holds the linker once its symbolic links are followed
-- (/usr/lib/llvm-19/lib on Debian and Ubuntu, /usr/lib on Arch,
-- /usr/lib64 on Fedora)
fn find_llvm_library(linker: string) -> string {
    let chosen = env_get("DAIMOND_LLVM_LIB")
    if chosen != "" { return chosen }
    let mut found = linker
    if string_contains(linker, "/") == false {
        -- $DAIMOND_LD as a name: the program is looked for on PATH
        let dirs = string_split(env_get("PATH"), ":")
        let mut i = 0
        while i < dirs.len() and found == linker {
            if dirs[i] != "" and file_exists(dirs[i] + "/" + linker) {
                found = dirs[i] + "/" + linker
            }
            i = i + 1
        }
    }
    let real = real_path(found)
    if real == "" { return "" }
    let root = dirname(dirname(real))
    if file_exists(root + "/lib/libLLVM.so") { return root + "/lib/libLLVM.so" }
    if file_exists(root + "/lib64/libLLVM.so") { return root + "/lib64/libLLVM.so" }
    return ""
}

-- The path of llvm_bridge.dm, the LLVM bridge that --link-llvm adds to
-- the program, or "" when it is not found: next to the input file,
-- else in stage5/ of the current directory, else in the current
-- directory
fn find_bridge_file(input_dir: string) -> string {
    if file_exists(input_dir + "/llvm_bridge.dm") {
        return input_dir + "/llvm_bridge.dm"
    }
    if file_exists("stage5/llvm_bridge.dm") {
        return "stage5/llvm_bridge.dm"
    }
    if file_exists("llvm_bridge.dm") {
        return "llvm_bridge.dm"
    }
    return ""
}

-- The LLVM bridge at path, parsed as file number `file` of the
-- program. Exits when it does not parse.
fn parse_llvm_bridge(path: string, file: int) -> SourceFile {
    let p = parser_new(tokenize_file(file_read(path), path, file))
    let bridge_sf = parse_source_file(p)
    let mut errors = bridge_sf.errors
    if errors.len() == 0 {
        errors = validate_source_file(bridge_sf)
    }
    if errors.len() > 0 {
        let mut i = 0
        while i < errors.len() {
            eprintln(path + ": " + errors[i])
            i = i + 1
        }
        exit(1)
    }
    return bridge_sf
}

-- ============================================================
-- TRAITS
-- Before the program is checked, complete_traits adds to it what its
-- traits stand for:
--
-- 1. A trait's method that has a body is the method of every type
--    that implements the trait and does not define the method itself.
--    Each such impl gets a copy of the method with expressions of its
--    own (fresh_function in ast.dm), which is then checked and
--    generated as any method of the type is.
--
-- 2. `dyn Trait` is the type of a value of any type that implements
--    the trait. It is an enum the compiler writes, named Dyn$Trait
--    (parse_type gives `dyn Trait` this name; no source can write it),
--    with one variant for each type that implements the trait,
--    V_Type(Box[Type]), and with the trait's methods: each matches the
--    variant and calls the method of the type in the box. A value of
--    an implementing type that goes where a `dyn Trait` is expected is
--    put in its variant (the checker marks the place, ck_settle;
--    generate_expr does it). So a `dyn Trait` is passed, returned,
--    kept in a field or a list and assigned as any enum value is, and
--    a method call on it reaches the method of the type it holds.
--    The enum has the methods that take `self` and do not name
--    `Self` elsewhere; a `mut self` method changes the value in the
--    box, which every copy of the `dyn Trait` shares.
--    A generic type that implements a trait is not among the types
--    of its `dyn`, and a trait that no type implements has no `dyn`.
-- ============================================================

-- The trait of the name, or one with no name
fn trait_named(sf: SourceFile, name: string) -> TraitDecl {
    let mut i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_TRAIT() {
            let td = *decl.trait_decl
            if td.name == name { return td }
        }
        i = i + 1
    }
    return trait_decl_new("", 0, 0)
}

fn has_method_named(methods: List[FunctionDecl], name: string) -> bool {
    let mut i = 0
    while i < methods.len() {
        if methods[i].name == name { return true }
        i = i + 1
    }
    return false
}

-- A type as source text: `dyn Shape` for Dyn$Shape
fn type_source_text(t: string) -> string {
    return string_replace(t, DYN_PREFIX(), "dyn ")
}

-- Whether a `dyn Trait` has the method: it takes `self`, has no type
-- parameters of its own and names `Self` nowhere else
fn dyn_takes_method(m: FunctionDecl) -> bool {
    if m.params.len() == 0 or m.generic_params.len() > 0 { return false }
    if m.params[0].name != "self" { return false }
    if type_names_self(m.return_type) { return false }
    let mut i = 1
    while i < m.params.len() {
        if type_names_self(m.params[i].type_name) { return false }
        i = i + 1
    }
    return true
}

-- Whether the type has the word Self in it
fn type_names_self(t: string) -> bool {
    let n = len(t)
    let mut i = 0
    while i < n {
        if ty_ident_byte(byte_at(t, i)) == false {
            i = i + 1
            continue
        }
        let start = i
        while i < n and ty_ident_byte(byte_at(t, i)) {
            i = i + 1
        }
        if substr(t, start, i - start) == "Self" { return true }
    }
    return false
}

-- The enum that is `dyn Trait` and its methods, as source. The enum is
-- called DynEnum here; complete_traits gives it its name.
fn dyn_enum_source(td: TraitDecl, types: List[string]) -> string {
    let mut src = "module dyn\nenum DynEnum {\n"
    let mut ti = 0
    while ti < types.len() {
        if ti > 0 { src = src + ",\n" }
        src = src + "    V_" + types[ti] + "(Box[" + types[ti] + "])"
        ti = ti + 1
    }
    src = src + "\n}\nimpl DynEnum {\n"
    let mut mi = 0
    while mi < td.methods.len() {
        let m = td.methods[mi]
        if dyn_takes_method(m) {
            let mut params = ""
            let mut args = ""
            let mut pi = 1
            while pi < m.params.len() {
                let p = m.params[pi]
                params = params + ", "
                if p.is_mut { params = params + "mut " }
                params = params + p.name + ": " + type_source_text(p.type_name)
                if pi > 1 { args = args + ", " }
                args = args + p.name
                pi = pi + 1
            }
            let returns = m.return_type != "" and m.return_type != "void"
            src = src + "    fn " + m.name + "(self" + params + ")"
            if returns { src = src + " -> " + type_source_text(m.return_type) }
            if m.effects.len() > 0 {
                src = src + " with ["
                let mut ei = 0
                while ei < m.effects.len() {
                    if ei > 0 { src = src + ", " }
                    src = src + m.effects[ei]
                    ei = ei + 1
                }
                src = src + "]"
            }
            src = src + " {\n        match self {\n"
            ti = 0
            while ti < types.len() {
                src = src + "            DynEnum.V_" + types[ti] + "(held__) => {\n"
                if m.params[0].is_mut {
                    -- The method changes the value, which goes back in the box
                    src = src + "                let mut value__ = *held__\n"
                    if returns {
                        src = src + "                let result__ = value__." + m.name + "(" + args + ")\n"
                        src = src + "                *held__ = value__\n"
                        src = src + "                return result__\n"
                    } else {
                        src = src + "                value__." + m.name + "(" + args + ")\n"
                        src = src + "                *held__ = value__\n"
                    }
                } else {
                    src = src + "                let value__ = *held__\n"
                    if returns {
                        src = src + "                return value__." + m.name + "(" + args + ")\n"
                    } else {
                        src = src + "                value__." + m.name + "(" + args + ")\n"
                    }
                }
                src = src + "            }\n"
                ti = ti + 1
            }
            src = src + "        }\n    }\n"
        }
        mi = mi + 1
    }
    return src + "}\n"
}

fn complete_traits(sf0: SourceFile) -> SourceFile {
    let mut sf = sf0
    -- 1. The methods a trait gives the types that implement it
    let mut decls: List[Declaration] = []
    let mut i = 0
    while i < sf.declarations.len() {
        let mut decl = sf.declarations[i]
        if decl.kind == DECL_IMPL() {
            let mut ib = *decl.impl_block
            if ib.trait_name != "" {
                let td = trait_named(sf, ib.trait_name)
                let mut mi = 0
                while mi < td.methods.len() {
                    let m = td.methods[mi]
                    if m.body.len() > 0 and has_method_named(ib.methods, m.name) == false {
                        ib.methods.push(fresh_function(m))
                    }
                    mi = mi + 1
                }
                decl.impl_block = Box_new(ib)
            }
        }
        decls.push(decl)
        i = i + 1
    }

    -- 2. dyn Trait, for each trait that a type implements
    i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_TRAIT() {
            let td = *decl.trait_decl
            let mut types: List[string] = []
            if td.generic_params.len() == 0 {
                let mut di = 0
                while di < sf.declarations.len() {
                    let other = sf.declarations[di]
                    if other.kind == DECL_IMPL() {
                        let ib = *other.impl_block
                        if ib.trait_name == td.name and ib.generic_params.len() == 0 and string_contains(ib.target_type, "[") == false {
                            let mut have = false
                            let mut hi = 0
                            while hi < types.len() {
                                if types[hi] == ib.target_type { have = true }
                                hi = hi + 1
                            }
                            if have == false { types.push(ib.target_type) }
                        }
                    }
                    di = di + 1
                }
            }
            if types.len() > 0 {
                -- The declarations are at the trait's line, in its file
                let lexed = tokenize(dyn_enum_source(td, types))
                let mut tokens: List[Token] = []
                let mut ti = 0
                while ti < lexed.len() {
                    let mut text = lexed[ti].value
                    if lexed[ti].kind == TK_IDENT() and text == "DynEnum" { text = DYN_PREFIX() + td.name }
                    tokens.push(token_new(lexed[ti].kind, text, td.line, td.col))
                    ti = ti + 1
                }
                let made = parse_source_file(parser_new(tokens))
                let mut mi = 0
                while mi < made.declarations.len() {
                    decls.push(made.declarations[mi])
                    mi = mi + 1
                }
            }
        }
        i = i + 1
    }
    sf.declarations = decls
    return sf
}

-- Add the LLVM bridge to the compiler's declarations. The compiler
-- declares each bridge function as an extern fn (llvm.dm). The
-- bridge's own function of that name takes the declaration's place.
fn add_llvm_bridge(sf: SourceFile, bridge_sf: SourceFile) -> SourceFile {
    let mut defined: Map[string, bool] = Map_new()
    let mut i = 0
    while i < bridge_sf.declarations.len() {
        let decl = bridge_sf.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.is_extern == false {
                defined.insert(fd.name, true)
            }
        }
        i = i + 1
    }
    let mut out = sf
    let mut decls: List[Declaration] = []
    i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        let mut replaced = false
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.is_extern and defined.contains(fd.name) {
                replaced = true
            }
        }
        if replaced == false {
            decls.push(decl)
        }
        i = i + 1
    }
    i = 0
    while i < bridge_sf.declarations.len() {
        decls.push(bridge_sf.declarations[i])
        i = i + 1
    }
    out.declarations = decls
    return out
}

-- ============================================================
-- IMPORT RESOLUTION
-- ============================================================

-- Resolve imports: read each imported file, tokenize, parse,
-- and merge declarations into the main source file.
-- Handles transitive and diamond imports via deduplication.
fn resolve_imports(sf: SourceFile, input_dir: string, verbose: bool) -> SourceFile {
    let mut merged = source_file_new(sf.module_name)
    -- The file being compiled is file number 0, and each imported file
    -- takes the next number as it is loaded
    let mut state = Imports { loaded: sf.files, decls: [] }

    -- Process each import
    let mut i = 0
    while i < sf.imports.len() {
        state = process_import(sf.imports[i], input_dir, state, verbose)
        i = i + 1
    }

    -- Add the original file's declarations after imported ones
    let mut j = 0
    while j < sf.declarations.len() {
        state.decls.push(sf.declarations[j])
        j = j + 1
    }

    merged.declarations = state.decls
    merged.files = state.loaded
    return merged
}

-- The runtime written in dAImond (prelude.dm, generated from
-- runtime/*.dm), parsed as file number `file` of the program
fn parse_prelude(file: int) -> SourceFile {
    return parse_source_file(parser_new(tokenize_file(prelude_source(), PRELUDE_NAME(), file)))
}

-- Add the runtime to a program's declarations. Its generic
-- declarations cost nothing until the program uses them.
fn add_prelude(sf: SourceFile, prelude_sf: SourceFile) -> SourceFile {
    let mut out = sf
    let mut i = 0
    while i < prelude_sf.declarations.len() {
        out.declarations.push(prelude_sf.declarations[i])
        i = i + 1
    }
    return out
}

-- ============================================================
-- PRECOMPILED RUNTIME
-- A -O0 build does not compile the runtime's functions into the
-- program. It links an object file that holds them, compiled once
-- with full optimization. The object is kept in a cache directory
-- under a name that carries this compiler's build ID, so each build
-- of the compiler makes and uses its own.
--
-- Generic runtime code, such as Map[K, V], has no object code until
-- a program names its types, so it is still compiled into the
-- program. At -O1 and above the whole runtime is, so that the
-- optimizer can inline it into the program.
-- ============================================================

extern fn getpid() -> i32

-- The directory that holds runtime objects, or "" when the
-- environment names none. There is no shared fallback such as /tmp,
-- where another user could leave an object for this one to link.
fn runtime_cache_dir() -> string {
    let chosen = env_get("DAIMOND_CACHE_DIR")
    if chosen != "" { return chosen }
    let xdg_cache = env_get("XDG_CACHE_HOME")
    if xdg_cache != "" { return xdg_cache + "/daimond" }
    let home = env_get("HOME")
    if home != "" { return home + "/.cache/daimond" }
    return ""
}

-- The functions the runtime object defines: every prelude function
-- that is neither generic nor a declaration of a C function
fn precompiled_function_names(prelude_sf: SourceFile) -> Map[string, bool] {
    let mut names: Map[string, bool] = Map_new()
    let mut i = 0
    while i < prelude_sf.declarations.len() {
        let decl = prelude_sf.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.is_extern == false and fd.generic_params.len() == 0 {
                names.insert(fd.name, true)
            }
        }
        i = i + 1
    }
    return names
}

-- Compile the runtime alone, optimized, to object file `obj_file`.
-- Returns 0 on success. It reports nothing on failure: the caller
-- then compiles the runtime into the program, which reports whatever
-- is wrong with it.
fn build_runtime_object(prelude_sf: SourceFile, checked: Checked, obj_file: string) -> int {
    let mut ir_gen = ir_generator_new()
    ir_gen.checked_types = checked.types
    ir_gen.checked_type_args = checked.type_args
    ir_gen.checked_targets = checked.targets
    ir_gen.checker = checked.checker
    ir_gen.has_checker = true
    ir_gen = generate_module(ir_gen, prelude_sf)
    if ir_gen.type_errors.len() > 0 { return 1 }
    let mut lg = llvm_gen_new(ir_gen.ir_mod, "daimond_runtime")
    lg = lg_generate_module(lg)
    let mut result = 1
    if lg.type_errors.len() == 0 and verify_module(lg) == 0 {
        result = emit_to_object(lg, obj_file, 2)
    }
    llvm_gen_dispose(lg)
    return result
}

-- The path of this compiler's runtime object, which is built first
-- when the cache does not have it. "" when it cannot be built or
-- kept in the cache directory.
fn find_or_build_runtime_object(prelude_sf: SourceFile, checked: Checked, verbose: bool) -> string {
    let cache_dir = runtime_cache_dir()
    if cache_dir == "" { return "" }
    let obj_file = cache_dir + "/runtime-" + runtime_build_id() + ".o"
    if file_exists(obj_file) { return obj_file }
    if verbose { println("  Building the runtime object: " + obj_file) }
    -- Build it under a name of this process's own and then rename it,
    -- so that other compilers running now see a whole object or none
    let mut pid = 0
    unsafe { pid = getpid() as int }
    let own_file = obj_file + "." + int_to_string(pid)
    let mut built = fs_mkdir(cache_dir) == 0
    if built { built = build_runtime_object(prelude_sf, checked, own_file) == 0 }
    if built { built = fs_rename(own_file, obj_file) == 0 }
    if built == false {
        fs_remove(own_file)
        if verbose { println("  No runtime object: compiling the runtime into the program") }
        return ""
    }
    return obj_file
}

-- Resolve an import path to a filesystem path.
-- "foo" -> "<dir>/foo.dm"
-- "std.io" -> "<dir>/std/io.dm"
fn resolve_import_path(import_path: string, base_dir: string) -> string {
    -- Replace dots with /
    let mut result = base_dir + "/"
    let path_len = len(import_path)
    let mut i = 0
    while i < path_len {
        let ch = char_at(import_path, i)
        if ch == "." {
            result = result + "/"
        } else {
            result = result + ch
        }
        i = i + 1
    }
    return result + ".dm"
}

extern fn compiler_realpath(path: string, resolved: int) -> int = "realpath"
extern fn compiler_strlen(s: int) -> int = "strlen"

-- The directory the running compiler is in, or "" when it can't be found
fn compiler_dir() -> string {
    let exe = real_path("/proc/self/exe")
    if exe == "" {
        return ""
    }
    return dirname(exe)
}

-- The file of the standard library module `import std.<module>` names,
-- or "" when there is none. The library is the directory
-- $DAIMOND_STDLIB, else stdlib/ beside the compiler's directory (the
-- compiler is build/daimond in a checkout), else stdlib/ in the
-- compiler's directory, else stdlib/ in the current directory.
fn stdlib_file(module_path: string) -> string {
    let chosen = env_get("DAIMOND_STDLIB")
    if chosen != "" {
        let in_chosen = resolve_import_path(module_path, chosen)
        if file_exists(in_chosen) { return in_chosen }
        return ""
    }
    let beside = compiler_dir()
    if beside != "" {
        let in_checkout = resolve_import_path(module_path, beside + "/../stdlib")
        if file_exists(in_checkout) { return in_checkout }
        let in_compiler_dir = resolve_import_path(module_path, beside + "/stdlib")
        if file_exists(in_compiler_dir) { return in_compiler_dir }
    }
    let in_current = resolve_import_path(module_path, "stdlib")
    if file_exists(in_current) { return in_current }
    return ""
}

-- The files imported so far and their declarations. It is passed to
-- process_import and returned by it, so that a file imported by two
-- files is known to both. `loaded` begins with the file being
-- compiled, so a file's place in it is its file number.
struct Imports {
    loaded: List[string],
    decls: List[Declaration]
}

-- Process a single import, recursively loading transitive imports
fn process_import(imp: ImportDecl, base_dir: string, imports: Imports, verbose: bool) -> Imports {
    let mut state = imports

    -- An import names a file next to the importing file. `std.<module>`
    -- with no such file is a module of the standard library.
    let mut file_path = resolve_import_path(imp.path, base_dir)
    if starts_with(imp.path, "std.") and file_exists(file_path) == false {
        let in_stdlib = stdlib_file(substr(imp.path, 4, len(imp.path) - 4))
        if in_stdlib != "" {
            file_path = in_stdlib
        }
    }

    -- A file is loaded once, however many files import it
    let mut li = 1
    while li < state.loaded.len() {
        if state.loaded[li] == file_path {
            return state
        }
        li = li + 1
    }
    let file = state.loaded.len()
    state.loaded.push(file_path)

    if verbose {
        println("  Loading import: " + file_path)
    }

    -- Check if file exists
    if file_exists(file_path) == false {
        if starts_with(imp.path, "std.") {
            eprintln("Error: import file not found: " + file_path + ", and the standard library has no module '" + imp.path + "' (set DAIMOND_STDLIB to the library's directory)")
        } else {
            eprintln("Error: import file not found: " + file_path)
        }
        exit(1)
    }

    -- Read, tokenize, and parse the imported file
    let source = file_read(file_path)
    let tokens = tokenize_file(source, file_path, file)
    let p = parser_new(tokens)
    let imp_sf = parse_source_file(p)
    if imp_sf.errors.len() > 0 {
        let mut ei = 0
        while ei < imp_sf.errors.len() {
            eprintln(file_path + ": " + imp_sf.errors[ei])
            ei = ei + 1
        }
        exit(1)
    }

    -- Recursively process imports from the imported file
    let import_dir = dirname(file_path)
    let mut k = 0
    while k < imp_sf.imports.len() {
        state = process_import(imp_sf.imports[k], import_dir, state, verbose)
        k = k + 1
    }

    -- Add declarations from the imported file
    let mut m = 0
    while m < imp_sf.declarations.len() {
        state.decls.push(imp_sf.declarations[m])
        m = m + 1
    }

    return state
}

-- ============================================================
-- COMMANDS: run, fmt and test
-- (check is the compilation pipeline, stopped before it builds)
-- ============================================================

-- A path in /tmp for a file of this process's own
fn temp_path(kind: string) -> string {
    let mut pid = 0
    unsafe { pid = getpid() as int }
    return "/tmp/daimond_" + kind + "_" + int_to_string(pid) + "_" + int_to_string(time_ms())
}

-- run: compile the file, run the program with the arguments after --,
-- and give its exit code. The program is deleted afterwards unless -o
-- named it.
fn run_file(cli: CLIArgs) -> int {
    let mut build = cli
    build.command = ""
    if cli.has_output == false {
        build.output_file = temp_path("run")
    }
    let built = compile(build)
    if built != 0 {
        return built
    }
    let mut program = build.output_file
    if string_contains(program, "/") == false {
        program = "./" + program
    }
    let mut command: List[string] = [program]
    let mut ai = 0
    while ai < cli.program_args.len() {
        command.push(cli.program_args[ai])
        ai = ai + 1
    }
    let code = run_command(command)
    if cli.has_output == false {
        fs_remove(build.output_file)
    }
    return code
}

-- Source with every line indented by the braces around it, four spaces
-- a level. Lines are otherwise unchanged, apart from the white space
-- at their ends; braces in strings and comments do not count.
fn format_source(source: string) -> string {
    let lines = string_split(source, "\n")
    -- The pieces of the result, joined at the end (see join_strings)
    let mut pieces: List[string] = []
    let mut level = 0
    let mut li = 0
    while li < lines.len() {
        if li > 0 {
            pieces.push("\n")
        }
        let line = string_trim(lines[li])
        if len(line) > 0 {
            let closes_first = char_at(line, 0) == "}"
            if closes_first and level > 0 {
                level = level - 1
            }
            let mut indent = 0
            while indent < level {
                pieces.push("    ")
                indent = indent + 1
            }
            pieces.push(line)

            -- The braces of this line change the level of the next ones
            let mut in_string = false
            let mut in_comment = false
            let mut prev = ""
            let mut ci = 0
            while ci < len(line) and in_comment == false {
                let ch = char_at(line, ci)
                if ch == "\"" and prev != "\\" {
                    in_string = in_string == false
                }
                let mut starts_comment = false
                if in_string == false {
                    if ch == "-" and prev == "-" {
                        starts_comment = true
                        in_comment = true
                    } else if ch == "{" {
                        level = level + 1
                    } else if ch == "}" and closes_first == false {
                        if level > 0 {
                            level = level - 1
                        }
                    }
                }
                if starts_comment == false {
                    prev = ch
                }
                ci = ci + 1
            }
        }
        li = li + 1
    }
    let out = join_strings(pieces)
    if len(out) > 0 and ends_with(out, "\n") == false {
        return out + "\n"
    }
    return out
}

-- fmt: format the file in place, or into the file -o names
fn format_file(cli: CLIArgs) -> int {
    if file_exists(cli.input_file) == false {
        eprintln("Error: file not found: " + cli.input_file)
        return 1
    }
    let mut target = cli.input_file
    if cli.has_output {
        target = cli.output_file
    }
    file_write(target, format_source(file_read(cli.input_file)))
    println("Formatted: " + target)
    return 0
}

-- The names of a file's tests: its functions named test_..., which
-- take no parameters
fn test_function_names(sf: SourceFile) -> List[string] {
    let mut names: List[string] = []
    let mut i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if starts_with(fd.name, "test_") and fd.params.len() == 0 and fd.is_extern == false {
                names.push(fd.name)
            }
        }
        i = i + 1
    }
    return names
}

-- The source of the main function of a test program. Given a test's
-- name as its argument, the program runs that test. Given none, it
-- runs itself once per test, so that a test that panics ends only its
-- own process, and reports each as PASS or FAIL with the panic's text.
fn test_main_source(names: List[string]) -> string {
    let mut src = "fn dm_test_run(name: string, log: string) -> bool {\n"
    src = src + "    print(\"  test \" + name + \" ... \")\n"
    src = src + "    flush()\n"
    src = src + "    let status = run_command_io([args_get(0), name], \"\", \"\", log)\n"
    src = src + "    if status == 0 {\n        println(\"PASS\")\n        return true\n    }\n"
    src = src + "    let mut message = \"\"\n"
    src = src + "    if file_exists(log) {\n        message = string_trim(file_read(log))\n    }\n"
    src = src + "    if starts_with(message, \"PANIC: \") {\n        message = substr(message, 7, len(message) - 7)\n    }\n"
    src = src + "    if message == \"\" {\n        println(\"FAIL\")\n    } else {\n        println(\"FAIL: \" + message)\n    }\n"
    src = src + "    return false\n}\n"
    src = src + "fn main() -> int {\n"
    src = src + "    if args_len() > 1 {\n        let name = args_get(1)\n"
    let mut i = 0
    while i < names.len() {
        src = src + "        if name == \"" + names[i] + "\" {\n            " + names[i] + "()\n        }\n"
        i = i + 1
    }
    src = src + "        return 0\n    }\n"
    src = src + "    let log = args_get(0) + \".stderr\"\n    let mut passed = 0\n    let mut failed = 0\n"
    i = 0
    while i < names.len() {
        src = src + "    if dm_test_run(\"" + names[i] + "\", log) {\n        passed = passed + 1\n    } else {\n        failed = failed + 1\n    }\n"
        i = i + 1
    }
    src = src + "    fs_remove(log)\n"
    src = src + "    println(\"\")\n"
    src = src + "    println(int_to_string(passed) + \" passed, \" + int_to_string(failed) + \" failed\")\n"
    src = src + "    if failed > 0 {\n        return 1\n    }\n    return 0\n}\n"
    return src
}

-- The file's declarations with a main that runs its tests in place of
-- its own main
fn with_test_main(sf: SourceFile) -> SourceFile {
    let test_sf = parse_source_file(parser_new(tokenize_file(test_main_source(test_function_names(sf)), TEST_MAIN_NAME(), sf.files.len())))
    let mut out = sf
    out.files.push(TEST_MAIN_NAME())
    let mut decls: List[Declaration] = []
    let mut i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        let mut is_main = false
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            is_main = fd.name == "main"
        }
        if is_main == false {
            decls.push(decl)
        }
        i = i + 1
    }
    i = 0
    while i < test_sf.declarations.len() {
        decls.push(test_sf.declarations[i])
        i = i + 1
    }
    out.declarations = decls
    return out
}

-- test: build the file's test program, run it and give its exit code:
-- 0 when every test passes
fn test_file(cli: CLIArgs) -> int {
    if file_exists(cli.input_file) == false {
        eprintln("Error: file not found: " + cli.input_file)
        return 1
    }
    let parsed = parse_source_file(parser_new(tokenize(file_read(cli.input_file))))
    let names = test_function_names(parsed)
    if parsed.errors.len() == 0 and names.len() == 0 {
        println("No test functions found (functions starting with 'test_').")
        return 0
    }
    if parsed.errors.len() == 0 {
        println("Found " + int_to_string(names.len()) + " test(s) in " + cli.input_file)
        println("")
        flush()
    }
    -- Parse errors are reported by the compilation that follows
    let mut build = cli
    build.output_file = temp_path("test")
    let built = compile(build)
    if built != 0 {
        return built
    }
    let code = run_command([build.output_file])
    fs_remove(build.output_file)
    return code
}

-- The text of the program to compile
fn source_text(cli: CLIArgs) -> string {
    if cli.from_stdin {
        return file_read("/dev/stdin")
    }
    return file_read(cli.input_file)
}

-- ============================================================
-- SYMBOLS: the declarations of a file, for editors
-- One line per declaration, its parts separated by tabs:
--   kind  name  line  column  last line  container  detail
-- kind is import, function, struct, field, enum, variant, trait, impl,
-- method or const. Line and column are where the name is, counted from
-- 1. The container is the type a field, variant or method belongs to.
-- The detail is the declaration as its source would start, such as
-- "fn add(a: int, b: int) -> int"; for an import it is the file.
-- ============================================================

-- The index of the first token at or after (line, col), from token
-- `from` on; the last token when there is none
fn token_at(tokens: List[Token], from: int, line: int, col: int) -> int {
    let mut i = from
    while i < tokens.len() {
        let tok = tokens[i]
        if tok.line > line or (tok.line == line and tok.col >= col) {
            return i
        }
        i = i + 1
    }
    return tokens.len() - 1
}

-- The index of the first identifier `name` from token `from` on;
-- `from` when there is none
fn name_token(tokens: List[Token], from: int, name: string) -> int {
    let mut i = from
    while i < tokens.len() {
        if tokens[i].kind == TK_IDENT() and tokens[i].value == name {
            return i
        }
        i = i + 1
    }
    return from
}

-- The last line of the declaration that starts at token `start`: the
-- line its braces close on, or the line it is written on when it has
-- none
fn declaration_end_line(tokens: List[Token], start: int) -> int {
    let mut depth = 0
    let mut last_line = tokens[start].line
    let mut i = start
    while i < tokens.len() {
        let kind = tokens[i].kind
        if kind == TK_EOF() {
            return last_line
        }
        if kind == TK_NEWLINE() {
            if depth == 0 {
                -- The body may open on the next line
                let mut next = i
                while next < tokens.len() and tokens[next].kind == TK_NEWLINE() {
                    next = next + 1
                }
                if next >= tokens.len() or tokens[next].kind != TK_LBRACE() {
                    return last_line
                }
            }
        } else {
            last_line = tokens[i].line
            if kind == TK_LBRACE() or kind == TK_LPAREN() or kind == TK_LBRACKET() {
                depth = depth + 1
            } else if kind == TK_RPAREN() or kind == TK_RBRACKET() {
                depth = depth - 1
            } else if kind == TK_RBRACE() {
                depth = depth - 1
                if depth <= 0 {
                    return last_line
                }
            }
        }
        i = i + 1
    }
    return last_line
}

-- "[T, U]" for the type parameters of a declaration; "" when it has none
fn type_parameters_text(names: List[string]) -> string {
    if names.len() == 0 {
        return ""
    }
    let mut text = "["
    let mut i = 0
    while i < names.len() {
        if i > 0 {
            text = text + ", "
        }
        text = text + names[i]
        i = i + 1
    }
    return text + "]"
}

-- A function as its declaration starts: "fn add(a: int, b: int) -> int"
fn function_signature(fd: FunctionDecl) -> string {
    -- The type parameters with their bounds: [T: Show + Named, U]
    let mut type_params: List[string] = []
    let mut gpi = 0
    while gpi < fd.generic_params.len() {
        let mut one = fd.generic_params[gpi]
        if gpi < fd.generic_bounds.len() {
            if fd.generic_bounds[gpi] != "" {
                one = one + ": " + string_replace(fd.generic_bounds[gpi], "+", " + ")
            }
        }
        type_params.push(one)
        gpi = gpi + 1
    }
    let mut sig = "fn " + fd.name + type_parameters_text(type_params) + "("
    if fd.is_extern {
        sig = "extern " + sig
    }
    let mut i = 0
    while i < fd.params.len() {
        let param = fd.params[i]
        if i > 0 {
            sig = sig + ", "
        }
        if param.is_mut {
            sig = sig + "mut "
        }
        if param.name == "self" {
            sig = sig + "self"
        } else {
            sig = sig + param.name + ": " + param.type_name
        }
        i = i + 1
    }
    if fd.is_variadic {
        sig = sig + ", ..."
    }
    sig = sig + ")"
    if fd.return_type != "void" and fd.return_type != "" {
        sig = sig + " -> " + fd.return_type
    }
    if fd.effects.len() > 0 {
        sig = sig + " with " + type_parameters_text(fd.effects)
    }
    return sig
}

-- Print a declaration. One that the parser read out of a syntax error
-- may have a name that is no name; it is left out.
fn print_symbol(kind: string, name: string, at: Token, end_line: int, container: string, detail: string) {
    if kind != "import" and (at.kind != TK_IDENT() or at.value != name) {
        return
    }
    println(kind + "\t" + name + "\t" + int_to_string(at.line) + "\t" + int_to_string(at.col) + "\t" + int_to_string(end_line) + "\t" + container + "\t" + source_name(detail))
}

-- Print the function whose `fn` is at token `start`
fn print_function_symbol(kind: string, fd: FunctionDecl, container: string, tokens: List[Token], start: int) {
    print_symbol(kind, fd.name, tokens[name_token(tokens, start, fd.name)], declaration_end_line(tokens, start), container, function_signature(fd))
}

-- symbols: print the declarations of the file. A file with syntax
-- errors gives the declarations that could be read.
fn symbols_file(cli: CLIArgs) -> int {
    if cli.from_stdin == false and file_exists(cli.input_file) == false {
        eprintln("Error: file not found: " + cli.input_file)
        return 1
    }
    let tokens = tokenize(source_text(cli))
    let sf = parse_source_file(parser_new(tokens))
    if tokens.len() == 0 {
        return 0
    }
    let base_dir = dirname(cli.input_file)

    let mut i = 0
    while i < sf.imports.len() {
        let imp = sf.imports[i]
        let at = tokens[token_at(tokens, 0, imp.line, imp.col)]
        print_symbol("import", imp.path, at, at.line, "", resolve_import_path(imp.path, base_dir))
        i = i + 1
    }

    -- Declarations are in source order, so each search starts where the
    -- declaration before began
    let mut from = 0
    i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.name != "__error__" {
                from = token_at(tokens, from, fd.line, fd.col)
                print_function_symbol("function", fd, "", tokens, from)
            }
        } else if decl.kind == DECL_STRUCT() {
            let sd = *decl.struct_decl
            from = token_at(tokens, from, sd.line, sd.col)
            let mut at = name_token(tokens, from, sd.name)
            print_symbol("struct", sd.name, tokens[at], declaration_end_line(tokens, from), "", "struct " + sd.name + type_parameters_text(sd.generic_params))
            let mut fi = 0
            while fi < sd.fields.len() {
                let field = sd.fields[fi]
                at = name_token(tokens, at + 1, field.name)
                print_symbol("field", field.name, tokens[at], tokens[at].line, sd.name, field.name + ": " + field.type_name)
                fi = fi + 1
            }
        } else if decl.kind == DECL_ENUM() {
            let ed = *decl.enum_decl
            from = token_at(tokens, from, ed.line, ed.col)
            let mut at = name_token(tokens, from, ed.name)
            print_symbol("enum", ed.name, tokens[at], declaration_end_line(tokens, from), "", "enum " + ed.name + type_parameters_text(ed.generic_params))
            let mut vi = 0
            while vi < ed.variants.len() {
                let variant = ed.variants[vi]
                at = name_token(tokens, at + 1, variant.name)
                let mut detail = variant.name
                if variant.has_payload {
                    let mut payload = ""
                    let mut pi = 0
                    while pi < variant.payload_types.len() {
                        if pi > 0 {
                            payload = payload + ", "
                        }
                        payload = payload + variant.payload_types[pi]
                        pi = pi + 1
                    }
                    detail = detail + "(" + payload + ")"
                }
                print_symbol("variant", variant.name, tokens[at], tokens[at].line, ed.name, detail)
                vi = vi + 1
            }
        } else if decl.kind == DECL_TRAIT() {
            let td = *decl.trait_decl
            from = token_at(tokens, from, td.line, td.col)
            print_symbol("trait", td.name, tokens[name_token(tokens, from, td.name)], declaration_end_line(tokens, from), "", "trait " + td.name + type_parameters_text(td.generic_params))
            let mut mi = 0
            while mi < td.methods.len() {
                let method = td.methods[mi]
                print_function_symbol("method", method, td.name, tokens, token_at(tokens, from, method.line, method.col))
                mi = mi + 1
            }
        } else if decl.kind == DECL_IMPL() {
            let ib = *decl.impl_block
            from = token_at(tokens, from, ib.line, ib.col)
            let mut detail = "impl " + ib.target_type + type_parameters_text(ib.generic_params)
            if ib.trait_name != "" {
                detail = "impl " + ib.trait_name + " for " + ib.target_type
            }
            print_symbol("impl", ib.target_type, tokens[name_token(tokens, from, ib.target_type)], declaration_end_line(tokens, from), "", detail)
            let mut mi = 0
            while mi < ib.methods.len() {
                let method = ib.methods[mi]
                print_function_symbol("method", method, ib.target_type, tokens, token_at(tokens, from, method.line, method.col))
                mi = mi + 1
            }
        } else if decl.kind == DECL_CONST() {
            let cd = *decl.const_decl
            from = token_at(tokens, from, cd.line, cd.col)
            let mut detail = "const " + cd.name
            if cd.type_name != "" {
                detail = detail + ": " + cd.type_name
            }
            print_symbol("const", cd.name, tokens[name_token(tokens, from, cd.name)], declaration_end_line(tokens, from), "", detail)
        }
        i = i + 1
    }
    return 0
}

-- ============================================================
-- COMPILATION PIPELINE
-- ============================================================

fn compile(cli: CLIArgs) -> int {
    let input_file = cli.input_file
    let output_file = cli.output_file
    let opt_level = cli.opt_level
    let verbose = cli.verbose

    if verbose {
        println("dAImond Compiler (LLVM backend)")
        println("Input:  " + input_file)
        println("Output: " + output_file)
    }

    -- Phase 1: Read source file
    if verbose { println("  [1/6] Reading source...") }

    if cli.from_stdin == false and file_exists(input_file) == false {
        eprintln("Error: file not found: " + input_file)
        return 1
    }

    let source = source_text(cli)

    -- Phase 2: Tokenize
    if verbose { println("  [2/6] Lexing...") }

    let tokens = tokenize(source)

    -- Phase 3: Parse
    if verbose { println("  [3/6] Parsing...") }

    let p = parser_new(tokens)
    let mut sf = parse_source_file(p)

    -- Check for parse errors
    if sf.errors.len() > 0 {
        let mut ei = 0
        while ei < sf.errors.len() {
            eprintln(sf.errors[ei])
            ei = ei + 1
        }
        exit(1)
    }

    -- Post-parse validation
    let validation_errors = validate_source_file(sf)
    if validation_errors.len() > 0 {
        let mut vi = 0
        while vi < validation_errors.len() {
            eprintln(validation_errors[vi])
            vi = vi + 1
        }
        exit(1)
    }

    -- Phase 3.5: Resolve imports
    sf.files.push(input_file)
    if sf.imports.len() > 0 {
        if verbose { println("  [3.5/6] Resolving imports...") }
        let input_dir = dirname(input_file)
        sf = resolve_imports(sf, input_dir, verbose)
    }
    -- A name declared twice, in the file or in two of the files it is
    -- made of
    let duplicate_errors = duplicate_declarations(sf)
    if duplicate_errors.len() > 0 {
        let mut di2 = 0
        while di2 < duplicate_errors.len() {
            eprintln(duplicate_errors[di2])
            di2 = di2 + 1
        }
        exit(1)
    }
    -- What the program's traits stand for (see TRAITS)
    sf = complete_traits(sf)
    -- --link-llvm: the program is the compiler, which needs the LLVM bridge
    if cli.link_llvm {
        let bridge_file = find_bridge_file(dirname(input_file))
        if bridge_file == "" {
            eprintln("Error: --link-llvm needs llvm_bridge.dm, which was not found next to " + input_file)
            return 1
        }
        if verbose { println("  LLVM bridge: " + bridge_file) }
        sf = add_llvm_bridge(sf, parse_llvm_bridge(bridge_file, sf.files.len()))
        sf.files.push(bridge_file)
    }
    -- test: a main that runs the file's tests takes the place of its own
    if cli.command == "test" {
        sf = with_test_main(sf)
    }
    let prelude_sf = parse_prelude(sf.files.len())
    sf.files.push(PRELUDE_NAME())
    sf = add_prelude(sf, prelude_sf)

    -- Names, types and effects (check.dm). The types found go to IR
    -- generation.
    let checked = check_program(sf)
    let check_errors = checked.errors
    if check_errors.len() > 0 {
        let mut ce = 0
        while ce < check_errors.len() {
            eprintln(check_errors[ce])
            ce = ce + 1
        }
        eprintln("Error: " + int_to_string(check_errors.len()) + " type errors")
        return 1
    }

    -- Phase 4: Generate IR
    if verbose { println("  [4/6] Generating IR...") }

    -- A -O0 binary takes the runtime's functions from the precompiled
    -- runtime object (see PRECOMPILED RUNTIME). --emit-ir, --emit-llvm
    -- and --emit-bitcode give the whole program, runtime included.
    let mut runtime_obj = ""
    if opt_level == 0 and cli.emit_ir == false and cli.emit_llvm == false and cli.emit_bitcode == false and cli.command != "check" {
        runtime_obj = find_or_build_runtime_object(prelude_sf, checked, verbose)
    }

    let mut ir_gen = ir_generator_new()
    if runtime_obj != "" {
        ir_gen.linked_fns = precompiled_function_names(prelude_sf)
    }
    ir_gen.debug_info = cli.debug_info
    ir_gen.checked_types = checked.types
    ir_gen.checked_type_args = checked.type_args
    ir_gen.checked_targets = checked.targets
    ir_gen.checker = checked.checker
    ir_gen.has_checker = true
    ir_gen = generate_module(ir_gen, sf)
    if ir_gen.type_errors.len() > 0 {
        let mut te = 0
        while te < ir_gen.type_errors.len() {
            eprintln(ir_gen.type_errors[te])
            te = te + 1
        }
        eprintln("Error: " + int_to_string(ir_gen.type_errors.len()) + " type errors")
        return 1
    }

    -- If --emit-ir, print the IR module and exit
    if cli.emit_ir {
        -- Print a summary of the IR module (functions and their basic blocks)
        println("dAImond IR Module:")
        println("  Functions: " + int_to_string(ir_gen.ir_mod.functions.len()))
        println("  Globals:   " + int_to_string(ir_gen.ir_mod.globals.len()))
        let mut fi = 0
        while fi < ir_gen.ir_mod.functions.len() {
            let func = ir_gen.ir_mod.functions[fi]
            let mut sig = "  fn " + func.name + "("
            let mut pi = 0
            while pi < func.params.len() {
                if pi > 0 { sig = sig + ", " }
                sig = sig + func.params[pi].name + ": " + func.params[pi].type_id
                pi = pi + 1
            }
            sig = sig + ") -> " + func.return_type
            if func.is_extern { sig = sig + " [extern]" }
            println(sig)
            let mut bi = 0
            while bi < func.blocks.len() {
                let block = func.blocks[bi]
                println("    " + block.label + ": (" + int_to_string(block.instructions.len()) + " instructions)")
                bi = bi + 1
            }
            fi = fi + 1
        }
        return 0
    }

    -- A binary needs an entry point
    let mut has_main = false
    let mut di = 0
    while di < sf.declarations.len() {
        let decl = sf.declarations[di]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.name == "main" {
                has_main = true
            }
        }
        di = di + 1
    }
    -- check takes a file that is only imported by others, too
    if has_main == false and cli.command != "check" {
        eprintln("Error: no main function found")
        exit(1)
    }

    -- Phase 5: Generate LLVM IR
    if verbose { println("  [5/6] Generating LLVM IR...") }

    let mut lg = llvm_gen_new(ir_gen.ir_mod, "daimond_module")
    lg.debug_info = cli.debug_info
    lg.debug_optimized = opt_level > 0
    lg = lg_generate_module(lg)

    -- Calls with mismatched argument types and field reads from non-structs
    -- mean IR generation failed to type an expression; report them at their
    -- source lines instead of handing invalid IR to the verifier.
    if lg.type_errors.len() > 0 {
        let mut ti = 0
        while ti < lg.type_errors.len() {
            eprintln(lg.type_errors[ti])
            ti = ti + 1
        }
        eprintln("Error: " + int_to_string(lg.type_errors.len()) + " type errors")
        llvm_gen_dispose(lg)
        return 1
    }

    -- Verify LLVM module (fatal: invalid IR must not produce any output;
    -- dm_llvm_module_verify prints the detailed diagnostic)
    let verify_result = verify_module(lg)
    if verify_result != 0 {
        eprintln("Error: LLVM module verification failed")
        llvm_gen_dispose(lg)
        return 1
    }

    -- check: the file has no errors, and nothing is built
    if cli.command == "check" {
        llvm_gen_dispose(lg)
        println("No errors found in " + input_file)
        return 0
    }

    -- If --emit-llvm, emit LLVM IR file and exit
    if cli.emit_llvm {
        let ll_file = output_file + ".ll"
        let ir_result = emit_to_ir_file(lg, ll_file)
        if ir_result != 0 {
            eprintln("Error: failed to emit LLVM IR to " + ll_file)
            llvm_gen_dispose(lg)
            return 1
        }
        if verbose {
            println("  Emitted LLVM IR: " + ll_file)
        } else {
            println(ll_file)
        }
        llvm_gen_dispose(lg)
        return 0
    }

    -- If --emit-bitcode, emit the module as it goes to code generation
    -- and exit
    if cli.emit_bitcode {
        let bc_file = output_file + ".bc"
        let bc_result = emit_to_bitcode(lg, bc_file, opt_level)
        llvm_gen_dispose(lg)
        if bc_result != 0 {
            eprintln("Error: failed to emit LLVM bitcode to " + bc_file)
            return 1
        }
        if verbose {
            println("  Emitted LLVM bitcode: " + bc_file)
        } else {
            println(bc_file)
        }
        return 0
    }

    -- Phase 6: Emit object file and link
    if verbose { println("  [6/6] Emitting binary...") }

    let obj_file = output_file + ".o"
    let emit_result = emit_to_object(lg, obj_file, opt_level)
    if emit_result != 0 {
        eprintln("Error: failed to emit object file")
        llvm_gen_dispose(lg)
        return 1
    }

    -- Dispose LLVM resources before linking
    llvm_gen_dispose(lg)

    -- Link: ld.lld puts the program's object between the C library's
    -- start-up objects and resolves the rest against the C library
    let linker = find_linker()
    if linker == "" {
        eprintln("Error: the linker ld.lld was not found on PATH or in /usr/lib/llvm-*/bin; install lld, or set DAIMOND_LD to its path")
        fs_remove(obj_file)
        return 1
    }
    let libc_dir = find_libc_dir()
    if libc_dir == "" {
        eprintln("Error: the C library's start-up object Scrt1.o was not found; install the C library's development files (libc6-dev or glibc-devel), or set DAIMOND_LIBC_DIR to their directory")
        fs_remove(obj_file)
        return 1
    }
    let mut link: List[string] = [linker, "-pie", "--eh-frame-hdr", "-dynamic-linker", dynamic_loader(), "-o", output_file]
    link.push(libc_dir + "/Scrt1.o")
    link.push(libc_dir + "/crti.o")
    link.push(obj_file)
    if runtime_obj != "" {
        link.push(runtime_obj)
    }
    link.push("-L" + libc_dir)

    -- --link-llvm: the LLVM library
    if cli.link_llvm {
        let llvm_library = find_llvm_library(linker)
        if llvm_library == "" {
            eprintln("Error: the LLVM library libLLVM.so was not found next to " + linker + "; install LLVM's development files, or set DAIMOND_LLVM_LIB to the library's path")
            fs_remove(obj_file)
            return 1
        }
        link.push(llvm_library)
    }

    -- The C library, and its last start-up object
    link.push("-lm")
    link.push("-lpthread")
    link.push("-lc")
    link.push(libc_dir + "/crtn.o")

    if verbose {
        let mut shown = "  Link:"
        let mut li = 0
        while li < link.len() {
            shown = shown + " " + link[li]
            li = li + 1
        }
        println(shown)
    }

    -- The linker is started as a program of its own, with no shell
    let link_result = run_command(link)

    -- Remove the temporary object file
    fs_remove(obj_file)

    if link_result != 0 {
        eprintln("Error: linking failed (exit code " + int_to_string(link_result) + ")")
        return 1
    }

    if verbose {
        println("Successfully compiled: " + output_file)
    }

    return 0
}

-- ============================================================
-- ENTRY POINT
-- ============================================================

fn main() -> int {
    let cli = parse_cli_args()

    if cli.show_help {
        print_usage()
        return 0
    }

    if cli.show_version {
        println("daimond-llvm " + VERSION())
        return 0
    }

    if cli.has_error {
        eprintln(cli.error_msg)
        return 1
    }

    if cli.command == "fmt" {
        return format_file(cli)
    }
    if cli.command == "run" {
        return run_file(cli)
    }
    if cli.command == "test" {
        return test_file(cli)
    }
    if cli.command == "symbols" {
        return symbols_file(cli)
    }
    return compile(cli)
}

-- ============================================================
-- CHECKS OF DECLARATIONS
-- ============================================================

-- An error for each function, type or constant that is declared more
-- than once. Functions are one set of names; structs, enums and traits
-- another; constants a third. An extern fn may be declared in several
-- files.
fn duplicate_declarations(sf: SourceFile) -> List[string] {
    let mut errors: List[string] = []
    let mut functions: Map[string, int] = Map_new()
    let mut types: Map[string, int] = Map_new()
    let mut constants: Map[string, int] = Map_new()
    let mut i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        let mut what = ""
        let mut name = ""
        let mut line = 0
        let mut first_line = -1
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.is_extern == false and fd.name != "__error__" {
                what = "function"
                name = fd.name
                line = fd.line
                if functions.contains(name) {
                    first_line = functions.get(name)
                } else {
                    functions.insert(name, line)
                }
            }
        } else if decl.kind == DECL_STRUCT() or decl.kind == DECL_ENUM() or decl.kind == DECL_TRAIT() {
            what = "type"
            if decl.kind == DECL_STRUCT() {
                let sd = *decl.struct_decl
                name = sd.name
                line = sd.line
            } else if decl.kind == DECL_ENUM() {
                let ed = *decl.enum_decl
                name = ed.name
                line = ed.line
            } else {
                let td = *decl.trait_decl
                name = td.name
                line = td.line
            }
            if types.contains(name) {
                first_line = types.get(name)
            } else {
                types.insert(name, line)
            }
        } else if decl.kind == DECL_CONST() {
            let cd = *decl.const_decl
            what = "constant"
            name = cd.name
            line = cd.line
            if constants.contains(name) {
                first_line = constants.get(name)
            } else {
                constants.insert(name, line)
            }
        }
        if first_line >= 0 {
            errors.push(error_at(sf.files, what + " '" + name + "' is declared more than once (first at " + line_ref(sf.files, first_line, line) + ")", line))
        }
        -- A call by a builtin's name is always the builtin's, so a
        -- function of that name could never be called
        if decl.kind == DECL_FUNCTION() {
            let named = *decl.func_decl
            if is_builtin_fn(named.name) or named.name == "Some" or named.name == "Ok" or named.name == "Err" {
                if named.is_extern {
                    errors.push(error_at(sf.files, "'" + named.name + "' is a builtin function; give the extern another name and bind it to the C symbol: extern fn c_" + named.name + "(...) = \"" + named.name + "\"", named.line))
                } else {
                    errors.push(error_at(sf.files, "'" + named.name + "' is a builtin function; give this function another name", named.line))
                }
            }
        }
        i = i + 1
    }
    return errors
}

fn validate_source_file(sf: SourceFile) -> List[string] {
    let mut errors: List[string] = []
    let mut i = 0
    while i < sf.declarations.len() {
        let decl = sf.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            -- Check for params with empty type (sign of a parse error)
            let mut pi = 0
            while pi < fd.params.len() {
                let param = fd.params[pi]
                if param.name != "self" and param.name != "mut self" and len(param.type_name) == 0 {
                    errors.push("Error: parameter '" + param.name + "' in function '" + fd.name + "' has no type")
                }
                pi = pi + 1
            }
        }
        i = i + 1
    }
    return errors
}
