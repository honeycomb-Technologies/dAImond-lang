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
import llvm
import llvm_gen
import llvm_gen_call
import llvm_gen_simd

-- ============================================================
-- dAImond Stage 4 Compiler - LLVM Backend (Self-Hosted)
--
-- Compiles dAImond source to native binaries via LLVM.
-- Uses its own frontend (lexer.dm, parser.dm) instead of
-- reusing Stage 0's Zig frontend.
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
    println("dAImond Stage 4 Compiler (LLVM Backend)")
    println("")
    println("Usage: daimond-llvm <file.dm> [options]")
    println("")
    println("Options:")
    println("  -o <file>     Output binary name (default: input without .dm)")
    println("  -O0           No optimization (default)")
    println("  -O1           Basic optimization")
    println("  -O2           Full optimization")
    println("  -O3           Aggressive optimization")
    println("  --emit-ir     Print dAImond IR and exit")
    println("  --emit-llvm   Emit LLVM IR file (.ll) and exit")
    println("  -v, --verbose Verbose output")
    println("  -h, --help    Show this help")
    println("  --version     Show version")
}

-- ============================================================
-- CLI ARGUMENT PARSING
-- ============================================================

struct CLIArgs {
    input_file: string,
    output_file: string,
    opt_level: int,
    emit_ir: bool,
    emit_llvm: bool,
    verbose: bool,
    show_help: bool,
    show_version: bool,
    has_error: bool,
    error_msg: string
}

fn parse_cli_args() -> CLIArgs {
    let mut args = CLIArgs {
        input_file: "",
        output_file: "",
        opt_level: 0,
        emit_ir: false,
        emit_llvm: false,
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

        if arg == "-h" or arg == "--help" {
            args.show_help = true
            return args
        } else if arg == "--version" {
            args.show_version = true
            return args
        } else if arg == "-o" {
            if i + 1 < argc {
                i = i + 1
                args.output_file = args_get(i)
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
        } else if arg == "--emit-ir" {
            args.emit_ir = true
        } else if arg == "--emit-llvm" {
            args.emit_llvm = true
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

-- Find the project root by looking for stage0/runtime/daimond_runtime.c
-- relative to the executable's working directory. Tries several strategies:
--   1. Relative to input file directory: ../stage0/runtime/
--   2. CWD-relative: ../stage0/runtime/ (if running from stage4/)
--   3. CWD-relative: stage0/runtime/ (if running from project root)
fn find_runtime_dir(input_dir: string) -> string {
    -- Strategy 1: Go up from input file dir
    let try1 = input_dir + "/../stage0/runtime"
    if file_exists(try1 + "/daimond_runtime.c") {
        return try1
    }

    -- Strategy 2: Relative (assume running from stage4/)
    if file_exists("../stage0/runtime/daimond_runtime.c") {
        return "../stage0/runtime"
    }

    -- Strategy 3: From project root
    if file_exists("stage0/runtime/daimond_runtime.c") {
        return "stage0/runtime"
    }

    -- Strategy 4: Absolute fallback (common development path)
    return "../stage0/runtime"
}

-- Find the Stage 3 runtime (llvm_wrappers.c) directory
fn find_wrappers_dir(input_dir: string) -> string {
    -- Strategy 1: Go up from input file dir
    let try1 = input_dir + "/../stage3/runtime"
    if file_exists(try1 + "/llvm_wrappers.c") {
        return try1
    }

    -- Also check stage4/runtime which may have its own copy
    let try1b = input_dir + "/../stage4/runtime"
    if file_exists(try1b + "/llvm_wrappers.c") {
        return try1b
    }

    -- Strategy 2: Relative (assume running from stage4/)
    if file_exists("runtime/llvm_wrappers.c") {
        return "runtime"
    }
    if file_exists("../stage3/runtime/llvm_wrappers.c") {
        return "../stage3/runtime"
    }

    -- Strategy 3: From project root
    if file_exists("stage4/runtime/llvm_wrappers.c") {
        return "stage4/runtime"
    }
    if file_exists("stage3/runtime/llvm_wrappers.c") {
        return "stage3/runtime"
    }

    -- Fallback
    return "runtime"
}

-- ============================================================
-- IMPORT RESOLUTION
-- ============================================================

-- Resolve imports: read each imported file, tokenize, parse,
-- and merge declarations into the main source file.
-- Handles transitive and diamond imports via deduplication.
fn resolve_imports(sf: SourceFile, input_dir: string, verbose: bool) -> SourceFile {
    let mut merged = source_file_new(sf.module_name)
    let mut loaded: Map[string, bool] = Map_new()
    let mut decls: List[Declaration] = []

    -- Process each import
    let mut i = 0
    while i < sf.imports.len() {
        let imp = sf.imports[i]
        decls = process_import(imp, input_dir, loaded, decls, verbose)
        i = i + 1
    }

    -- Add the original file's declarations after imported ones
    let mut j = 0
    while j < sf.declarations.len() {
        decls.push(sf.declarations[j])
        j = j + 1
    }

    merged.declarations = decls
    return merged
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

-- Process a single import, recursively loading transitive imports
fn process_import(imp: ImportDecl, base_dir: string, loaded: Map[string, bool], decls: List[Declaration], verbose: bool) -> List[Declaration] {
    let mut result_decls = decls

    let file_path = resolve_import_path(imp.path, base_dir)

    -- Skip already-loaded imports (diamond import deduplication)
    if loaded.contains(file_path) {
        return result_decls
    }
    loaded.insert(file_path, true)

    if verbose {
        println("  Loading import: " + file_path)
    }

    -- Check if file exists
    if file_exists(file_path) == false {
        eprintln("Error: import file not found: " + file_path)
        return result_decls
    }

    -- Read, tokenize, and parse the imported file
    let source = file_read(file_path)
    let tokens = tokenize(source)
    let p = parser_new(tokens)
    let imp_sf = parse_source_file(p)

    -- Recursively process imports from the imported file
    let import_dir = dirname(file_path)
    let mut k = 0
    while k < imp_sf.imports.len() {
        result_decls = process_import(imp_sf.imports[k], import_dir, loaded, result_decls, verbose)
        k = k + 1
    }

    -- Add declarations from the imported file
    let mut m = 0
    while m < imp_sf.declarations.len() {
        result_decls.push(imp_sf.declarations[m])
        m = m + 1
    }

    return result_decls
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
        println("dAImond Stage 4 Compiler (LLVM Backend)")
        println("Input:  " + input_file)
        println("Output: " + output_file)
    }

    -- Phase 1: Read source file
    if verbose { println("  [1/6] Reading source...") }

    if file_exists(input_file) == false {
        eprintln("Error: file not found: " + input_file)
        return 1
    }

    let source = file_read(input_file)

    -- Phase 2: Tokenize
    if verbose { println("  [2/6] Lexing...") }

    let tokens = tokenize(source)

    -- Phase 3: Parse
    if verbose { println("  [3/6] Parsing...") }

    let p = parser_new(tokens)
    let mut sf = parse_source_file(p)

    -- Check for parse errors
    if p.errors.len() > 0 {
        let mut ei = 0
        while ei < p.errors.len() {
            eprintln(p.errors[ei])
            ei = ei + 1
        }
        return 1
    }

    -- Phase 3.5: Resolve imports
    if sf.imports.len() > 0 {
        if verbose { println("  [3.5/6] Resolving imports...") }
        let input_dir = dirname(input_file)
        sf = resolve_imports(sf, input_dir, verbose)
    }

    -- Phase 4: Generate IR
    if verbose { println("  [4/6] Generating IR...") }

    let mut ir_gen = ir_generator_new()
    ir_gen = generate_module(ir_gen, sf)

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

    -- Phase 5: Generate LLVM IR
    if verbose { println("  [5/6] Generating LLVM IR...") }

    let mut lg = llvm_gen_new(ir_gen.ir_mod, "daimond_module")
    lg = lg_generate_module(lg)

    -- Verify LLVM module
    let verify_result = verify_module(lg)
    if verify_result != 0 {
        eprintln("Error: LLVM module verification failed")
        llvm_gen_dispose(lg)
        return 1
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

    -- Find runtime and wrapper source paths
    let input_dir = dirname(input_file)
    let runtime_dir = find_runtime_dir(input_dir)
    let wrappers_dir = find_wrappers_dir(input_dir)

    let runtime_c = runtime_dir + "/daimond_runtime.c"
    let runtime_h_dir = runtime_dir
    let wrappers_c = wrappers_dir + "/llvm_wrappers.c"

    -- Build the link command
    -- cc -o <output> <obj> <runtime.c> <wrappers.c> -I<runtime_dir> -lm -lpthread
    let mut link_cmd = "cc"
    link_cmd = link_cmd + " -o " + output_file
    link_cmd = link_cmd + " " + obj_file

    -- Add runtime source (compile inline)
    if file_exists(runtime_c) {
        link_cmd = link_cmd + " " + runtime_c
    } else {
        eprintln("Warning: runtime not found at " + runtime_c)
    }

    -- Add ABI wrappers source
    if file_exists(wrappers_c) {
        link_cmd = link_cmd + " " + wrappers_c
    } else {
        eprintln("Warning: LLVM wrappers not found at " + wrappers_c)
    }

    -- Include path for runtime headers
    link_cmd = link_cmd + " -I" + runtime_h_dir

    -- Link system libraries
    link_cmd = link_cmd + " -lm -lpthread"

    if verbose {
        println("  Link: " + link_cmd)
    }

    let link_result = system(link_cmd)
    if link_result != 0 {
        eprintln("Error: linking failed (exit code " + int_to_string(link_result) + ")")
        -- Clean up object file on failure
        system("rm -f " + obj_file)
        return 1
    }

    -- Clean up temporary object file
    system("rm -f " + obj_file)

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

    return compile(cli)
}
