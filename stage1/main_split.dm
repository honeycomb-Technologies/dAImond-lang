module main

import token
import lexer
import compiler
import compile_expr
import compile_stmt
import compile_match
import compile_decl
import runtime
import imports
import package

-- ============================================================
-- MAIN ENTRY POINT
-- ============================================================

fn print_help() {
    println("dAImond Stage 1 Compiler v0.2.0")
    println("Usage: daimond1 [command] <file.dm> [options]")
    println("")
    println("Commands:")
    println("  <file.dm>          Compile a dAImond source file")
    println("  run <file.dm>      Compile and run")
    println("  compile <file.dm>  Compile (same as no command)")
    println("  lex <file.dm>      Show tokens")
    println("  parse <file.dm>    Show declarations")
    println("  check <file.dm>    Type check only")
    println("  fmt <file.dm>      Format source code")
    println("  test <file.dm>     Run test_* functions")
    println("  pkg init           Create daimond.toml manifest")
    println("  pkg add <name>     Add a dependency")
    println("  pkg list           List dependencies")
    println("  clean              Remove generated files")
    println("")
    println("Options:")
    println("  -o <file>       Output file path")
    println("  -c              Compile to C only (no binary)")
    println("  --emit-c        Emit C code alongside binary")
    println("  -v, --verbose   Verbose output")
    println("  --version       Show version")
    println("  -h, --help      Show this help")
}

fn main() {
    let argc = args_len()
    if argc < 2 {
        print_help()
        exit(1)
    }

    -- Handle clean command early (no filename needed)
    if args_get(1) == "clean" {
        println("Clean: no compilation cache in Stage 1")
        exit(0)
    }

    -- Handle pkg commands early (before filename parsing)
    if args_get(1) == "pkg" {
        if argc < 3 {
            println("Usage: daimond1 pkg <command>")
            println("  pkg init        Create daimond.toml manifest")
            println("  pkg add <name>  Add a dependency")
            println("  pkg list        List dependencies")
            exit(1)
        }
        let pkg_cmd = args_get(2)
        if pkg_cmd == "init" {
            pkg_init()
            exit(0)
        }
        if pkg_cmd == "add" {
            if argc < 4 {
                eprintln("Error: 'pkg add' requires a package name")
                exit(1)
            }
            pkg_add(args_get(3))
            exit(0)
        }
        if pkg_cmd == "list" {
            pkg_list()
            exit(0)
        }
        eprintln("Error: unknown pkg command '" + pkg_cmd + "'")
        exit(1)
    }

    -- Parse arguments
    let mut filename = ""
    let mut command = "compile"
    let mut do_run = false
    let mut output_file = ""
    let mut c_only = false
    let mut emit_c = false
    let mut verbose = false
    let mut i = 1
    while i < argc {
        let arg = args_get(i)
        if arg == "--version" {
            println("dAImond Stage 1 Compiler v0.2.0")
            exit(0)
        }
        if arg == "-h" or arg == "--help" {
            print_help()
            exit(0)
        }
        if arg == "run" and filename == "" {
            command = "run"
            do_run = true
            i = i + 1
            continue
        }
        if arg == "compile" and filename == "" {
            command = "compile"
            i = i + 1
            continue
        }
        if arg == "lex" and filename == "" {
            command = "lex"
            i = i + 1
            continue
        }
        if arg == "parse" and filename == "" {
            command = "parse"
            i = i + 1
            continue
        }
        if arg == "check" and filename == "" {
            command = "check"
            i = i + 1
            continue
        }
        if arg == "fmt" and filename == "" {
            command = "fmt"
            i = i + 1
            continue
        }
        if arg == "test" and filename == "" {
            command = "test"
            i = i + 1
            continue
        }
        if arg == "-o" and i + 1 < argc {
            i = i + 1
            output_file = args_get(i)
            i = i + 1
            continue
        }
        if arg == "-c" {
            c_only = true
            i = i + 1
            continue
        }
        if arg == "--emit-c" {
            emit_c = true
            i = i + 1
            continue
        }
        if arg == "-v" or arg == "--verbose" {
            verbose = true
            i = i + 1
            continue
        }
        if filename == "" {
            filename = arg
        }
        i = i + 1
    }

    if filename == "" {
        eprintln("Error: no input file specified")
        exit(1)
    }

    -- Read source
    let raw_source = file_read(filename)
    if verbose {
        println("Reading: " + filename)
    }

    -- Resolve imports (concatenate imported files before tokenizing)
    let source = resolve_imports(raw_source, filename)

    -- ============================================================
    -- Command: lex - Show tokens
    -- ============================================================
    if command == "lex" {
        let tokens = tokenize(source)
        let mut ti = 0
        while ti < tokens.len() {
            let t = tokens[ti]
            if t.kind != TK_NEWLINE() and t.kind != TK_EOF() {
                println(int_to_string(t.line) + ":" + int_to_string(t.col) + " " + token_kind_name(t.kind) + " " + t.value)
            }
            ti = ti + 1
        }
        return
    }

    -- ============================================================
    -- Command: parse - Show declarations
    -- ============================================================
    if command == "parse" {
        let tokens = tokenize(source)
        let mut ti = 0
        while ti < tokens.len() {
            let t = tokens[ti]
            if t.kind == TK_FN() {
                let mut fname = "<anonymous>"
                if ti + 1 < tokens.len() {
                    let fname_tok = tokens[ti + 1]
                    fname = fname_tok.value
                }
                println("fn " + fname + " at line " + int_to_string(t.line))
            }
            if t.kind == TK_STRUCT() {
                let mut sname = "<anonymous>"
                if ti + 1 < tokens.len() {
                    let stok = tokens[ti + 1]
                    sname = stok.value
                }
                println("struct " + sname + " at line " + int_to_string(t.line))
            }
            if t.kind == TK_ENUM() {
                let mut ename = "<anonymous>"
                if ti + 1 < tokens.len() {
                    let etok = tokens[ti + 1]
                    ename = etok.value
                }
                println("enum " + ename + " at line " + int_to_string(t.line))
            }
            if t.kind == TK_TRAIT() {
                let mut tname = "<anonymous>"
                if ti + 1 < tokens.len() {
                    let ttok = tokens[ti + 1]
                    tname = ttok.value
                }
                println("trait " + tname + " at line " + int_to_string(t.line))
            }
            if t.kind == TK_IMPL() {
                println("impl block at line " + int_to_string(t.line))
            }
            if t.kind == TK_IMPORT() {
                let mut iname = ""
                if ti + 1 < tokens.len() {
                    let itok = tokens[ti + 1]
                    iname = itok.value
                }
                println("import " + iname + " at line " + int_to_string(t.line))
            }
            ti = ti + 1
        }
        return
    }

    -- ============================================================
    -- Command: fmt - Format source code
    -- ============================================================
    if command == "fmt" {
        let mut formatted = ""
        let mut indent = 0
        let lines = string_split(source, "\n")
        let mut li = 0
        while li < lines.len() {
            let line = string_trim(lines[li])
            if line == "" {
                formatted = formatted + "\n"
            } else {
                -- Dedent for lines starting with }
                if starts_with(line, "}") {
                    indent = indent - 1
                    if indent < 0 {
                        indent = 0
                    }
                }
                let mut ind = ""
                let mut ii = 0
                while ii < indent {
                    ind = ind + "    "
                    ii = ii + 1
                }
                formatted = formatted + ind + line + "\n"
                -- Indent after lines ending with {
                if ends_with(line, "{") {
                    indent = indent + 1
                }
            }
            li = li + 1
        }
        file_write(filename, formatted)
        println("Formatted: " + filename)
        return
    }

    -- Tokenize
    let tokens = tokenize(source)
    if verbose {
        println("Tokens: " + int_to_string(tokens.len()))
    }

    -- ============================================================
    -- Command: check - Type check only (compile but don't emit binary)
    -- ============================================================
    if command == "check" {
        let mut comp = compiler_new(tokens)
        comp = compile_source(comp)
        if comp.errors.len() > 0 {
            let mut ei = 0
            while ei < comp.errors.len() {
                eprintln(comp.errors[ei])
                ei = ei + 1
            }
            exit(1)
        }
        println("OK: no errors found")
        return
    }

    -- ============================================================
    -- Command: test - Run test_* functions
    -- ============================================================
    if command == "test" {
        -- Find test_* functions by scanning tokens
        let mut test_names: List[string] = []
        let mut ti = 0
        while ti < tokens.len() {
            let test_tok = tokens[ti]
            if test_tok.kind == TK_FN() and ti + 1 < tokens.len() {
                let fname_tok = tokens[ti + 1]
                let fname = fname_tok.value
                if starts_with(fname, "test_") {
                    test_names.push(fname)
                }
            }
            ti = ti + 1
        }
        if test_names.len() == 0 {
            println("No test functions found")
            return
        }
        println("Found " + int_to_string(test_names.len()) + " test(s)")
        -- Compile the file normally then run it
        -- (test functions use assert/assert_eq internally)
        let mut comp = compiler_new(tokens)
        comp = compile_source(comp)
        if comp.errors.len() > 0 {
            eprintln("Compilation errors:")
            let mut ei = 0
            while ei < comp.errors.len() {
                eprintln(comp.errors[ei])
                ei = ei + 1
            }
            exit(1)
        }
        let c_code = assemble_output(comp)
        let c_file = string_replace(filename, ".dm", ".c")
        let bin_file = string_replace(filename, ".dm", "")
        file_write(c_file, c_code)
        let compile_cmd = "cc -o " + bin_file + " " + c_file + " -lm"
        let cc_exit = system(compile_cmd)
        if cc_exit != 0 {
            eprintln("C compilation failed")
            exit(1)
        }
        let run_exit = system("./" + bin_file)
        if run_exit != 0 {
            eprintln("Tests failed")
            exit(1)
        }
        -- Print results
        let mut ti2 = 0
        while ti2 < test_names.len() {
            println("  PASS: " + test_names[ti2])
            ti2 = ti2 + 1
        }
        println(int_to_string(test_names.len()) + " test(s) passed")
        return
    }

    -- ============================================================
    -- Command: compile / run - Full compilation pipeline
    -- ============================================================

    -- Compile
    let mut comp = compiler_new(tokens)
    comp = compile_source(comp)

    -- Check for errors
    if comp.errors.len() > 0 {
        eprintln("Compilation errors:")
        let mut ei = 0
        while ei < comp.errors.len() {
            eprintln(comp.errors[ei])
            ei = ei + 1
        }
        exit(1)
    }

    -- Assemble C output
    let c_code = assemble_output(comp)

    -- Determine output paths
    let c_file = string_replace(filename, ".dm", ".c")
    let mut bin_file = string_replace(filename, ".dm", "")
    if output_file != "" {
        bin_file = output_file
    }

    if c_only {
        -- Only emit C, no binary
        file_write(c_file, c_code)
        println("Generated: " + c_file)
        return
    }

    -- Write C file
    file_write(c_file, c_code)
    if verbose or emit_c {
        println("Generated: " + c_file)
    }

    -- Compile C to binary
    let compile_cmd = "cc -o " + bin_file + " " + c_file + " -lm"
    if verbose {
        println("Running: " + compile_cmd)
    }
    let exit_code = system(compile_cmd)
    if exit_code != 0 {
        eprintln("C compilation failed")
        exit(1)
    }
    println("Compiled: " + bin_file)

    -- Optionally run
    if do_run {
        if verbose {
            println("Running: " + bin_file)
        }
        let run_code = system("./" + bin_file)
    }
}
