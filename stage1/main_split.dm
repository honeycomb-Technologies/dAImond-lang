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

-- ============================================================
-- MAIN ENTRY POINT
-- ============================================================

fn main() {
    let argc = args_len()
    if argc < 2 {
        println("dAImond Stage 1 Compiler v0.2.0")
        println("Usage: daimond1 [command] <file.dm> [options]")
        println("")
        println("Commands:")
        println("  <file.dm>       Compile a dAImond source file")
        println("  run <file.dm>   Compile and run")
        println("  compile <file.dm>  Compile (same as no command)")
        println("")
        println("Options:")
        println("  -o <file>       Output file path")
        println("  -c              Compile to C only (no binary)")
        println("  --emit-c        Emit C code alongside binary")
        println("  -v, --verbose   Verbose output")
        println("  --version       Show version")
        println("  -h, --help      Show this help")
        exit(1)
    }

    -- Parse arguments
    let mut filename = ""
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
            println("dAImond Stage 1 Compiler v0.2.0")
            println("Usage: daimond1 [command] <file.dm> [options]")
            println("")
            println("Commands:")
            println("  <file.dm>       Compile a dAImond source file")
            println("  run <file.dm>   Compile and run")
            println("  compile <file.dm>  Compile (same as no command)")
            println("")
            println("Options:")
            println("  -o <file>       Output file path")
            println("  -c              Compile to C only (no binary)")
            println("  --emit-c        Emit C code alongside binary")
            println("  -v, --verbose   Verbose output")
            println("  --version       Show version")
            println("  -h, --help      Show this help")
            exit(0)
        }
        if arg == "run" and filename == "" {
            do_run = true
            i = i + 1
            continue
        }
        if arg == "compile" and filename == "" {
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

    -- Tokenize
    let tokens = tokenize(source)
    if verbose {
        println("Tokens: " + int_to_string(tokens.len()))
    }

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
