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
        println("dAImond Stage 1 Compiler")
        println("Usage: daimond1 <file.dm>")
        println("       daimond1 run <file.dm>")
        exit(1)
    }

    let mut filename = args_get(1)
    let mut do_run = false
    if filename == "run" and argc > 2 {
        do_run = true
        filename = args_get(2)
    }

    -- Read source
    let raw_source = file_read(filename)

    -- Resolve imports (concatenate imported files before tokenizing)
    let source = resolve_imports(raw_source, filename)

    -- Tokenize
    let tokens = tokenize(source)

    -- Compile
    let mut comp = compiler_new(tokens)
    comp = compile_source(comp)

    -- Check for errors
    if comp.errors.len() > 0 {
        eprintln("Compilation errors:")
        let mut i = 0
        while i < comp.errors.len() {
            eprintln(comp.errors[i])
            i = i + 1
        }
        exit(1)
    }

    -- Assemble C output
    let c_code = assemble_output(comp)

    -- Write C file
    let c_file = string_replace(filename, ".dm", ".c")
    file_write(c_file, c_code)
    println("Generated: " + c_file)

    -- Compile C to binary
    let bin_file = string_replace(filename, ".dm", "")
    let compile_cmd = "cc -o " + bin_file + " " + c_file + " -lm"
    let exit_code = system(compile_cmd)
    if exit_code != 0 {
        eprintln("C compilation failed")
        exit(1)
    }
    println("Compiled: " + bin_file)

    -- Optionally run
    if do_run {
        println("Running:")
        println("============================================================")
        let run_code = system("./" + bin_file)
        println("============================================================")
    }
}
