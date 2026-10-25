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
        -- Compile, then rewrite the generated C like Stage 0's testFile:
        -- (a) make dm_panic/dm_panic_cstr and the inline assert/assert_eq
        --     failures longjmp to the runner instead of exit(1), and
        -- (b) replace main() with a runner that calls each test_* function
        --     under setjmp and reports real PASS/FAIL per test.
        -- (Stage 1 assert expands inline without calling dm_panic, and its
        -- dm_panic takes dm_string, so the needles differ from Stage 0's.)
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
        let mut test_code = assemble_output(comp)
        -- (a) Test-aware panics (exact spellings from stage1/runtime.dm)
        let panic_cstr_old = "static inline void dm_panic_cstr(const char* msg) { fprintf(stderr, \"PANIC: %s\\n\", msg); exit(1); }"
        let panic_old = "static inline void dm_panic(dm_string msg) { fprintf(stderr, \"PANIC: \"); fwrite(msg.data, 1, msg.len, stderr); fputc('\\n', stderr); exit(1); }"
        if string_contains(test_code, panic_cstr_old) == false or string_contains(test_code, panic_old) == false {
            eprintln("Error: generated C has no recognizable dm_panic to rewrite for testing")
            exit(1)
        }
        let mut panic_cstr_new = "#include <setjmp.h>\n"
        panic_cstr_new = panic_cstr_new + "static jmp_buf __test_jmp;\n"
        panic_cstr_new = panic_cstr_new + "static int __test_active = 0;\n"
        panic_cstr_new = panic_cstr_new + "static const char* __test_fail_msg = NULL;\n"
        panic_cstr_new = panic_cstr_new + "static char __test_fail_buf[512];\n"
        panic_cstr_new = panic_cstr_new + "static inline void dm_panic_cstr(const char* msg) { if (__test_active) { __test_fail_msg = msg; longjmp(__test_jmp, 1); } fprintf(stderr, \"PANIC: %s\\n\", msg); exit(1); }"
        test_code = string_replace(test_code, panic_cstr_old, panic_cstr_new)
        let panic_new = "static inline void dm_panic(dm_string msg) { if (__test_active) { size_t __n = msg.len < 511 ? msg.len : 511; memcpy(__test_fail_buf, msg.data, __n); __test_fail_buf[__n] = '\\0'; __test_fail_msg = __test_fail_buf; longjmp(__test_jmp, 1); } fprintf(stderr, \"PANIC: \"); fwrite(msg.data, 1, msg.len, stderr); fputc('\\n', stderr); exit(1); }"
        test_code = string_replace(test_code, panic_old, panic_new)
        -- Inline assert/assert_eq failures (exact spellings from compile_expr.dm)
        test_code = string_replace(test_code, "fprintf(stderr, \"PANIC: assertion failed\\n\"); exit(1);", "if (__test_active) { __test_fail_msg = \"assertion failed\"; longjmp(__test_jmp, 1); } fprintf(stderr, \"PANIC: assertion failed\\n\"); exit(1);")
        test_code = string_replace(test_code, "fprintf(stderr, \"PANIC: assert_eq failed\\n\"); exit(1);", "if (__test_active) { __test_fail_msg = \"assert_eq failed\"; longjmp(__test_jmp, 1); } fprintf(stderr, \"PANIC: assert_eq failed\\n\"); exit(1);")
        -- (b) Replace main() with the test runner
        let main_pos = string_find(test_code, "int main(int argc, char** argv)")
        if main_pos < 0 {
            eprintln("Error: generated C has no main() to replace with the test runner")
            exit(1)
        }
        let mut runner = substr(test_code, 0, main_pos)
        runner = runner + "int main(int argc, char** argv) {\n"
        runner = runner + "    dm_argc = argc;\n"
        runner = runner + "    dm_argv = argv;\n"
        runner = runner + "    int __passed = 0, __failed = 0;\n"
        let mut ti2 = 0
        while ti2 < test_names.len() {
            let tname = test_names[ti2]
            runner = runner + "    printf(\"  test " + tname + " ... \");\n"
            runner = runner + "    fflush(stdout);\n"
            runner = runner + "    __test_active = 1;\n"
            runner = runner + "    __test_fail_msg = NULL;\n"
            runner = runner + "    if (setjmp(__test_jmp) == 0) {\n"
            runner = runner + "        dm_" + tname + "();\n"
            runner = runner + "        __test_active = 0;\n"
            runner = runner + "        printf(\"PASS\\n\"); __passed++;\n"
            runner = runner + "    } else {\n"
            runner = runner + "        __test_active = 0;\n"
            runner = runner + "        if (__test_fail_msg) printf(\"FAIL: %s\\n\", __test_fail_msg);\n"
            runner = runner + "        else printf(\"FAIL\\n\");\n"
            runner = runner + "        __failed++;\n"
            runner = runner + "    }\n"
            ti2 = ti2 + 1
        }
        runner = runner + "    printf(\"\\n%d passed, %d failed\\n\", __passed, __failed);\n"
        runner = runner + "    return __failed > 0 ? 1 : 0;\n"
        runner = runner + "}\n"
        let c_file = string_replace(filename, ".dm", ".c")
        let bin_file = string_replace(filename, ".dm", "")
        file_write(c_file, runner)
        let compile_cmd = "cc -o " + bin_file + " " + c_file + " -lm"
        let cc_exit = system(compile_cmd)
        if cc_exit != 0 {
            eprintln("C compilation failed")
            exit(1)
        }
        let mut run_cmd = "./" + bin_file
        if starts_with(bin_file, "/") {
            run_cmd = bin_file
        }
        let run_exit = system(run_cmd)
        if run_exit != 0 {
            exit(1)
        }
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
