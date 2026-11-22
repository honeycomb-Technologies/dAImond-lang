module gen_bootstrap

-- Writes main.dm and prelude.dm from the compiler's modules.
--
-- main.dm is the modules as one file, which is what bootstrap.sh and a
-- self-build compile. prelude.dm holds the runtime's sources
-- (runtime/*.dm) as one string, which the compiler parses into every
-- program, and the build ID, a hash of everything the compiler is made
-- from.
--
-- Usage: daimond run stage5/gen_bootstrap.dm [-- [--check] [<directory>]]
--   <directory>  the compiler's directory (default: the current
--                directory when it holds main_split.dm, else stage5)
--   --check      write nothing; exit with 1 when main.dm or prelude.dm
--                is not what the modules give

-- The modules, dependencies before dependents
fn modules() -> List[string] {
    return ["token.dm", "lexer.dm", "ast.dm", "ir.dm", "ir_builder.dm", "llvm.dm", "parser.dm",
        "ir_gen.dm", "ir_gen_decl.dm", "ir_gen_stmt.dm", "ir_gen_expr.dm", "ir_gen_builtins.dm",
        "ir_gen_comptime.dm", "check.dm", "llvm_gen.dm", "llvm_gen_call.dm", "llvm_gen_simd.dm"]
}

-- The runtime's sources, which the compiler embeds as one string, so
-- that compiled programs need no runtime files on disk
fn runtime_sources() -> List[string] {
    return ["runtime/string.dm", "runtime/convert.dm", "runtime/list.dm", "runtime/map.dm", "runtime/os.dm",
        "runtime/path.dm", "runtime/io.dm", "runtime/net.dm", "runtime/thread.dm"]
}

-- ============================================================
-- THE BUILD ID
-- Two polynomial hashes of the source, each below a prime near 2^31,
-- as 16 hex digits. It names the precompiled runtime object in the
-- cache, so a changed compiler builds its own.
-- ============================================================

struct Hash { a: int, b: int }

fn hash_text(h: Hash, text: string) -> Hash {
    let mut a = h.a
    let mut b = h.b
    let n = len(text)
    let mut i = 0
    while i < n {
        let c = byte_at(text, i)
        a = (a * 31 + c + 1) % 2147483647
        b = (b * 131 + c + 1) % 2147483629
        i = i + 1
    }
    return Hash { a: a, b: b }
}

fn hex8(n: int) -> string {
    let digits = "0123456789abcdef"
    let mut out = ""
    let mut rest = n
    let mut i = 0
    while i < 8 {
        out = char_at(digits, rest % 16) + out
        rest = rest / 16
        i = i + 1
    }
    return out
}

-- ============================================================
-- TEXT
-- ============================================================

fn begins_at(text: string, at: int, word: string) -> bool {
    let n = len(word)
    if at + n > len(text) { return false }
    let mut i = 0
    while i < n {
        if byte_at(text, at + i) != byte_at(word, i) { return false }
        i = i + 1
    }
    return true
}

-- The text without the lines that begin with "module ", and without
-- those that begin with "import " when `imports` is true
fn without_declarations(text: string, imports: bool) -> string {
    let n = len(text)
    let mut out = ""
    let mut kept_from = 0
    let mut line_start = 0
    let mut i = 0
    while i <= n {
        if i == n or byte_at(text, i) == 10 {
            let line_end = if i == n { n } else { i + 1 }
            let dropped = begins_at(text, line_start, "module ") or (imports and begins_at(text, line_start, "import "))
            if line_end > line_start and dropped {
                out = out + substr(text, kept_from, line_start - kept_from)
                kept_from = line_end
            }
            line_start = line_end
        }
        i = i + 1
    }
    return out + substr(text, kept_from, n - kept_from)
}

fn count_lines(text: string) -> int {
    let n = len(text)
    let mut lines = 0
    let mut i = 0
    while i < n {
        if byte_at(text, i) == 10 { lines = lines + 1 }
        i = i + 1
    }
    return lines
}

fn read_source(dir: string, name: string) -> string {
    let path = dir + "/" + name
    if file_exists(path) == false {
        eprintln("error: " + path + " is missing")
        exit(1)
    }
    return file_read(path)
}

-- A file's lines, each ending in a newline
fn whole_lines(text: string) -> string {
    if len(text) > 0 and byte_at(text, len(text) - 1) != 10 { return text + "\n" }
    return text
}

-- The text as it stands between the quotes of a string literal
fn escape_literal(text: string) -> string {
    return string_replace(string_replace(string_replace(text, "\\", "\\\\"), "\"", "\\\""), "\n", "\\n")
}

fn join_names(names: List[string]) -> string {
    let mut out = ""
    let mut i = 0
    while i < names.len() {
        if i > 0 { out = out + " " }
        out = out + names[i]
        i = i + 1
    }
    return out
}

-- ============================================================
-- MAIN
-- ============================================================

fn main() -> int {
    let mut dir = ""
    let mut check = false
    let mut ai = 1
    while ai < args_len() {
        let arg = args_get(ai)
        if arg == "--check" {
            check = true
        } else if starts_with(arg, "-") {
            eprintln("error: unknown option: " + arg)
            eprintln("usage: daimond run gen_bootstrap.dm [-- [--check] [<directory>]]")
            return 2
        } else {
            dir = arg
        }
        ai = ai + 1
    }
    if dir == "" {
        dir = if file_exists("main_split.dm") { "." } else { "stage5" }
    }

    let mods = modules()
    let runtime = runtime_sources()

    -- The modules, as they go into main.dm, and the build ID
    let mut hash = Hash { a: 0, b: 0 }
    let mut main_text = "module main\n"
    let mut i = 0
    while i < mods.len() {
        let text = whole_lines(read_source(dir, mods[i]))
        hash = hash_text(hash, text)
        main_text = main_text + without_declarations(text, true)
        i = i + 1
    }
    let entry = whole_lines(read_source(dir, "main_split.dm"))
    hash = hash_text(hash, entry)
    let mut runtime_text = ""
    i = 0
    while i < runtime.len() {
        let text = whole_lines(read_source(dir, runtime[i]))
        hash = hash_text(hash, text)
        runtime_text = runtime_text + without_declarations(text, false)
        i = i + 1
    }
    hash = hash_text(hash, whole_lines(read_source(dir, "llvm_bridge.dm")))
    let build_id = hex8(hash.a) + hex8(hash.b)

    -- The runtime as string literals of whole lines, a few thousand
    -- bytes each, which the compiler joins. One literal of the whole
    -- runtime cost a compiler from before 2026-11-22 its length squared
    -- in memory (see join_strings in lexer.dm), and the seed's
    -- compiler may be such a compiler.
    let mut prelude = "module prelude\n\n"
    prelude = prelude + "-- Generated by gen_bootstrap.dm from " + join_names(runtime) + "; edit those files instead.\n"
    prelude = prelude + "-- The compiler parses this source into every program it compiles.\n\n"
    prelude = prelude + "fn prelude_source() -> string {\n    let mut pieces: List[string] = []\n"
    let total = len(runtime_text)
    let mut piece_start = 0
    while piece_start < total {
        -- The piece ends after the last newline within 4000 bytes
        let mut piece_end = piece_start + 4000
        if piece_end >= total {
            piece_end = total
        } else {
            while piece_end > piece_start and byte_at(runtime_text, piece_end - 1) != 10 {
                piece_end = piece_end - 1
            }
            if piece_end == piece_start { piece_end = piece_start + 4000 }
        }
        prelude = prelude + "    pieces.push(\"" + escape_literal(substr(runtime_text, piece_start, piece_end - piece_start)) + "\")\n"
        piece_start = piece_end
    }
    prelude = prelude + "    return join_strings(pieces)\n}\n\n"
    prelude = prelude + "-- A hash of the compiler's and the runtime's source\n"
    prelude = prelude + "fn runtime_build_id() -> string {\n    return \"" + build_id + "\"\n}\n"

    main_text = main_text + without_declarations(prelude, true) + without_declarations(entry, true)

    let main_path = dir + "/main.dm"
    let prelude_path = dir + "/prelude.dm"
    if check {
        let mut stale = 0
        if file_exists(prelude_path) == false or file_read(prelude_path) != prelude {
            eprintln(prelude_path + " is not what the runtime's sources give; run gen_bootstrap.dm")
            stale = stale + 1
        }
        if file_exists(main_path) == false or file_read(main_path) != main_text {
            eprintln(main_path + " is not what the modules give; run gen_bootstrap.dm")
            stale = stale + 1
        }
        if stale > 0 { return 1 }
        println("main.dm and prelude.dm are in step with the modules (build ID " + build_id + ")")
        return 0
    }
    file_write(prelude_path, prelude)
    file_write(main_path, main_text)
    println("Generated " + main_path + " (" + int_to_string(count_lines(main_text)) + " lines, build ID " + build_id + ")")
    return 0
}
