module sources

import buffer
import json
import text
import tables

-- ============================================================
-- The server's state: the files it knows, and what the compiler says
-- about them
--
-- The server does not read programs itself. It runs the compiler:
-- `daimond check` for a file's errors and `daimond symbols` for its
-- declarations, each with the document's text on standard input.
--
-- A source is a file the server knows: a document the editor has open,
-- or a file on disk that one of them imports. It is a record of cells
-- (see buffer.dm), so that every copy of the Server struct sees the
-- same sources.
-- ============================================================

extern fn lsp_c_stat(path: string, to: int) -> i32 = "stat"
extern fn lsp_c_getpid() -> i32 = "getpid"

struct Server {
    -- The compiler to run: a path, or a name that is looked for on PATH
    compiler: string,
    -- How long the editor must be quiet before changed documents are
    -- checked; 0 checks right after every change
    debounce_ms: int,
    -- Buffers (see buffer.dm), each used for one thing at a time:
    inbox: int,       -- bytes from the editor that are not yet used
    message: int,     -- the message being handled
    out: int,         -- the message being written
    frame: int,       -- its header
    uri: int,         -- the URI the message names
    path: int,        -- that URI's path
    scratch: int,     -- what the compiler printed; names seen
    note: int,        -- a text on its way into `out`
    visit: int,       -- sources, as a list of ints: a file and what it imports
    file_text: int,   -- the text of a file that is not open
    stack: int,       -- open brackets, as a list of ints
    found: int,       -- declarations, as a list of ints: a source, and where its line starts in that source's symbols
    -- Cells:
    poll_fd: int,     -- a struct pollfd
    stat_at: int,     -- a struct stat
    state: int,       -- see STATE_...
    -- Files of this process's own in /tmp
    tmp_text: string,
    tmp_out: string,
    tmp_err: string,
    tmp_fmt: string,
    -- The compiler's command lines, made once: running the compiler
    -- writes the words that differ into them and allocates nothing
    text_args: List[string],   -- compiler, command, name, --stdin
    file_args: List[string],   -- compiler, command, file
    fmt_args: List[string],    -- compiler, fmt, tmp_text, -o, tmp_fmt
    builtins: List[string],
    words: List[string],
    methods: List[string]
}

-- The cells of Server.state
fn STATE_SOURCES() -> int { return 0 }     -- the first source
fn STATE_SHUTDOWN() -> int { return 8 }    -- 1 after a shutdown request
fn STATE_EXIT() -> int { return 16 }       -- 0 while running, else the exit code plus 1
fn STATE_SCAN_AT() -> int { return 24 }    -- where the scan of a text for names goes on,
fn STATE_SCAN_MODE() -> int { return 32 }  -- and whether it is in code, a string or a comment

-- The cells of a source
fn SRC_NEXT() -> int { return 0 }          -- the next source
fn SRC_URI() -> int { return 8 }           -- buffer: its URI, as the editor wrote it
fn SRC_PATH() -> int { return 16 }         -- buffer: its path; empty when the URI is not a file's
fn SRC_TEXT() -> int { return 24 }         -- buffer: its text, when open
fn SRC_SYMBOLS() -> int { return 32 }      -- buffer: what `daimond symbols` printed for it
fn SRC_SYMBOLS_OK() -> int { return 40 }   -- 1 when the symbols are those of the text or file
fn SRC_OPEN() -> int { return 48 }         -- 1 while the editor has it open
fn SRC_DIRTY() -> int { return 56 }        -- 1 when its errors must be sent
fn SRC_STAMP() -> int { return 64 }        -- of a file on disk: when it was changed,
fn SRC_SIZE() -> int { return 72 }         -- and its size, as of the symbols

fn server_new(compiler: string, debounce_ms: int) -> Server {
    let mut pid = 0
    unsafe {
        pid = lsp_c_getpid() as int
    }
    let stem = "/tmp/daimond_lsp_" + int_to_string(pid) + "_" + int_to_string(time_ms())
    return Server {
        compiler: compiler,
        debounce_ms: debounce_ms,
        inbox: buf_new(65536),
        message: buf_new(65536),
        out: buf_new(65536),
        frame: buf_new(64),
        uri: buf_new(256),
        path: buf_new(256),
        scratch: buf_new(4096),
        note: buf_new(1024),
        visit: buf_new(256),
        file_text: buf_new(4096),
        stack: buf_new(256),
        found: buf_new(256),
        poll_fd: cells_new(1),
        stat_at: cells_new(18),
        state: cells_new(5),
        tmp_text: stem + ".dm",
        tmp_out: stem + ".out",
        tmp_err: stem + ".err",
        tmp_fmt: stem + ".fmt",
        text_args: [compiler, "", "", "--stdin"],
        file_args: [compiler, "", ""],
        fmt_args: [compiler, "fmt", stem + ".dm", "-o", stem + ".fmt"],
        builtins: builtin_functions(),
        words: known_words(),
        methods: builtin_methods()
    }
}

-- Delete the server's files in /tmp
fn server_cleanup(server: Server) {
    fs_remove(server.tmp_text)
    fs_remove(server.tmp_out)
    fs_remove(server.tmp_err)
    fs_remove(server.tmp_fmt)
}

-- ---- Sources ----

fn source_new(server: Server) -> int {
    let src = cells_new(10)
    set_cell(src + SRC_URI(), buf_new(128))
    set_cell(src + SRC_PATH(), buf_new(128))
    set_cell(src + SRC_TEXT(), buf_new(0))
    set_cell(src + SRC_SYMBOLS(), buf_new(0))
    set_cell(src + SRC_NEXT(), cell(server.state + STATE_SOURCES()))
    set_cell(server.state + STATE_SOURCES(), src)
    return src
}

-- The source with this path, or 0
fn source_with_path(server: Server, path: string) -> int {
    let mut src = cell(server.state + STATE_SOURCES())
    while src != 0 {
        if buf_view(cell(src + SRC_PATH())) == path {
            return src
        }
        src = cell(src + SRC_NEXT())
    }
    return 0
}

-- The source with this URI, or 0
fn source_with_uri(server: Server, uri: string) -> int {
    let mut src = cell(server.state + STATE_SOURCES())
    while src != 0 {
        if buf_view(cell(src + SRC_URI())) == uri {
            return src
        }
        src = cell(src + SRC_NEXT())
    }
    return 0
}

-- The source the editor calls `uri`, or 0 when there is none and
-- `create` is false. Two URIs that spell one path are one source.
fn source_of_uri(server: Server, uri: string, create: bool) -> int {
    let path = server.path
    buf_reset(path)
    uri_path_into(path, uri)
    let mut src = 0
    if buf_len(path) > 0 {
        src = source_with_path(server, buf_view(path))
    } else {
        src = source_with_uri(server, uri)
    }
    if src == 0 and create {
        src = source_new(server)
        buf_add(cell(src + SRC_PATH()), buf_view(path))
    }
    if src != 0 and create {
        -- Keep the editor's spelling: it is what the editor is answered with
        let own = cell(src + SRC_URI())
        if buf_view(own) != uri {
            buf_reset(own)
            buf_add(own, uri)
        }
    }
    return src
}

-- The source of the file at `path`
fn source_of_path(server: Server, path: string) -> int {
    let mut src = source_with_path(server, path)
    if src == 0 {
        src = source_new(server)
        buf_add(cell(src + SRC_PATH()), path)
        path_uri_into(cell(src + SRC_URI()), path)
    }
    return src
}

-- The name the compiler is given for a source: its path, which is
-- where its imports are looked for
fn source_name(src: int) -> string {
    if buf_len(cell(src + SRC_PATH())) > 0 {
        return buf_view(cell(src + SRC_PATH()))
    }
    return "untitled.dm"
}

-- The text of a source: the editor's when it is open, else the file's
-- (empty when it cannot be read). Valid until the next call for a
-- source that is not open.
fn source_text(server: Server, src: int) -> string {
    if cell(src + SRC_OPEN()) != 0 {
        return buf_view(cell(src + SRC_TEXT()))
    }
    buf_read_file(server.file_text, buf_view(cell(src + SRC_PATH())))
    return buf_view(server.file_text)
}

-- ---- Running the compiler ----

-- Run `daimond <what> <name>`, with the text in the file tmp_text on
-- its standard input when `from_text`. Its output goes to the files
-- tmp_out and tmp_err. Gives its exit code (127 when it cannot be
-- run). The compiler is started directly, with no shell between.
fn run_compiler(server: Server, what: string, name: string, from_text: bool) -> int {
    if from_text {
        -- A copy of the list's header: its words are the server's own
        let mut text_args = server.text_args
        text_args[1] = what
        text_args[2] = name
        return run_command_io(text_args, server.tmp_text, server.tmp_out, server.tmp_err)
    }
    let mut file_args = server.file_args
    file_args[1] = what
    file_args[2] = name
    -- Not the server's own input, which is the editor
    return run_command_io(file_args, "/dev/null", server.tmp_out, server.tmp_err)
}

-- Whether the compiler can be run at all
fn compiler_runs(server: Server) -> bool {
    return run_command_io([server.compiler, "--version"], "/dev/null", "/dev/null", "/dev/null") == 0
}

-- Read the file's time of change and size into the server's two stamp
-- values; false when there is no such file. The offsets are those of
-- struct stat on x86-64 Linux.
fn file_stamp(server: Server, path: string) -> bool {
    unsafe {
        if lsp_c_stat(path, server.stat_at) as int != 0 {
            return false
        }
    }
    return true
}

fn stamp_time(server: Server) -> int {
    return cell(server.stat_at + 88) * 1000000000 + cell(server.stat_at + 96)
}

fn stamp_size(server: Server) -> int {
    return cell(server.stat_at + 48)
}

-- The buffer with the source's declarations, one line each as
-- `daimond symbols` prints them, brought up to date first: with the
-- editor's text for an open document, with the file for another.
fn source_symbols(server: Server, src: int) -> int {
    let symbols = cell(src + SRC_SYMBOLS())
    if cell(src + SRC_OPEN()) != 0 {
        if cell(src + SRC_SYMBOLS_OK()) != 0 {
            return symbols
        }
        buf_reset(symbols)
        if buf_write_file(cell(src + SRC_TEXT()), server.tmp_text) {
            run_compiler(server, "symbols", source_name(src), true)
            buf_read_file(symbols, server.tmp_out)
        }
        set_cell(src + SRC_SYMBOLS_OK(), 1)
        return symbols
    }
    let path = buf_view(cell(src + SRC_PATH()))
    if len(path) == 0 or file_stamp(server, path) == false {
        buf_reset(symbols)
        set_cell(src + SRC_SYMBOLS_OK(), 0)
        return symbols
    }
    let time = stamp_time(server)
    let size = stamp_size(server)
    if cell(src + SRC_SYMBOLS_OK()) != 0 and cell(src + SRC_STAMP()) == time and cell(src + SRC_SIZE()) == size {
        return symbols
    }
    buf_reset(symbols)
    run_compiler(server, "symbols", path, false)
    buf_read_file(symbols, server.tmp_out)
    set_cell(src + SRC_SYMBOLS_OK(), 1)
    set_cell(src + SRC_STAMP(), time)
    set_cell(src + SRC_SIZE(), size)
    return symbols
}

-- Make Server.visit the list of `first` and every file it imports,
-- directly or through another, each with its symbols up to date
fn collect_sources(server: Server, first: int) {
    let visit = server.visit
    buf_reset(visit)
    buf_i64(visit, first)
    let mut i = 0
    while i < buf_len(visit) / 8 and i < 500 {
        let src = buf_get_i64(visit, i)
        let symbols = buf_view(source_symbols(server, src))
        let mut at = 0
        while at < len(symbols) {
            if tab_field(symbols, at, 0) == "import" {
                let imported = source_of_path(server, tab_field(symbols, at, 6))
                let mut seen = false
                let mut k = 0
                while k < buf_len(visit) / 8 {
                    if buf_get_i64(visit, k) == imported {
                        seen = true
                    }
                    k = k + 1
                }
                if seen == false {
                    buf_i64(visit, imported)
                }
            }
            at = next_line(symbols, at)
        }
        i = i + 1
    }
}

-- ---- Symbols ----
-- A line of `daimond symbols` has the fields
--   0 kind, 1 name, 2 line, 3 column, 4 last line, 5 container, 6 detail

-- 1 for a kind declared at the top of a file, 2 for one declared in a
-- type, 0 for an impl block, which is not looked up by name
fn kind_class(kind: string) -> int {
    if kind == "function" or kind == "struct" or kind == "enum" or kind == "trait" or kind == "const" or kind == "import" {
        return 1
    }
    if kind == "method" or kind == "field" or kind == "variant" {
        return 2
    }
    return 0
}

-- A symbol found in the sources of Server.visit
struct Match {
    found: bool,
    -- The source it is in, and where its line starts in that source's
    -- symbols
    src: int,
    line_at: int,
    -- Where the search goes on: the index in Server.visit, and the
    -- offset in that source's symbols
    index: int,
    next_at: int
}

fn no_match() -> Match {
    return Match { found: false, src: 0, line_at: 0, index: 0, next_at: 0 }
}

-- The next symbol after `after` named `name` whose kind has the class
-- `class`; the first one when `after` is no_match(). Call
-- collect_sources first.
fn next_match(server: Server, name: string, class: int, after: Match) -> Match {
    let mut index = after.index
    let mut at = after.next_at
    while index < buf_len(server.visit) / 8 {
        let src = buf_get_i64(server.visit, index)
        let symbols = buf_view(cell(src + SRC_SYMBOLS()))
        while at < len(symbols) {
            let here = at
            at = next_line(symbols, at)
            if tab_field(symbols, here, 1) == name and kind_class(tab_field(symbols, here, 0)) == class {
                return Match { found: true, src: src, line_at: here, index: index, next_at: at }
            }
        }
        index = index + 1
        at = 0
    }
    return no_match()
}

-- Where, in a source's own `symbols`, the line of the function or
-- method around line `line` (from 1) of the source starts; -1 when the
-- line is in none
fn enclosing_function_at(symbols: string, line: int) -> int {
    let mut found = -1
    let mut at = 0
    while at < len(symbols) {
        let kind = tab_field(symbols, at, 0)
        if kind == "function" or kind == "method" {
            let first = json_int(symbols, tab_field_at(symbols, at, 2), 0)
            let last = json_int(symbols, tab_field_at(symbols, at, 4), 0)
            if first <= line and line <= last {
                found = at
            }
        }
        at = next_line(symbols, at)
    }
    return found
}
