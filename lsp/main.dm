module main

import buffer
import json
import text
import tables
import sources
import transport
import lookup
import documents
import features

-- ============================================================
-- daimond-lsp: the dAImond language server
--
-- It speaks the Language Server Protocol on standard input and output:
-- errors as you type, hover, go to definition, find references, the
-- outline of a document, completion, signature help and formatting.
--
-- It reads no dAImond itself: it runs the compiler, `daimond check`
-- for a document's errors and `daimond symbols` for its declarations,
-- with the editor's text on the compiler's standard input.
--
-- Usage: daimond-lsp [--compiler <path>] [--debounce <ms>]
--   --compiler <path>  the compiler to run. Default: $DAIMOND, else the
--                      file `daimond` next to this program, else the
--                      `daimond` on PATH.
--   --debounce <ms>    how long the editor must be quiet before changed
--                      documents are checked (default 200). With 0 they
--                      are checked right after every change.
--
-- A string is never freed in dAImond, and a server runs for hours, so
-- this program makes no strings while it serves: it works in buffers
-- that it reuses (buffer.dm) and with views of them.
-- ============================================================

fn VERSION() -> string { return "0.1.0" }

fn print_usage() {
    println("daimond-lsp: the dAImond language server")
    println("")
    println("Usage: daimond-lsp [--compiler <path>] [--debounce <ms>]")
    println("")
    println("It speaks the Language Server Protocol on standard input and output.")
    println("")
    println("Options:")
    println("  --compiler <path>  The compiler to run (default: $DAIMOND, else the file")
    println("                     daimond next to this program, else daimond on PATH)")
    println("  --debounce <ms>    How long the editor must be quiet before changed")
    println("                     documents are checked (default: 200; 0 checks them")
    println("                     right after every change)")
    println("  --stdio            Accepted and ignored: this is the only transport")
    println("  -h, --help         Show this help")
    println("  --version          Show the version")
}

-- The compiler to run when --compiler names none
fn default_compiler() -> string {
    let from_env = env_get("DAIMOND")
    if from_env != "" {
        return from_env
    }
    let own = args_get(0)
    if string_contains(own, "/") {
        let beside = path_join(path_dirname(own), "daimond")
        if file_exists(beside) {
            return beside
        }
    }
    return "daimond"
}

-- initialize: what the server can do
fn initialize(server: Server, id_at: int) {
    begin_reply(server, id_at)
    let out = server.out
    buf_add(out, "{\"capabilities\":{")
    -- Documents are sent whole (1) when they change
    buf_add(out, "\"textDocumentSync\":{\"openClose\":true,\"change\":1,\"save\":true},")
    buf_add(out, "\"completionProvider\":{\"triggerCharacters\":[\".\"]},")
    buf_add(out, "\"hoverProvider\":true,")
    buf_add(out, "\"definitionProvider\":true,")
    buf_add(out, "\"referencesProvider\":true,")
    buf_add(out, "\"documentSymbolProvider\":true,")
    buf_add(out, "\"signatureHelpProvider\":{\"triggerCharacters\":[\"(\",\",\"]},")
    buf_add(out, "\"documentFormattingProvider\":true")
    buf_add(out, "},\"serverInfo\":{\"name\":\"daimond-lsp\",\"version\":")
    buf_json(out, VERSION())
    buf_add(out, "}}")
    end_reply(server)
}

-- initialized: tell the user when the compiler cannot be run, since
-- nothing else would work
fn initialized(server: Server) {
    if compiler_runs(server) {
        return
    }
    let note = server.note
    buf_reset(note)
    buf_add(note, "daimond-lsp cannot run the compiler '")
    buf_add(note, server.compiler)
    buf_add(note, "'. Start it with --compiler <path>, or set DAIMOND to the compiler's path.")
    begin_notification(server, "window/showMessage")
    buf_add(server.out, "{\"type\":1,\"message\":")
    buf_json(server.out, buf_view(note))
    buf_add(server.out, "}")
    end_reply(server)
}

-- Handle the message in Server.message
fn handle_message(server: Server) {
    let msg = buf_view(server.message)
    let root = message_root(msg)
    let method = json_member(msg, root, "method")
    let id_at = json_member(msg, root, "id")
    if method < 0 {
        -- A reply to a request; the server makes none
        return
    }
    if id_at < 0 {
        -- A notification
        if json_string_is(msg, method, "textDocument/didOpen") {
            did_open(server)
        } else if json_string_is(msg, method, "textDocument/didChange") {
            did_change(server)
        } else if json_string_is(msg, method, "textDocument/didSave") {
            did_save(server)
        } else if json_string_is(msg, method, "textDocument/didClose") {
            did_close(server)
        } else if json_string_is(msg, method, "initialized") {
            initialized(server)
        } else if json_string_is(msg, method, "exit") {
            -- 0 after a shutdown request, 1 without one
            set_cell(server.state + STATE_EXIT(), 2 - cell(server.state + STATE_SHUTDOWN()))
        }
        return
    }
    if json_string_is(msg, method, "initialize") {
        initialize(server, id_at)
    } else if json_string_is(msg, method, "shutdown") {
        set_cell(server.state + STATE_SHUTDOWN(), 1)
        reply(server, id_at, "null")
    } else if cell(server.state + STATE_SHUTDOWN()) != 0 {
        reply_error(server, id_at, -32600, "the server is shut down")
    } else if json_string_is(msg, method, "textDocument/hover") {
        hover(server, id_at)
    } else if json_string_is(msg, method, "textDocument/definition") {
        definition(server, id_at)
    } else if json_string_is(msg, method, "textDocument/references") {
        references(server, id_at)
    } else if json_string_is(msg, method, "textDocument/documentSymbol") {
        document_symbols(server, id_at)
    } else if json_string_is(msg, method, "textDocument/completion") {
        completion(server, id_at)
    } else if json_string_is(msg, method, "textDocument/signatureHelp") {
        signature_help(server, id_at)
    } else if json_string_is(msg, method, "textDocument/formatting") {
        formatting(server, id_at)
    } else {
        reply_error(server, id_at, -32601, "method not found")
    }
}

fn main() -> int {
    let mut compiler = ""
    let mut debounce_ms = 200
    let mut i = 1
    while i < args_len() {
        let arg = args_get(i)
        if arg == "-h" or arg == "--help" {
            print_usage()
            return 0
        } else if arg == "--version" {
            println("daimond-lsp " + VERSION())
            return 0
        } else if arg == "--compiler" and i + 1 < args_len() {
            i = i + 1
            compiler = args_get(i)
        } else if arg == "--debounce" and i + 1 < args_len() {
            i = i + 1
            debounce_ms = parse_int(args_get(i))
        } else if arg != "--stdio" {
            eprintln("Error: unknown option: " + arg)
            return 2
        }
        i = i + 1
    }
    if compiler == "" {
        compiler = default_compiler()
    }
    if debounce_ms < 0 {
        debounce_ms = 0
    }
    let server = server_new(compiler, debounce_ms)

    while cell(server.state + STATE_EXIT()) == 0 {
        -- Changed documents are checked once the editor is quiet
        if any_dirty(server) and (debounce_ms == 0 or input_waiting(server, debounce_ms) == false) {
            publish_dirty(server)
        } else if read_message(server.inbox, server.message) {
            handle_message(server)
        } else {
            -- The editor is gone without saying exit
            set_cell(server.state + STATE_EXIT(), 2)
        }
    }
    server_cleanup(server)
    return cell(server.state + STATE_EXIT()) - 1
}
