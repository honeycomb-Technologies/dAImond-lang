module documents

import buffer
import json
import text
import sources
import transport

-- ============================================================
-- The documents the editor has open, and their errors
-- ============================================================

-- Where the message being handled starts
fn message_root(msg: string) -> int {
    return json_skip_space(msg, 0)
}

-- The source that the message's params.textDocument.uri names; 0 when
-- it names none, or the server does not know it and `create` is false
fn request_source(server: Server, create: bool) -> int {
    let msg = buf_view(server.message)
    let document = json_member(msg, json_member(msg, message_root(msg), "params"), "textDocument")
    buf_reset(server.uri)
    if json_string_into(server.uri, msg, json_member(msg, document, "uri")) == false {
        return 0
    }
    return source_of_uri(server, buf_view(server.uri), create)
}

-- textDocument/didOpen
fn did_open(server: Server) {
    let src = request_source(server, true)
    if src == 0 {
        return
    }
    let msg = buf_view(server.message)
    let document = json_member(msg, json_member(msg, message_root(msg), "params"), "textDocument")
    let text = cell(src + SRC_TEXT())
    buf_reset(text)
    json_string_into(text, msg, json_member(msg, document, "text"))
    set_cell(src + SRC_OPEN(), 1)
    set_cell(src + SRC_SYMBOLS_OK(), 0)
    set_cell(src + SRC_DIRTY(), 1)
}

-- textDocument/didChange. The server asks for whole documents, so the
-- last change holds the new text.
fn did_change(server: Server) {
    let src = request_source(server, false)
    if src == 0 or cell(src + SRC_OPEN()) == 0 {
        return
    }
    let msg = buf_view(server.message)
    let changes = json_member(msg, json_member(msg, message_root(msg), "params"), "contentChanges")
    let last = json_item(msg, changes, json_count(msg, changes) - 1)
    let new_text = json_member(msg, last, "text")
    if new_text < 0 or json_member(msg, last, "range") >= 0 {
        return
    }
    let text = cell(src + SRC_TEXT())
    buf_reset(text)
    json_string_into(text, msg, new_text)
    set_cell(src + SRC_SYMBOLS_OK(), 0)
    set_cell(src + SRC_DIRTY(), 1)
}

-- textDocument/didSave: a file changed on disk, which may change the
-- errors of every document that imports it
fn did_save(server: Server) {
    let mut src = cell(server.state + STATE_SOURCES())
    while src != 0 {
        if cell(src + SRC_OPEN()) != 0 {
            set_cell(src + SRC_DIRTY(), 1)
        }
        src = cell(src + SRC_NEXT())
    }
}

-- textDocument/didClose
fn did_close(server: Server) {
    let src = request_source(server, false)
    if src == 0 or cell(src + SRC_OPEN()) == 0 {
        return
    }
    set_cell(src + SRC_OPEN(), 0)
    set_cell(src + SRC_SYMBOLS_OK(), 0)
    buf_reset(cell(src + SRC_TEXT()))
    -- A closed document shows no errors
    publish_diagnostics(server, src)
}

-- Whether the errors of a document are still to be sent
fn any_dirty(server: Server) -> bool {
    let mut src = cell(server.state + STATE_SOURCES())
    while src != 0 {
        if cell(src + SRC_DIRTY()) != 0 {
            return true
        }
        src = cell(src + SRC_NEXT())
    }
    return false
}

-- Send the errors of every document whose errors are still to be sent
fn publish_dirty(server: Server) {
    let mut src = cell(server.state + STATE_SOURCES())
    while src != 0 {
        if cell(src + SRC_DIRTY()) != 0 {
            publish_diagnostics(server, src)
        }
        src = cell(src + SRC_NEXT())
    }
}

-- ---- Errors ----

-- Add an error to the list being written to `out`. `line` is counted
-- from 1. `column`, from 1, is where the error is; with 0 the error is
-- the whole line.
fn add_diagnostic(out: int, text: string, line: int, column: int, message: string) {
    if buf_last_byte(out) != 91 {
        buf_add(out, ",")
    }
    let mut row = line - 1
    if row < 0 {
        row = 0
    }
    let last_row = line_of_offset(text, len(text))
    if row > last_row {
        row = last_row
    }
    let first = line_start(text, row)
    let end = line_end(text, first)
    let mut start = skip_blanks(text, first)
    let mut stop = end
    if column > 0 {
        start = first + column - 1
        if start > end {
            start = end
        }
        -- The name there, or one character
        stop = word_end(text, start)
        if stop == start {
            stop = start + utf8_length(byte_or_zero(text, start))
        }
        if stop > end {
            stop = end
        }
    }
    buf_add(out, "{\"range\":")
    add_range(out, row, character_of_offset(text, first, start), row, character_of_offset(text, first, stop))
    buf_add(out, ",\"severity\":1,\"source\":\"daimond\",\"message\":")
    buf_json(out, message)
    buf_add(out, "}")
}

-- The offset in `line` after the "L:C:" that starts at `at`
fn after_position(line: string, at: int) -> int {
    let mut i = at
    let mut colons = 0
    while i < len(line) and colons < 2 {
        if byte_at(line, i) == 58 {
            colons = colons + 1
        }
        i = i + 1
    }
    return skip_blanks(line, i)
}

-- The number after the first `:` at or after `at` in `line`
fn number_after_colon(line: string, at: int) -> int {
    let mut i = at
    while i < len(line) and byte_at(line, i) != 58 {
        i = i + 1
    }
    return json_int(line, i + 1, 0)
}

-- Add the error that a line of the compiler's output reports, if it
-- reports one, to the list being written to Server.out. `text` is the
-- text of the source `src`, which was checked.
fn add_error_line(server: Server, src: int, text: string, line: string) {
    let out = server.out
    let note = server.note
    buf_reset(note)

    -- A syntax error: "Error at LINE:COLUMN: message"
    if starts_with(line, "Error at ") {
        let message_at = after_position(line, 9)
        buf_add_part(note, line, message_at, len(line) - message_at)
        add_diagnostic(out, text, json_int(line, 9, 1), number_after_colon(line, 9), buf_view(note))
        return
    }

    -- An error in an imported file: "FILE: Error at LINE:COLUMN:
    -- message" or "FILE: Error: message at line LINE". It is shown
    -- where the file is imported, as "FILE:LINE: message".
    if starts_with(line, "Error") == false {
        let syntax_at = string_find(line, ": Error at ")
        let other_at = string_find(line, ": Error: ")
        let mut name_end = syntax_at
        if syntax_at <= 0 {
            name_end = other_at
        }
        if name_end <= 0 {
            return
        }
        let file = view(line, 0, name_end)
        buf_add(note, file)
        buf_add(note, ":")
        if syntax_at > 0 {
            let position_at = syntax_at + 11
            buf_add_part(note, line, position_at, len(line) - position_at)
        } else {
            let body_at = other_at + 9
            let at_line = find_last(line, " at line ")
            if at_line >= body_at and all_digits(view(line, at_line + 9, len(line) - at_line - 9)) {
                let mut message_end = at_line
                let in_function = find_last(view(line, 0, at_line), " (in function '")
                if in_function >= body_at and byte_at(line, at_line - 1) == 41 {
                    message_end = in_function
                }
                buf_add_part(note, line, at_line + 9, len(line) - at_line - 9)
                buf_add(note, ": ")
                buf_add_part(note, line, body_at, message_end - body_at)
            } else {
                buf_add(note, " ")
                buf_add_part(note, line, body_at, len(line) - body_at)
            }
        }
        let symbols = buf_view(source_symbols(server, src))
        let mut import_line = 1
        let mut import_column = 0
        let mut at = 0
        while at < len(symbols) {
            if tab_field(symbols, at, 0) == "import" and tab_field(symbols, at, 6) == file {
                import_line = json_int(symbols, tab_field_at(symbols, at, 2), 1)
                import_column = json_int(symbols, tab_field_at(symbols, at, 3), 0)
            }
            at = next_line(symbols, at)
        }
        add_diagnostic(out, text, import_line, import_column, buf_view(note))
        return
    }

    if starts_with(line, "Error: ") == false {
        return
    }
    let body = view(line, 7, len(line) - 7)
    -- "Error: 2 type errors" only counts the errors before it
    if ends_with(body, " type errors") and all_digits(view(body, 0, len(body) - 12)) {
        return
    }

    -- "Error: message at line LINE", the message sometimes followed by
    -- " (in function 'name')"
    let at_line = find_last(body, " at line ")
    if at_line >= 0 and all_digits(view(body, at_line + 9, len(body) - at_line - 9)) {
        let mut message_end = at_line
        let in_function = find_last(view(body, 0, at_line), " (in function '")
        if in_function >= 0 and byte_at(body, at_line - 1) == 41 {
            message_end = in_function
        }
        buf_add_part(note, body, 0, message_end)
        add_diagnostic(out, text, json_int(body, at_line + 9, 1), 0, buf_view(note))
        return
    }

    -- An error with no line. One that names a function is shown at
    -- that function, a missing import where it is imported, another on
    -- the first line.
    buf_add(note, body)
    let mut line_number = 1
    let mut column = 0
    if starts_with(body, "import file not found: ") {
        let file = view(body, 23, len(body) - 23)
        let symbols = buf_view(source_symbols(server, src))
        let mut at = 0
        while at < len(symbols) {
            if tab_field(symbols, at, 0) == "import" and tab_field(symbols, at, 6) == file {
                line_number = json_int(symbols, tab_field_at(symbols, at, 2), 1)
                column = json_int(symbols, tab_field_at(symbols, at, 3), 0)
            }
            at = next_line(symbols, at)
        }
    }
    let named = string_find(body, "function '")
    if named >= 0 {
        let name_at = named + 10
        let mut name_end = name_at
        while name_end < len(body) and byte_at(body, name_end) != 39 {
            name_end = name_end + 1
        }
        let name = view(body, name_at, name_end - name_at)
        let symbols = buf_view(source_symbols(server, src))
        let mut at = 0
        while at < len(symbols) {
            let kind = tab_field(symbols, at, 0)
            if (kind == "function" or kind == "method") and tab_field(symbols, at, 1) == name and column == 0 {
                line_number = json_int(symbols, tab_field_at(symbols, at, 2), 1)
                column = json_int(symbols, tab_field_at(symbols, at, 3), 0)
            }
            at = next_line(symbols, at)
        }
    }
    add_diagnostic(out, text, line_number, column, buf_view(note))
}

-- Check a document with the compiler and send the editor its errors:
-- none for a document that is not open
fn publish_diagnostics(server: Server, src: int) {
    set_cell(src + SRC_DIRTY(), 0)
    let text = buf_view(cell(src + SRC_TEXT()))
    let errors = server.scratch
    buf_reset(errors)
    let mut code = 0
    if cell(src + SRC_OPEN()) != 0 {
        if buf_write_file(cell(src + SRC_TEXT()), server.tmp_text) {
            code = run_compiler(server, "check", source_name(src), true)
            buf_read_file(errors, server.tmp_err)
        } else {
            code = 1
            buf_add(errors, "cannot write ")
            buf_add(errors, server.tmp_text)
        }
    }

    begin_notification(server, "textDocument/publishDiagnostics")
    let out = server.out
    buf_add(out, "{\"uri\":")
    buf_json(out, buf_view(cell(src + SRC_URI())))
    buf_add(out, ",\"diagnostics\":[")
    let output = buf_view(errors)
    let mut at = 0
    while at < len(output) {
        add_error_line(server, src, text, view(output, at, line_end(output, at) - at))
        at = next_line(output, at)
    }
    if code != 0 and buf_last_byte(out) == 91 {
        -- The compiler failed and reported nothing the server can
        -- place: show the first line of what it did print
        let note = server.note
        buf_reset(note)
        let mut first = 0
        while first < len(output) and line_end(output, first) == first {
            first = next_line(output, first)
        }
        if first < len(output) {
            buf_add(note, "the compiler failed: ")
            buf_add_part(note, output, first, line_end(output, first) - first)
        } else {
            buf_add(note, "the compiler failed with exit code ")
            buf_int(note, code)
        }
        add_diagnostic(out, text, 1, 0, buf_view(note))
    }
    buf_add(out, "]}")
    end_reply(server)
}
