module features

import buffer
import json
import text
import tables
import sources
import transport
import lookup
import documents

-- ============================================================
-- Answers to the editor's questions about a place in a document
-- ============================================================

-- The open document and the place in it that a request names
struct Target {
    ok: bool,
    src: int,
    -- The offset of the place in the document's text
    at: int
}

fn request_target(server: Server) -> Target {
    let src = request_source(server, false)
    if src == 0 or cell(src + SRC_OPEN()) == 0 {
        return Target { ok: false, src: 0, at: 0 }
    }
    let msg = buf_view(server.message)
    let position = json_member(msg, json_member(msg, message_root(msg), "params"), "position")
    let line = json_int(msg, json_member(msg, position, "line"), -1)
    let character = json_int(msg, json_member(msg, position, "character"), -1)
    if line < 0 or character < 0 {
        return Target { ok: false, src: 0, at: 0 }
    }
    let text = buf_view(cell(src + SRC_TEXT()))
    return Target { ok: true, src: src, at: offset_of_position(text, line, character) }
}

-- ---- Hover ----

-- Add what an editor shows for a variable of a function
fn add_local_note(note: int, text: string, local: Local, name: string) {
    buf_add(note, "```daimond\n")
    if local.is_param {
        -- name: Type, up to the `,` or `)` that ends the parameter
        let mut end = local.at + len(name)
        let mut depth = 0
        while end < len(text) {
            let c = byte_at(text, end)
            if c == 40 or c == 91 {
                depth = depth + 1
            } else if c == 41 or c == 93 {
                if depth == 0 {
                    break
                }
                depth = depth - 1
            } else if (c == 44 and depth == 0) or c == 10 {
                break
            }
            end = end + 1
        }
        buf_add_part(note, text, local.at, end - local.at)
        buf_add(note, "\n```\nA parameter.")
        return
    }
    -- The line that declares it, without the `{` that opens a block
    let start = skip_blanks(text, line_start(text, line_of_offset(text, local.at)))
    let mut end = line_end(text, local.at)
    if end > start and byte_at(text, end - 1) == 123 {
        end = before_blanks(text, end - 1, false) + 1
    }
    buf_add_part(note, text, start, end - start)
    buf_add(note, "\n```")
}

-- Add what an editor shows for the declaration whose line starts at
-- `line_at` in the symbols of the source
fn add_symbol_note(server: Server, note: int, src: int, line_at: int) {
    let symbols = buf_view(cell(src + SRC_SYMBOLS()))
    let kind = tab_field(symbols, line_at, 0)
    let container = tab_field(symbols, line_at, 5)
    buf_add(note, "```daimond\n")
    if kind == "import" {
        buf_add(note, "import ")
        buf_add(note, tab_field(symbols, line_at, 1))
        buf_add(note, "\n```\n`")
        buf_add(note, tab_field(symbols, line_at, 6))
        buf_add(note, "`")
        return
    }
    buf_add(note, tab_field(symbols, line_at, 6))
    buf_add(note, "\n```")
    if len(container) > 0 {
        if kind == "method" {
            buf_add(note, "\nA method of `")
        } else if kind == "field" {
            buf_add(note, "\nA field of `")
        } else {
            buf_add(note, "\nA variant of `")
        }
        buf_add(note, container)
        buf_add(note, "`.")
    }
    let line = json_int(symbols, tab_field_at(symbols, line_at, 2), 1) - 1
    add_comment_above(note, source_text(server, src), line)
}

-- textDocument/hover
fn hover(server: Server, id_at: int) {
    let target = request_target(server)
    if target.ok == false {
        reply(server, id_at, "null")
        return
    }
    let text = buf_view(cell(target.src + SRC_TEXT()))
    let start = word_start(text, target.at)
    if start < 0 {
        reply(server, id_at, "null")
        return
    }
    let end = word_end(text, start)
    let word = view(text, start, end - start)
    let note = server.note
    buf_reset(note)

    let mut local = Local { found: false, at: 0, is_param: false }
    if is_member(text, start) == false {
        local = find_local(server, text, scope_at(server, target.src, text, target.at), word, target.at)
    }
    if local.found {
        add_local_note(note, text, local, word)
    } else {
        let count = collect_matches(server, target.src, text, word, start)
        let mut i = 0
        while i < count and i < 8 {
            if i > 0 {
                buf_add(note, "\n\n---\n\n")
            }
            add_symbol_note(server, note, buf_get_i64(server.found, i * 2), buf_get_i64(server.found, i * 2 + 1))
            i = i + 1
        }
        if count == 0 {
            let builtin = find_builtin(server.builtins, word)
            let known = find_known_word(server.words, word)
            if len(builtin) > 0 {
                buf_add(note, "```daimond\n")
                buf_add(note, entry_head(builtin))
                buf_add(note, "\n```\n")
                buf_add(note, entry_text(builtin))
            } else if len(known) > 0 {
                buf_add(note, entry_text(known))
            }
        }
    }
    if buf_len(note) == 0 {
        reply(server, id_at, "null")
        return
    }
    begin_reply(server, id_at)
    let out = server.out
    buf_add(out, "{\"contents\":{\"kind\":\"markdown\",\"value\":")
    buf_json(out, buf_view(note))
    buf_add(out, "},\"range\":")
    add_range_of(out, text, line_of_offset(text, start), start, end)
    buf_add(out, "}")
    end_reply(server)
}

-- ---- Go to definition ----

-- textDocument/definition
fn definition(server: Server, id_at: int) {
    let target = request_target(server)
    if target.ok == false {
        reply(server, id_at, "null")
        return
    }
    let text = buf_view(cell(target.src + SRC_TEXT()))
    let start = word_start(text, target.at)
    if start < 0 {
        reply(server, id_at, "null")
        return
    }
    let word = view(text, start, word_end(text, start) - start)
    if is_member(text, start) == false {
        let local = find_local(server, text, scope_at(server, target.src, text, target.at), word, target.at)
        if local.found {
            begin_reply(server, id_at)
            add_location(server.out, target.src, text, local.at, len(word))
            end_reply(server)
            return
        }
    }
    let count = collect_matches(server, target.src, text, word, start)
    if count == 0 {
        reply(server, id_at, "null")
        return
    }
    begin_reply(server, id_at)
    let out = server.out
    buf_add(out, "[")
    let mut i = 0
    while i < count {
        if i > 0 {
            buf_add(out, ",")
        }
        add_symbol_location(server, out, buf_get_i64(server.found, i * 2), buf_get_i64(server.found, i * 2 + 1))
        i = i + 1
    }
    buf_add(out, "]")
    end_reply(server)
}

-- ---- Find references ----

-- Whether a declaration in Server.found has its name at line `line`
-- and column `column`, both from 1, of the source
fn is_declared_at(server: Server, src: int, line: int, column: int) -> bool {
    let mut i = 0
    while i < buf_len(server.found) / 16 {
        let found_src = buf_get_i64(server.found, i * 2)
        if found_src == src {
            let symbols = buf_view(cell(src + SRC_SYMBOLS()))
            let line_at = buf_get_i64(server.found, i * 2 + 1)
            if json_int(symbols, tab_field_at(symbols, line_at, 2), 0) == line and json_int(symbols, tab_field_at(symbols, line_at, 3), 0) == column {
                return true
            }
        }
        i = i + 1
    }
    return false
}

-- textDocument/references: the places a name is written. For a
-- variable, they are in its function. For another name, they are in
-- the document, the files it imports and the other open documents;
-- they are found by the name alone, so two fields of one name in
-- different types are not told apart.
fn references(server: Server, id_at: int) {
    let target = request_target(server)
    if target.ok == false {
        reply(server, id_at, "[]")
        return
    }
    let text = buf_view(cell(target.src + SRC_TEXT()))
    let start = word_start(text, target.at)
    if start < 0 {
        reply(server, id_at, "[]")
        return
    }
    let word = view(text, start, word_end(text, start) - start)
    let msg = buf_view(server.message)
    let context = json_member(msg, json_member(msg, message_root(msg), "params"), "context")
    let declaration_too = json_member(msg, context, "includeDeclaration")
    let skip_declaration = declaration_too >= 0 and view(msg, declaration_too, 5) == "false"

    let scope = scope_at(server, target.src, text, target.at)
    let mut local = Local { found: false, at: 0, is_param: false }
    if is_member(text, start) == false {
        local = find_local(server, text, scope, word, target.at)
    }
    if local.found {
        begin_reply(server, id_at)
        let out = server.out
        buf_add(out, "[")
        scan_begin(server, scope.start)
        let mut at = scan_next(server, text, word, scope.end)
        while at >= 0 {
            if is_member(text, at) == false and (skip_declaration == false or at != local.at) {
                if buf_last_byte(out) != 91 {
                    buf_add(out, ",")
                }
                add_location(out, target.src, text, at, len(word))
            }
            at = scan_next(server, text, word, scope.end)
        }
        buf_add(out, "]")
        end_reply(server)
        return
    }

    -- Every declaration of the name, to leave them out when asked to
    collect_sources(server, target.src)
    buf_reset(server.found)
    let mut class = 1
    while class <= 2 {
        let mut m = next_match(server, word, class, no_match())
        while m.found {
            buf_i64(server.found, m.src)
            buf_i64(server.found, m.line_at)
            m = next_match(server, word, class, m)
        }
        class = class + 1
    }
    -- The sources to search: the document and what it imports, then
    -- the other open documents
    let list = server.stack
    buf_reset(list)
    let mut i = 0
    while i < buf_len(server.visit) / 8 {
        buf_i64(list, buf_get_i64(server.visit, i))
        i = i + 1
    }
    let mut other = cell(server.state + STATE_SOURCES())
    while other != 0 {
        if cell(other + SRC_OPEN()) != 0 {
            let mut listed = false
            i = 0
            while i < buf_len(list) / 8 {
                if buf_get_i64(list, i) == other {
                    listed = true
                }
                i = i + 1
            }
            if listed == false {
                buf_i64(list, other)
            }
        }
        other = cell(other + SRC_NEXT())
    }

    begin_reply(server, id_at)
    let out = server.out
    buf_add(out, "[")
    i = 0
    while i < buf_len(list) / 8 {
        let src = buf_get_i64(list, i)
        let source = source_text(server, src)
        scan_begin(server, 0)
        let mut at = scan_next(server, source, word, len(source))
        while at >= 0 {
            let mut wanted = true
            if skip_declaration {
                let line = line_of_offset(source, at)
                wanted = is_declared_at(server, src, line + 1, at - line_start(source, line) + 1) == false
            }
            if wanted {
                if buf_last_byte(out) != 91 {
                    buf_add(out, ",")
                }
                add_location(out, src, source, at, len(word))
            }
            at = scan_next(server, source, word, len(source))
        }
        i = i + 1
    }
    buf_add(out, "]")
    end_reply(server)
}

-- ---- Document symbols ----

-- The protocol's SymbolKind for a kind of declaration
fn symbol_kind_number(kind: string) -> int {
    if kind == "function" { return 12 }
    if kind == "method" { return 6 }
    if kind == "struct" { return 23 }
    if kind == "field" { return 8 }
    if kind == "enum" { return 10 }
    if kind == "variant" { return 22 }
    if kind == "trait" { return 11 }
    if kind == "const" { return 14 }
    -- An impl block
    return 3
}

-- textDocument/documentSymbol: the declarations of the document, with
-- the fields, variants and methods of a type inside it
fn document_symbols(server: Server, id_at: int) {
    let src = request_source(server, false)
    if src == 0 or cell(src + SRC_OPEN()) == 0 {
        reply(server, id_at, "[]")
        return
    }
    let symbols = buf_view(source_symbols(server, src))
    let text = buf_view(cell(src + SRC_TEXT()))
    begin_reply(server, id_at)
    let out = server.out
    buf_add(out, "[")
    let mut in_type = false
    let mut at = 0
    while at < len(symbols) {
        let kind = tab_field(symbols, at, 0)
        let is_child = kind_class(kind) == 2
        if kind != "import" and (is_child == false or in_type) {
            if is_child == false and in_type {
                buf_add(out, "]}")
                in_type = false
            }
            if buf_last_byte(out) != 91 {
                buf_add(out, ",")
            }
            let name = tab_field(symbols, at, 1)
            let line = json_int(symbols, tab_field_at(symbols, at, 2), 1) - 1
            let column = json_int(symbols, tab_field_at(symbols, at, 3), 1) - 1
            let mut last_line = json_int(symbols, tab_field_at(symbols, at, 4), 1) - 1
            if last_line < line {
                last_line = line
            }
            let name_at = line_start(text, line) + column
            let last_start = line_start(text, last_line)
            buf_add(out, "{\"name\":")
            buf_json(out, name)
            buf_add(out, ",\"detail\":")
            buf_json(out, tab_field(symbols, at, 6))
            buf_add(out, ",\"kind\":")
            buf_int(out, symbol_kind_number(kind))
            buf_add(out, ",\"range\":")
            add_range(out, line, 0, last_line, character_of_offset(text, last_start, line_end(text, last_start)))
            buf_add(out, ",\"selectionRange\":")
            add_range_of(out, text, line, name_at, name_at + len(name))
            if is_child {
                buf_add(out, "}")
            } else {
                buf_add(out, ",\"children\":[")
                in_type = true
            }
        }
        at = next_line(symbols, at)
    }
    if in_type {
        buf_add(out, "]}")
    }
    buf_add(out, "]")
    end_reply(server)
}

-- ---- Completion ----

-- Add a completion item to the list being written to `out`
fn add_item(out: int, label: string, kind: int, detail: string) {
    if len(label) == 0 {
        return
    }
    if buf_last_byte(out) != 91 {
        buf_add(out, ",")
    }
    buf_add(out, "{\"label\":")
    buf_json(out, label)
    buf_add(out, ",\"kind\":")
    buf_int(out, kind)
    if len(detail) > 0 {
        buf_add(out, ",\"detail\":")
        buf_json(out, detail)
    }
    buf_add(out, "}")
}

-- The protocol's CompletionItemKind for a kind of declaration
fn item_kind_number(kind: string) -> int {
    if kind == "function" { return 3 }
    if kind == "method" { return 2 }
    if kind == "struct" { return 22 }
    if kind == "field" { return 5 }
    if kind == "enum" { return 13 }
    if kind == "variant" { return 20 }
    if kind == "trait" { return 8 }
    if kind == "const" { return 21 }
    -- An import
    return 9
}

-- textDocument/completion. After a `.`: the fields, methods and
-- variants of the document and its imports, and the methods of the
-- builtin types. Elsewhere: the variables of the function, what the
-- document and its imports declare, the builtin functions, the
-- keywords and the builtin types. The editor picks by what is typed.
fn completion(server: Server, id_at: int) {
    let target = request_target(server)
    if target.ok == false {
        reply(server, id_at, "{\"isIncomplete\":false,\"items\":[]}")
        return
    }
    let text = buf_view(cell(target.src + SRC_TEXT()))
    let mut start = word_start(text, target.at)
    if start < 0 {
        start = target.at
    }
    let after_dot = is_member(text, start)
    let scope = scope_at(server, target.src, text, target.at)
    collect_sources(server, target.src)

    begin_reply(server, id_at)
    let out = server.out
    buf_add(out, "{\"isIncomplete\":false,\"items\":[")

    if after_dot == false and scope.found {
        -- The variables declared before the name being typed, each once
        let seen = server.scratch
        buf_reset(seen)
        buf_add(seen, "\t")
        scan_begin(server, scope.start)
        let mut at = scan_word(server, text, start)
        while at >= 0 {
            let count = word_end(text, at) - at
            let kind = declaration_kind(text, at, count, scope.body_start)
            if kind != 0 {
                let mark = buf_len(seen)
                buf_add_part(seen, text, at, count)
                buf_add(seen, "\t")
                let names = buf_view(seen)
                -- "\tname\t", unless it is there already
                if string_find(view(names, 0, mark), view(names, mark - 1, count + 2)) >= 0 {
                    buf_set_len(seen, mark)
                } else if kind == 2 {
                    add_item(out, view(text, at, count), 6, "parameter")
                } else {
                    add_item(out, view(text, at, count), 6, "variable")
                }
            }
            at = scan_word(server, text, start)
        }
    }

    let mut class = 1
    if after_dot {
        class = 2
    }
    let mut i = 0
    while i < buf_len(server.visit) / 8 {
        let symbols = buf_view(cell(buf_get_i64(server.visit, i) + SRC_SYMBOLS()))
        let mut at = 0
        while at < len(symbols) {
            let kind = tab_field(symbols, at, 0)
            if kind_class(kind) == class {
                add_item(out, tab_field(symbols, at, 1), item_kind_number(kind), tab_field(symbols, at, 6))
            }
            at = next_line(symbols, at)
        }
        i = i + 1
    }

    if after_dot {
        i = 0
        while i < server.methods.len() {
            let method = entry_head(server.methods[i])
            add_item(out, view(method, 0, string_find(method, "(")), 2, method)
            i = i + 1
        }
    } else {
        i = 0
        while i < server.builtins.len() {
            let declaration = entry_head(server.builtins[i])
            add_item(out, declared_name(declaration), 3, declaration)
            i = i + 1
        }
        i = 0
        while i < server.words.len() {
            let entry = server.words[i]
            -- The text of a type starts with **
            if starts_with(entry_text(entry), "**") {
                add_item(out, entry_head(entry), 7, "")
            } else {
                add_item(out, entry_head(entry), 14, "")
            }
            i = i + 1
        }
    }
    buf_add(out, "]}")
    end_reply(server)
}

-- ---- Signature help ----

-- The offset of the `(` of the call that offset `at` is in the
-- arguments of; -1 when it is in none. The text is read from `from`,
-- which must be in code, so that brackets in strings and comments do
-- not count.
fn call_open(server: Server, text: string, from: int, at: int) -> int {
    -- The brackets that are open, innermost last
    let open = server.stack
    buf_reset(open)
    let mut mode = 0
    let mut i = from
    while i < at and i < len(text) {
        let c = byte_at(text, i)
        if mode == 3 {
            if c == 10 {
                mode = 0
            }
        } else {
            mode = scan_state(mode, c, byte_or_zero(text, i + 1))
            if mode == 0 {
                if c == 40 or c == 91 or c == 123 {
                    buf_i64(open, i)
                } else if (c == 41 or c == 93 or c == 125) and buf_len(open) > 0 {
                    buf_set_len(open, buf_len(open) - 8)
                }
            }
        }
        i = i + 1
    }
    -- The innermost `(`. A `[` or `{` inside it is part of an
    -- argument: a list, or the fields of a struct.
    let mut k = buf_len(open) / 8 - 1
    while k >= 0 {
        let bracket = buf_get_i64(open, k)
        if byte_at(text, bracket) == 40 {
            return bracket
        }
        k = k - 1
    }
    return -1
}

-- How many arguments are complete between the `(` at `open` and
-- offset `at`: the commas there that are not inside brackets or strings
fn arguments_before(text: string, open: int, at: int) -> int {
    let mut commas = 0
    let mut depth = 0
    let mut mode = 0
    let mut i = open + 1
    while i < at and i < len(text) {
        let c = byte_at(text, i)
        if mode == 3 {
            if c == 10 {
                mode = 0
            }
        } else {
            mode = scan_state(mode, c, byte_or_zero(text, i + 1))
            if mode == 0 {
                if c == 40 or c == 91 or c == 123 {
                    depth = depth + 1
                } else if c == 41 or c == 93 or c == 125 {
                    depth = depth - 1
                } else if c == 44 and depth == 0 {
                    commas = commas + 1
                }
            }
        }
        i = i + 1
    }
    return commas
}

-- Add the parameters of a declaration such as
-- "fn add(a: int, b: int) -> int" as the protocol's list of
-- parameters, without `self` when `skip_self`. Gives how many.
fn add_parameters(out: int, signature: string, skip_self: bool) -> int {
    let open = string_find(signature, "(")
    let mut count = 0
    if open < 0 {
        return 0
    }
    let mut depth = 0
    let mut start = open + 1
    let mut i = open + 1
    while i < len(signature) {
        let c = byte_at(signature, i)
        let closes = (c == 41 or c == 93) and depth == 0
        if closes or (c == 44 and depth == 0) {
            let from = skip_blanks(signature, start)
            let piece = view(signature, from, i - from)
            let is_self = piece == "self" or piece == "mut self"
            if len(piece) > 0 and (is_self == false or skip_self == false) {
                if count > 0 {
                    buf_add(out, ",")
                }
                buf_add(out, "{\"label\":")
                buf_json(out, piece)
                buf_add(out, "}")
                count = count + 1
            }
            start = i + 1
            if closes {
                return count
            }
        } else if c == 40 or c == 91 {
            depth = depth + 1
        } else if c == 41 or c == 93 {
            depth = depth - 1
        }
        i = i + 1
    }
    return count
}

-- textDocument/signatureHelp
fn signature_help(server: Server, id_at: int) {
    let target = request_target(server)
    if target.ok == false {
        reply(server, id_at, "null")
        return
    }
    let text = buf_view(cell(target.src + SRC_TEXT()))
    let scope = scope_at(server, target.src, text, target.at)
    let open = call_open(server, text, scope.start, target.at)
    let name = word_ending_at(text, open - 1)
    if open < 0 or len(name) == 0 {
        reply(server, id_at, "null")
        return
    }
    let name_at = open - len(name)
    -- The declaration: the document's or an import's, else a builtin's
    let note = server.note
    buf_reset(note)
    if collect_matches(server, target.src, text, name, name_at) > 0 {
        let symbols = buf_view(cell(buf_get_i64(server.found, 0) + SRC_SYMBOLS()))
        let line_at = buf_get_i64(server.found, 1)
        let kind = tab_field(symbols, line_at, 0)
        if kind == "function" or kind == "method" {
            buf_add(note, tab_field(symbols, line_at, 6))
        }
    }
    if buf_len(note) == 0 {
        buf_add(note, entry_head(find_builtin(server.builtins, name)))
    }
    if buf_len(note) == 0 {
        reply(server, id_at, "null")
        return
    }
    begin_reply(server, id_at)
    let out = server.out
    buf_add(out, "{\"signatures\":[{\"label\":")
    buf_json(out, buf_view(note))
    buf_add(out, ",\"parameters\":[")
    -- A method called on a value is not given its `self`
    let count = add_parameters(out, buf_view(note), is_member(text, name_at))
    let mut active = arguments_before(text, open, target.at)
    if active >= count and count > 0 {
        active = count - 1
    }
    buf_add(out, "]}],\"activeSignature\":0,\"activeParameter\":")
    buf_int(out, active)
    buf_add(out, "}")
    end_reply(server)
}

-- ---- Formatting ----

-- textDocument/formatting: the document as `daimond fmt` writes it
fn formatting(server: Server, id_at: int) {
    let src = request_source(server, false)
    if src == 0 or cell(src + SRC_OPEN()) == 0 {
        reply(server, id_at, "null")
        return
    }
    let text = buf_view(cell(src + SRC_TEXT()))
    let formatted = server.scratch
    buf_reset(formatted)
    let mut ran = false
    if buf_write_file(cell(src + SRC_TEXT()), server.tmp_text) {
        if run_command_io(server.fmt_args, "/dev/null", "/dev/null", server.tmp_err) == 0 {
            ran = buf_read_file(formatted, server.tmp_fmt)
        }
    }
    if ran == false {
        reply_error(server, id_at, -32603, "daimond fmt failed")
        return
    }
    if buf_view(formatted) == text {
        reply(server, id_at, "[]")
        return
    }
    -- One edit that replaces the whole document
    let last_line = line_of_offset(text, len(text))
    begin_reply(server, id_at)
    let out = server.out
    buf_add(out, "[{\"range\":")
    add_range(out, 0, 0, last_line, character_of_offset(text, line_start(text, last_line), len(text)))
    buf_add(out, ",\"newText\":")
    buf_json(out, buf_view(formatted))
    buf_add(out, "}]")
    end_reply(server)
}
