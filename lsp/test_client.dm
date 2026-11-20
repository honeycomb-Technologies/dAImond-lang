module test_client

import buffer
import json
import text

-- ============================================================
-- A scripted editor, for testing the language server
--
--   test_client send <script> <directory>
--       writes the messages of the script to standard output, as an
--       editor frames them
--   test_client receive <directory>
--       reads the server's messages from standard input and prints
--       one a line, with the directory written as $DIR
--
-- So a session is:
--   test_client send script dir | daimond-lsp | test_client receive dir
--
-- A line of the script is one of:
--   # a comment
--   open FILE                  the editor opens FILE, a file of the directory
--   change FILE NEW            FILE now has the text of the file NEW
--   save FILE
--   close FILE
--   ask METHOD FILE            the request textDocument/METHOD for FILE
--   ask METHOD FILE LINE CHARACTER   and for a place in it, both from 0
--   { ... }                    a message as written, $DIR being the directory
-- Requests are numbered from 1.
-- ============================================================

fn send_frame(body: string) {
    print("Content-Length: " + int_to_string(len(body)) + "\r\n\r\n" + body)
}

fn uri_of(directory: string, file: string) -> string {
    let b = buf_new(64)
    path_uri_into(b, directory + "/" + file)
    return buf_take(b)
}

fn notify(method: string, params: string) {
    send_frame("{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/" + method + "\",\"params\":" + params + "}")
}

fn send_script(script: string, directory: string) -> int {
    let lines = string_split(file_read(script), "\n")
    let mut next_id = 1
    let mut i = 0
    while i < lines.len() {
        let line = string_trim(lines[i])
        let words = string_split(line, " ")
        if line == "" or starts_with(line, "#") {
            -- nothing to send
        } else if starts_with(line, "{") {
            send_frame(string_replace(line, "$DIR", directory))
        } else if words[0] == "open" and words.len() == 2 {
            let uri = json_quote(uri_of(directory, words[1]))
            let text = json_quote(file_read(directory + "/" + words[1]))
            notify("didOpen", "{\"textDocument\":{\"uri\":" + uri + ",\"languageId\":\"daimond\",\"version\":1,\"text\":" + text + "}}")
        } else if words[0] == "change" and words.len() == 3 {
            let uri = json_quote(uri_of(directory, words[1]))
            let text = json_quote(file_read(directory + "/" + words[2]))
            notify("didChange", "{\"textDocument\":{\"uri\":" + uri + ",\"version\":2},\"contentChanges\":[{\"text\":" + text + "}]}")
        } else if (words[0] == "save" or words[0] == "close") and words.len() == 2 {
            let uri = json_quote(uri_of(directory, words[1]))
            let mut method = "didSave"
            if words[0] == "close" {
                method = "didClose"
            }
            notify(method, "{\"textDocument\":{\"uri\":" + uri + "}}")
        } else if words[0] == "ask" and (words.len() == 3 or words.len() == 5) {
            let mut params = "{\"textDocument\":{\"uri\":" + json_quote(uri_of(directory, words[2])) + "}"
            if words.len() == 5 {
                params = params + ",\"position\":{\"line\":" + words[3] + ",\"character\":" + words[4] + "}"
            }
            send_frame("{\"jsonrpc\":\"2.0\",\"id\":" + int_to_string(next_id) + ",\"method\":\"textDocument/" + words[1] + "\",\"params\":" + params + "}}")
            next_id = next_id + 1
        } else {
            eprintln("test_client: line " + int_to_string(i + 1) + " of " + script + " is not understood: " + line)
            return 2
        }
        i = i + 1
    }
    flush()
    return 0
}

fn receive(directory: string) -> int {
    let input = file_read("/dev/stdin")
    let mut at = 0
    while at < len(input) {
        let rest = view(input, at, len(input) - at)
        let header_end = string_find(rest, "\r\n\r\n")
        if starts_with(rest, "Content-Length: ") == false or header_end < 0 {
            println("NOT A MESSAGE: " + rest)
            return 1
        }
        let length = json_int(rest, 16, -1)
        let body_at = at + header_end + 4
        if length < 0 or body_at + length > len(input) {
            println("A MESSAGE SHORTER THAN ITS Content-Length: " + rest)
            return 1
        }
        println(string_replace(substr(input, body_at, length), directory, "$DIR"))
        at = body_at + length
    }
    return 0
}

fn main() -> int {
    if args_len() == 4 and args_get(1) == "send" {
        return send_script(args_get(2), args_get(3))
    }
    if args_len() == 3 and args_get(1) == "receive" {
        return receive(args_get(2))
    }
    eprintln("Usage: test_client send <script> <directory>")
    eprintln("       test_client receive <directory>")
    return 2
}
