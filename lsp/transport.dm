module transport

import buffer
import json
import text
import sources

-- ============================================================
-- Messages to and from the editor
-- A message is a header, "Content-Length: N" and an empty line, each
-- ended by CR LF, and then N bytes of JSON.
-- ============================================================

extern fn lsp_c_read(fd: i32, to: int, n: int) -> int = "read"
extern fn lsp_c_poll(fds: int, n: int, timeout_ms: i32) -> i32 = "poll"

-- Read more of standard input into the inbox. False at its end.
fn read_more(inbox: int) -> bool {
    buf_reserve(inbox, 65536)
    let mut got = 0
    unsafe {
        got = lsp_c_read(0, buf_data(inbox) + buf_len(inbox), 65536)
    }
    if got <= 0 {
        return false
    }
    buf_set_len(inbox, buf_len(inbox) + got)
    return true
}

-- Whether the editor has sent something that is not yet handled, or
-- sends something within `wait_ms` milliseconds. True at the end of
-- input, too: there is something to learn by reading.
fn input_waiting(server: Server, wait_ms: int) -> bool {
    if buf_len(server.inbox) > 0 {
        return true
    }
    unsafe {
        -- struct pollfd: the descriptor, the events asked about (1 is
        -- POLLIN), and the events found
        store_i32(server.poll_fd, 0)
        store_i16(server.poll_fd + 4, 1)
        store_i16(server.poll_fd + 6, 0)
        return lsp_c_poll(server.poll_fd, 1, wait_ms as i32) as int != 0
    }
}

-- The number a Content-Length header gives, among the header lines in
-- `headers`; -1 when there is none
fn content_length(headers: string) -> int {
    let mut at = 0
    while at < len(headers) {
        -- The name of a header is in any case
        let name = "content-length:"
        let mut k = 0
        while k < len(name) and at + k < len(headers) {
            let mut c = byte_at(headers, at + k)
            if c >= 65 and c <= 90 {
                c = c + 32
            }
            if c != byte_at(name, k) {
                break
            }
            k = k + 1
        }
        if k == len(name) {
            return json_int(headers, skip_blanks(headers, at + k), -1)
        }
        at = next_line(headers, at)
    }
    return -1
}

-- Read the next message into the buffer `message`. False when the
-- input ends, or is not the protocol.
fn read_message(inbox: int, message: int) -> bool {
    while true {
        let mut header_end = string_find(buf_view(inbox), "\r\n\r\n")
        while header_end < 0 {
            if buf_len(inbox) > 65536 {
                return false
            }
            if read_more(inbox) == false {
                return false
            }
            header_end = string_find(buf_view(inbox), "\r\n\r\n")
        }
        let length = content_length(view(buf_view(inbox), 0, header_end + 2))
        let body = header_end + 4
        if length > 268435456 {
            return false
        }
        if length < 0 {
            -- A header without a length has no body to read
            buf_drop_front(inbox, body)
        } else {
            while buf_len(inbox) < body + length {
                if read_more(inbox) == false {
                    return false
                }
            }
            buf_reset(message)
            buf_add_part(message, buf_view(inbox), body, length)
            buf_drop_front(inbox, body + length)
            return true
        }
    }
    return false
}

-- Send the message in Server.out
fn send_message(server: Server) {
    let frame = server.frame
    buf_reset(frame)
    buf_add(frame, "Content-Length: ")
    buf_int(frame, buf_len(server.out))
    buf_add(frame, "\r\n\r\n")
    print(buf_view(frame))
    print(buf_view(server.out))
    flush()
}

-- Start the reply to the request whose id is at `id_at` in the message
-- being handled. The result is written to Server.out next, and
-- end_reply sends it.
fn begin_reply(server: Server, id_at: int) {
    let msg = buf_view(server.message)
    let out = server.out
    buf_reset(out)
    buf_add(out, "{\"jsonrpc\":\"2.0\",\"id\":")
    if id_at < 0 {
        buf_add(out, "null")
    } else {
        buf_add_part(out, msg, id_at, json_end(msg, id_at) - id_at)
    }
    buf_add(out, ",\"result\":")
}

fn end_reply(server: Server) {
    buf_add(server.out, "}")
    send_message(server)
}

-- Reply with the result `result`, which is JSON
fn reply(server: Server, id_at: int, result: string) {
    begin_reply(server, id_at)
    buf_add(server.out, result)
    end_reply(server)
}

-- Reply that the request failed
fn reply_error(server: Server, id_at: int, code: int, message: string) {
    begin_reply(server, id_at)
    let out = server.out
    -- An error takes the place of the result
    buf_set_len(out, buf_len(out) - len("\"result\":"))
    buf_add(out, "\"error\":{\"code\":")
    buf_int(out, code)
    buf_add(out, ",\"message\":")
    buf_json(out, message)
    buf_add(out, "}")
    end_reply(server)
}

-- Start a notification. Its parameters are written to Server.out next,
-- and end_reply sends it.
fn begin_notification(server: Server, method: string) {
    let out = server.out
    buf_reset(out)
    buf_add(out, "{\"jsonrpc\":\"2.0\",\"method\":")
    buf_json(out, method)
    buf_add(out, ",\"params\":")
}

-- Add the range from (line, character) to (end_line, end_character),
-- all from 0, as the protocol writes it
fn add_range(out: int, line: int, character: int, end_line: int, end_character: int) {
    buf_add(out, "{\"start\":{\"line\":")
    buf_int(out, line)
    buf_add(out, ",\"character\":")
    buf_int(out, character)
    buf_add(out, "},\"end\":{\"line\":")
    buf_int(out, end_line)
    buf_add(out, ",\"character\":")
    buf_int(out, end_character)
    buf_add(out, "}}")
}

-- Add the range of the bytes from `start` to `end` of `text`, which
-- are on line `line` (from 0)
fn add_range_of(out: int, text: string, line: int, start: int, end: int) {
    let first = line_start(text, line)
    add_range(out, line, character_of_offset(text, first, start), line, character_of_offset(text, first, end))
}

-- The number, from 0, of the line that offset `at` of the text is on
fn line_of_offset(text: string, at: int) -> int {
    let mut line = 0
    let mut i = 0
    while i < at and i < len(text) {
        if byte_at(text, i) == 10 {
            line = line + 1
        }
        i = i + 1
    }
    return line
}
