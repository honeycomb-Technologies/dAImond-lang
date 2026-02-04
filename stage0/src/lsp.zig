//! dAImond Language Server Protocol (LSP) Implementation
//!
//! Provides IDE integration via the Language Server Protocol over JSON-RPC/stdio.
//! Supports:
//!   - textDocument/didOpen, didChange, didClose
//!   - textDocument/publishDiagnostics (real-time error reporting)
//!   - textDocument/completion (keyword + builtin completion)
//!   - textDocument/hover (type information)
//!   - initialize/shutdown/exit lifecycle

const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("lexer.zig").Lexer;
const TokenType = @import("lexer.zig").TokenType;
const parser_mod = @import("parser.zig");
const Parser = parser_mod.Parser;
const checker_mod = @import("checker.zig");
const TypeChecker = checker_mod.TypeChecker;

// ============================================================================
// JSON Helpers (minimal subset for LSP)
// ============================================================================

const JsonWriter = struct {
    buf: std.ArrayList(u8),

    fn init(allocator: Allocator) JsonWriter {
        return .{ .buf = std.ArrayList(u8).init(allocator) };
    }

    fn deinit(self: *JsonWriter) void {
        self.buf.deinit();
    }

    fn reset(self: *JsonWriter) void {
        self.buf.clearRetainingCapacity();
    }

    fn raw(self: *JsonWriter, s: []const u8) void {
        self.buf.appendSlice(s) catch {};
    }

    fn string(self: *JsonWriter, s: []const u8) void {
        self.raw("\"");
        for (s) |ch| {
            switch (ch) {
                '"' => self.raw("\\\""),
                '\\' => self.raw("\\\\"),
                '\n' => self.raw("\\n"),
                '\r' => self.raw("\\r"),
                '\t' => self.raw("\\t"),
                else => self.buf.append(ch) catch {},
            }
        }
        self.raw("\"");
    }

    fn int(self: *JsonWriter, n: i64) void {
        var num_buf: [20]u8 = undefined;
        const s = std.fmt.bufPrint(&num_buf, "{d}", .{n}) catch "0";
        self.raw(s);
    }

    fn boolean(self: *JsonWriter, b: bool) void {
        self.raw(if (b) "true" else "false");
    }

    fn toSlice(self: *JsonWriter) []const u8 {
        return self.buf.items;
    }
};

// ============================================================================
// Simple JSON Parser (for LSP requests)
// ============================================================================

fn jsonGetString(json: []const u8, key: []const u8) ?[]const u8 {
    // Find "key": "value" pattern
    var search_buf: [256]u8 = undefined;
    const search = std.fmt.bufPrint(&search_buf, "\"{s}\"", .{key}) catch return null;

    if (std.mem.indexOf(u8, json, search)) |key_pos| {
        var i = key_pos + search.len;
        // Skip : and whitespace
        while (i < json.len and (json[i] == ':' or json[i] == ' ' or json[i] == '\t')) : (i += 1) {}
        if (i < json.len and json[i] == '"') {
            i += 1;
            const start = i;
            while (i < json.len and json[i] != '"') {
                if (json[i] == '\\') i += 1; // skip escaped char
                i += 1;
            }
            return json[start..i];
        }
    }
    return null;
}

fn jsonGetInt(json: []const u8, key: []const u8) ?i64 {
    var search_buf: [256]u8 = undefined;
    const search = std.fmt.bufPrint(&search_buf, "\"{s}\"", .{key}) catch return null;

    if (std.mem.indexOf(u8, json, search)) |key_pos| {
        var i = key_pos + search.len;
        // Skip : and whitespace
        while (i < json.len and (json[i] == ':' or json[i] == ' ' or json[i] == '\t')) : (i += 1) {}
        if (i < json.len and (json[i] >= '0' and json[i] <= '9' or json[i] == '-')) {
            const start = i;
            if (json[i] == '-') i += 1;
            while (i < json.len and json[i] >= '0' and json[i] <= '9') : (i += 1) {}
            return std.fmt.parseInt(i64, json[start..i], 10) catch null;
        }
    }
    return null;
}

fn jsonGetObject(json: []const u8, key: []const u8) ?[]const u8 {
    var search_buf: [256]u8 = undefined;
    const search = std.fmt.bufPrint(&search_buf, "\"{s}\"", .{key}) catch return null;

    if (std.mem.indexOf(u8, json, search)) |key_pos| {
        var i = key_pos + search.len;
        while (i < json.len and (json[i] == ':' or json[i] == ' ' or json[i] == '\t')) : (i += 1) {}
        if (i < json.len and json[i] == '{') {
            var depth: i32 = 0;
            const start = i;
            while (i < json.len) : (i += 1) {
                if (json[i] == '{') depth += 1;
                if (json[i] == '}') {
                    depth -= 1;
                    if (depth == 0) return json[start .. i + 1];
                }
                if (json[i] == '"') {
                    i += 1;
                    while (i < json.len and json[i] != '"') {
                        if (json[i] == '\\') i += 1;
                        i += 1;
                    }
                }
            }
        }
    }
    return null;
}

// ============================================================================
// LSP Server
// ============================================================================

const LspServer = struct {
    allocator: Allocator,
    writer: JsonWriter,
    stderr: std.fs.File.Writer,
    documents: std.StringHashMap([]const u8), // uri -> content
    initialized: bool = false,
    shutdown_requested: bool = false,

    fn init(allocator: Allocator) LspServer {
        return .{
            .allocator = allocator,
            .writer = JsonWriter.init(allocator),
            .stderr = std.io.getStdErr().writer(),
            .documents = std.StringHashMap([]const u8).init(allocator),
        };
    }

    fn deinit(self: *LspServer) void {
        var it = self.documents.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.documents.deinit();
        self.writer.deinit();
    }

    fn log(self: *LspServer, comptime fmt: []const u8, args: anytype) void {
        self.stderr.print("[daimond-lsp] " ++ fmt ++ "\n", args) catch {};
    }

    // ========================================================================
    // Message I/O
    // ========================================================================

    fn readMessage(self: *LspServer) !?[]const u8 {
        _ = self;
        const stdin = std.io.getStdIn().reader();

        // Read Content-Length header
        var header_buf: [1024]u8 = undefined;
        var content_length: usize = 0;

        while (true) {
            const line = stdin.readUntilDelimiter(&header_buf, '\n') catch |err| switch (err) {
                error.EndOfStream => return null,
                else => return err,
            };
            const trimmed = std.mem.trim(u8, line, "\r\n ");

            if (trimmed.len == 0) break; // Empty line = end of headers

            if (std.mem.startsWith(u8, trimmed, "Content-Length:")) {
                const len_str = std.mem.trim(u8, trimmed["Content-Length:".len..], " ");
                content_length = std.fmt.parseInt(usize, len_str, 10) catch 0;
            }
        }

        if (content_length == 0) return null;

        // Read body
        const body = try std.heap.page_allocator.alloc(u8, content_length);
        try stdin.readNoEof(body);
        return body;
    }

    fn sendMessage(_: *LspServer, content: []const u8) void {
        const stdout = std.io.getStdOut().writer();
        stdout.print("Content-Length: {d}\r\n\r\n{s}", .{ content.len, content }) catch {};
    }

    fn sendResponse(self: *LspServer, id: ?i64, id_str: ?[]const u8, result: []const u8) void {
        self.writer.reset();
        self.writer.raw("{\"jsonrpc\":\"2.0\",\"id\":");
        if (id) |n| {
            self.writer.int(n);
        } else if (id_str) |s| {
            self.writer.string(s);
        } else {
            self.writer.raw("null");
        }
        self.writer.raw(",\"result\":");
        self.writer.raw(result);
        self.writer.raw("}");
        self.sendMessage(self.writer.toSlice());
    }

    fn sendNotification(self: *LspServer, method: []const u8, params: []const u8) void {
        self.writer.reset();
        self.writer.raw("{\"jsonrpc\":\"2.0\",\"method\":");
        self.writer.string(method);
        self.writer.raw(",\"params\":");
        self.writer.raw(params);
        self.writer.raw("}");
        self.sendMessage(self.writer.toSlice());
    }

    // ========================================================================
    // Request Handling
    // ========================================================================

    fn handleMessage(self: *LspServer, msg: []const u8) !void {
        const method = jsonGetString(msg, "method") orelse return;
        const id = jsonGetInt(msg, "id");

        self.log("Received: {s}", .{method});

        if (std.mem.eql(u8, method, "initialize")) {
            try self.handleInitialize(id);
        } else if (std.mem.eql(u8, method, "initialized")) {
            self.initialized = true;
            self.log("Server initialized", .{});
        } else if (std.mem.eql(u8, method, "shutdown")) {
            self.shutdown_requested = true;
            self.sendResponse(id, null, "null");
        } else if (std.mem.eql(u8, method, "exit")) {
            std.process.exit(if (self.shutdown_requested) 0 else 1);
        } else if (std.mem.eql(u8, method, "textDocument/didOpen")) {
            try self.handleDidOpen(msg);
        } else if (std.mem.eql(u8, method, "textDocument/didChange")) {
            try self.handleDidChange(msg);
        } else if (std.mem.eql(u8, method, "textDocument/didClose")) {
            try self.handleDidClose(msg);
        } else if (std.mem.eql(u8, method, "textDocument/completion")) {
            try self.handleCompletion(id, msg);
        } else if (std.mem.eql(u8, method, "textDocument/hover")) {
            try self.handleHover(id, msg);
        } else {
            // Unknown method - send null result for requests with id
            if (id != null) {
                self.sendResponse(id, null, "null");
            }
        }
    }

    fn handleInitialize(self: *LspServer, id: ?i64) !void {
        var result = JsonWriter.init(self.allocator);
        defer result.deinit();

        result.raw("{");
        result.raw("\"capabilities\":{");
        result.raw("\"textDocumentSync\":1,"); // Full sync
        result.raw("\"completionProvider\":{\"triggerCharacters\":[\".\",\":\"]},");
        result.raw("\"hoverProvider\":true");
        result.raw("},");
        result.raw("\"serverInfo\":{\"name\":\"daimond-lsp\",\"version\":\"0.1.0\"}");
        result.raw("}");

        self.sendResponse(id, null, result.toSlice());
    }

    fn handleDidOpen(self: *LspServer, msg: []const u8) !void {
        const params = jsonGetObject(msg, "params") orelse return;
        const text_doc = jsonGetObject(params, "textDocument") orelse return;
        const uri = jsonGetString(text_doc, "uri") orelse return;
        const text = jsonGetString(text_doc, "text") orelse return;

        // Store document content
        const uri_copy = try self.allocator.dupe(u8, uri);
        const text_copy = try self.allocator.dupe(u8, text);

        if (self.documents.getPtr(uri_copy)) |existing| {
            self.allocator.free(existing.*);
            existing.* = text_copy;
            self.allocator.free(uri_copy);
        } else {
            try self.documents.put(uri_copy, text_copy);
        }

        // Run diagnostics
        self.publishDiagnostics(uri, text);
    }

    fn handleDidChange(self: *LspServer, msg: []const u8) !void {
        const params = jsonGetObject(msg, "params") orelse return;
        const text_doc = jsonGetObject(params, "textDocument") orelse return;
        const uri = jsonGetString(text_doc, "uri") orelse return;

        // For full sync, the entire text is in contentChanges[0].text
        // Find contentChanges array
        if (std.mem.indexOf(u8, params, "\"contentChanges\"")) |cc_pos| {
            // Find "text" within the contentChanges
            if (jsonGetString(params[cc_pos..], "text")) |new_text| {
                const text_copy = try self.allocator.dupe(u8, new_text);

                if (self.documents.getPtr(uri)) |existing| {
                    self.allocator.free(existing.*);
                    existing.* = text_copy;
                } else {
                    const uri_copy = try self.allocator.dupe(u8, uri);
                    try self.documents.put(uri_copy, text_copy);
                }

                self.publishDiagnostics(uri, new_text);
            }
        }
    }

    fn handleDidClose(self: *LspServer, msg: []const u8) !void {
        const params = jsonGetObject(msg, "params") orelse return;
        const text_doc = jsonGetObject(params, "textDocument") orelse return;
        const uri = jsonGetString(text_doc, "uri") orelse return;

        // Remove document and clear diagnostics
        if (self.documents.fetchRemove(uri)) |kv| {
            self.allocator.free(kv.key);
            self.allocator.free(kv.value);
        }

        // Publish empty diagnostics to clear them
        var diag = JsonWriter.init(self.allocator);
        defer diag.deinit();
        diag.raw("{\"uri\":");
        diag.string(uri);
        diag.raw(",\"diagnostics\":[]}");
        self.sendNotification("textDocument/publishDiagnostics", diag.toSlice());
    }

    fn handleCompletion(self: *LspServer, id: ?i64, msg: []const u8) !void {
        _ = msg;

        var result = JsonWriter.init(self.allocator);
        defer result.deinit();

        result.raw("{\"isIncomplete\":false,\"items\":[");

        // Add keyword completions
        const keywords = [_][]const u8{
            "fn",       "let",     "mut",       "if",          "else",
            "while",    "for",     "in",        "return",      "true",
            "false",    "struct",  "enum",      "impl",        "trait",
            "match",    "module",  "import",    "break",       "continue",
            "loop",     "region",  "comptime",  "extern",      "with",
            "and",      "or",      "not",       "as",          "dyn",
        };

        var first = true;
        for (keywords) |kw| {
            if (!first) result.raw(",");
            first = false;
            result.raw("{\"label\":");
            result.string(kw);
            result.raw(",\"kind\":14"); // Keyword
            result.raw("}");
        }

        // Add builtin function completions
        const builtins = [_]struct { name: []const u8, detail: []const u8 }{
            .{ .name = "println", .detail = "(s: string) -> void" },
            .{ .name = "print", .detail = "(s: string) -> void" },
            .{ .name = "eprintln", .detail = "(s: string) -> void" },
            .{ .name = "eprint", .detail = "(s: string) -> void" },
            .{ .name = "panic", .detail = "(msg: string) -> void" },
            .{ .name = "exit", .detail = "(code: int) -> void" },
            .{ .name = "assert", .detail = "(condition: bool) -> void" },
            .{ .name = "assert_eq", .detail = "(actual, expected) -> void" },
            .{ .name = "len", .detail = "(s: string) -> int" },
            .{ .name = "char_at", .detail = "(s: string, i: int) -> string" },
            .{ .name = "substr", .detail = "(s: string, start: int, len: int) -> string" },
            .{ .name = "int_to_string", .detail = "(n: int) -> string" },
            .{ .name = "float_to_string", .detail = "(f: float) -> string" },
            .{ .name = "bool_to_string", .detail = "(b: bool) -> string" },
            .{ .name = "parse_int", .detail = "(s: string) -> int" },
            .{ .name = "parse_float", .detail = "(s: string) -> float" },
            .{ .name = "string_contains", .detail = "(s: string, sub: string) -> bool" },
            .{ .name = "string_find", .detail = "(s: string, sub: string) -> int" },
            .{ .name = "starts_with", .detail = "(s: string, prefix: string) -> bool" },
            .{ .name = "ends_with", .detail = "(s: string, suffix: string) -> bool" },
            .{ .name = "string_trim", .detail = "(s: string) -> string" },
            .{ .name = "string_replace", .detail = "(s: string, old: string, new: string) -> string" },
            .{ .name = "string_to_upper", .detail = "(s: string) -> string" },
            .{ .name = "string_to_lower", .detail = "(s: string) -> string" },
            .{ .name = "file_read", .detail = "(path: string) -> string" },
            .{ .name = "file_write", .detail = "(path: string, content: string) -> void" },
            .{ .name = "args_len", .detail = "() -> int" },
            .{ .name = "args_get", .detail = "(i: int) -> string" },
            .{ .name = "system", .detail = "(cmd: string) -> int" },
            .{ .name = "Box_new", .detail = "(value: T) -> Box[T]" },
            .{ .name = "Box_null", .detail = "() -> Box[T]" },
        };

        for (builtins) |b| {
            result.raw(",{\"label\":");
            result.string(b.name);
            result.raw(",\"kind\":3"); // Function
            result.raw(",\"detail\":");
            result.string(b.detail);
            result.raw("}");
        }

        // Add type completions
        const types = [_][]const u8{
            "int", "float", "bool", "string", "void",
            "List", "Map", "Box", "Option", "Result",
        };

        for (types) |t| {
            result.raw(",{\"label\":");
            result.string(t);
            result.raw(",\"kind\":7"); // Class (used for types)
            result.raw("}");
        }

        result.raw("]}");
        self.sendResponse(id, null, result.toSlice());
    }

    fn handleHover(self: *LspServer, id: ?i64, msg: []const u8) !void {
        const params = jsonGetObject(msg, "params") orelse {
            self.sendResponse(id, null, "null");
            return;
        };
        const text_doc = jsonGetObject(params, "textDocument") orelse {
            self.sendResponse(id, null, "null");
            return;
        };
        const uri = jsonGetString(text_doc, "uri") orelse {
            self.sendResponse(id, null, "null");
            return;
        };
        const position = jsonGetObject(params, "position") orelse {
            self.sendResponse(id, null, "null");
            return;
        };
        const line = jsonGetInt(position, "line") orelse {
            self.sendResponse(id, null, "null");
            return;
        };
        const character = jsonGetInt(position, "character") orelse {
            self.sendResponse(id, null, "null");
            return;
        };

        const source = self.documents.get(uri) orelse {
            self.sendResponse(id, null, "null");
            return;
        };

        // Find the word at the given position
        const word = getWordAtPosition(source, @intCast(line), @intCast(character)) orelse {
            self.sendResponse(id, null, "null");
            return;
        };

        // Provide hover info for known identifiers
        const hover_text = getHoverInfo(word) orelse {
            self.sendResponse(id, null, "null");
            return;
        };

        var result = JsonWriter.init(self.allocator);
        defer result.deinit();
        result.raw("{\"contents\":{\"kind\":\"markdown\",\"value\":");
        result.string(hover_text);
        result.raw("}}");
        self.sendResponse(id, null, result.toSlice());
    }

    // ========================================================================
    // Diagnostics
    // ========================================================================

    fn publishDiagnostics(self: *LspServer, uri: []const u8, source: []const u8) void {
        var diag = JsonWriter.init(self.allocator);
        defer diag.deinit();

        diag.raw("{\"uri\":");
        diag.string(uri);
        diag.raw(",\"diagnostics\":[");

        var has_diag = false;

        // Run lexer
        var lexer = Lexer.init(source, self.allocator);
        defer lexer.deinit();
        const tokens = lexer.scanAll() catch {
            diag.raw("]}");
            self.sendNotification("textDocument/publishDiagnostics", diag.toSlice());
            return;
        };
        defer self.allocator.free(tokens);

        if (lexer.hasErrors()) {
            // Report lexer errors
            for (lexer.getErrors()) |err| {
                if (has_diag) diag.raw(",");
                has_diag = true;
                diag.raw("{\"range\":{\"start\":{\"line\":");
                diag.int(@intCast(if (err.location.line > 0) err.location.line - 1 else 0));
                diag.raw(",\"character\":");
                diag.int(@intCast(if (err.location.column > 0) err.location.column - 1 else 0));
                diag.raw("},\"end\":{\"line\":");
                diag.int(@intCast(if (err.location.line > 0) err.location.line - 1 else 0));
                diag.raw(",\"character\":");
                diag.int(@intCast(err.location.column));
                diag.raw("}},\"severity\":1,\"source\":\"daimond\",\"message\":");
                diag.string(err.message);
                diag.raw("}");
            }
        } else {
            // Run parser
            var parser = Parser.init(tokens, self.allocator);
            defer parser.deinit();
            const ast_result = parser.parse() catch {
                diag.raw("]}");
                self.sendNotification("textDocument/publishDiagnostics", diag.toSlice());
                return;
            };

            if (parser.hasErrors()) {
                for (parser.getErrors()) |err| {
                    if (has_diag) diag.raw(",");
                    has_diag = true;
                    diag.raw("{\"range\":{\"start\":{\"line\":");
                    diag.int(@intCast(if (err.location.line > 0) err.location.line - 1 else 0));
                    diag.raw(",\"character\":");
                    diag.int(@intCast(if (err.location.column > 0) err.location.column - 1 else 0));
                    diag.raw("},\"end\":{\"line\":");
                    diag.int(@intCast(if (err.location.line > 0) err.location.line - 1 else 0));
                    diag.raw(",\"character\":");
                    diag.int(@intCast(err.location.column + 1));
                    diag.raw("}},\"severity\":1,\"source\":\"daimond\",\"message\":");
                    diag.string(err.message);
                    diag.raw("}");
                }
            } else {
                // Run type checker
                var type_checker = TypeChecker.init(self.allocator) catch {
                    diag.raw("]}");
                    self.sendNotification("textDocument/publishDiagnostics", diag.toSlice());
                    return;
                };
                defer type_checker.deinit();
                type_checker.setSource(source);
                type_checker.checkSourceFile(ast_result) catch {};

                if (type_checker.hasErrors()) {
                    for (type_checker.getDiagnostics()) |err| {
                        if (has_diag) diag.raw(",");
                        has_diag = true;
                        diag.raw("{\"range\":{\"start\":{\"line\":");
                        diag.int(@intCast(if (err.location.line > 0) err.location.line - 1 else 0));
                        diag.raw(",\"character\":");
                        diag.int(@intCast(if (err.location.column > 0) err.location.column - 1 else 0));
                        diag.raw("},\"end\":{\"line\":");
                        diag.int(@intCast(if (err.location.line > 0) err.location.line - 1 else 0));
                        diag.raw(",\"character\":");
                        diag.int(@intCast(err.location.column + 1));
                        diag.raw("}},\"severity\":1,\"source\":\"daimond\",\"message\":");
                        diag.string(err.message);
                        diag.raw("}");
                    }
                }
            }
        }

        diag.raw("]}");
        self.sendNotification("textDocument/publishDiagnostics", diag.toSlice());
    }
};

// ============================================================================
// Utility Functions
// ============================================================================

fn getWordAtPosition(source: []const u8, line_num: usize, col: usize) ?[]const u8 {
    // Find the line
    var current_line: usize = 0;
    var line_start: usize = 0;
    for (source, 0..) |ch, i| {
        if (current_line == line_num) {
            line_start = i;
            break;
        }
        if (ch == '\n') current_line += 1;
    }

    // Find line end
    var line_end = line_start;
    while (line_end < source.len and source[line_end] != '\n') : (line_end += 1) {}

    if (line_start + col >= source.len) return null;

    // Find word boundaries at cursor position
    const pos = line_start + col;
    if (pos >= line_end) return null;

    // Check if we're on a word character
    if (!isWordChar(source[pos])) return null;

    // Find word start
    var word_start = pos;
    while (word_start > line_start and isWordChar(source[word_start - 1])) : (word_start -= 1) {}

    // Find word end
    var word_end = pos;
    while (word_end < line_end and isWordChar(source[word_end])) : (word_end += 1) {}

    if (word_start == word_end) return null;
    return source[word_start..word_end];
}

fn isWordChar(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or (ch >= '0' and ch <= '9') or ch == '_';
}

fn getHoverInfo(word: []const u8) ?[]const u8 {
    // Keywords
    if (std.mem.eql(u8, word, "fn")) return "```daimond\nfn name(params) -> ReturnType { body }\n```\nDeclare a function.";
    if (std.mem.eql(u8, word, "let")) return "```daimond\nlet x = value\nlet mut x = value\n```\nDeclare a variable. Immutable by default.";
    if (std.mem.eql(u8, word, "struct")) return "```daimond\nstruct Name { field: Type, ... }\n```\nDeclare a struct type.";
    if (std.mem.eql(u8, word, "enum")) return "```daimond\nenum Name { Variant1, Variant2(Type), ... }\n```\nDeclare an enum type.";
    if (std.mem.eql(u8, word, "trait")) return "```daimond\ntrait Name { fn method(self) -> Type }\n```\nDeclare a trait (interface).";
    if (std.mem.eql(u8, word, "impl")) return "```daimond\nimpl TypeName { fn method(self) { ... } }\nimpl TraitName for TypeName { ... }\n```\nImplement methods or traits for a type.";
    if (std.mem.eql(u8, word, "match")) return "```daimond\nmatch value { Pattern => expr, ... }\n```\nPattern matching expression.";
    if (std.mem.eql(u8, word, "region")) return "```daimond\nregion name { ... }\n```\nScoped arena memory allocation.";
    if (std.mem.eql(u8, word, "comptime")) return "```daimond\ncomptime { ... }\n```\nCompile-time evaluated expression.";
    if (std.mem.eql(u8, word, "extern")) return "```daimond\nextern fn name(params) -> ReturnType\n```\nDeclare an external C function.";
    if (std.mem.eql(u8, word, "import")) return "```daimond\nimport module_name\nimport std.io\n```\nImport a module.";

    // Builtin functions
    if (std.mem.eql(u8, word, "println")) return "```daimond\nfn println(s: string) -> void\n```\nPrint string to stdout with newline.";
    if (std.mem.eql(u8, word, "print")) return "```daimond\nfn print(s: string) -> void\n```\nPrint string to stdout without newline.";
    if (std.mem.eql(u8, word, "panic")) return "```daimond\nfn panic(msg: string) -> void\n```\nPrint error message and exit.";
    if (std.mem.eql(u8, word, "len")) return "```daimond\nfn len(s: string) -> int\n```\nGet string length.";
    if (std.mem.eql(u8, word, "int_to_string")) return "```daimond\nfn int_to_string(n: int) -> string\n```\nConvert integer to string.";
    if (std.mem.eql(u8, word, "parse_int")) return "```daimond\nfn parse_int(s: string) -> int\n```\nParse string as integer.";
    if (std.mem.eql(u8, word, "assert")) return "```daimond\nfn assert(condition: bool) -> void\n```\nPanics if condition is false.";
    if (std.mem.eql(u8, word, "assert_eq")) return "```daimond\nfn assert_eq(actual, expected) -> void\n```\nPanics if actual != expected.";

    // Types
    if (std.mem.eql(u8, word, "int")) return "**int** — 64-bit signed integer type (i64)";
    if (std.mem.eql(u8, word, "float")) return "**float** — 64-bit floating point type (f64)";
    if (std.mem.eql(u8, word, "bool")) return "**bool** — Boolean type (true/false)";
    if (std.mem.eql(u8, word, "string")) return "**string** — Immutable string type";
    if (std.mem.eql(u8, word, "void")) return "**void** — No return value";
    if (std.mem.eql(u8, word, "List")) return "**List[T]** — Dynamic array type. Methods: .push(item), .pop(), .len(), [i] indexing";
    if (std.mem.eql(u8, word, "Map")) return "**Map[K,V]** — Hash map type. Methods: .insert(k,v), .get(k), .contains(k), .remove(k), .len()";
    if (std.mem.eql(u8, word, "Box")) return "**Box[T]** — Heap-allocated value. Use Box_new(val) to create, Box_null() for null.";
    if (std.mem.eql(u8, word, "Option")) return "**Option[T]** — Optional value. Variants: Some(T), None";
    if (std.mem.eql(u8, word, "Result")) return "**Result[T, E]** — Error-handling type. Variants: Ok(T), Err(E)";

    return null;
}

// ============================================================================
// Entry Point
// ============================================================================

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var server = LspServer.init(allocator);
    defer server.deinit();

    server.log("daimond-lsp starting...", .{});

    while (true) {
        const msg = try server.readMessage() orelse break;
        defer std.heap.page_allocator.free(msg);

        server.handleMessage(msg) catch |err| {
            server.log("Error handling message: {}", .{err});
        };
    }

    server.log("daimond-lsp shutting down", .{});
}

// ============================================================================
// Tests
// ============================================================================

test "json get string" {
    const json = "{\"method\":\"initialize\",\"id\":1}";
    const method = jsonGetString(json, "method");
    try std.testing.expect(method != null);
    try std.testing.expectEqualStrings("initialize", method.?);
}

test "json get int" {
    const json = "{\"method\":\"test\",\"id\":42}";
    const id = jsonGetInt(json, "id");
    try std.testing.expect(id != null);
    try std.testing.expectEqual(@as(i64, 42), id.?);
}

test "json get object" {
    const json = "{\"params\":{\"textDocument\":{\"uri\":\"file:///test.dm\"}}}";
    const params = jsonGetObject(json, "params");
    try std.testing.expect(params != null);
    const uri = jsonGetString(params.?, "uri");
    try std.testing.expect(uri != null);
    try std.testing.expectEqualStrings("file:///test.dm", uri.?);
}

test "get word at position" {
    const source = "fn main() {\n    println(\"hello\")\n}";
    const word = getWordAtPosition(source, 0, 3);
    try std.testing.expect(word != null);
    try std.testing.expectEqualStrings("main", word.?);

    const word2 = getWordAtPosition(source, 1, 6);
    try std.testing.expect(word2 != null);
    try std.testing.expectEqualStrings("println", word2.?);
}

test "get hover info" {
    const info = getHoverInfo("fn");
    try std.testing.expect(info != null);

    const no_info = getHoverInfo("xyz_unknown");
    try std.testing.expect(no_info == null);
}
