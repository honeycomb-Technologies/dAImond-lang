//! dAImond Lexer
//!
//! This module implements the lexical analyzer for the dAImond programming language.
//! It converts source text into a stream of tokens for the parser to consume.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// All token types in the dAImond language
pub const TokenType = enum {
    // Literals
    integer, // 42, 1_000_000
    float, // 3.14, 1e10, 2.5e-3
    string, // "hello"
    raw_string, // r"raw\nstring"
    byte_string, // b"bytes"
    char_literal, // 'a'
    identifier, // foo, bar_baz

    // Keywords
    kw_fn,
    kw_let,
    kw_mut,
    kw_const,
    kw_if,
    kw_else,
    kw_match,
    kw_for,
    kw_while,
    kw_loop,
    kw_break,
    kw_continue,
    kw_return,
    kw_struct,
    kw_enum,
    kw_trait,
    kw_impl,
    kw_import,
    kw_module,
    kw_with,
    kw_region,
    kw_comptime,
    kw_true,
    kw_false,
    kw_and,
    kw_or,
    kw_not,
    kw_in,
    kw_private,
    kw_requires,
    kw_ensures,
    kw_discard,
    kw_expect,

    // Operators
    plus, // +
    minus, // -
    star, // *
    slash, // /
    percent, // %
    caret, // ^
    ampersand, // &
    pipe, // |
    tilde, // ~
    eq, // =
    eq_eq, // ==
    bang, // !
    bang_eq, // !=
    lt, // <
    gt, // >
    lt_eq, // <=
    gt_eq, // >=
    lt_lt, // <<
    gt_gt, // >>
    arrow, // ->
    fat_arrow, // =>
    question, // ?
    question_question, // ??
    pipe_gt, // |>
    at, // @

    // Compound assignment
    plus_eq, // +=
    minus_eq, // -=
    star_eq, // *=
    slash_eq, // /=
    percent_eq, // %=
    ampersand_eq, // &=
    pipe_eq, // |=
    caret_eq, // ^=
    lt_lt_eq, // <<=
    gt_gt_eq, // >>=

    // Punctuation
    dot, // .
    dot_dot, // ..
    dot_dot_eq, // ..=
    colon, // :
    colon_colon, // ::
    semicolon, // ;
    comma, // ,
    underscore, // _ (when standalone)

    // Delimiters
    lparen, // (
    rparen, // )
    lbrace, // {
    rbrace, // }
    lbracket, // [
    rbracket, // ]

    // Special
    eof,
    invalid,
};

/// Represents the location of a token in source code
pub const SourceLocation = struct {
    line: usize,
    column: usize,
    offset: usize, // byte offset from start of source
};

/// A token produced by the lexer
pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    location: SourceLocation,

    /// Create a token with the given attributes
    pub fn init(token_type: TokenType, lexeme: []const u8, location: SourceLocation) Token {
        return .{
            .type = token_type,
            .lexeme = lexeme,
            .location = location,
        };
    }

    /// Format a token for debugging
    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("Token{{ .type = {s}, .lexeme = \"{s}\", .line = {}, .col = {} }}", .{
            @tagName(self.type),
            self.lexeme,
            self.location.line,
            self.location.column,
        });
    }
};

/// Lexer error information
pub const LexError = struct {
    message: []const u8,
    location: SourceLocation,
    source_line: ?[]const u8, // The line of source code where error occurred

    pub fn format(
        self: LexError,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("Lex error at line {}, column {}: {s}", .{
            self.location.line,
            self.location.column,
            self.message,
        });
        if (self.source_line) |line| {
            try writer.print("\n  | {s}", .{line});
        }
    }
};

/// The dAImond lexer
pub const Lexer = struct {
    source: []const u8,
    allocator: Allocator,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    line_start: usize, // offset where current line starts
    errors: std.ArrayList(LexError),

    const Self = @This();

    /// Initialize a new lexer with the given source code
    pub fn init(source: []const u8, allocator: Allocator) Self {
        return .{
            .source = source,
            .allocator = allocator,
            .start = 0,
            .current = 0,
            .line = 1,
            .column = 1,
            .line_start = 0,
            .errors = std.ArrayList(LexError).init(allocator),
        };
    }

    /// Clean up lexer resources
    pub fn deinit(self: *Self) void {
        self.errors.deinit();
    }

    /// Scan all tokens from the source
    pub fn scanAll(self: *Self) ![]Token {
        var tokens = std.ArrayList(Token).init(self.allocator);
        errdefer tokens.deinit();

        while (true) {
            const token = self.scanToken();
            try tokens.append(token);
            if (token.type == .eof) break;
        }

        return tokens.toOwnedSlice();
    }

    /// Scan the next token
    pub fn scanToken(self: *Self) Token {
        self.skipWhitespaceAndComments();
        self.start = self.current;

        if (self.isAtEnd()) {
            return self.makeToken(.eof);
        }

        const c = self.advance();

        // Raw strings: r"..." (check before identifiers)
        if (c == 'r' and self.peek() == '"') {
            _ = self.advance(); // consume "
            return self.rawString();
        }

        // Byte strings: b"..." (check before identifiers)
        if (c == 'b' and self.peek() == '"') {
            _ = self.advance(); // consume "
            return self.byteString();
        }

        // Identifiers and keywords
        if (isAlpha(c)) return self.identifier();

        // Numbers
        if (isDigit(c)) return self.number();

        // String literals
        if (c == '"') return self.string();

        // Character literals
        if (c == '\'') return self.charLiteral();

        // Operators and punctuation
        return switch (c) {
            '(' => self.makeToken(.lparen),
            ')' => self.makeToken(.rparen),
            '{' => self.makeToken(.lbrace),
            '}' => self.makeToken(.rbrace),
            '[' => self.makeToken(.lbracket),
            ']' => self.makeToken(.rbracket),
            ';' => self.makeToken(.semicolon),
            ',' => self.makeToken(.comma),
            '@' => self.makeToken(.at),
            '~' => self.makeToken(.tilde),

            ':' => if (self.match(':')) self.makeToken(.colon_colon) else self.makeToken(.colon),

            '.' => blk: {
                if (self.match('.')) {
                    break :blk if (self.match('=')) self.makeToken(.dot_dot_eq) else self.makeToken(.dot_dot);
                }
                break :blk self.makeToken(.dot);
            },

            '+' => if (self.match('=')) self.makeToken(.plus_eq) else self.makeToken(.plus),
            '-' => blk: {
                if (self.match('>')) break :blk self.makeToken(.arrow);
                if (self.match('=')) break :blk self.makeToken(.minus_eq);
                break :blk self.makeToken(.minus);
            },
            '*' => if (self.match('=')) self.makeToken(.star_eq) else self.makeToken(.star),
            '/' => if (self.match('=')) self.makeToken(.slash_eq) else self.makeToken(.slash),
            '%' => if (self.match('=')) self.makeToken(.percent_eq) else self.makeToken(.percent),
            '^' => if (self.match('=')) self.makeToken(.caret_eq) else self.makeToken(.caret),

            '&' => if (self.match('=')) self.makeToken(.ampersand_eq) else self.makeToken(.ampersand),
            '|' => blk: {
                if (self.match('>')) break :blk self.makeToken(.pipe_gt);
                if (self.match('=')) break :blk self.makeToken(.pipe_eq);
                break :blk self.makeToken(.pipe);
            },

            '=' => blk: {
                if (self.match('=')) break :blk self.makeToken(.eq_eq);
                if (self.match('>')) break :blk self.makeToken(.fat_arrow);
                break :blk self.makeToken(.eq);
            },
            '!' => if (self.match('=')) self.makeToken(.bang_eq) else self.makeToken(.bang),

            '<' => blk: {
                if (self.match('<')) {
                    break :blk if (self.match('=')) self.makeToken(.lt_lt_eq) else self.makeToken(.lt_lt);
                }
                if (self.match('=')) break :blk self.makeToken(.lt_eq);
                break :blk self.makeToken(.lt);
            },
            '>' => blk: {
                if (self.match('>')) {
                    break :blk if (self.match('=')) self.makeToken(.gt_gt_eq) else self.makeToken(.gt_gt);
                }
                if (self.match('=')) break :blk self.makeToken(.gt_eq);
                break :blk self.makeToken(.gt);
            },

            '?' => if (self.match('?')) self.makeToken(.question_question) else self.makeToken(.question),

            else => self.errorToken("Unexpected character"),
        };
    }

    /// Scan an identifier or keyword
    fn identifier(self: *Self) Token {
        while (isAlphaNumeric(self.peek())) _ = self.advance();

        const text = self.source[self.start..self.current];

        // Check for underscore as standalone token
        if (std.mem.eql(u8, text, "_")) {
            return self.makeToken(.underscore);
        }

        return self.makeToken(self.identifierType(text));
    }

    /// Determine if an identifier is a keyword
    fn identifierType(self: *Self, text: []const u8) TokenType {
        _ = self;
        const keywords = std.StaticStringMap(TokenType).initComptime(.{
            .{ "fn", .kw_fn },
            .{ "let", .kw_let },
            .{ "mut", .kw_mut },
            .{ "const", .kw_const },
            .{ "if", .kw_if },
            .{ "else", .kw_else },
            .{ "match", .kw_match },
            .{ "for", .kw_for },
            .{ "while", .kw_while },
            .{ "loop", .kw_loop },
            .{ "break", .kw_break },
            .{ "continue", .kw_continue },
            .{ "return", .kw_return },
            .{ "struct", .kw_struct },
            .{ "enum", .kw_enum },
            .{ "trait", .kw_trait },
            .{ "impl", .kw_impl },
            .{ "import", .kw_import },
            .{ "module", .kw_module },
            .{ "with", .kw_with },
            .{ "region", .kw_region },
            .{ "comptime", .kw_comptime },
            .{ "true", .kw_true },
            .{ "false", .kw_false },
            .{ "and", .kw_and },
            .{ "or", .kw_or },
            .{ "not", .kw_not },
            .{ "in", .kw_in },
            .{ "private", .kw_private },
            .{ "requires", .kw_requires },
            .{ "ensures", .kw_ensures },
            .{ "discard", .kw_discard },
            .{ "expect", .kw_expect },
        });
        return keywords.get(text) orelse .identifier;
    }

    /// Scan a number literal (integer or float)
    fn number(self: *Self) Token {
        // Consume integer part (with optional underscores)
        while (isDigit(self.peek()) or self.peek() == '_') _ = self.advance();

        var is_float = false;

        // Check for decimal point
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            is_float = true;
            _ = self.advance(); // consume '.'
            while (isDigit(self.peek()) or self.peek() == '_') _ = self.advance();
        }

        // Check for scientific notation
        if (self.peek() == 'e' or self.peek() == 'E') {
            is_float = true;
            _ = self.advance(); // consume 'e' or 'E'
            if (self.peek() == '+' or self.peek() == '-') {
                _ = self.advance();
            }
            while (isDigit(self.peek()) or self.peek() == '_') _ = self.advance();
        }

        return self.makeToken(if (is_float) .float else .integer);
    }

    /// Scan a regular string literal
    fn string(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 0;
                self.line_start = self.current + 1;
            }
            // Handle escape sequences
            if (self.peek() == '\\' and !self.isAtEnd()) {
                _ = self.advance(); // consume backslash
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string");
        }

        _ = self.advance(); // closing quote
        return self.makeToken(.string);
    }

    /// Scan a raw string literal (no escape processing)
    fn rawString(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 0;
                self.line_start = self.current + 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated raw string");
        }

        _ = self.advance(); // closing quote
        return self.makeToken(.raw_string);
    }

    /// Scan a byte string literal
    fn byteString(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                return self.errorToken("Newline in byte string");
            }
            if (self.peek() == '\\' and !self.isAtEnd()) {
                _ = self.advance(); // consume backslash
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated byte string");
        }

        _ = self.advance(); // closing quote
        return self.makeToken(.byte_string);
    }

    /// Scan a character literal
    fn charLiteral(self: *Self) Token {
        if (self.isAtEnd()) {
            return self.errorToken("Unterminated character literal");
        }

        // Handle escape sequences
        if (self.peek() == '\\') {
            _ = self.advance(); // consume backslash
            if (self.isAtEnd()) {
                return self.errorToken("Unterminated escape sequence");
            }
            _ = self.advance(); // consume escaped character
        } else {
            _ = self.advance(); // consume character
        }

        if (self.peek() != '\'') {
            return self.errorToken("Unterminated character literal");
        }

        _ = self.advance(); // closing quote
        return self.makeToken(.char_literal);
    }

    /// Skip whitespace and comments
    fn skipWhitespaceAndComments(self: *Self) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                    _ = self.advance();
                    self.line_start = self.current;
                },
                '-' => {
                    // Check for comment: --
                    if (self.peekNext() == '-') {
                        // Skip comment until end of line
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    /// Advance to the next character
    fn advance(self: *Self) u8 {
        const c = self.source[self.current];
        self.current += 1;
        self.column += 1;
        return c;
    }

    /// Check if current character matches expected and consume it
    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        self.column += 1;
        return true;
    }

    /// Look at current character without consuming
    fn peek(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    /// Look at next character without consuming
    fn peekNext(self: *Self) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    /// Check if we've reached the end of source
    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    /// Create a token with current lexeme
    fn makeToken(self: *Self, token_type: TokenType) Token {
        return Token.init(
            token_type,
            self.source[self.start..self.current],
            .{
                .line = self.line,
                .column = self.column - (self.current - self.start),
                .offset = self.start,
            },
        );
    }

    /// Create an error token
    fn errorToken(self: *Self, message: []const u8) Token {
        // Record the error
        const err = LexError{
            .message = message,
            .location = .{
                .line = self.line,
                .column = self.column,
                .offset = self.current,
            },
            .source_line = self.getCurrentLine(),
        };
        self.errors.append(err) catch {};

        return Token.init(
            .invalid,
            message,
            .{
                .line = self.line,
                .column = self.column,
                .offset = self.start,
            },
        );
    }

    /// Get the current line of source code
    fn getCurrentLine(self: *Self) ?[]const u8 {
        var end = self.line_start;
        while (end < self.source.len and self.source[end] != '\n') {
            end += 1;
        }
        if (self.line_start >= self.source.len) return null;
        return self.source[self.line_start..end];
    }

    /// Check if there were any lexing errors
    pub fn hasErrors(self: *Self) bool {
        return self.errors.items.len > 0;
    }

    /// Get all accumulated errors
    pub fn getErrors(self: *Self) []const LexError {
        return self.errors.items;
    }
};

// Helper functions

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlphaNumeric(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}

// ============================================================================
// TESTS
// ============================================================================

const testing = std.testing;

test "lex empty source" {
    var lexer = Lexer.init("", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expectEqual(@as(usize, 1), tokens.len);
    try testing.expectEqual(TokenType.eof, tokens[0].type);
}

test "lex basic tokens" {
    var lexer = Lexer.init("fn main() { }", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expectEqual(@as(usize, 7), tokens.len); // Includes EOF
    try testing.expectEqual(TokenType.kw_fn, tokens[0].type);
    try testing.expectEqual(TokenType.identifier, tokens[1].type);
    try testing.expectEqualStrings("main", tokens[1].lexeme);
    try testing.expectEqual(TokenType.lparen, tokens[2].type);
    try testing.expectEqual(TokenType.rparen, tokens[3].type);
    try testing.expectEqual(TokenType.lbrace, tokens[4].type);
    try testing.expectEqual(TokenType.rbrace, tokens[5].type);
    try testing.expectEqual(TokenType.eof, tokens[6].type);
}

test "lex all keywords" {
    const source =
        \\fn let mut const if else match for while loop
        \\break continue return struct enum trait impl
        \\import module with region comptime true false
        \\and or not in private requires ensures discard expect
    ;

    var lexer = Lexer.init(source, testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    const expected_keywords = [_]TokenType{
        .kw_fn,       .kw_let,      .kw_mut,      .kw_const,    .kw_if,      .kw_else,
        .kw_match,    .kw_for,      .kw_while,    .kw_loop,     .kw_break,   .kw_continue,
        .kw_return,   .kw_struct,   .kw_enum,     .kw_trait,    .kw_impl,    .kw_import,
        .kw_module,   .kw_with,     .kw_region,   .kw_comptime, .kw_true,    .kw_false,
        .kw_and,      .kw_or,       .kw_not,      .kw_in,       .kw_private, .kw_requires,
        .kw_ensures,  .kw_discard,  .kw_expect,
    };

    for (expected_keywords, 0..) |expected, i| {
        try testing.expectEqual(expected, tokens[i].type);
    }
}

test "lex integer literals" {
    var lexer = Lexer.init("42 1_000_000 0 123456789", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expectEqual(@as(usize, 5), tokens.len);
    for (tokens[0..4]) |token| {
        try testing.expectEqual(TokenType.integer, token.type);
    }
    try testing.expectEqualStrings("42", tokens[0].lexeme);
    try testing.expectEqualStrings("1_000_000", tokens[1].lexeme);
}

test "lex float literals" {
    var lexer = Lexer.init("3.14 1.0 0.5 1e10 2.5e-3 1E+5", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expectEqual(@as(usize, 7), tokens.len);
    for (tokens[0..6]) |token| {
        try testing.expectEqual(TokenType.float, token.type);
    }
}

test "lex string literals" {
    var lexer = Lexer.init(
        \\"hello" "world" "with\nescape"
    , testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expectEqual(@as(usize, 4), tokens.len);
    for (tokens[0..3]) |token| {
        try testing.expectEqual(TokenType.string, token.type);
    }
}

test "lex operators" {
    var lexer = Lexer.init("+ - * / % ^ & | ~ = == != < > <= >= << >> -> => ? ?? |>", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    const expected_ops = [_]TokenType{
        .plus,      .minus,      .star,    .slash,
        .percent,   .caret,      .ampersand, .pipe,
        .tilde,     .eq,         .eq_eq,   .bang_eq,
        .lt,        .gt,         .lt_eq,   .gt_eq,
        .lt_lt,     .gt_gt,      .arrow,   .fat_arrow,
        .question,  .question_question, .pipe_gt,
    };

    for (expected_ops, 0..) |expected, i| {
        try testing.expectEqual(expected, tokens[i].type);
    }
}

test "lex compound assignment" {
    var lexer = Lexer.init("+= -= *= /= %= &= |= ^= <<= >>=", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    const expected = [_]TokenType{
        .plus_eq,   .minus_eq, .star_eq, .slash_eq,
        .percent_eq, .ampersand_eq, .pipe_eq, .caret_eq,
        .lt_lt_eq,  .gt_gt_eq,
    };

    for (expected, 0..) |exp, i| {
        try testing.expectEqual(exp, tokens[i].type);
    }
}

test "lex punctuation" {
    var lexer = Lexer.init(". .. ..= : :: ; , @ ( ) { } [ ]", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    const expected = [_]TokenType{
        .dot,       .dot_dot,   .dot_dot_eq, .colon,
        .colon_colon, .semicolon, .comma,    .at,
        .lparen,    .rparen,    .lbrace,     .rbrace,
        .lbracket,  .rbracket,
    };

    for (expected, 0..) |exp, i| {
        try testing.expectEqual(exp, tokens[i].type);
    }
}

test "lex comments" {
    const source =
        \\let x = 42 -- this is a comment
        \\let y = 10
    ;

    var lexer = Lexer.init(source, testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    // Should have: let x = 42 let y = 10 eof (8 tokens, comment stripped)
    try testing.expectEqual(@as(usize, 9), tokens.len);
    try testing.expectEqual(TokenType.kw_let, tokens[0].type);
    try testing.expectEqual(TokenType.identifier, tokens[1].type);
    try testing.expectEqualStrings("x", tokens[1].lexeme);
    try testing.expectEqual(TokenType.eq, tokens[2].type);
    try testing.expectEqual(TokenType.integer, tokens[3].type);
    try testing.expectEqual(TokenType.kw_let, tokens[4].type);
}

test "lex line tracking" {
    const source =
        \\fn main() {
        \\    let x = 42
        \\}
    ;

    var lexer = Lexer.init(source, testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    // fn should be on line 1
    try testing.expectEqual(@as(usize, 1), tokens[0].location.line);
    // let should be on line 2
    try testing.expectEqual(@as(usize, 2), tokens[5].location.line);
    // } should be on line 3
    try testing.expectEqual(@as(usize, 3), tokens[9].location.line);
}

test "lex raw and byte strings" {
    var lexer = Lexer.init(
        \\r"raw\nstring" b"bytes"
    , testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expectEqual(@as(usize, 3), tokens.len); // raw_string, byte_string, EOF
    try testing.expectEqual(TokenType.raw_string, tokens[0].type);
    try testing.expectEqual(TokenType.byte_string, tokens[1].type);
    try testing.expectEqual(TokenType.eof, tokens[2].type);
}

test "lex character literals" {
    var lexer = Lexer.init("'a' '\\n' '\\''", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expectEqual(@as(usize, 4), tokens.len);
    for (tokens[0..3]) |token| {
        try testing.expectEqual(TokenType.char_literal, token.type);
    }
}

test "lex underscore as standalone" {
    var lexer = Lexer.init("let _ = value", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_let, tokens[0].type);
    try testing.expectEqual(TokenType.underscore, tokens[1].type);
    try testing.expectEqual(TokenType.eq, tokens[2].type);
    try testing.expectEqual(TokenType.identifier, tokens[3].type);
}

test "lex calculator expression" {
    var lexer = Lexer.init("let result = (2 + 3) * 4 / 2.0", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    const expected = [_]TokenType{
        .kw_let,    .identifier, .eq,       .lparen,
        .integer,   .plus,       .integer,  .rparen,
        .star,      .integer,    .slash,    .float,
        .eof,
    };

    try testing.expectEqual(expected.len, tokens.len);
    for (expected, 0..) |exp, i| {
        try testing.expectEqual(exp, tokens[i].type);
    }
}

test "lex function definition" {
    const source =
        \\fn add(a: int, b: int) -> int {
        \\    return a + b
        \\}
    ;

    var lexer = Lexer.init(source, testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    // Verify key tokens: fn add ( a : int , b : int ) -> int { return a + b }
    // 0:fn 1:add 2:( 3:a 4:: 5:int 6:, 7:b 8:: 9:int 10:) 11:-> 12:int
    try testing.expectEqual(TokenType.kw_fn, tokens[0].type);
    try testing.expectEqual(TokenType.identifier, tokens[1].type);
    try testing.expectEqualStrings("add", tokens[1].lexeme);
    try testing.expectEqual(TokenType.lparen, tokens[2].type);
    try testing.expectEqual(TokenType.identifier, tokens[3].type); // a
    try testing.expectEqual(TokenType.colon, tokens[4].type);
    try testing.expectEqual(TokenType.identifier, tokens[5].type); // int
    try testing.expectEqual(TokenType.rparen, tokens[10].type);
    try testing.expectEqual(TokenType.arrow, tokens[11].type);
}

test "lex generic syntax" {
    var lexer = Lexer.init("fn first[T](list: List[T]) -> Option[T]", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    // Verify brackets are tokenized correctly
    var bracket_count: usize = 0;
    for (tokens) |token| {
        if (token.type == .lbracket or token.type == .rbracket) {
            bracket_count += 1;
        }
    }
    try testing.expectEqual(@as(usize, 6), bracket_count);
}

test "lex match expression" {
    const source =
        \\match value {
        \\    Some(x) => use(x),
        \\    None => handle(),
        \\}
    ;

    var lexer = Lexer.init(source, testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_match, tokens[0].type);
    // Check for fat arrow tokens
    var fat_arrow_count: usize = 0;
    for (tokens) |token| {
        if (token.type == .fat_arrow) {
            fat_arrow_count += 1;
        }
    }
    try testing.expectEqual(@as(usize, 2), fat_arrow_count);
}

test "lex effects syntax" {
    var lexer = Lexer.init("fn read_file(path: str) -> str with [IO]", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    // Find the 'with' keyword
    var found_with = false;
    for (tokens) |token| {
        if (token.type == .kw_with) {
            found_with = true;
            break;
        }
    }
    try testing.expect(found_with);
}

test "lex region syntax" {
    const source =
        \\region scratch {
        \\    let buffer = alloc[byte](1000)
        \\}
    ;

    var lexer = Lexer.init(source, testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_region, tokens[0].type);
    try testing.expectEqual(TokenType.identifier, tokens[1].type);
    try testing.expectEqualStrings("scratch", tokens[1].lexeme);
}

test "lex pipeline operator" {
    var lexer = Lexer.init("data |> parse |> validate |> transform", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    var pipe_count: usize = 0;
    for (tokens) |token| {
        if (token.type == .pipe_gt) {
            pipe_count += 1;
        }
    }
    try testing.expectEqual(@as(usize, 3), pipe_count);
}

test "error on unexpected character" {
    var lexer = Lexer.init("let x = $invalid", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expect(lexer.hasErrors());
    // Should have an invalid token
    var has_invalid = false;
    for (tokens) |token| {
        if (token.type == .invalid) {
            has_invalid = true;
            break;
        }
    }
    try testing.expect(has_invalid);
}

test "error on unterminated string" {
    var lexer = Lexer.init("let x = \"unterminated", testing.allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer testing.allocator.free(tokens);

    try testing.expect(lexer.hasErrors());
}
