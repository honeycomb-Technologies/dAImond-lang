//! dAImond Error Handling Infrastructure
//!
//! This module provides comprehensive error handling and diagnostic reporting
//! for the dAImond compiler. It supports rich error messages with source context,
//! colored output, and structured error codes.
//!
//! Error Code Ranges:
//!   E0001-E0099: Syntax errors (lexer/parser)
//!   E0100-E0199: Type errors
//!   E0200-E0299: Name resolution errors
//!   E0300-E0399: Borrow/region errors
//!   E0400-E0499: Effect errors
//!   E0500-E0599: Contract errors

const std = @import("std");
const Allocator = std.mem.Allocator;
const lexer_mod = @import("lexer.zig");
const SourceLocation = lexer_mod.SourceLocation;
const TokenType = lexer_mod.TokenType;

// ============================================================================
// Error Codes
// ============================================================================

/// Error code categories for the dAImond compiler
pub const ErrorCode = enum(u16) {
    // Syntax errors (E0001-E0099)
    unexpected_token = 1,
    unexpected_eof = 2,
    unterminated_string = 3,
    unterminated_char = 4,
    invalid_escape_sequence = 5,
    invalid_number_literal = 6,
    expected_expression = 7,
    expected_statement = 8,
    expected_type = 9,
    expected_identifier = 10,
    expected_semicolon = 11,
    expected_colon = 12,
    expected_comma = 13,
    expected_lparen = 14,
    expected_rparen = 15,
    expected_lbrace = 16,
    expected_rbrace = 17,
    expected_lbracket = 18,
    expected_rbracket = 19,
    expected_arrow = 20,
    expected_fat_arrow = 21,
    expected_equals = 22,
    unmatched_delimiter = 23,
    invalid_operator = 24,
    reserved_keyword = 25,
    empty_match = 26,
    missing_match_arm = 27,
    invalid_pattern = 28,
    duplicate_pattern = 29,
    trailing_comma_required = 30,

    // Type errors (E0100-E0199)
    type_mismatch = 100,
    cannot_infer_type = 101,
    invalid_cast = 102,
    incompatible_types = 103,
    expected_function_type = 104,
    expected_array_type = 105,
    expected_struct_type = 106,
    expected_enum_type = 107,
    expected_trait_type = 108,
    expected_numeric_type = 109,
    expected_boolean_type = 110,
    expected_string_type = 111,
    expected_option_type = 112,
    expected_result_type = 113,
    cyclic_type = 114,
    infinite_type = 115,
    type_parameter_count_mismatch = 116,
    constraint_not_satisfied = 117,
    ambiguous_type = 118,
    recursive_type_alias = 119,
    invalid_type_argument = 120,

    // Name resolution errors (E0200-E0299)
    undefined_variable = 200,
    undefined_function = 201,
    undefined_type = 202,
    undefined_module = 203,
    undefined_field = 204,
    undefined_method = 205,
    undefined_variant = 206,
    duplicate_definition = 207,
    duplicate_field = 208,
    duplicate_method = 209,
    duplicate_variant = 210,
    duplicate_parameter = 211,
    duplicate_type_parameter = 212,
    name_collision = 213,
    private_access = 214,
    import_not_found = 215,
    cyclic_import = 216,
    ambiguous_import = 217,
    shadowed_variable = 218,
    unused_variable = 219,
    unused_import = 220,

    // Borrow/region errors (E0300-E0399)
    use_after_move = 300,
    double_move = 301,
    borrow_of_moved = 302,
    mut_borrow_conflict = 303,
    borrow_conflict = 304,
    dangling_reference = 305,
    lifetime_mismatch = 306,
    region_escape = 307,
    invalid_region = 308,
    region_not_in_scope = 309,
    cannot_borrow_as_mutable = 310,
    cannot_move_borrowed = 311,
    partial_move = 312,
    uninitialized_use = 313,
    reassign_immutable = 314,
    missing_copy = 315,

    // Effect errors (E0400-E0499)
    unhandled_effect = 400,
    effect_not_declared = 401,
    effect_mismatch = 402,
    invalid_effect_handler = 403,
    missing_effect_handler = 404,
    effect_escape = 405,
    pure_function_has_effects = 406,
    conflicting_effects = 407,
    effect_not_resumable = 408,
    resume_outside_handler = 409,
    duplicate_effect = 410,
    invalid_resume_type = 411,

    // Contract errors (E0500-E0599)
    precondition_violated = 500,
    postcondition_violated = 501,
    invariant_violated = 502,
    assertion_failed = 503,
    unreachable_code = 504,
    contract_type_error = 505,
    invalid_contract_expression = 506,
    contract_references_mutable = 507,
    old_outside_ensures = 508,
    result_outside_ensures = 509,
    contract_has_side_effects = 510,

    /// Get the numeric error code (e.g., 42 for E0042)
    pub fn code(self: ErrorCode) u16 {
        return @intFromEnum(self);
    }

    /// Format as "E0042" style string
    pub fn format(
        self: ErrorCode,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("E{d:0>4}", .{self.code()});
    }

    /// Get the error category name
    pub fn category(self: ErrorCode) []const u8 {
        const c = self.code();
        if (c < 100) return "Syntax";
        if (c < 200) return "Type";
        if (c < 300) return "Name Resolution";
        if (c < 400) return "Borrow/Region";
        if (c < 500) return "Effect";
        return "Contract";
    }
};

// ============================================================================
// Severity Levels
// ============================================================================

/// Diagnostic severity levels
pub const Severity = enum {
    @"error",
    warning,
    hint,
    note,

    pub fn format(
        self: Severity,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const name = switch (self) {
            .@"error" => "error",
            .warning => "warning",
            .hint => "hint",
            .note => "note",
        };
        try writer.writeAll(name);
    }
};

// ============================================================================
// Source Span and Labels
// ============================================================================

/// A span in the source code (start to end location)
pub const SourceSpan = struct {
    start: SourceLocation,
    end: SourceLocation,
    source_file: ?[]const u8 = null,

    /// Create a span from a single location with a given length
    pub fn fromLocation(loc: SourceLocation, length: usize) SourceSpan {
        return .{
            .start = loc,
            .end = .{
                .line = loc.line,
                .column = loc.column + length,
                .offset = loc.offset + length,
            },
        };
    }

    /// Create a span covering two locations
    pub fn spanning(start: SourceLocation, end: SourceLocation) SourceSpan {
        return .{ .start = start, .end = end };
    }

    /// Get the length of the span in bytes
    pub fn length(self: SourceSpan) usize {
        return self.end.offset - self.start.offset;
    }

    /// Check if this span contains a location
    pub fn contains(self: SourceSpan, loc: SourceLocation) bool {
        return loc.offset >= self.start.offset and loc.offset < self.end.offset;
    }
};

/// A labeled span in source code for error messages
pub const Label = struct {
    span: SourceSpan,
    message: []const u8,
    style: LabelStyle = .secondary,

    pub const LabelStyle = enum {
        primary, // The main error location (^^^)
        secondary, // Supporting context (---)
    };
};

// ============================================================================
// Fix Suggestions
// ============================================================================

/// A suggested fix for an error
pub const Suggestion = struct {
    message: []const u8,
    span: SourceSpan,
    replacement: []const u8,

    /// Create a suggestion to insert text at a location
    pub fn insert(loc: SourceLocation, message: []const u8, text: []const u8) Suggestion {
        return .{
            .message = message,
            .span = SourceSpan.fromLocation(loc, 0),
            .replacement = text,
        };
    }

    /// Create a suggestion to replace a span with new text
    pub fn replace(span: SourceSpan, message: []const u8, text: []const u8) Suggestion {
        return .{
            .message = message,
            .span = span,
            .replacement = text,
        };
    }

    /// Create a suggestion to delete a span
    pub fn delete(span: SourceSpan, message: []const u8) Suggestion {
        return .{
            .message = message,
            .span = span,
            .replacement = "",
        };
    }
};

// ============================================================================
// ANSI Color Support
// ============================================================================

/// ANSI color codes for terminal output
pub const Color = struct {
    pub const reset = "\x1b[0m";
    pub const bold = "\x1b[1m";
    pub const dim = "\x1b[2m";
    pub const italic = "\x1b[3m";
    pub const underline = "\x1b[4m";

    pub const red = "\x1b[31m";
    pub const green = "\x1b[32m";
    pub const yellow = "\x1b[33m";
    pub const blue = "\x1b[34m";
    pub const magenta = "\x1b[35m";
    pub const cyan = "\x1b[36m";
    pub const white = "\x1b[37m";

    pub const bright_red = "\x1b[91m";
    pub const bright_green = "\x1b[92m";
    pub const bright_yellow = "\x1b[93m";
    pub const bright_blue = "\x1b[94m";
    pub const bright_magenta = "\x1b[95m";
    pub const bright_cyan = "\x1b[96m";
    pub const bright_white = "\x1b[97m";
};

/// Color configuration for diagnostic output
pub const ColorConfig = struct {
    enabled: bool = true,

    pub fn error_style(self: ColorConfig) []const u8 {
        return if (self.enabled) Color.bold ++ Color.bright_red else "";
    }

    pub fn warning_style(self: ColorConfig) []const u8 {
        return if (self.enabled) Color.bold ++ Color.bright_yellow else "";
    }

    pub fn hint_style(self: ColorConfig) []const u8 {
        return if (self.enabled) Color.bold ++ Color.bright_cyan else "";
    }

    pub fn note_style(self: ColorConfig) []const u8 {
        return if (self.enabled) Color.bold ++ Color.bright_blue else "";
    }

    pub fn reset(self: ColorConfig) []const u8 {
        return if (self.enabled) Color.reset else "";
    }

    pub fn bold(self: ColorConfig) []const u8 {
        return if (self.enabled) Color.bold else "";
    }

    pub fn dim(self: ColorConfig) []const u8 {
        return if (self.enabled) Color.dim else "";
    }

    pub fn cyan(self: ColorConfig) []const u8 {
        return if (self.enabled) Color.cyan else "";
    }

    pub fn blue(self: ColorConfig) []const u8 {
        return if (self.enabled) Color.blue else "";
    }

    /// Detect if stdout is a TTY and colors should be enabled
    pub fn detect() ColorConfig {
        const stdout = std.io.getStdOut();
        const is_tty = stdout.isTty();
        return .{ .enabled = is_tty };
    }

    /// Force colors on
    pub fn always() ColorConfig {
        return .{ .enabled = true };
    }

    /// Force colors off
    pub fn never() ColorConfig {
        return .{ .enabled = false };
    }
};

// ============================================================================
// Diagnostic
// ============================================================================

/// A diagnostic message with full context
pub const Diagnostic = struct {
    code: ErrorCode,
    severity: Severity,
    message: []const u8,
    location: SourceLocation,
    source_file: ?[]const u8 = null,
    source_line: ?[]const u8 = null,
    labels: []const Label = &.{},
    notes: []const []const u8 = &.{},
    suggestions: []const Suggestion = &.{},

    const Self = @This();

    /// Format the diagnostic for display
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try self.render(writer, ColorConfig.never());
    }

    /// Render the diagnostic with the given color configuration
    pub fn render(self: Self, writer: anytype, colors: ColorConfig) !void {
        // Header: Error[E0042]: Type mismatch
        try self.renderHeader(writer, colors);

        // Source location and context
        try self.renderSourceContext(writer, colors);

        // Additional labels
        try self.renderLabels(writer, colors);

        // Notes and help
        try self.renderNotes(writer, colors);

        // Suggestions
        try self.renderSuggestions(writer, colors);

        try writer.writeByte('\n');
    }

    fn renderHeader(self: Self, writer: anytype, colors: ColorConfig) !void {
        const severity_style = switch (self.severity) {
            .@"error" => colors.error_style(),
            .warning => colors.warning_style(),
            .hint => colors.hint_style(),
            .note => colors.note_style(),
        };

        const severity_name = switch (self.severity) {
            .@"error" => "Error",
            .warning => "Warning",
            .hint => "Hint",
            .note => "Note",
        };

        try writer.print("{s}{s}[{s}]{s}: {s}{s}\n", .{
            severity_style,
            severity_name,
            self.code,
            colors.reset(),
            colors.bold(),
            self.message,
        });
        try writer.writeAll(colors.reset());
    }

    fn renderSourceContext(self: Self, writer: anytype, colors: ColorConfig) !void {
        const file_name = self.source_file orelse "src/main.dm";

        // Location line: ┌─ src/main.dm:15:12
        try writer.writeByte('\n');
        try writer.print("   {s}{s}{s} {s}:{d}:{d}\n", .{
            colors.cyan(),
            BOX_TOP_LEFT ++ BOX_HORIZONTAL,
            colors.reset(),
            file_name,
            self.location.line,
            self.location.column,
        });

        // Render the source line if available
        if (self.source_line) |line| {
            try self.renderSourceLine(writer, colors, self.location.line, line);
        }
    }

    fn renderSourceLine(self: Self, writer: anytype, colors: ColorConfig, line_num: usize, line: []const u8) !void {
        // Print empty pipe line
        try writer.print("   {s}{s}{s}\n", .{ colors.cyan(), BOX_VERTICAL, colors.reset() });

        // Print line number and content
        try writer.print("{s}{d:>3}{s} {s}{s}{s} {s}\n", .{
            colors.dim(),
            line_num,
            colors.reset(),
            colors.cyan(),
            BOX_VERTICAL,
            colors.reset(),
            line,
        });

        // Print underline/pointer
        const severity_style = switch (self.severity) {
            .@"error" => colors.error_style(),
            .warning => colors.warning_style(),
            .hint => colors.hint_style(),
            .note => colors.note_style(),
        };

        try writer.print("   {s}{s}{s} ", .{ colors.cyan(), BOX_VERTICAL, colors.reset() });

        // Calculate column position and draw carets
        const col = if (self.location.column > 0) self.location.column - 1 else 0;
        try writer.writeByteNTimes(' ', col);
        try writer.writeAll(severity_style);

        // Draw carets for the span (minimum 1)
        const caret_count: usize = blk: {
            for (self.labels) |label| {
                if (label.style == .primary) {
                    break :blk @max(1, label.span.length());
                }
            }
            break :blk 1;
        };

        try writer.writeByteNTimes('^', caret_count);
        try writer.writeAll(colors.reset());

        // Print the primary label message if any
        for (self.labels) |label| {
            if (label.style == .primary and label.message.len > 0) {
                try writer.print(" {s}", .{label.message});
            }
        }

        try writer.writeByte('\n');
    }

    fn renderLabels(self: Self, writer: anytype, colors: ColorConfig) !void {
        for (self.labels) |label| {
            if (label.style == .secondary and label.message.len > 0) {
                try writer.print("   {s}{s}{s}\n", .{ colors.cyan(), BOX_VERTICAL, colors.reset() });
                try writer.print("   {s}{s}{s}  {s}note:{s} {s}\n", .{
                    colors.cyan(),
                    BOX_VERTICAL,
                    colors.reset(),
                    colors.note_style(),
                    colors.reset(),
                    label.message,
                });
            }
        }
    }

    fn renderNotes(self: Self, writer: anytype, colors: ColorConfig) !void {
        for (self.notes) |note_text| {
            try writer.print("   {s}{s}{s}\n", .{ colors.cyan(), BOX_VERTICAL, colors.reset() });
            try writer.print("   {s}={s} {s}help:{s} {s}\n", .{
                colors.cyan(),
                colors.reset(),
                colors.note_style(),
                colors.reset(),
                note_text,
            });
        }
    }

    fn renderSuggestions(self: Self, writer: anytype, colors: ColorConfig) !void {
        for (self.suggestions) |suggestion| {
            try writer.print("   {s}{s}{s}\n", .{ colors.cyan(), BOX_VERTICAL, colors.reset() });
            try writer.print("   {s}={s} {s}suggestion:{s} {s}\n", .{
                colors.cyan(),
                colors.reset(),
                colors.hint_style(),
                colors.reset(),
                suggestion.message,
            });
            if (suggestion.replacement.len > 0) {
                try writer.print("   {s}{s}{s}    {s}`{s}`{s}\n", .{
                    colors.cyan(),
                    BOX_VERTICAL,
                    colors.reset(),
                    colors.dim(),
                    suggestion.replacement,
                    colors.reset(),
                });
            }
        }
    }

    // Box drawing characters
    const BOX_VERTICAL = "\u{2502}"; // │
    const BOX_HORIZONTAL = "\u{2500}"; // ─
    const BOX_TOP_LEFT = "\u{250C}"; // ┌
    const BOX_BOTTOM_LEFT = "\u{2514}"; // └
};

// ============================================================================
// DiagnosticBuilder
// ============================================================================

/// Builder for constructing diagnostics fluently
pub const DiagnosticBuilder = struct {
    allocator: Allocator,
    code: ErrorCode,
    severity: Severity = .@"error",
    message: []const u8 = "",
    location: SourceLocation = .{ .line = 0, .column = 0, .offset = 0 },
    source_file: ?[]const u8 = null,
    source_line: ?[]const u8 = null,
    labels: std.ArrayList(Label),
    notes: std.ArrayList([]const u8),
    suggestions: std.ArrayList(Suggestion),

    const Self = @This();

    pub fn init(allocator: Allocator, code: ErrorCode) Self {
        return .{
            .allocator = allocator,
            .code = code,
            .labels = std.ArrayList(Label).init(allocator),
            .notes = std.ArrayList([]const u8).init(allocator),
            .suggestions = std.ArrayList(Suggestion).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.labels.deinit();
        self.notes.deinit();
        self.suggestions.deinit();
    }

    pub fn withSeverity(self: *Self, severity: Severity) *Self {
        self.severity = severity;
        return self;
    }

    pub fn withMessage(self: *Self, message: []const u8) *Self {
        self.message = message;
        return self;
    }

    pub fn withLocation(self: *Self, location: SourceLocation) *Self {
        self.location = location;
        return self;
    }

    pub fn withSourceFile(self: *Self, file: []const u8) *Self {
        self.source_file = file;
        return self;
    }

    pub fn withSourceLine(self: *Self, line: []const u8) *Self {
        self.source_line = line;
        return self;
    }

    pub fn addLabel(self: *Self, label: Label) !void {
        try self.labels.append(label);
    }

    pub fn addPrimaryLabel(self: *Self, span: SourceSpan, message: []const u8) !void {
        try self.labels.append(.{
            .span = span,
            .message = message,
            .style = .primary,
        });
    }

    pub fn addSecondaryLabel(self: *Self, span: SourceSpan, message: []const u8) !void {
        try self.labels.append(.{
            .span = span,
            .message = message,
            .style = .secondary,
        });
    }

    pub fn addNote(self: *Self, note: []const u8) !void {
        try self.notes.append(note);
    }

    pub fn addSuggestion(self: *Self, suggestion: Suggestion) !void {
        try self.suggestions.append(suggestion);
    }

    pub fn build(self: Self) Diagnostic {
        return .{
            .code = self.code,
            .severity = self.severity,
            .message = self.message,
            .location = self.location,
            .source_file = self.source_file,
            .source_line = self.source_line,
            .labels = self.labels.items,
            .notes = self.notes.items,
            .suggestions = self.suggestions.items,
        };
    }
};

// ============================================================================
// DiagnosticBag
// ============================================================================

/// A collection of diagnostics with utilities for management
pub const DiagnosticBag = struct {
    allocator: Allocator,
    diagnostics: std.ArrayList(Diagnostic),
    error_count: usize = 0,
    warning_count: usize = 0,
    max_errors: ?usize = null,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .diagnostics = std.ArrayList(Diagnostic).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.diagnostics.deinit();
    }

    /// Set maximum number of errors before stopping
    pub fn setMaxErrors(self: *Self, max: usize) void {
        self.max_errors = max;
    }

    /// Add a diagnostic to the bag
    pub fn add(self: *Self, diagnostic: Diagnostic) !void {
        // Check for duplicates based on location and code
        for (self.diagnostics.items) |existing| {
            if (existing.code == diagnostic.code and
                existing.location.line == diagnostic.location.line and
                existing.location.column == diagnostic.location.column)
            {
                return; // Duplicate, skip
            }
        }

        try self.diagnostics.append(diagnostic);

        switch (diagnostic.severity) {
            .@"error" => self.error_count += 1,
            .warning => self.warning_count += 1,
            else => {},
        }
    }

    /// Check if we've hit the error limit
    pub fn hasReachedLimit(self: Self) bool {
        if (self.max_errors) |max| {
            return self.error_count >= max;
        }
        return false;
    }

    /// Check if there are any errors
    pub fn hasErrors(self: Self) bool {
        return self.error_count > 0;
    }

    /// Check if there are any warnings
    pub fn hasWarnings(self: Self) bool {
        return self.warning_count > 0;
    }

    /// Get all diagnostics
    pub fn items(self: Self) []const Diagnostic {
        return self.diagnostics.items;
    }

    /// Sort diagnostics by location
    pub fn sort(self: *Self) void {
        std.mem.sort(Diagnostic, self.diagnostics.items, {}, struct {
            fn lessThan(_: void, a: Diagnostic, b: Diagnostic) bool {
                if (a.location.line != b.location.line) {
                    return a.location.line < b.location.line;
                }
                return a.location.column < b.location.column;
            }
        }.lessThan);
    }

    /// Render all diagnostics to a writer
    pub fn render(self: Self, writer: anytype, colors: ColorConfig) !void {
        for (self.diagnostics.items) |diagnostic| {
            try diagnostic.render(writer, colors);
        }

        // Print summary
        if (self.error_count > 0 or self.warning_count > 0) {
            try writer.print("{s}aborting due to {d} error(s) and {d} warning(s){s}\n", .{
                colors.bold(),
                self.error_count,
                self.warning_count,
                colors.reset(),
            });
        }
    }

    /// Clear all diagnostics
    pub fn clear(self: *Self) void {
        self.diagnostics.clearRetainingCapacity();
        self.error_count = 0;
        self.warning_count = 0;
    }
};

// ============================================================================
// Common Error Constructors
// ============================================================================

/// Pre-built error constructors for common error types
pub const Errors = struct {
    allocator: Allocator,
    source: ?[]const u8 = null,
    source_file: ?[]const u8 = null,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{ .allocator = allocator };
    }

    pub fn withSource(self: Self, source: []const u8) Self {
        var copy = self;
        copy.source = source;
        return copy;
    }

    pub fn withSourceFile(self: Self, file: []const u8) Self {
        var copy = self;
        copy.source_file = file;
        return copy;
    }

    /// Get a line from the source at the given location
    fn getSourceLine(self: Self, location: SourceLocation) ?[]const u8 {
        if (self.source) |source| {
            return extractLine(source, location.line);
        }
        return null;
    }

    // ---- Syntax Errors ----

    /// Create an "unexpected token" error
    pub fn unexpectedToken(
        self: Self,
        expected: []const u8,
        found: TokenType,
        found_lexeme: []const u8,
        location: SourceLocation,
    ) Diagnostic {
        var buf: [256]u8 = undefined;
        const found_name = @tagName(found);
        const msg = std.fmt.bufPrint(&buf, "expected {s}, found `{s}` ({s})", .{
            expected,
            found_lexeme,
            found_name,
        }) catch "unexpected token";

        const span = SourceSpan.fromLocation(location, found_lexeme.len);

        return .{
            .code = .unexpected_token,
            .severity = .@"error",
            .message = msg,
            .location = location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(location),
            .labels = &[_]Label{.{
                .span = span,
                .message = "unexpected token here",
                .style = .primary,
            }},
        };
    }

    /// Create an "unexpected EOF" error
    pub fn unexpectedEof(self: Self, expected: []const u8, location: SourceLocation) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "unexpected end of file, expected {s}", .{expected}) catch "unexpected EOF";

        return .{
            .code = .unexpected_eof,
            .severity = .@"error",
            .message = msg,
            .location = location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(location),
        };
    }

    // ---- Type Errors ----

    /// Create a "type mismatch" error
    pub fn typeMismatch(
        self: Self,
        expected: []const u8,
        found: []const u8,
        location: SourceLocation,
    ) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "type mismatch: expected `{s}`, found `{s}`", .{
            expected,
            found,
        }) catch "type mismatch";

        return .{
            .code = .type_mismatch,
            .severity = .@"error",
            .message = msg,
            .location = location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(location),
            .labels = &[_]Label{.{
                .span = SourceSpan.fromLocation(location, 1),
                .message = blk: {
                    var label_buf: [128]u8 = undefined;
                    break :blk std.fmt.bufPrint(&label_buf, "found `{s}`", .{found}) catch "found value";
                },
                .style = .primary,
            }},
        };
    }

    // ---- Name Resolution Errors ----

    /// Create an "undefined variable" error
    pub fn undefinedVariable(self: Self, name: []const u8, location: SourceLocation) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "undefined variable `{s}`", .{name}) catch "undefined variable";

        return .{
            .code = .undefined_variable,
            .severity = .@"error",
            .message = msg,
            .location = location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(location),
            .labels = &[_]Label{.{
                .span = SourceSpan.fromLocation(location, name.len),
                .message = "not found in this scope",
                .style = .primary,
            }},
            .notes = &[_][]const u8{
                "variables must be declared with `let` or `const` before use",
            },
        };
    }

    /// Create an "undefined type" error
    pub fn undefinedType(self: Self, name: []const u8, location: SourceLocation) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "undefined type `{s}`", .{name}) catch "undefined type";

        return .{
            .code = .undefined_type,
            .severity = .@"error",
            .message = msg,
            .location = location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(location),
            .labels = &[_]Label{.{
                .span = SourceSpan.fromLocation(location, name.len),
                .message = "type not found",
                .style = .primary,
            }},
        };
    }

    /// Create a "duplicate definition" error
    pub fn duplicateDefinition(
        self: Self,
        name: []const u8,
        original_loc: SourceLocation,
        duplicate_loc: SourceLocation,
    ) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "duplicate definition of `{s}`", .{name}) catch "duplicate definition";

        return .{
            .code = .duplicate_definition,
            .severity = .@"error",
            .message = msg,
            .location = duplicate_loc,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(duplicate_loc),
            .labels = &[_]Label{
                .{
                    .span = SourceSpan.fromLocation(duplicate_loc, name.len),
                    .message = "duplicate definition here",
                    .style = .primary,
                },
                .{
                    .span = SourceSpan.fromLocation(original_loc, name.len),
                    .message = "first defined here",
                    .style = .secondary,
                },
            },
        };
    }

    /// Create a "missing field" error
    pub fn missingField(
        self: Self,
        struct_name: []const u8,
        field: []const u8,
        location: SourceLocation,
    ) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "struct `{s}` has no field `{s}`", .{
            struct_name,
            field,
        }) catch "missing field";

        return .{
            .code = .undefined_field,
            .severity = .@"error",
            .message = msg,
            .location = location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(location),
            .labels = &[_]Label{.{
                .span = SourceSpan.fromLocation(location, field.len),
                .message = "unknown field",
                .style = .primary,
            }},
        };
    }

    /// Create a "wrong argument count" error
    pub fn wrongArgumentCount(
        self: Self,
        func_name: []const u8,
        expected: usize,
        found: usize,
        location: SourceLocation,
    ) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "function `{s}` expects {d} argument(s), found {d}", .{
            func_name,
            expected,
            found,
        }) catch "wrong argument count";

        return .{
            .code = .type_parameter_count_mismatch,
            .severity = .@"error",
            .message = msg,
            .location = location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(location),
            .labels = &[_]Label{.{
                .span = SourceSpan.fromLocation(location, 1),
                .message = blk: {
                    var label_buf: [64]u8 = undefined;
                    break :blk std.fmt.bufPrint(&label_buf, "expected {d}, found {d}", .{ expected, found }) catch "wrong count";
                },
                .style = .primary,
            }},
        };
    }

    // ---- Borrow/Region Errors ----

    /// Create a "use after move" error
    pub fn useAfterMove(
        self: Self,
        name: []const u8,
        move_location: SourceLocation,
        use_location: SourceLocation,
    ) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "use of moved value `{s}`", .{name}) catch "use after move";

        return .{
            .code = .use_after_move,
            .severity = .@"error",
            .message = msg,
            .location = use_location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(use_location),
            .labels = &[_]Label{
                .{
                    .span = SourceSpan.fromLocation(use_location, name.len),
                    .message = "value used here after move",
                    .style = .primary,
                },
                .{
                    .span = SourceSpan.fromLocation(move_location, name.len),
                    .message = "value moved here",
                    .style = .secondary,
                },
            },
            .notes = &[_][]const u8{
                "consider cloning the value if you need to use it after the move",
            },
        };
    }

    /// Create a "cannot borrow as mutable" error
    pub fn cannotBorrowMutable(
        self: Self,
        name: []const u8,
        location: SourceLocation,
    ) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "cannot borrow `{s}` as mutable", .{name}) catch "cannot borrow as mutable";

        return .{
            .code = .cannot_borrow_as_mutable,
            .severity = .@"error",
            .message = msg,
            .location = location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(location),
            .labels = &[_]Label{.{
                .span = SourceSpan.fromLocation(location, name.len),
                .message = "cannot borrow as mutable",
                .style = .primary,
            }},
            .notes = &[_][]const u8{
                "consider declaring the variable with `let mut` instead of `let`",
            },
        };
    }

    // ---- Effect Errors ----

    /// Create an "unhandled effect" error
    pub fn unhandledEffect(
        self: Self,
        effect_name: []const u8,
        location: SourceLocation,
    ) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "unhandled effect `{s}`", .{effect_name}) catch "unhandled effect";

        return .{
            .code = .unhandled_effect,
            .severity = .@"error",
            .message = msg,
            .location = location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(location),
            .labels = &[_]Label{.{
                .span = SourceSpan.fromLocation(location, effect_name.len),
                .message = "effect not handled",
                .style = .primary,
            }},
            .notes = &[_][]const u8{
                "add a handler using `with` or declare the effect in the function signature",
            },
        };
    }

    // ---- Contract Errors ----

    /// Create a "precondition violated" error
    pub fn preconditionViolated(
        self: Self,
        condition: []const u8,
        location: SourceLocation,
    ) Diagnostic {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "precondition violated: {s}", .{condition}) catch "precondition violated";

        return .{
            .code = .precondition_violated,
            .severity = .@"error",
            .message = msg,
            .location = location,
            .source_file = self.source_file,
            .source_line = self.getSourceLine(location),
            .labels = &[_]Label{.{
                .span = SourceSpan.fromLocation(location, 1),
                .message = "precondition not satisfied here",
                .style = .primary,
            }},
        };
    }
};

// ============================================================================
// Utility Functions
// ============================================================================

/// Extract a line from source text by line number (1-indexed)
pub fn extractLine(source: []const u8, line_num: usize) ?[]const u8 {
    if (line_num == 0) return null;

    var current_line: usize = 1;
    var line_start: usize = 0;

    for (source, 0..) |c, i| {
        if (current_line == line_num) {
            // Find end of this line
            var line_end = i;
            while (line_end < source.len and source[line_end] != '\n') {
                line_end += 1;
            }
            return source[line_start..line_end];
        }

        if (c == '\n') {
            current_line += 1;
            line_start = i + 1;
        }
    }

    // Handle last line without trailing newline
    if (current_line == line_num and line_start < source.len) {
        return source[line_start..];
    }

    return null;
}

/// Get token type name as a human-readable string
pub fn tokenTypeName(token_type: TokenType) []const u8 {
    return switch (token_type) {
        .integer => "integer literal",
        .float => "float literal",
        .string => "string literal",
        .raw_string => "raw string literal",
        .byte_string => "byte string literal",
        .char_literal => "character literal",
        .identifier => "identifier",
        .kw_fn => "`fn`",
        .kw_let => "`let`",
        .kw_mut => "`mut`",
        .kw_const => "`const`",
        .kw_if => "`if`",
        .kw_else => "`else`",
        .kw_match => "`match`",
        .kw_for => "`for`",
        .kw_while => "`while`",
        .kw_loop => "`loop`",
        .kw_break => "`break`",
        .kw_continue => "`continue`",
        .kw_return => "`return`",
        .kw_struct => "`struct`",
        .kw_enum => "`enum`",
        .kw_trait => "`trait`",
        .kw_impl => "`impl`",
        .kw_import => "`import`",
        .kw_module => "`module`",
        .kw_with => "`with`",
        .kw_region => "`region`",
        .kw_comptime => "`comptime`",
        .kw_true => "`true`",
        .kw_false => "`false`",
        .kw_and => "`and`",
        .kw_or => "`or`",
        .kw_not => "`not`",
        .kw_in => "`in`",
        .kw_private => "`private`",
        .kw_requires => "`requires`",
        .kw_ensures => "`ensures`",
        .kw_discard => "`discard`",
        .kw_expect => "`expect`",
        .plus => "`+`",
        .minus => "`-`",
        .star => "`*`",
        .slash => "`/`",
        .percent => "`%`",
        .caret => "`^`",
        .ampersand => "`&`",
        .pipe => "`|`",
        .tilde => "`~`",
        .eq => "`=`",
        .eq_eq => "`==`",
        .bang => "`!`",
        .bang_eq => "`!=`",
        .lt => "`<`",
        .gt => "`>`",
        .lt_eq => "`<=`",
        .gt_eq => "`>=`",
        .lt_lt => "`<<`",
        .gt_gt => "`>>`",
        .arrow => "`->`",
        .fat_arrow => "`=>`",
        .question => "`?`",
        .question_question => "`??`",
        .pipe_gt => "`|>`",
        .at => "`@`",
        .plus_eq => "`+=`",
        .minus_eq => "`-=`",
        .star_eq => "`*=`",
        .slash_eq => "`/=`",
        .percent_eq => "`%=`",
        .ampersand_eq => "`&=`",
        .pipe_eq => "`|=`",
        .caret_eq => "`^=`",
        .lt_lt_eq => "`<<=`",
        .gt_gt_eq => "`>>=`",
        .dot => "`.`",
        .dot_dot => "`..`",
        .dot_dot_eq => "`..=`",
        .colon => "`:`",
        .colon_colon => "`::`",
        .semicolon => "`;`",
        .comma => "`,`",
        .underscore => "`_`",
        .lparen => "`(`",
        .rparen => "`)`",
        .lbrace => "`{`",
        .rbrace => "`}`",
        .lbracket => "`[`",
        .rbracket => "`]`",
        .eof => "end of file",
        .invalid => "invalid token",
    };
}

// ============================================================================
// TESTS
// ============================================================================

const testing = std.testing;

test "error code formatting" {
    const code = ErrorCode.type_mismatch;
    var buf: [16]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{s}", .{code}) catch unreachable;
    try testing.expectEqualStrings("E0100", result);
}

test "error code categories" {
    try testing.expectEqualStrings("Syntax", ErrorCode.unexpected_token.category());
    try testing.expectEqualStrings("Type", ErrorCode.type_mismatch.category());
    try testing.expectEqualStrings("Name Resolution", ErrorCode.undefined_variable.category());
    try testing.expectEqualStrings("Borrow/Region", ErrorCode.use_after_move.category());
    try testing.expectEqualStrings("Effect", ErrorCode.unhandled_effect.category());
    try testing.expectEqualStrings("Contract", ErrorCode.precondition_violated.category());
}

test "source span creation" {
    const loc = SourceLocation{ .line = 1, .column = 5, .offset = 10 };
    const span = SourceSpan.fromLocation(loc, 8);

    try testing.expectEqual(@as(usize, 1), span.start.line);
    try testing.expectEqual(@as(usize, 5), span.start.column);
    try testing.expectEqual(@as(usize, 10), span.start.offset);
    try testing.expectEqual(@as(usize, 1), span.end.line);
    try testing.expectEqual(@as(usize, 13), span.end.column);
    try testing.expectEqual(@as(usize, 18), span.end.offset);
    try testing.expectEqual(@as(usize, 8), span.length());
}

test "extract line from source" {
    const source =
        \\line one
        \\line two
        \\line three
    ;

    try testing.expectEqualStrings("line one", extractLine(source, 1).?);
    try testing.expectEqualStrings("line two", extractLine(source, 2).?);
    try testing.expectEqualStrings("line three", extractLine(source, 3).?);
    try testing.expectEqual(@as(?[]const u8, null), extractLine(source, 0));
    try testing.expectEqual(@as(?[]const u8, null), extractLine(source, 4));
}

test "diagnostic bag deduplication" {
    var bag = DiagnosticBag.init(testing.allocator);
    defer bag.deinit();

    const loc = SourceLocation{ .line = 5, .column = 10, .offset = 50 };

    // Add same error twice
    try bag.add(.{
        .code = .type_mismatch,
        .severity = .@"error",
        .message = "test error",
        .location = loc,
    });

    try bag.add(.{
        .code = .type_mismatch,
        .severity = .@"error",
        .message = "test error",
        .location = loc,
    });

    // Should only have one error due to deduplication
    try testing.expectEqual(@as(usize, 1), bag.items().len);
    try testing.expectEqual(@as(usize, 1), bag.error_count);
}

test "diagnostic bag sorting" {
    var bag = DiagnosticBag.init(testing.allocator);
    defer bag.deinit();

    // Add errors in reverse order
    try bag.add(.{
        .code = .type_mismatch,
        .severity = .@"error",
        .message = "error C",
        .location = .{ .line = 30, .column = 1, .offset = 300 },
    });

    try bag.add(.{
        .code = .type_mismatch,
        .severity = .@"error",
        .message = "error A",
        .location = .{ .line = 10, .column = 1, .offset = 100 },
    });

    try bag.add(.{
        .code = .type_mismatch,
        .severity = .@"error",
        .message = "error B",
        .location = .{ .line = 20, .column = 1, .offset = 200 },
    });

    bag.sort();

    try testing.expectEqual(@as(usize, 10), bag.items()[0].location.line);
    try testing.expectEqual(@as(usize, 20), bag.items()[1].location.line);
    try testing.expectEqual(@as(usize, 30), bag.items()[2].location.line);
}

test "diagnostic rendering without colors" {
    const diagnostic = Diagnostic{
        .code = .type_mismatch,
        .severity = .@"error",
        .message = "type mismatch",
        .location = .{ .line = 15, .column = 12, .offset = 100 },
        .source_file = "src/main.dm",
        .source_line = "    return find_user(id)",
        .labels = &[_]Label{.{
            .span = SourceSpan.fromLocation(
                .{ .line = 15, .column = 12, .offset = 100 },
                13,
            ),
            .message = "found `Option[User]`",
            .style = .primary,
        }},
        .notes = &[_][]const u8{
            "`find_user` returns `Option[User]` because the user might not exist",
        },
    };

    var buf = std.ArrayList(u8).init(testing.allocator);
    defer buf.deinit();

    try diagnostic.render(buf.writer(), ColorConfig.never());

    const output = buf.items;

    // Verify key parts of the output
    try testing.expect(std.mem.indexOf(u8, output, "Error[E0100]") != null);
    try testing.expect(std.mem.indexOf(u8, output, "type mismatch") != null);
    try testing.expect(std.mem.indexOf(u8, output, "src/main.dm:15:12") != null);
    try testing.expect(std.mem.indexOf(u8, output, "return find_user(id)") != null);
    try testing.expect(std.mem.indexOf(u8, output, "help:") != null);
}

test "color config detection" {
    // Test explicit configurations
    const always = ColorConfig.always();
    try testing.expect(always.enabled);

    const never = ColorConfig.never();
    try testing.expect(!never.enabled);

    // Test style methods
    try testing.expect(always.error_style().len > 0);
    try testing.expectEqual(@as(usize, 0), never.error_style().len);
}

test "common error constructors" {
    const errors = Errors.init(testing.allocator)
        .withSourceFile("test.dm")
        .withSource("let x = undefined_var");

    // Test undefined variable error
    const undefined_err = errors.undefinedVariable(
        "undefined_var",
        .{ .line = 1, .column = 9, .offset = 8 },
    );

    try testing.expectEqual(ErrorCode.undefined_variable, undefined_err.code);
    try testing.expectEqual(Severity.@"error", undefined_err.severity);
    try testing.expect(std.mem.indexOf(u8, undefined_err.message, "undefined_var") != null);
}

test "diagnostic builder" {
    var builder = DiagnosticBuilder.init(testing.allocator, .type_mismatch);
    defer builder.deinit();

    _ = builder.withSeverity(.@"error")
        .withMessage("expected int, found string")
        .withLocation(.{ .line = 10, .column = 5, .offset = 100 })
        .withSourceFile("test.dm")
        .withSourceLine("let x: int = \"hello\"");

    try builder.addNote("integers cannot be created from string literals");
    try builder.addSuggestion(Suggestion{
        .message = "use parseInt to convert",
        .span = SourceSpan.fromLocation(.{ .line = 10, .column = 14, .offset = 109 }, 7),
        .replacement = "parseInt(\"hello\")",
    });

    const diagnostic = builder.build();

    try testing.expectEqual(ErrorCode.type_mismatch, diagnostic.code);
    try testing.expectEqual(Severity.@"error", diagnostic.severity);
    try testing.expectEqual(@as(usize, 1), diagnostic.notes.len);
    try testing.expectEqual(@as(usize, 1), diagnostic.suggestions.len);
}

test "diagnostic bag max errors" {
    var bag = DiagnosticBag.init(testing.allocator);
    defer bag.deinit();

    bag.setMaxErrors(2);

    try bag.add(.{
        .code = .type_mismatch,
        .severity = .@"error",
        .message = "error 1",
        .location = .{ .line = 1, .column = 1, .offset = 0 },
    });
    try testing.expect(!bag.hasReachedLimit());

    try bag.add(.{
        .code = .undefined_variable,
        .severity = .@"error",
        .message = "error 2",
        .location = .{ .line = 2, .column = 1, .offset = 10 },
    });
    try testing.expect(bag.hasReachedLimit());
}

test "suggestion constructors" {
    const loc = SourceLocation{ .line = 1, .column = 5, .offset = 4 };
    const span = SourceSpan.fromLocation(loc, 5);

    const insert_sug = Suggestion.insert(loc, "add semicolon", ";");
    try testing.expectEqualStrings(";", insert_sug.replacement);
    try testing.expectEqual(@as(usize, 0), insert_sug.span.length());

    const replace_sug = Suggestion.replace(span, "use correct name", "correct");
    try testing.expectEqualStrings("correct", replace_sug.replacement);
    try testing.expectEqual(@as(usize, 5), replace_sug.span.length());

    const delete_sug = Suggestion.delete(span, "remove this");
    try testing.expectEqualStrings("", delete_sug.replacement);
}

test "token type names" {
    try testing.expectEqualStrings("integer literal", tokenTypeName(.integer));
    try testing.expectEqualStrings("`fn`", tokenTypeName(.kw_fn));
    try testing.expectEqualStrings("end of file", tokenTypeName(.eof));
    try testing.expectEqualStrings("`+`", tokenTypeName(.plus));
}
