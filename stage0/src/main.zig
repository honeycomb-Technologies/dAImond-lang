//! dAImond Compiler - Stage 0
//!
//! This is the bootstrap compiler for the dAImond programming language,
//! written in Zig. It compiles dAImond source code to C.
//!
//! Pipeline:
//!   Source -> Lexer -> Tokens -> Parser -> AST -> Checker -> Typed AST -> Codegen -> C Code -> CC -> Binary

const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const SourceLocation = @import("lexer.zig").SourceLocation;
const parser_mod = @import("parser.zig");
const Parser = parser_mod.Parser;
const ParseError = parser_mod.ParseError;
const ast = @import("ast.zig");
const SourceFile = ast.SourceFile;
const errors = @import("errors.zig");
const ColorConfig = errors.ColorConfig;
const DiagnosticBag = errors.DiagnosticBag;
const checker_mod = @import("checker.zig");
const TypeChecker = checker_mod.TypeChecker;
const codegen = @import("codegen.zig");
const CodeGenerator = codegen.CodeGenerator;
const package = @import("package.zig");

const version = "0.1.0";

// ============================================================================
// Compiler Options
// ============================================================================

const OptLevel = enum {
    O0,
    O1,
    O2,
    O3,

    pub fn toCcFlag(self: OptLevel) []const u8 {
        return switch (self) {
            .O0 => "-O0",
            .O1 => "-O1",
            .O2 => "-O2",
            .O3 => "-O3",
        };
    }
};

const Command = enum {
    compile, // daimond <file.dm> - Full compile to executable
    build, // daimond build <file.dm> - Compile to C only
    run, // daimond run <file.dm> - Compile and run
    lex, // daimond lex <file.dm> - Show tokens
    parse, // daimond parse <file.dm> - Show AST
    check, // daimond check <file.dm> - Type check only
    fmt, // daimond fmt <file.dm> - Format code
    test_run, // daimond test <file.dm> - Run test functions
    pkg, // daimond pkg <subcommand> - Package management
};

const CompilerOptions = struct {
    command: Command = .compile,
    input_file: ?[]const u8 = null,
    output_file: ?[]const u8 = null,
    compile_to_c_only: bool = false, // -c flag
    emit_c: bool = false, // --emit-c flag
    no_color: bool = false,
    opt_level: OptLevel = .O0,
    verbose: bool = false,
    show_help: bool = false,
    show_version: bool = false,

    // Derived paths
    c_output_path: ?[]const u8 = null,
    exe_output_path: ?[]const u8 = null,
};

// ============================================================================
// Exit Codes
// ============================================================================

const ExitCode = enum(u8) {
    success = 0,
    error_compile = 1,
    error_usage = 2,
    error_runtime = 3,
};

// ============================================================================
// Timing Utilities
// ============================================================================

const Timer = struct {
    start_time: i64,
    verbose: bool,

    pub fn init(verbose: bool) Timer {
        return .{
            .start_time = std.time.milliTimestamp(),
            .verbose = verbose,
        };
    }

    pub fn elapsedMs(self: Timer) i64 {
        return std.time.milliTimestamp() - self.start_time;
    }

    pub fn report(self: Timer, writer: anytype, phase: []const u8) !void {
        if (self.verbose) {
            try writer.print("        ({d}ms)\n", .{self.elapsedMs()});
        } else {
            _ = phase;
        }
    }
};

// ============================================================================
// Main Entry Point
// ============================================================================

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var opts = parseArgs(args) catch |err| {
        const stderr = std.io.getStdErr().writer();
        switch (err) {
            error.InvalidOption => {},
            error.MissingOutputArg => try stderr.print("Error: -o requires an argument\n", .{}),
            else => try stderr.print("Error: {}\n", .{err}),
        }
        try printUsage();
        std.process.exit(@intFromEnum(ExitCode.error_usage));
    };

    // Handle help and version early
    if (opts.show_help) {
        try printUsage();
        return;
    }

    if (opts.show_version) {
        try printVersion();
        return;
    }

    // Need an input file for most commands (but not pkg)
    if (opts.input_file == null and opts.command != .pkg) {
        try printUsage();
        return;
    }

    // Pkg command doesn't need output paths
    if (opts.command == .pkg) {
        const exit_code = executePkgCommand(args, allocator) catch |err| {
            const stderr = std.io.getStdErr().writer();
            try stderr.print("Package error: {}\n", .{err});
            std.process.exit(@intFromEnum(ExitCode.error_compile));
        };
        if (exit_code != .success) {
            std.process.exit(@intFromEnum(exit_code));
        }
        return;
    }

    // Derive output paths
    const input_file = opts.input_file.?;
    opts = deriveOutputPaths(opts, input_file, allocator) catch |err| {
        const stderr = std.io.getStdErr().writer();
        try stderr.print("Error deriving output paths: {}\n", .{err});
        std.process.exit(@intFromEnum(ExitCode.error_compile));
    };
    defer {
        if (opts.c_output_path) |p| allocator.free(p);
        if (opts.exe_output_path) |p| allocator.free(p);
    }

    // Execute the command
    const exit_code = executeCommand(opts, allocator) catch |err| {
        const stderr = std.io.getStdErr().writer();
        try stderr.print("Internal compiler error: {}\n", .{err});
        std.process.exit(@intFromEnum(ExitCode.error_compile));
    };

    if (exit_code != .success) {
        std.process.exit(@intFromEnum(exit_code));
    }
}

// ============================================================================
// Argument Parsing
// ============================================================================

fn parseArgs(args: []const []const u8) !CompilerOptions {
    var opts = CompilerOptions{};

    if (args.len < 2) {
        opts.show_help = true;
        return opts;
    }

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];

        // Help and version flags
        if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            opts.show_help = true;
            return opts;
        }

        if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
            opts.show_version = true;
            return opts;
        }

        // Commands
        if (std.mem.eql(u8, arg, "build")) {
            opts.command = .build;
            opts.compile_to_c_only = true;
            continue;
        }

        if (std.mem.eql(u8, arg, "run")) {
            opts.command = .run;
            continue;
        }

        if (std.mem.eql(u8, arg, "lex")) {
            opts.command = .lex;
            continue;
        }

        if (std.mem.eql(u8, arg, "parse")) {
            opts.command = .parse;
            continue;
        }

        if (std.mem.eql(u8, arg, "check")) {
            opts.command = .check;
            continue;
        }

        if (std.mem.eql(u8, arg, "fmt")) {
            opts.command = .fmt;
            continue;
        }

        if (std.mem.eql(u8, arg, "test")) {
            opts.command = .test_run;
            continue;
        }

        if (std.mem.eql(u8, arg, "pkg")) {
            opts.command = .pkg;
            // Remaining args are pkg subcommand args, stored via input_file hack
            // We'll handle them in executePkgCommand
            return opts;
        }

        // Options with arguments
        if (std.mem.eql(u8, arg, "-o")) {
            i += 1;
            if (i >= args.len) {
                return error.MissingOutputArg;
            }
            opts.output_file = args[i];
            continue;
        }

        // Flags
        if (std.mem.eql(u8, arg, "-c")) {
            opts.compile_to_c_only = true;
            continue;
        }

        if (std.mem.eql(u8, arg, "--emit-c")) {
            opts.emit_c = true;
            continue;
        }

        if (std.mem.eql(u8, arg, "--no-color")) {
            opts.no_color = true;
            continue;
        }

        if (std.mem.eql(u8, arg, "--verbose")) {
            opts.verbose = true;
            continue;
        }

        // Optimization levels
        if (std.mem.eql(u8, arg, "-O0")) {
            opts.opt_level = .O0;
            continue;
        }
        if (std.mem.eql(u8, arg, "-O1")) {
            opts.opt_level = .O1;
            continue;
        }
        if (std.mem.eql(u8, arg, "-O2")) {
            opts.opt_level = .O2;
            continue;
        }
        if (std.mem.eql(u8, arg, "-O3")) {
            opts.opt_level = .O3;
            continue;
        }

        // Unknown options
        if (std.mem.startsWith(u8, arg, "-")) {
            const stderr = std.io.getStdErr().writer();
            try stderr.print("Error: Unknown option '{s}'\n", .{arg});
            return error.InvalidOption;
        }

        // Input file
        if (opts.input_file == null) {
            opts.input_file = arg;
        } else {
            const stderr = std.io.getStdErr().writer();
            try stderr.print("Error: Multiple input files not supported\n", .{});
            return error.InvalidOption;
        }
    }

    return opts;
}

fn deriveOutputPaths(opts: CompilerOptions, input_file: []const u8, allocator: Allocator) !CompilerOptions {
    var result = opts;

    // Get the base name without extension
    const basename = std.fs.path.stem(input_file);

    // Derive C output path
    if (opts.output_file) |out| {
        if (opts.compile_to_c_only) {
            result.c_output_path = try allocator.dupe(u8, out);
        } else {
            result.exe_output_path = try allocator.dupe(u8, out);
            const c_path = try std.fmt.allocPrint(allocator, "{s}.c", .{basename});
            result.c_output_path = c_path;
        }
    } else {
        // Default output names
        const c_path = try std.fmt.allocPrint(allocator, "{s}.c", .{basename});
        result.c_output_path = c_path;

        if (!opts.compile_to_c_only) {
            const exe_path = try allocator.dupe(u8, basename);
            result.exe_output_path = exe_path;
        }
    }

    return result;
}

// ============================================================================
// Command Execution
// ============================================================================

fn executeCommand(opts: CompilerOptions, allocator: Allocator) !ExitCode {
    return switch (opts.command) {
        .compile => try compileFile(opts, allocator),
        .build => try compileFile(opts, allocator),
        .run => try runFile(opts, allocator),
        .lex => try lexFile(opts, allocator),
        .parse => try parseFile(opts, allocator),
        .check => try checkFile(opts, allocator),
        .fmt => try formatFile(opts, allocator),
        .test_run => try testFile(opts, allocator),
        .pkg => unreachable, // handled in main() before executeCommand
    };
}

// ============================================================================
// LEX Command
// ============================================================================

fn lexFile(opts: CompilerOptions, allocator: Allocator) !ExitCode {
    const stdout = std.io.getStdOut().writer();
    const path = opts.input_file.?;
    const colors = if (opts.no_color) ColorConfig.never() else ColorConfig.detect();

    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ path, err });
        return .error_compile;
    };
    defer allocator.free(source);

    // Tokenize
    var lexer = Lexer.init(source, allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer allocator.free(tokens);

    // Print header
    try stdout.print("{s}Tokens from '{s}':{s}\n", .{
        colors.bold(),
        path,
        colors.reset(),
    });
    try stdout.print("{s}============================================================{s}\n", .{ colors.dim(), colors.reset() });

    // Print tokens
    for (tokens) |token| {
        const type_name = @tagName(token.type);
        const truncated_lexeme = if (token.lexeme.len > 40) token.lexeme[0..40] else token.lexeme;

        try stdout.print("{s}{d:>4}{s}:{s}{d:<4}{s} {s}{s:<16}{s} '{s}'\n", .{
            colors.dim(),
            token.location.line,
            colors.reset(),
            colors.dim(),
            token.location.column,
            colors.reset(),
            colors.cyan(),
            type_name,
            colors.reset(),
            truncated_lexeme,
        });
    }

    try stdout.print("{s}============================================================{s}\n", .{ colors.dim(), colors.reset() });
    try stdout.print("Total: {s}{d}{s} tokens\n", .{ colors.bold(), tokens.len, colors.reset() });

    // Report any errors
    if (lexer.hasErrors()) {
        try stdout.print("\n{s}Lexer Errors:{s}\n", .{ colors.error_style(), colors.reset() });
        for (lexer.getErrors()) |err| {
            try stdout.print("  Line {d}, Col {d}: {s}\n", .{
                err.location.line,
                err.location.column,
                err.message,
            });
            if (err.source_line) |line| {
                try stdout.print("    {s}|{s} {s}\n", .{ colors.cyan(), colors.reset(), line });
            }
        }
        return .error_compile;
    }

    return .success;
}

// ============================================================================
// PARSE Command
// ============================================================================

fn parseFile(opts: CompilerOptions, allocator: Allocator) !ExitCode {
    const stdout = std.io.getStdOut().writer();
    const path = opts.input_file.?;
    const colors = if (opts.no_color) ColorConfig.never() else ColorConfig.detect();

    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ path, err });
        return .error_compile;
    };
    defer allocator.free(source);

    // Tokenize
    var lexer = Lexer.init(source, allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer allocator.free(tokens);

    if (lexer.hasErrors()) {
        try displayLexerErrors(&lexer, source, path, colors, stdout);
        return .error_compile;
    }

    // Parse
    var parser = Parser.init(tokens, allocator);
    defer parser.deinit();

    const ast_result = parser.parse() catch |err| {
        try stdout.print("{s}Parse error:{s} {}\n", .{ colors.error_style(), colors.reset(), err });
        return .error_compile;
    };

    if (parser.hasErrors()) {
        try displayParseErrors(&parser, source, path, colors, stdout);
        return .error_compile;
    }

    // Print AST summary
    try stdout.print("{s}AST for '{s}':{s}\n", .{ colors.bold(), path, colors.reset() });
    try stdout.print("{s}============================================================{s}\n", .{ colors.dim(), colors.reset() });

    try printAstSummary(ast_result, colors, stdout);

    try stdout.print("{s}============================================================{s}\n", .{ colors.dim(), colors.reset() });
    try stdout.print("{s}{s}Parse successful!{s}\n", .{ colors.bold(), colors.cyan(), colors.reset() });

    return .success;
}

fn printAstSummary(source_file: *SourceFile, colors: ColorConfig, writer: anytype) !void {
    // Print imports
    if (source_file.imports.len > 0) {
        try writer.print("\n{s}Imports:{s} ({d})\n", .{ colors.bold(), colors.reset(), source_file.imports.len });
        for (source_file.imports) |imp| {
            try writer.print("  - ", .{});
            for (imp.path.segments, 0..) |seg, i| {
                if (i > 0) try writer.print("::", .{});
                try writer.print("{s}", .{seg.name});
            }
            try writer.print("\n", .{});
        }
    }

    // Print declarations
    try writer.print("\n{s}Declarations:{s} ({d})\n", .{ colors.bold(), colors.reset(), source_file.declarations.len });

    for (source_file.declarations) |decl| {
        const visibility = if (decl.visibility == .private) "private " else "";

        switch (decl.kind) {
            .function => |func| {
                try writer.print("  {s}fn{s} {s}{s}{s}", .{
                    colors.cyan(),
                    colors.reset(),
                    visibility,
                    colors.bold(),
                    func.name.name,
                });
                try writer.print("{s}(", .{colors.reset()});

                // Print parameters
                for (func.params, 0..) |param, i| {
                    if (i > 0) try writer.print(", ", .{});
                    // type_expr is required for function parameters, so always print type marker
                    _ = param.type_expr; // Acknowledge the required type_expr field
                    try writer.print("{s}: ...", .{param.name.name});
                }

                try writer.print(")", .{});

                if (func.return_type) |_| {
                    try writer.print(" -> ...", .{});
                }

                if (func.effects) |effs| {
                    try writer.print(" with [{d} effects]", .{effs.len});
                }

                try writer.print("\n", .{});
            },
            .struct_def => |s| {
                try writer.print("  {s}struct{s} {s}{s}{s} ({d} fields)\n", .{
                    colors.cyan(),
                    colors.reset(),
                    visibility,
                    colors.bold(),
                    s.name.name,
                    s.fields.len,
                });
            },
            .enum_def => |e| {
                try writer.print("  {s}enum{s} {s}{s}{s} ({d} variants)\n", .{
                    colors.cyan(),
                    colors.reset(),
                    visibility,
                    colors.bold(),
                    e.name.name,
                    e.variants.len,
                });
            },
            .trait_def => |t| {
                try writer.print("  {s}trait{s} {s}{s}{s} ({d} items)\n", .{
                    colors.cyan(),
                    colors.reset(),
                    visibility,
                    colors.bold(),
                    t.name.name,
                    t.items.len,
                });
            },
            .impl_block => |impl| {
                try writer.print("  {s}impl{s} ", .{ colors.cyan(), colors.reset() });
                if (impl.trait_type) |_| {
                    try writer.print("... for ", .{});
                }
                try writer.print("... ({d} items)\n", .{impl.items.len});
            },
            .constant => |c| {
                try writer.print("  {s}const{s} {s}{s}{s}\n", .{
                    colors.cyan(),
                    colors.reset(),
                    visibility,
                    colors.bold(),
                    c.name.name,
                });
            },
        }
    }
}

// ============================================================================
// CHECK Command
// ============================================================================

fn checkFile(opts: CompilerOptions, allocator: Allocator) !ExitCode {
    const stdout = std.io.getStdOut().writer();
    const path = opts.input_file.?;
    const colors = if (opts.no_color) ColorConfig.never() else ColorConfig.detect();

    // Phase timers
    var total_timer = Timer.init(opts.verbose);

    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ path, err });
        return .error_compile;
    };
    defer allocator.free(source);

    if (opts.verbose) {
        try stdout.print("{s}Type checking '{s}'...{s}\n", .{ colors.bold(), path, colors.reset() });
    }

    // Phase 1: Lexing
    var lex_timer = Timer.init(opts.verbose);
    var lexer = Lexer.init(source, allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer allocator.free(tokens);

    if (opts.verbose) {
        try stdout.print("  [1/3] Lexing... {d} tokens", .{tokens.len});
        try lex_timer.report(stdout, "lex");
    }

    if (lexer.hasErrors()) {
        try displayLexerErrors(&lexer, source, path, colors, stdout);
        return .error_compile;
    }

    // Phase 2: Parsing
    var parse_timer = Timer.init(opts.verbose);
    var parser = Parser.init(tokens, allocator);
    defer parser.deinit();

    const ast_result = parser.parse() catch |err| {
        try stdout.print("{s}Parse error:{s} {}\n", .{ colors.error_style(), colors.reset(), err });
        return .error_compile;
    };

    if (opts.verbose) {
        try stdout.print("  [2/3] Parsing... {d} declarations", .{ast_result.declarations.len});
        try parse_timer.report(stdout, "parse");
    }

    if (parser.hasErrors()) {
        try displayParseErrors(&parser, source, path, colors, stdout);
        return .error_compile;
    }

    // Phase 3: Type Checking
    var check_timer = Timer.init(opts.verbose);
    if (opts.verbose) {
        try stdout.print("  [3/3] Type checking...", .{});
    }

    var type_checker = TypeChecker.init(allocator) catch |err| {
        try stdout.print("{s}Error initializing type checker:{s} {}\n", .{
            colors.error_style(),
            colors.reset(),
            err,
        });
        return .error_compile;
    };
    defer type_checker.deinit();

    // Set source for error messages
    type_checker.setSource(source);
    type_checker.setSourceFile(path);

    // Run type checking on the AST
    type_checker.checkSourceFile(ast_result) catch |err| {
        try stdout.print("{s}Type check error:{s} {}\n", .{
            colors.error_style(),
            colors.reset(),
            err,
        });
        return .error_compile;
    };

    if (opts.verbose) {
        try check_timer.report(stdout, "check");
    }

    // Check for type errors
    if (type_checker.hasErrors()) {
        try displayTypeErrors(&type_checker, colors, stdout);
        return .error_compile;
    }

    if (opts.verbose) {
        try stdout.print("\n{s}Total time: {d}ms{s}\n", .{ colors.dim(), total_timer.elapsedMs(), colors.reset() });
    }

    try stdout.print("{s}{s}Type check passed!{s} No errors found.\n", .{
        colors.bold(),
        colors.cyan(),
        colors.reset(),
    });

    return .success;
}

// ============================================================================
// FMT Command
// ============================================================================

fn formatFile(opts: CompilerOptions, allocator: Allocator) !ExitCode {
    const stdout = std.io.getStdOut().writer();
    const path = opts.input_file.?;
    const colors = if (opts.no_color) ColorConfig.never() else ColorConfig.detect();

    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, path, 1024 * 1024) catch |err| {
        try stdout.print("{s}Error:{s} Cannot read file '{s}': {}\n", .{
            colors.error_style(), colors.reset(), path, err,
        });
        return .error_compile;
    };
    defer allocator.free(source);

    // Format the source
    const formatted = formatSource(source, allocator) catch |err| {
        try stdout.print("{s}Error:{s} Formatting failed: {}\n", .{
            colors.error_style(), colors.reset(), err,
        });
        return .error_compile;
    };
    defer allocator.free(formatted);

    // If output file specified, write there; otherwise write to stdout
    if (opts.output_file) |out_path| {
        const file = try std.fs.cwd().createFile(out_path, .{});
        defer file.close();
        try file.writeAll(formatted);
        try stdout.print("Formatted: {s}\n", .{out_path});
    } else {
        // Write back to the original file
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();
        try file.writeAll(formatted);
        try stdout.print("Formatted: {s}\n", .{path});
    }

    return .success;
}

/// Format dAImond source code with consistent indentation and spacing.
/// Uses a line-by-line approach: normalizes indentation based on braces.
fn formatSource(source: []const u8, allocator: Allocator) ![]const u8 {
    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    var indent_level: usize = 0;
    const indent_str = "    "; // 4 spaces

    var lines = std.mem.splitSequence(u8, source, "\n");
    var first_line = true;
    while (lines.next()) |raw_line| {
        if (!first_line) try result.append('\n');
        first_line = false;

        // Trim leading/trailing whitespace from the line
        const trimmed = std.mem.trim(u8, raw_line, " \t\r");

        // Skip empty lines (preserve them as blank lines)
        if (trimmed.len == 0) {
            continue;
        }

        // Check if line starts with a closing brace — dedent before writing
        if (trimmed[0] == '}' and indent_level > 0) {
            indent_level -= 1;
        }

        // Write indentation
        for (0..indent_level) |_| {
            try result.appendSlice(indent_str);
        }

        // Write the trimmed line content
        try result.appendSlice(trimmed);

        // Count braces to adjust indent level for subsequent lines
        var in_string = false;
        var in_comment = false;
        var prev_char: u8 = 0;
        for (trimmed) |ch| {
            if (in_comment) break;
            if (ch == '"' and prev_char != '\\') {
                in_string = !in_string;
            }
            if (!in_string) {
                if (ch == '-' and prev_char == '-') {
                    in_comment = true;
                    continue;
                }
                if (ch == '{') {
                    indent_level += 1;
                } else if (ch == '}' and trimmed[0] != '}') {
                    // Closing brace not at start of line (inline)
                    if (indent_level > 0) indent_level -= 1;
                }
            }
            prev_char = ch;
        }
    }

    // Ensure file ends with newline
    if (result.items.len > 0 and result.items[result.items.len - 1] != '\n') {
        try result.append('\n');
    }

    return try result.toOwnedSlice();
}

// ============================================================================
// TEST Command
// ============================================================================

fn testFile(opts: CompilerOptions, allocator: Allocator) !ExitCode {
    const stdout = std.io.getStdOut().writer();
    const path = opts.input_file.?;
    const colors = if (opts.no_color) ColorConfig.never() else ColorConfig.detect();

    // Read source
    const source = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch |err| {
        try stdout.print("{s}Error:{s} Cannot read '{s}': {}\n", .{
            colors.error_style(), colors.reset(), path, err,
        });
        return .error_compile;
    };
    defer allocator.free(source);

    // Lex
    var lexer = Lexer.init(source, allocator);
    defer lexer.deinit();
    const tokens = try lexer.scanAll();
    defer allocator.free(tokens);
    if (lexer.hasErrors()) {
        // Display lex errors via the lexer's error infrastructure
        try stdout.print("Lex errors in {s}\n", .{path});
        return .error_compile;
    }

    // Parse
    var parser = Parser.init(tokens, allocator);
    defer parser.deinit();
    const ast_result = try parser.parse();
    if (parser.hasErrors()) {
        try displayParseErrors(&parser, source, path, colors, stdout);
        return .error_compile;
    }

    // Scan for test functions (fn test_*())
    var test_names = std.ArrayList([]const u8).init(allocator);
    defer test_names.deinit();
    for (ast_result.declarations) |decl| {
        switch (decl.kind) {
            .function => |f| {
                if (std.mem.startsWith(u8, f.name.name, "test_")) {
                    try test_names.append(f.name.name);
                }
            },
            else => {},
        }
    }

    if (test_names.items.len == 0) {
        try stdout.print("No test functions found (functions starting with 'test_').\n", .{});
        return .success;
    }

    try stdout.print("Found {d} test(s) in {s}\n\n", .{ test_names.items.len, path });

    // Type check
    var type_checker = TypeChecker.init(allocator) catch |err| {
        try stdout.print("{s}Error:{s} Type checker init failed: {}\n", .{
            colors.error_style(), colors.reset(), err,
        });
        return .error_compile;
    };
    defer type_checker.deinit();
    type_checker.setSource(source);
    type_checker.setSourceFile(path);
    type_checker.checkSourceFile(ast_result) catch {};
    if (type_checker.hasErrors()) {
        try displayTypeErrors(&type_checker, colors, stdout);
        return .error_compile;
    }

    // Generate C code
    var code_generator = CodeGenerator.init(allocator);
    defer code_generator.deinit();
    if (type_checker.getTypeContext()) |ctx| {
        code_generator.setTypeContext(ctx);
    }
    const c_code = code_generator.generate(ast_result) catch |err| {
        try stdout.print("{s}Error:{s} Code generation failed: {}\n", .{
            colors.error_style(), colors.reset(), err,
        });
        return .error_compile;
    };

    // Append test runner to generated C code
    var full_code = std.ArrayList(u8).init(allocator);
    defer full_code.deinit();

    // Replace dm_panic with a test-aware version that uses setjmp/longjmp
    // to catch panics and report them as test failures instead of aborting.
    const panic_needle = "static inline void dm_panic(const char* message) {\n    fprintf(stderr, \"PANIC: %s\\n\", message);\n    exit(1);\n}";
    const panic_replacement =
        "#include <setjmp.h>\n" ++
        "static jmp_buf __test_jmp;\n" ++
        "static int __test_active = 0;\n" ++
        "static const char* __test_fail_msg = NULL;\n" ++
        "static inline void dm_panic(const char* message) {\n" ++
        "    if (__test_active) {\n" ++
        "        __test_fail_msg = message;\n" ++
        "        longjmp(__test_jmp, 1);\n" ++
        "    }\n" ++
        "    fprintf(stderr, \"PANIC: %s\\n\", message);\n" ++
        "    exit(1);\n" ++
        "}";

    if (std.mem.indexOf(u8, c_code, panic_needle)) |panic_pos| {
        try full_code.appendSlice(c_code[0..panic_pos]);
        try full_code.appendSlice(panic_replacement);
        try full_code.appendSlice(c_code[panic_pos + panic_needle.len ..]);
    } else {
        // Fallback: just use the original code
        try full_code.appendSlice(c_code);
    }

    // Now replace main() with our test runner
    const main_code = full_code.items;
    if (std.mem.indexOf(u8, main_code, "int main(int argc, char** argv)")) |main_pos| {
        // Rebuild: everything before main + test runner
        var final_code = std.ArrayList(u8).init(allocator);
        defer final_code.deinit();
        try final_code.appendSlice(main_code[0..main_pos]);

        // Add test runner main
        try final_code.appendSlice("int main(int argc, char** argv) {\n");
        try final_code.appendSlice("    dm_argc = argc;\n");
        try final_code.appendSlice("    dm_argv = argv;\n");
        try final_code.appendSlice("    int __passed = 0, __failed = 0;\n");

        for (test_names.items) |test_name| {
            try final_code.appendSlice("    printf(\"  test ");
            try final_code.appendSlice(test_name);
            try final_code.appendSlice(" ... \");\n");
            try final_code.appendSlice("    fflush(stdout);\n");
            try final_code.appendSlice("    __test_active = 1;\n");
            try final_code.appendSlice("    __test_fail_msg = NULL;\n");
            try final_code.appendSlice("    if (setjmp(__test_jmp) == 0) {\n");
            try final_code.appendSlice("        dm_");
            try final_code.appendSlice(test_name);
            try final_code.appendSlice("();\n");
            try final_code.appendSlice("        __test_active = 0;\n");
            try final_code.appendSlice("        printf(\"PASS\\n\"); __passed++;\n");
            try final_code.appendSlice("    } else {\n");
            try final_code.appendSlice("        __test_active = 0;\n");
            try final_code.appendSlice("        if (__test_fail_msg) printf(\"FAIL: %s\\n\", __test_fail_msg);\n");
            try final_code.appendSlice("        else printf(\"FAIL\\n\");\n");
            try final_code.appendSlice("        __failed++;\n");
            try final_code.appendSlice("    }\n");
        }

        try final_code.appendSlice("    printf(\"\\n%d passed, %d failed\\n\", __passed, __failed);\n");
        try final_code.appendSlice("    return __failed > 0 ? 1 : 0;\n");
        try final_code.appendSlice("}\n");

        // Replace full_code contents with final_code
        full_code.clearRetainingCapacity();
        try full_code.appendSlice(final_code.items);
    }

    // Write C file to temp location
    const c_path = try std.fmt.allocPrint(allocator, "/tmp/daimond_test_{d}.c", .{std.time.milliTimestamp()});
    defer allocator.free(c_path);
    const bin_path = try std.fmt.allocPrint(allocator, "/tmp/daimond_test_{d}", .{std.time.milliTimestamp()});
    defer allocator.free(bin_path);

    {
        const file = try std.fs.cwd().createFile(c_path, .{});
        defer file.close();
        try file.writeAll(full_code.items);
    }
    defer std.fs.cwd().deleteFile(c_path) catch {};
    defer std.fs.cwd().deleteFile(bin_path) catch {};

    // Compile C code
    const cc = findCCompiler() orelse {
        try stdout.print("{s}Error:{s} No C compiler found\n", .{ colors.error_style(), colors.reset() });
        return .error_compile;
    };

    {
        var child = std.process.Child.init(&[_][]const u8{ cc, "-o", bin_path, c_path, "-lm", "-lpthread" }, allocator);
        child.stderr_behavior = .Pipe;
        child.stdout_behavior = .Pipe;
        child.spawn() catch |err| {
            try stdout.print("{s}Error:{s} Cannot spawn C compiler: {}\n", .{
                colors.error_style(), colors.reset(), err,
            });
            return .error_compile;
        };
        const cc_wait = child.wait() catch {
            return .error_compile;
        };
        switch (cc_wait) {
            .Exited => |code| {
                if (code != 0) {
                    try stdout.print("{s}Error:{s} C compilation failed (exit code {d})\n", .{
                        colors.error_style(), colors.reset(), code,
                    });
                    return .error_compile;
                }
            },
            else => return .error_compile,
        }
    }

    // Run tests
    {
        var child = std.process.Child.init(&[_][]const u8{bin_path}, allocator);
        child.stderr_behavior = .Pipe;
        child.stdout_behavior = .Pipe;
        child.spawn() catch |err| {
            try stdout.print("{s}Error:{s} Cannot spawn test binary: {}\n", .{
                colors.error_style(), colors.reset(), err,
            });
            return .error_compile;
        };

        // Read stdout/stderr
        const child_stdout = child.stdout.?.reader().readAllAlloc(allocator, 1024 * 1024) catch "";
        defer allocator.free(child_stdout);
        const child_stderr = child.stderr.?.reader().readAllAlloc(allocator, 1024 * 1024) catch "";
        defer allocator.free(child_stderr);

        const wait_result = child.wait() catch {
            return .error_runtime;
        };

        try stdout.writeAll(child_stdout);
        if (child_stderr.len > 0) {
            try stdout.writeAll(child_stderr);
        }

        switch (wait_result) {
            .Exited => |code| {
                if (code != 0) return .error_runtime;
            },
            else => return .error_runtime,
        }
    }

    return .success;
}

// ============================================================================
// PKG Command
// ============================================================================

fn executePkgCommand(args: []const []const u8, allocator: Allocator) !ExitCode {
    // Find "pkg" in args, subcommand args follow it
    var pkg_args_start: usize = 0;
    for (args, 0..) |arg, idx| {
        if (std.mem.eql(u8, arg, "pkg")) {
            pkg_args_start = idx + 1;
            break;
        }
    }
    const pkg_args = if (pkg_args_start < args.len) args[pkg_args_start..] else &[_][]const u8{};

    const parsed = package.parsePkgCommand(pkg_args);

    switch (parsed.cmd) {
        .init_pkg => {
            try package.executePkgInit(allocator);
        },
        .add => {
            if (parsed.args.len == 0) {
                const stdout = std.io.getStdOut().writer();
                try stdout.print("Usage: daimond pkg add <name> [version|--path <path>|--git <url>]\n", .{});
                return .error_usage;
            }
            const dep_name = parsed.args[0];
            const extra = if (parsed.args.len > 1) parsed.args[1..] else &[_][]const u8{};
            try package.executePkgAdd(dep_name, extra, allocator);
        },
        .list => {
            try package.executePkgList(allocator);
        },
        .help => {
            try package.printPkgHelp();
        },
    }

    return .success;
}

// ============================================================================
// C Compiler Utilities
// ============================================================================

/// Find a C compiler available on the system
fn findCCompiler() ?[]const u8 {
    // Try common C compilers in order of preference
    const compilers = [_][]const u8{ "cc", "gcc", "clang" };

    for (compilers) |compiler| {
        // Try to run the compiler with --version to check if it exists
        var child = std.process.Child.init(&.{ compiler, "--version" }, std.heap.page_allocator);
        child.stdout_behavior = .Ignore;
        child.stderr_behavior = .Ignore;

        child.spawn() catch continue;
        const result = child.wait() catch continue;

        switch (result) {
            .Exited => |code| {
                if (code == 0) return compiler;
            },
            else => {},
        }
    }

    return null;
}

/// Get the path to the dAImond runtime directory
fn getRuntimePath(allocator: Allocator) ![]const u8 {
    // Try to find runtime relative to executable
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const exe_path = std.fs.selfExePath(&buf) catch {
        // If we can't get exe path, try relative paths
        return try findRuntimeRelative(allocator);
    };

    const exe_dir = std.fs.path.dirname(exe_path) orelse ".";

    // Check ../runtime (if exe is in zig-out/bin)
    const runtime_path1 = try std.fs.path.join(allocator, &.{ exe_dir, "..", "..", "runtime" });
    if (std.fs.cwd().access(runtime_path1, .{})) |_| {
        return runtime_path1;
    } else |_| {
        allocator.free(runtime_path1);
    }

    // Check ./runtime (if exe is in stage0)
    const runtime_path2 = try std.fs.path.join(allocator, &.{ exe_dir, "runtime" });
    if (std.fs.cwd().access(runtime_path2, .{})) |_| {
        return runtime_path2;
    } else |_| {
        allocator.free(runtime_path2);
    }

    // Fall back to relative path search
    return try findRuntimeRelative(allocator);
}

/// Find runtime directory using relative paths from current directory
fn findRuntimeRelative(allocator: Allocator) ![]const u8 {
    // Common relative paths to try
    const paths = [_][]const u8{
        "runtime",
        "stage0/runtime",
        "../runtime",
        "../stage0/runtime",
    };

    for (paths) |path| {
        if (std.fs.cwd().access(path, .{})) |_| {
            return try allocator.dupe(u8, path);
        } else |_| {}
    }

    return error.RuntimeNotFound;
}

/// Find the stdlib directory relative to the compiler executable or working directory.
/// Stdlib is at `<project_root>/stdlib/`.
/// Executable is at `<project_root>/stage0/zig-out/bin/daimond`.
fn findStdlibDir(allocator: Allocator) ![]const u8 {
    // Try relative to executable first
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    if (std.fs.selfExePath(&buf)) |exe_path| {
        const exe_dir = std.fs.path.dirname(exe_path) orelse ".";

        // Check ../../../stdlib (exe is in stage0/zig-out/bin/)
        const path1 = try std.fs.path.join(allocator, &.{ exe_dir, "..", "..", "..", "stdlib" });
        if (std.fs.cwd().access(path1, .{})) |_| {
            return path1;
        } else |_| {
            allocator.free(path1);
        }

        // Check ../../stdlib (exe is in zig-out/bin/)
        const path2 = try std.fs.path.join(allocator, &.{ exe_dir, "..", "..", "stdlib" });
        if (std.fs.cwd().access(path2, .{})) |_| {
            return path2;
        } else |_| {
            allocator.free(path2);
        }
    } else |_| {}

    // Try relative paths from working directory
    const paths = [_][]const u8{
        "stdlib",
        "../stdlib",
        "stage0/../stdlib",
    };

    for (paths) |path| {
        if (std.fs.cwd().access(path, .{})) |_| {
            return try allocator.dupe(u8, path);
        } else |_| {}
    }

    return error.StdlibNotFound;
}

// ============================================================================
// Module Loader (Multi-file Import Support)
// ============================================================================

const ModuleLoader = struct {
    allocator: Allocator,
    loaded_files: std.StringHashMap(LoadedFile),
    in_progress: std.StringHashMap(void),
    all_declarations: std.ArrayList(*ast.Declaration),
    verbose: bool,

    const LoadedFile = struct {
        source: []const u8,
        source_file: *SourceFile,
    };

    pub fn init(allocator: Allocator, verbose: bool) ModuleLoader {
        return .{
            .allocator = allocator,
            .loaded_files = std.StringHashMap(LoadedFile).init(allocator),
            .in_progress = std.StringHashMap(void).init(allocator),
            .all_declarations = std.ArrayList(*ast.Declaration).init(allocator),
            .verbose = verbose,
        };
    }

    pub fn deinit(self: *ModuleLoader) void {
        // Free sources we loaded (not the entry file, which is managed externally)
        var iter = self.loaded_files.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.value_ptr.source);
        }
        self.loaded_files.deinit();
        self.in_progress.deinit();
        self.all_declarations.deinit();
    }

    /// Resolve an import path to a file system path.
    /// `import foo` -> `<dir>/foo.dm`
    /// `import foo::bar` -> `<dir>/foo/bar.dm`
    /// `import std.io` -> `<stdlib_dir>/io.dm`
    fn resolveImportPath(self: *ModuleLoader, import_decl: *const ast.ImportDecl, importing_dir: []const u8) ![]const u8 {
        const segments = import_decl.path.segments;
        if (segments.len == 0) return error.EmptyImportPath;

        // Check if this is a stdlib import (first segment is "std")
        if (segments.len >= 2 and std.mem.eql(u8, segments[0].name, "std")) {
            // Try to find stdlib directory relative to compiler executable
            if (findStdlibDir(self.allocator)) |stdlib_dir| {
                defer self.allocator.free(stdlib_dir);

                var path_parts = std.ArrayList([]const u8).init(self.allocator);
                defer path_parts.deinit();

                try path_parts.append(stdlib_dir);
                // Skip the "std" segment, use remaining segments
                for (segments[1..]) |seg| {
                    try path_parts.append(seg.name);
                }

                const joined = try std.fs.path.join(self.allocator, path_parts.items);
                defer self.allocator.free(joined);

                const full_path = try std.fmt.allocPrint(self.allocator, "{s}.dm", .{joined});

                // Verify file exists before returning
                if (std.fs.cwd().access(full_path, .{})) |_| {
                    return full_path;
                } else |_| {
                    self.allocator.free(full_path);
                    // Fall through to relative path resolution
                }
            } else |_| {
                // Can't find stdlib dir, fall through to relative path resolution
            }
        }

        // Build path: join segments with '/' and append '.dm'
        var path_parts = std.ArrayList([]const u8).init(self.allocator);
        defer path_parts.deinit();

        try path_parts.append(importing_dir);
        for (segments) |seg| {
            try path_parts.append(seg.name);
        }

        // Join all parts with path separator
        const joined = try std.fs.path.join(self.allocator, path_parts.items);
        defer self.allocator.free(joined);

        // Append .dm extension
        const full_path = try std.fmt.allocPrint(self.allocator, "{s}.dm", .{joined});
        return full_path;
    }

    /// Load and parse a single file. Returns the parsed SourceFile.
    fn loadFile(self: *ModuleLoader, file_path: []const u8) !LoadedFile {
        // Check cache first
        if (self.loaded_files.get(file_path)) |loaded| {
            return loaded;
        }

        // Read the file
        const source = std.fs.cwd().readFileAlloc(self.allocator, file_path, 10 * 1024 * 1024) catch |err| {
            const stderr = std.io.getStdErr().writer();
            try stderr.print("Error reading imported file '{s}': {}\n", .{ file_path, err });
            return error.ImportFileNotFound;
        };

        // Lex
        var lexer = Lexer.init(source, self.allocator);
        defer lexer.deinit();
        const tokens = try lexer.scanAll();
        defer self.allocator.free(tokens);

        if (lexer.hasErrors()) {
            const stderr = std.io.getStdErr().writer();
            try stderr.print("Lexer errors in imported file '{s}'\n", .{file_path});
            return error.ImportLexError;
        }

        // Parse
        var parser = Parser.init(tokens, self.allocator);
        defer parser.deinit();
        const ast_result = parser.parse() catch |err| {
            const stderr = std.io.getStdErr().writer();
            try stderr.print("Parse error in imported file '{s}': {}\n", .{ file_path, err });
            return error.ImportParseError;
        };

        if (parser.hasErrors()) {
            const stderr = std.io.getStdErr().writer();
            try stderr.print("Parse errors in imported file '{s}'\n", .{file_path});
            return error.ImportParseError;
        }

        const loaded = LoadedFile{ .source = source, .source_file = ast_result };

        // Cache by path
        const cached_path = try self.allocator.dupe(u8, file_path);
        try self.loaded_files.put(cached_path, loaded);

        return loaded;
    }

    /// Recursively process imports from a source file.
    /// Adds imported declarations to all_declarations.
    fn processImports(self: *ModuleLoader, source_file: *SourceFile, file_dir: []const u8) !void {
        for (source_file.imports) |import_decl| {
            const resolved_path = try self.resolveImportPath(import_decl, file_dir);
            defer self.allocator.free(resolved_path);

            // Check for circular imports
            if (self.in_progress.contains(resolved_path)) {
                const stderr = std.io.getStdErr().writer();
                try stderr.print("Error: Circular import detected: '{s}'\n", .{resolved_path});
                return error.CircularImport;
            }

            // Check if already loaded (skip)
            if (self.loaded_files.contains(resolved_path)) continue;

            // Mark as in-progress
            const progress_key = try self.allocator.dupe(u8, resolved_path);
            try self.in_progress.put(progress_key, {});

            if (self.verbose) {
                const stdout = std.io.getStdOut().writer();
                try stdout.print("        Loading import: {s}\n", .{resolved_path});
            }

            // Load and parse the imported file
            const loaded = try self.loadFile(resolved_path);

            // Get directory of imported file for nested imports
            const import_dir = std.fs.path.dirname(resolved_path) orelse ".";

            // Recursively process imports from the loaded file
            try self.processImports(loaded.source_file, import_dir);

            // Add declarations from imported file, filtered by import items
            try self.addDeclarations(loaded.source_file, import_decl);

            // Remove from in-progress
            _ = self.in_progress.remove(resolved_path);
        }
    }

    /// Add declarations from an imported source file, respecting selective imports.
    fn addDeclarations(self: *ModuleLoader, source_file: *SourceFile, import_decl: *const ast.ImportDecl) !void {
        if (import_decl.items) |items| {
            // Selective import: only add named declarations
            for (source_file.declarations) |decl| {
                const decl_name = getDeclName(decl) orelse continue;
                for (items) |item| {
                    if (std.mem.eql(u8, decl_name, item.name.name)) {
                        try self.all_declarations.append(decl);
                        break;
                    }
                }
            }
        } else {
            // Import all declarations
            for (source_file.declarations) |decl| {
                try self.all_declarations.append(decl);
            }
        }
    }

    /// Get the name of a declaration for filtering
    fn getDeclName(decl: *ast.Declaration) ?[]const u8 {
        return switch (decl.kind) {
            .function => |f| f.name.name,
            .struct_def => |s| s.name.name,
            .enum_def => |e| e.name.name,
            .trait_def => |t| t.name.name,
            .constant => |c| c.name.name,
            .impl_block => null, // impl blocks are always included
        };
    }

    /// Build a merged SourceFile with all imported + entry declarations.
    fn getMergedSourceFile(self: *ModuleLoader, entry_file: *SourceFile) !*SourceFile {
        // Add entry file declarations after imports
        for (entry_file.declarations) |decl| {
            try self.all_declarations.append(decl);
        }

        // Create merged source file
        const merged = try self.allocator.create(SourceFile);
        merged.* = .{
            .module_decl = entry_file.module_decl,
            .imports = &[_]*ast.ImportDecl{}, // imports are resolved
            .declarations = try self.all_declarations.toOwnedSlice(),
            .span = entry_file.span,
        };
        return merged;
    }
};

// ============================================================================
// COMPILE Command (Full Pipeline)
// ============================================================================

fn compileFile(opts: CompilerOptions, allocator: Allocator) !ExitCode {
    const stdout = std.io.getStdOut().writer();
    const path = opts.input_file.?;
    const colors = if (opts.no_color) ColorConfig.never() else ColorConfig.detect();

    // Check file extension
    if (!std.mem.endsWith(u8, path, ".dm")) {
        std.debug.print("{s}Error:{s} Expected a .dm file, got '{s}'\n", .{
            colors.error_style(),
            colors.reset(),
            path,
        });
        return .error_compile;
    }

    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ path, err });
        return .error_compile;
    };
    defer allocator.free(source);

    // Create arena for compilation data (AST, types, codegen)
    var compile_arena = std.heap.ArenaAllocator.init(allocator);
    defer compile_arena.deinit();
    const compile_allocator = compile_arena.allocator();

    var total_timer = Timer.init(opts.verbose);

    try stdout.print("{s}Compiling '{s}'...{s}\n", .{ colors.bold(), path, colors.reset() });

    // Phase 1: Lexing
    var lex_timer = Timer.init(opts.verbose);
    try stdout.print("  [1/5] Lexing...", .{});

    var lexer = Lexer.init(source, allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer allocator.free(tokens);

    if (opts.verbose) {
        try stdout.print(" {d} tokens", .{tokens.len});
    }
    try lex_timer.report(stdout, "lex");
    if (!opts.verbose) try stdout.print("\n", .{});

    if (lexer.hasErrors()) {
        try displayLexerErrors(&lexer, source, path, colors, stdout);
        return .error_compile;
    }

    // Phase 2: Parsing
    var parse_timer = Timer.init(opts.verbose);
    try stdout.print("  [2/5] Parsing...", .{});

    var parser = Parser.init(tokens, compile_allocator);
    defer parser.deinit();

    const ast_result = parser.parse() catch |err| {
        try stdout.print("\n{s}Parse error:{s} {}\n", .{ colors.error_style(), colors.reset(), err });
        return .error_compile;
    };

    if (opts.verbose) {
        try stdout.print(" {d} declarations", .{ast_result.declarations.len});
    }
    try parse_timer.report(stdout, "parse");
    if (!opts.verbose) try stdout.print("\n", .{});

    if (parser.hasErrors()) {
        try displayParseErrors(&parser, source, path, colors, stdout);
        return .error_compile;
    }

    // Phase 2.5: Module Loading (resolve imports)
    var final_ast = ast_result;
    var module_loader: ?ModuleLoader = null;
    defer if (module_loader) |*ml| ml.deinit();

    if (ast_result.imports.len > 0) {
        const input_dir = std.fs.path.dirname(path) orelse ".";
        var loader = ModuleLoader.init(compile_allocator, opts.verbose);
        try loader.processImports(ast_result, input_dir);
        final_ast = try loader.getMergedSourceFile(ast_result);
        module_loader = loader;

        if (opts.verbose) {
            try stdout.print("        Resolved {d} import(s), {d} total declarations\n", .{
                ast_result.imports.len,
                final_ast.declarations.len,
            });
        }
    }

    // Phase 3: Type Checking
    var check_timer = Timer.init(opts.verbose);
    try stdout.print("  [3/5] Type checking...", .{});

    var type_checker = TypeChecker.init(compile_allocator) catch |err| {
        try stdout.print("\n{s}Error initializing type checker:{s} {}\n", .{
            colors.error_style(),
            colors.reset(),
            err,
        });
        return .error_compile;
    };
    defer type_checker.deinit();

    // Set source for error messages
    type_checker.setSource(source);
    type_checker.setSourceFile(path);

    // Run type checking on the merged AST
    type_checker.checkSourceFile(final_ast) catch |err| {
        try stdout.print("\n{s}Type check error:{s} {}\n", .{
            colors.error_style(),
            colors.reset(),
            err,
        });
        return .error_compile;
    };

    if (opts.verbose) {
        try stdout.print(" ok", .{});
    }
    try check_timer.report(stdout, "check");
    if (!opts.verbose) try stdout.print("\n", .{});

    // Check for type errors
    if (type_checker.hasErrors()) {
        try displayTypeErrors(&type_checker, colors, stdout);
        return .error_compile;
    }

    // Phase 4: Code Generation
    var codegen_timer = Timer.init(opts.verbose);
    try stdout.print("  [4/5] Generating C code...", .{});

    var code_generator = CodeGenerator.init(compile_allocator);
    defer code_generator.deinit();

    // Wire type context from type checker to code generator
    if (type_checker.getTypeContext()) |ctx| {
        code_generator.setTypeContext(ctx);
    }

    const c_code = code_generator.generate(final_ast) catch |err| {
        try stdout.print("\n{s}Code generation error:{s} {}\n", .{
            colors.error_style(),
            colors.reset(),
            err,
        });
        return .error_compile;
    };

    // Write C code to output file
    const c_output_path = opts.c_output_path.?;
    std.fs.cwd().writeFile(.{
        .sub_path = c_output_path,
        .data = c_code,
    }) catch |err| {
        try stdout.print("\n{s}Error writing C file:{s} {}\n", .{
            colors.error_style(),
            colors.reset(),
            err,
        });
        return .error_compile;
    };

    if (opts.verbose) {
        try stdout.print(" -> {s}", .{c_output_path});
    }
    try codegen_timer.report(stdout, "codegen");
    if (!opts.verbose) try stdout.print("\n", .{});

    // If only compiling to C, stop here
    if (opts.compile_to_c_only) {
        try stdout.print("\n{s}{s}Successfully generated C code:{s} {s}\n", .{
            colors.bold(),
            colors.cyan(),
            colors.reset(),
            c_output_path,
        });
        return .success;
    }

    // Phase 5: C Compiler Invocation
    var cc_timer = Timer.init(opts.verbose);
    try stdout.print("  [5/5] Compiling C code...", .{});

    const exe_output_path = opts.exe_output_path.?;

    // Find the runtime path relative to the executable or use a default
    const runtime_path = getRuntimePath(allocator) catch null;
    defer if (runtime_path) |rp| allocator.free(rp);

    // Construct compiler arguments
    var cc_args = std.ArrayList([]const u8).init(allocator);
    defer cc_args.deinit();

    // Track allocated strings that need to be freed after compilation
    var temp_strings = std.ArrayList([]const u8).init(allocator);
    defer {
        for (temp_strings.items) |s| allocator.free(s);
        temp_strings.deinit();
    }

    // Try to find a C compiler (cc, gcc, clang)
    const cc_name = findCCompiler() orelse {
        try stdout.print("\n{s}Error:{s} No C compiler found (tried cc, gcc, clang)\n", .{
            colors.error_style(),
            colors.reset(),
        });
        return .error_compile;
    };

    try cc_args.append(cc_name);
    try cc_args.append("-o");
    try cc_args.append(exe_output_path);
    try cc_args.append(c_output_path);

    // Add runtime include path and source if available
    if (runtime_path) |rp| {
        const include_flag = try std.fmt.allocPrint(allocator, "-I{s}", .{rp});
        try cc_args.append(include_flag);
        try temp_strings.append(include_flag);

        const runtime_c = try std.fmt.allocPrint(allocator, "{s}/daimond_runtime.c", .{rp});

        // Only add runtime.c if it exists
        if (std.fs.cwd().access(runtime_c, .{})) |_| {
            try cc_args.append(runtime_c);
            try temp_strings.append(runtime_c);
        } else |_| {
            allocator.free(runtime_c);
        }
    }

    // Add optimization level
    try cc_args.append(opts.opt_level.toCcFlag());

    // Add standard flags
    try cc_args.append("-std=c11");
    try cc_args.append("-Wall");
    try cc_args.append("-Wextra");
    try cc_args.append("-pedantic");

    // Link math library (needed for extern fn wrapping math.h)
    try cc_args.append("-lm");
    // Link pthread library (needed for threading primitives)
    try cc_args.append("-lpthread");

    if (opts.verbose) {
        try stdout.print("\n        Running: ", .{});
        for (cc_args.items) |arg| {
            try stdout.print("{s} ", .{arg});
        }
        try stdout.print("\n        ", .{});
    }

    // Spawn the C compiler process
    var child = std.process.Child.init(cc_args.items, allocator);
    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Pipe;

    child.spawn() catch |err| {
        try stdout.print("\n{s}Error spawning C compiler:{s} {}\n", .{
            colors.error_style(),
            colors.reset(),
            err,
        });
        return .error_compile;
    };

    // Wait for completion and get result
    const result = child.wait() catch |err| {
        try stdout.print("\n{s}Error waiting for C compiler:{s} {}\n", .{
            colors.error_style(),
            colors.reset(),
            err,
        });
        return .error_compile;
    };

    switch (result) {
        .Exited => |code| {
            if (code != 0) {
                try stdout.print("\n{s}C compiler failed with exit code {d}{s}\n", .{
                    colors.error_style(),
                    code,
                    colors.reset(),
                });

                // Try to read and display compiler errors
                if (child.stderr) |stderr| {
                    const err_output = stderr.reader().readAllAlloc(allocator, 10 * 1024) catch "";
                    defer allocator.free(err_output);
                    if (err_output.len > 0) {
                        try stdout.print("{s}\n", .{err_output});
                    }
                }

                return .error_compile;
            }
        },
        else => {
            try stdout.print("\n{s}C compiler terminated abnormally{s}\n", .{
                colors.error_style(),
                colors.reset(),
            });
            return .error_compile;
        },
    }

    if (opts.verbose) {
        try stdout.print("-> {s}", .{exe_output_path});
    }
    try cc_timer.report(stdout, "cc");
    if (!opts.verbose) try stdout.print("\n", .{});

    // Clean up intermediate C file unless --emit-c was specified
    if (!opts.emit_c) {
        std.fs.cwd().deleteFile(c_output_path) catch {};
    }

    if (opts.verbose) {
        try stdout.print("\n{s}Total time: {d}ms{s}\n", .{ colors.dim(), total_timer.elapsedMs(), colors.reset() });
    }

    try stdout.print("\n{s}{s}Successfully compiled:{s} {s}\n", .{
        colors.bold(),
        colors.cyan(),
        colors.reset(),
        exe_output_path,
    });

    return .success;
}

// ============================================================================
// RUN Command
// ============================================================================

fn runFile(opts: CompilerOptions, allocator: Allocator) !ExitCode {
    const stdout = std.io.getStdOut().writer();
    const colors = if (opts.no_color) ColorConfig.never() else ColorConfig.detect();

    // First compile
    const compile_result = try compileFile(opts, allocator);
    if (compile_result != .success) {
        return compile_result;
    }

    // Get the executable path
    const exe_path = opts.exe_output_path orelse {
        try stdout.print("{s}Error:{s} No executable path available\n", .{
            colors.error_style(),
            colors.reset(),
        });
        return .error_compile;
    };

    try stdout.print("{s}Running:{s} {s}\n", .{ colors.bold(), colors.reset(), exe_path });
    try stdout.print("{s}============================================================{s}\n", .{ colors.dim(), colors.reset() });

    // Make the path executable-friendly (prepend ./ if needed)
    const run_path = if (std.mem.startsWith(u8, exe_path, "/") or std.mem.startsWith(u8, exe_path, "./"))
        exe_path
    else blk: {
        break :blk try std.fmt.allocPrint(allocator, "./{s}", .{exe_path});
    };
    defer if (run_path.ptr != exe_path.ptr) allocator.free(run_path);

    // Run the executable
    var child = std.process.Child.init(&.{run_path}, allocator);
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;
    child.stdin_behavior = .Inherit;

    child.spawn() catch |err| {
        try stdout.print("{s}Error running executable:{s} {}\n", .{
            colors.error_style(),
            colors.reset(),
            err,
        });
        return .error_compile;
    };

    const result = child.wait() catch |err| {
        try stdout.print("{s}Error waiting for process:{s} {}\n", .{
            colors.error_style(),
            colors.reset(),
            err,
        });
        return .error_compile;
    };

    try stdout.print("{s}============================================================{s}\n", .{ colors.dim(), colors.reset() });

    switch (result) {
        .Exited => |code| {
            if (code != 0) {
                try stdout.print("{s}Process exited with code {d}{s}\n", .{
                    colors.warning_style(),
                    code,
                    colors.reset(),
                });
            } else {
                try stdout.print("{s}Process exited successfully{s}\n", .{
                    colors.cyan(),
                    colors.reset(),
                });
            }
        },
        .Signal => |sig| {
            try stdout.print("{s}Process terminated by signal {d}{s}\n", .{
                colors.error_style(),
                sig,
                colors.reset(),
            });
            return .error_compile;
        },
        else => {
            try stdout.print("{s}Process terminated abnormally{s}\n", .{
                colors.error_style(),
                colors.reset(),
            });
            return .error_compile;
        },
    }

    return .success;
}

// ============================================================================
// Error Display
// ============================================================================

fn displayLexerErrors(lexer: *Lexer, source: []const u8, file_path: []const u8, colors: ColorConfig, writer: anytype) !void {
    try writer.print("\n{s}Lexer Errors:{s}\n", .{ colors.error_style(), colors.reset() });

    for (lexer.getErrors()) |err| {
        try writer.print("\n{s}Error{s} at {s}:{d}:{d}: {s}\n", .{
            colors.error_style(),
            colors.reset(),
            file_path,
            err.location.line,
            err.location.column,
            err.message,
        });

        // Show source context
        if (errors.extractLine(source, err.location.line)) |line| {
            try writer.print("   {s}|{s}\n", .{ colors.cyan(), colors.reset() });
            try writer.print("{s}{d:>3}{s} {s}|{s} {s}\n", .{
                colors.dim(),
                err.location.line,
                colors.reset(),
                colors.cyan(),
                colors.reset(),
                line,
            });
            try writer.print("   {s}|{s} ", .{ colors.cyan(), colors.reset() });

            // Draw caret
            const col = if (err.location.column > 0) err.location.column - 1 else 0;
            try writer.writeByteNTimes(' ', col);
            try writer.print("{s}^{s}\n", .{ colors.error_style(), colors.reset() });
        }
    }

    const error_count = lexer.getErrors().len;
    try writer.print("\n{s}aborting due to {d} lexer error(s){s}\n", .{
        colors.bold(),
        error_count,
        colors.reset(),
    });
}

fn displayParseErrors(parser: *Parser, source: []const u8, file_path: []const u8, colors: ColorConfig, writer: anytype) !void {
    try writer.print("\n{s}Parse Errors:{s}\n", .{ colors.error_style(), colors.reset() });

    for (parser.getErrors()) |err| {
        try writer.print("\n{s}Error{s} at {s}:{d}:{d}: {s}\n", .{
            colors.error_style(),
            colors.reset(),
            file_path,
            err.location.line,
            err.location.column,
            err.message,
        });

        if (err.expected) |exp| {
            try writer.print("  expected: {s}\n", .{exp});
        }
        if (err.found) |fnd| {
            try writer.print("  found: {s}\n", .{fnd});
        }

        // Show source context
        if (errors.extractLine(source, err.location.line)) |line| {
            try writer.print("   {s}|{s}\n", .{ colors.cyan(), colors.reset() });
            try writer.print("{s}{d:>3}{s} {s}|{s} {s}\n", .{
                colors.dim(),
                err.location.line,
                colors.reset(),
                colors.cyan(),
                colors.reset(),
                line,
            });
            try writer.print("   {s}|{s} ", .{ colors.cyan(), colors.reset() });

            // Draw caret
            const col = if (err.location.column > 0) err.location.column - 1 else 0;
            try writer.writeByteNTimes(' ', col);
            try writer.print("{s}^{s}\n", .{ colors.error_style(), colors.reset() });
        }
    }

    const error_count = parser.getErrors().len;
    try writer.print("\n{s}aborting due to {d} parse error(s){s}\n", .{
        colors.bold(),
        error_count,
        colors.reset(),
    });
}

fn displayTypeErrors(type_checker: *TypeChecker, colors: ColorConfig, writer: anytype) !void {
    try writer.print("\n{s}Type Errors:{s}\n", .{ colors.error_style(), colors.reset() });

    const diagnostics = type_checker.getDiagnostics();
    for (diagnostics) |diagnostic| {
        try diagnostic.render(writer, colors);
    }

    var error_count: usize = 0;
    for (diagnostics) |d| {
        if (d.severity == .@"error") {
            error_count += 1;
        }
    }

    try writer.print("\n{s}aborting due to {d} type error(s){s}\n", .{
        colors.bold(),
        error_count,
        colors.reset(),
    });
}

// ============================================================================
// Help and Version
// ============================================================================

fn printUsage() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print(
        \\{s}dAImond Compiler v{s}{s}
        \\
        \\{s}Usage:{s} daimond [command] [options] <file.dm>
        \\
        \\{s}Commands:{s}
        \\  <file.dm>           Compile a dAImond source file to executable
        \\  build <file.dm>     Compile to C code only
        \\  run <file.dm>       Compile and run immediately
        \\  lex <file.dm>       Tokenize and display tokens (debugging)
        \\  parse <file.dm>     Parse and display AST structure (debugging)
        \\  check <file.dm>     Type check only, don't generate code
        \\  fmt <file.dm>       Format source code (not yet implemented)
        \\
        \\{s}Options:{s}
        \\  -o <output>         Specify output file name
        \\  -c                  Compile to C only, don't invoke C compiler
        \\  --emit-c            Keep generated C file after compilation
        \\  --no-color          Disable colored output
        \\  -O0/-O1/-O2/-O3     Optimization level (default: -O0)
        \\  --verbose           Show detailed compilation progress
        \\  -h, --help          Show this help message
        \\  -v, --version       Show version information
        \\
        \\{s}Examples:{s}
        \\  daimond hello.dm                Compile hello.dm to executable
        \\  daimond build hello.dm          Compile hello.dm to hello.c
        \\  daimond run hello.dm            Compile and run hello.dm
        \\  daimond -O2 -o myapp hello.dm   Compile with -O2 to 'myapp'
        \\  daimond lex hello.dm            Show tokens in hello.dm
        \\  daimond parse hello.dm          Show AST for hello.dm
        \\  daimond check hello.dm          Type check hello.dm
        \\
        \\{s}Pipeline:{s}
        \\  Source -> Lexer -> Tokens -> Parser -> AST -> Checker -> Codegen -> CC -> Binary
        \\
    , .{
        "\x1b[1m",       version, "\x1b[0m",
        "\x1b[1m",       "\x1b[0m",
        "\x1b[1m",       "\x1b[0m",
        "\x1b[1m",       "\x1b[0m",
        "\x1b[1m",       "\x1b[0m",
        "\x1b[1m\x1b[2m", "\x1b[0m",
    });
}

fn printVersion() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print(
        \\dAImond Compiler v{s} (Stage 0 - Zig Bootstrap)
        \\
        \\Target: C code generation (then native via CC)
        \\Build:  Zig {s}
        \\
    , .{ version, @import("builtin").zig_version_string });
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "parseArgs - help flag" {
    const args = [_][]const u8{ "daimond", "--help" };
    const opts = try parseArgs(&args);
    try testing.expect(opts.show_help);
}

test "parseArgs - version flag" {
    const args = [_][]const u8{ "daimond", "-v" };
    const opts = try parseArgs(&args);
    try testing.expect(opts.show_version);
}

test "parseArgs - lex command" {
    const args = [_][]const u8{ "daimond", "lex", "test.dm" };
    const opts = try parseArgs(&args);
    try testing.expectEqual(Command.lex, opts.command);
    try testing.expectEqualStrings("test.dm", opts.input_file.?);
}

test "parseArgs - build command" {
    const args = [_][]const u8{ "daimond", "build", "test.dm" };
    const opts = try parseArgs(&args);
    try testing.expectEqual(Command.build, opts.command);
    try testing.expect(opts.compile_to_c_only);
}

test "parseArgs - optimization levels" {
    const args = [_][]const u8{ "daimond", "-O2", "test.dm" };
    const opts = try parseArgs(&args);
    try testing.expectEqual(OptLevel.O2, opts.opt_level);
}

test "parseArgs - output file" {
    const args = [_][]const u8{ "daimond", "-o", "output", "test.dm" };
    const opts = try parseArgs(&args);
    try testing.expectEqualStrings("output", opts.output_file.?);
    try testing.expectEqualStrings("test.dm", opts.input_file.?);
}

test "parseArgs - multiple flags" {
    const args = [_][]const u8{ "daimond", "--verbose", "--no-color", "-O3", "test.dm" };
    const opts = try parseArgs(&args);
    try testing.expect(opts.verbose);
    try testing.expect(opts.no_color);
    try testing.expectEqual(OptLevel.O3, opts.opt_level);
}

test "OptLevel to CC flag" {
    try testing.expectEqualStrings("-O0", OptLevel.O0.toCcFlag());
    try testing.expectEqualStrings("-O1", OptLevel.O1.toCcFlag());
    try testing.expectEqualStrings("-O2", OptLevel.O2.toCcFlag());
    try testing.expectEqualStrings("-O3", OptLevel.O3.toCcFlag());
}
