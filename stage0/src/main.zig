//! dAImond Compiler - Stage 0
//!
//! This is the bootstrap compiler for the dAImond programming language,
//! written in Zig. It compiles dAImond source code to C.

const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

const version = "0.1.0";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const command = args[1];

    if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        try printUsage();
        return;
    }

    if (std.mem.eql(u8, command, "--version") or std.mem.eql(u8, command, "-v")) {
        try printVersion();
        return;
    }

    if (std.mem.eql(u8, command, "lex")) {
        if (args.len < 3) {
            std.debug.print("Error: 'lex' command requires a file path\n", .{});
            return;
        }
        try lexFile(args[2], allocator);
        return;
    }

    // Default: treat argument as source file to compile
    try compileFile(command, allocator);
}

fn printUsage() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print(
        \\dAImond Compiler v{s}
        \\
        \\Usage: daimond [command] [options] <file.dm>
        \\
        \\Commands:
        \\  <file.dm>      Compile a dAImond source file
        \\  lex <file>     Tokenize a file and print tokens (debugging)
        \\
        \\Options:
        \\  -h, --help     Show this help message
        \\  -v, --version  Show version information
        \\
        \\Examples:
        \\  daimond examples/calculator.dm    Compile calculator.dm
        \\  daimond lex examples/hello.dm     Show tokens in hello.dm
        \\
    , .{version});
}

fn printVersion() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("dAImond Compiler v{s} (Stage 0 - Zig)\n", .{version});
}

fn lexFile(path: []const u8, allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ path, err });
        return;
    };
    defer allocator.free(source);

    // Tokenize
    var lexer = Lexer.init(source, allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer allocator.free(tokens);

    // Print tokens
    try stdout.print("Tokens from '{s}':\n", .{path});
    try stdout.print("{'=':[<60]}\n", .{""});

    for (tokens) |token| {
        try stdout.print("{d:>4}:{d:<4} {s:<16} '{s}'\n", .{
            token.location.line,
            token.location.column,
            @tagName(token.type),
            if (token.lexeme.len > 40) token.lexeme[0..40] else token.lexeme,
        });
    }

    try stdout.print("{'=':[<60]}\n", .{""});
    try stdout.print("Total: {} tokens\n", .{tokens.len});

    // Report any errors
    if (lexer.hasErrors()) {
        try stdout.print("\nLexer Errors:\n", .{});
        for (lexer.getErrors()) |err| {
            try stdout.print("  Line {}, Col {}: {s}\n", .{
                err.location.line,
                err.location.column,
                err.message,
            });
        }
    }
}

fn compileFile(path: []const u8, allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    // Check file extension
    if (!std.mem.endsWith(u8, path, ".dm")) {
        std.debug.print("Error: Expected a .dm file, got '{s}'\n", .{path});
        return;
    }

    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ path, err });
        return;
    };
    defer allocator.free(source);

    try stdout.print("Compiling '{s}'...\n", .{path});

    // Phase 1: Lexing
    try stdout.print("  [1/4] Lexing...\n", .{});
    var lexer = Lexer.init(source, allocator);
    defer lexer.deinit();

    const tokens = try lexer.scanAll();
    defer allocator.free(tokens);

    if (lexer.hasErrors()) {
        try stdout.print("\nLexer errors:\n", .{});
        for (lexer.getErrors()) |err| {
            try stdout.print("  Line {}, Col {}: {s}\n", .{
                err.location.line,
                err.location.column,
                err.message,
            });
        }
        return;
    }

    try stdout.print("        Found {} tokens\n", .{tokens.len});

    // Phase 2: Parsing (TODO)
    try stdout.print("  [2/4] Parsing... (not yet implemented)\n", .{});

    // Phase 3: Type Checking (TODO)
    try stdout.print("  [3/4] Type checking... (not yet implemented)\n", .{});

    // Phase 4: Code Generation (TODO)
    try stdout.print("  [4/4] Code generation... (not yet implemented)\n", .{});

    try stdout.print("\nNote: Only lexing is currently implemented.\n", .{});
    try stdout.print("      Parser, type checker, and codegen coming soon!\n", .{});
}
