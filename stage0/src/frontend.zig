//! dAImond Frontend Module
//!
//! Re-exports the frontend pipeline (lexer, parser, checker) for use
//! by downstream compilation stages (e.g., Stage 3 LLVM backend).

pub const ast = @import("ast.zig");
pub const types = @import("types.zig");
pub const lexer_mod = @import("lexer.zig");
pub const parser_mod = @import("parser.zig");
pub const checker_mod = @import("checker.zig");
pub const errors = @import("errors.zig");

// Re-export key types for convenience
pub const Lexer = lexer_mod.Lexer;
pub const Token = lexer_mod.Token;
pub const TokenType = lexer_mod.TokenType;
pub const Parser = parser_mod.Parser;
pub const TypeChecker = checker_mod.TypeChecker;
pub const SourceFile = ast.SourceFile;
pub const DiagnosticBag = errors.DiagnosticBag;
pub const ColorConfig = errors.ColorConfig;
