//! dAImond Parser
//!
//! This module implements a recursive descent parser with Pratt parsing for
//! expression precedence. It converts a stream of tokens into an Abstract
//! Syntax Tree (AST) for the dAImond programming language.

const std = @import("std");
const Allocator = std.mem.Allocator;
const lexer_mod = @import("lexer.zig");
const Lexer = lexer_mod.Lexer;
const Token = lexer_mod.Token;
const TokenType = lexer_mod.TokenType;
const SourceLocation = lexer_mod.SourceLocation;
const ast = @import("ast.zig");

// Re-export AST types for convenience
pub const Span = ast.Span;
pub const Identifier = ast.Identifier;
pub const Path = ast.Path;
pub const SourceFile = ast.SourceFile;
pub const Declaration = ast.Declaration;
pub const FunctionDecl = ast.FunctionDecl;
pub const FunctionParam = ast.FunctionParam;
pub const GenericParam = ast.GenericParam;
pub const StructDecl = ast.StructDecl;
pub const StructField = ast.StructField;
pub const EnumDecl = ast.EnumDecl;
pub const EnumVariant = ast.EnumVariant;
pub const TraitDecl = ast.TraitDecl;
pub const TraitItem = ast.TraitItem;
pub const ImplBlock = ast.ImplBlock;
pub const ImplItem = ast.ImplItem;
pub const ConstDecl = ast.ConstDecl;
pub const Statement = ast.Statement;
pub const LetBinding = ast.LetBinding;
pub const Assignment = ast.Assignment;
pub const ReturnStmt = ast.ReturnStmt;
pub const ForLoop = ast.ForLoop;
pub const WhileLoop = ast.WhileLoop;
pub const LoopStmt = ast.LoopStmt;
pub const BreakStmt = ast.BreakStmt;
pub const ContinueStmt = ast.ContinueStmt;
pub const DiscardStmt = ast.DiscardStmt;
pub const Expr = ast.Expr;
pub const Literal = ast.Literal;
pub const BinaryExpr = ast.BinaryExpr;
pub const UnaryExpr = ast.UnaryExpr;
pub const FieldAccess = ast.FieldAccess;
pub const IndexAccess = ast.IndexAccess;
pub const MethodCall = ast.MethodCall;
pub const FunctionCall = ast.FunctionCall;
pub const StructLiteral = ast.StructLiteral;
pub const ArrayLiteral = ast.ArrayLiteral;
pub const TupleLiteral = ast.TupleLiteral;
pub const IfExpr = ast.IfExpr;
pub const MatchExpr = ast.MatchExpr;
pub const MatchArm = ast.MatchArm;
pub const BlockExpr = ast.BlockExpr;
pub const LambdaExpr = ast.LambdaExpr;
pub const LambdaParam = ast.LambdaParam;
pub const PipelineExpr = ast.PipelineExpr;
pub const ErrorPropagateExpr = ast.ErrorPropagateExpr;
pub const CoalesceExpr = ast.CoalesceExpr;
pub const RangeExpr = ast.RangeExpr;
pub const ComptimeExpr = ast.ComptimeExpr;
pub const Pattern = ast.Pattern;
pub const TuplePattern = ast.TuplePattern;
pub const StructPattern = ast.StructPattern;
pub const FieldPattern = ast.FieldPattern;
pub const EnumVariantPattern = ast.EnumVariantPattern;
pub const SlicePattern = ast.SlicePattern;
pub const OrPattern = ast.OrPattern;
pub const TypeExpr = ast.TypeExpr;
pub const NamedType = ast.NamedType;
pub const FunctionType = ast.FunctionType;
pub const ArrayType = ast.ArrayType;
pub const SliceType = ast.SliceType;
pub const ReferenceType = ast.ReferenceType;
pub const TupleType = ast.TupleType;
pub const OptionType = ast.OptionType;
pub const ImportDecl = ast.ImportDecl;
pub const ModuleDecl = ast.ModuleDecl;

// ============================================================================
// Parser Error Types
// ============================================================================

/// Parse error information
pub const ParseError = struct {
    message: []const u8,
    location: SourceLocation,
    expected: ?[]const u8,
    found: ?[]const u8,

    pub fn format(
        self: ParseError,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("Parse error at line {}, column {}: {s}", .{
            self.location.line,
            self.location.column,
            self.message,
        });
        if (self.expected) |exp| {
            try writer.print("\n  expected: {s}", .{exp});
        }
        if (self.found) |fnd| {
            try writer.print("\n  found: {s}", .{fnd});
        }
    }
};

// ============================================================================
// Helper: Convert SourceLocation to Span.Position
// ============================================================================

fn toPosition(loc: SourceLocation) Span.Position {
    return .{
        .line = loc.line,
        .column = loc.column,
        .offset = loc.offset,
    };
}

fn makeSpan(start: SourceLocation, end: SourceLocation) Span {
    return .{
        .start = toPosition(start),
        .end = toPosition(end),
    };
}

fn makeIdentifier(name: []const u8, loc: SourceLocation) Identifier {
    return .{
        .name = name,
        .span = makeSpan(loc, loc),
    };
}

// ============================================================================
// Parser Implementation
// ============================================================================

/// Operator precedence levels (lowest to highest)
const Precedence = enum(u8) {
    none = 0,
    pipeline = 1, // |>
    @"or" = 2, // or
    @"and" = 3, // and
    equality = 4, // == !=
    comparison = 5, // < > <= >=
    bit_or = 6, // |
    bit_xor = 7, // ^
    bit_and = 8, // &
    shift = 9, // << >>
    term = 10, // + -
    factor = 11, // * / %
    power = 12, // ^ (right associative - note: same as bit_xor in most langs)
    unary = 13, // - not !
    postfix = 14, // calls, field access, indexing, ?, ??

    fn next(self: Precedence) Precedence {
        if (@intFromEnum(self) >= @intFromEnum(Precedence.postfix)) {
            return self;
        }
        return @enumFromInt(@intFromEnum(self) + 1);
    }
};

/// The dAImond parser
pub const Parser = struct {
    tokens: []const Token,
    current: usize,
    allocator: Allocator,
    errors: std.ArrayList(ParseError),
    panic_mode: bool,

    const Self = @This();

    /// Initialize a new parser
    pub fn init(tokens: []const Token, allocator: Allocator) Self {
        return .{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
            .errors = std.ArrayList(ParseError).init(allocator),
            .panic_mode = false,
        };
    }

    /// Clean up parser resources (AST nodes use allocator, caller manages their lifetime)
    pub fn deinit(self: *Self) void {
        self.errors.deinit();
    }

    /// Get the allocator for AST node allocation (caller-provided allocator)
    fn astAllocator(self: *Self) Allocator {
        return self.allocator;
    }

    // ========================================================================
    // Entry Point
    // ========================================================================

    /// Parse the entire source into a SourceFile AST
    pub fn parse(self: *Self) !*SourceFile {
        var imports = std.ArrayList(*ImportDecl).init(self.astAllocator());
        var declarations = std.ArrayList(*Declaration).init(self.astAllocator());
        errdefer imports.deinit();
        errdefer declarations.deinit();

        const start_loc = self.currentLocation();

        // Handle optional module declaration at the start
        var module_decl: ?*ModuleDecl = null;
        if (self.match(.kw_module)) {
            module_decl = try self.parseModuleDecl();
        }

        while (!self.isAtEnd()) {
            // Handle imports first
            if (self.check(.kw_import)) {
                if (self.parseImportDecl()) |imp| {
                    try imports.append(imp);
                } else |_| {
                    self.synchronize();
                }
            } else if (self.parseDeclaration()) |decl| {
                try declarations.append(decl);
            } else |_| {
                self.synchronize();
            }
        }

        const source_file = try self.astAllocator().create(SourceFile);
        source_file.* = .{
            .module_decl = module_decl,
            .imports = try imports.toOwnedSlice(),
            .declarations = try declarations.toOwnedSlice(),
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return source_file;
    }

    /// Parse a module declaration: `module name.subpath`
    fn parseModuleDecl(self: *Self) !*ModuleDecl {
        const start_loc = self.currentLocation();

        // Parse the module path (e.g., "tests.generics" or just "hello")
        var segments = std.ArrayList(Identifier).init(self.astAllocator());
        const first = try self.expectIdentifier("module name");
        try segments.append(makeIdentifier(first, self.previousLocation()));

        // Handle dotted paths like "tests.generics"
        while (self.match(.dot)) {
            const seg = try self.expectIdentifier("module path segment");
            try segments.append(makeIdentifier(seg, self.previousLocation()));
        }

        const path = Path{
            .segments = try segments.toOwnedSlice(),
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        const module_decl = try self.astAllocator().create(ModuleDecl);
        module_decl.* = .{
            .name = path,
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return module_decl;
    }

    // ========================================================================
    // Declaration Parsing
    // ========================================================================

    /// Parse a top-level declaration
    pub fn parseDeclaration(self: *Self) !*Declaration {
        const start_loc = self.currentLocation();
        const visibility: Declaration.Visibility = if (self.match(.kw_private)) .private else .public;

        const decl = try self.astAllocator().create(Declaration);

        if (self.match(.kw_fn)) {
            decl.* = .{
                .kind = .{ .function = try self.parseFunction() },
                .visibility = visibility,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
        } else if (self.match(.kw_struct)) {
            decl.* = .{
                .kind = .{ .struct_def = try self.parseStruct() },
                .visibility = visibility,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
        } else if (self.match(.kw_enum)) {
            decl.* = .{
                .kind = .{ .enum_def = try self.parseEnum() },
                .visibility = visibility,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
        } else if (self.match(.kw_trait)) {
            decl.* = .{
                .kind = .{ .trait_def = try self.parseTrait() },
                .visibility = visibility,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
        } else if (self.match(.kw_impl)) {
            decl.* = .{
                .kind = .{ .impl_block = try self.parseImpl() },
                .visibility = visibility,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
        } else if (self.match(.kw_const)) {
            decl.* = .{
                .kind = .{ .constant = try self.parseConstDecl() },
                .visibility = visibility,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
        } else {
            return self.errorAtCurrent("Expected declaration (fn, struct, enum, trait, impl, or const)");
        }

        return decl;
    }

    /// Parse a function definition
    pub fn parseFunction(self: *Self) !*FunctionDecl {
        const start_loc = self.currentLocation();
        const is_comptime = self.match(.kw_comptime);
        const name = try self.expectIdentifier("function name");
        const name_ident = makeIdentifier(name, start_loc);

        // Generic parameters
        const generic_params = try self.parseGenericParams();

        // Parameters
        try self.expect(.lparen, "'(' after function name");
        var params = try self.parseFunctionParams();
        try self.expect(.rparen, "')' after parameters");

        // Return type
        var return_type: ?*TypeExpr = null;
        if (self.match(.arrow)) {
            return_type = try self.parseTypeExpr();
        }

        // Effects
        var effects: ?[]const *TypeExpr = null;
        if (self.match(.kw_with)) {
            try self.expect(.lbracket, "'[' after 'with'");
            var effect_list = std.ArrayList(*TypeExpr).init(self.astAllocator());
            while (!self.check(.rbracket) and !self.isAtEnd()) {
                const effect = try self.parseTypeExpr();
                try effect_list.append(effect);
                if (!self.match(.comma)) break;
            }
            try self.expect(.rbracket, "']' after effects");
            effects = try effect_list.toOwnedSlice();
        }

        // Contracts
        var contracts: ?FunctionDecl.Contracts = null;
        var requires_list = std.ArrayList(*Expr).init(self.astAllocator());
        var ensures_list = std.ArrayList(*Expr).init(self.astAllocator());

        while (self.match(.kw_requires) or self.match(.kw_ensures)) {
            const is_requires = self.previous().type == .kw_requires;
            const condition = try self.parseExpr();
            if (is_requires) {
                try requires_list.append(condition);
            } else {
                try ensures_list.append(condition);
            }
        }

        if (requires_list.items.len > 0 or ensures_list.items.len > 0) {
            contracts = .{
                .requires = try requires_list.toOwnedSlice(),
                .ensures = try ensures_list.toOwnedSlice(),
            };
        }

        // Body (optional for trait method signatures)
        var body: ?FunctionDecl.FunctionBody = null;
        if (self.check(.lbrace)) {
            body = .{ .block = try self.parseBlockExpr() };
        } else if (self.match(.eq)) {
            // Expression body: fn foo() -> int = 42
            body = .{ .expression = try self.parseExpr() };
        }

        const func = try self.astAllocator().create(FunctionDecl);
        func.* = .{
            .name = name_ident,
            .generic_params = generic_params,
            .params = try params.toOwnedSlice(),
            .return_type = return_type,
            .effects = effects,
            .contracts = contracts,
            .body = body,
            .is_comptime = is_comptime,
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return func;
    }

    /// Parse generic parameters: [T, U: Trait]
    fn parseGenericParams(self: *Self) !?[]const *GenericParam {
        if (!self.match(.lbracket)) return null;

        var params = std.ArrayList(*GenericParam).init(self.astAllocator());

        while (!self.check(.rbracket) and !self.isAtEnd()) {
            const start_loc = self.currentLocation();
            const name = try self.expectIdentifier("generic parameter name");
            const name_ident = makeIdentifier(name, start_loc);

            var bounds = std.ArrayList(*TypeExpr).init(self.astAllocator());
            var default: ?*TypeExpr = null;

            if (self.match(.colon)) {
                // Parse trait bounds
                const bound = try self.parseTypeExpr();
                try bounds.append(bound);
                while (self.match(.plus)) {
                    const next_bound = try self.parseTypeExpr();
                    try bounds.append(next_bound);
                }
            }

            if (self.match(.eq)) {
                // Default type
                default = try self.parseTypeExpr();
            }

            const param = try self.astAllocator().create(GenericParam);
            param.* = .{
                .name = name_ident,
                .bounds = try bounds.toOwnedSlice(),
                .default = default,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            try params.append(param);

            if (!self.match(.comma)) break;
        }

        try self.expect(.rbracket, "']' after generic parameters");
        return try params.toOwnedSlice();
    }

    /// Parse function parameters
    fn parseFunctionParams(self: *Self) !std.ArrayList(*FunctionParam) {
        var params = std.ArrayList(*FunctionParam).init(self.astAllocator());

        while (!self.check(.rparen) and !self.isAtEnd()) {
            const start_loc = self.currentLocation();
            const is_mut = self.match(.kw_mut);
            const name = try self.expectIdentifier("parameter name");
            const name_ident = makeIdentifier(name, start_loc);

            try self.expect(.colon, "':' after parameter name");
            const type_ann = try self.parseTypeExpr();

            var default_val: ?*Expr = null;
            if (self.match(.eq)) {
                default_val = try self.parseExpr();
            }

            const param = try self.astAllocator().create(FunctionParam);
            param.* = .{
                .name = name_ident,
                .type_expr = type_ann,
                .default_value = default_val,
                .is_mut = is_mut,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            try params.append(param);

            if (!self.match(.comma)) break;
        }

        return params;
    }

    /// Parse a struct definition
    pub fn parseStruct(self: *Self) !*StructDecl {
        const start_loc = self.currentLocation();
        const name = try self.expectIdentifier("struct name");
        const name_ident = makeIdentifier(name, start_loc);
        const generic_params = try self.parseGenericParams();

        try self.expect(.lbrace, "'{' after struct name");

        var fields = std.ArrayList(*StructField).init(self.astAllocator());
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const field_start = self.currentLocation();
            const field_visibility: Declaration.Visibility = if (self.match(.kw_private)) .private else .public;
            const field_name = try self.expectIdentifier("field name");
            const field_name_ident = makeIdentifier(field_name, field_start);

            try self.expect(.colon, "':' after field name");
            const field_type = try self.parseTypeExpr();

            var default_val: ?*Expr = null;
            if (self.match(.eq)) {
                default_val = try self.parseExpr();
            }

            const field = try self.astAllocator().create(StructField);
            field.* = .{
                .name = field_name_ident,
                .type_expr = field_type,
                .default_value = default_val,
                .visibility = field_visibility,
                .span = makeSpan(field_start, self.previousLocation()),
            };
            try fields.append(field);

            if (!self.match(.comma)) {
                if (!self.check(.rbrace)) {
                    return self.errorAtCurrent("Expected ',' or '}' after field");
                }
            }
        }

        try self.expect(.rbrace, "'}' after struct fields");

        const s = try self.astAllocator().create(StructDecl);
        s.* = .{
            .name = name_ident,
            .generic_params = generic_params,
            .fields = try fields.toOwnedSlice(),
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return s;
    }

    /// Parse an enum definition
    pub fn parseEnum(self: *Self) !*EnumDecl {
        const start_loc = self.currentLocation();
        const name = try self.expectIdentifier("enum name");
        const name_ident = makeIdentifier(name, start_loc);
        const generic_params = try self.parseGenericParams();

        try self.expect(.lbrace, "'{' after enum name");

        var variants = std.ArrayList(*EnumVariant).init(self.astAllocator());
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const variant_start = self.currentLocation();
            const variant_name = try self.expectIdentifier("variant name");
            const variant_name_ident = makeIdentifier(variant_name, variant_start);

            var payload: EnumVariant.Payload = .none;

            if (self.match(.lparen)) {
                // Tuple-style variant: Some(T)
                var types = std.ArrayList(*TypeExpr).init(self.astAllocator());
                while (!self.check(.rparen) and !self.isAtEnd()) {
                    const field_type = try self.parseTypeExpr();
                    try types.append(field_type);
                    if (!self.match(.comma)) break;
                }
                try self.expect(.rparen, "')' after variant types");
                payload = .{ .tuple = try types.toOwnedSlice() };
            } else if (self.match(.lbrace)) {
                // Struct-style variant
                var fields = std.ArrayList(*StructField).init(self.astAllocator());
                while (!self.check(.rbrace) and !self.isAtEnd()) {
                    const field_start = self.currentLocation();
                    const field_name = try self.expectIdentifier("field name");
                    const field_name_ident = makeIdentifier(field_name, field_start);
                    try self.expect(.colon, "':' after field name");
                    const field_type = try self.parseTypeExpr();

                    const field = try self.astAllocator().create(StructField);
                    field.* = .{
                        .name = field_name_ident,
                        .type_expr = field_type,
                        .default_value = null,
                        .visibility = .public,
                        .span = makeSpan(field_start, self.previousLocation()),
                    };
                    try fields.append(field);
                    if (!self.match(.comma)) break;
                }
                try self.expect(.rbrace, "'}' after variant fields");
                payload = .{ .struct_fields = try fields.toOwnedSlice() };
            }

            const variant = try self.astAllocator().create(EnumVariant);
            variant.* = .{
                .name = variant_name_ident,
                .payload = payload,
                .span = makeSpan(variant_start, self.previousLocation()),
            };
            try variants.append(variant);

            if (!self.match(.comma)) {
                if (!self.check(.rbrace)) {
                    return self.errorAtCurrent("Expected ',' or '}' after variant");
                }
            }
        }

        try self.expect(.rbrace, "'}' after enum variants");

        const e = try self.astAllocator().create(EnumDecl);
        e.* = .{
            .name = name_ident,
            .generic_params = generic_params,
            .variants = try variants.toOwnedSlice(),
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return e;
    }

    /// Parse a trait definition
    pub fn parseTrait(self: *Self) !*TraitDecl {
        const start_loc = self.currentLocation();
        const name = try self.expectIdentifier("trait name");
        const name_ident = makeIdentifier(name, start_loc);
        const generic_params = try self.parseGenericParams();

        // Super traits: trait Child: Parent1 + Parent2
        var super_traits = std.ArrayList(*TypeExpr).init(self.astAllocator());
        if (self.match(.colon)) {
            const first = try self.parseTypeExpr();
            try super_traits.append(first);
            while (self.match(.plus)) {
                const next_trait = try self.parseTypeExpr();
                try super_traits.append(next_trait);
            }
        }

        try self.expect(.lbrace, "'{' after trait name");

        var items = std.ArrayList(*TraitItem).init(self.astAllocator());
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const item_start = self.currentLocation();

            if (self.match(.kw_fn)) {
                const func = try self.parseFunction();
                const item = try self.astAllocator().create(TraitItem);
                item.* = .{
                    .kind = .{ .function = func },
                    .span = makeSpan(item_start, self.previousLocation()),
                };
                try items.append(item);
            } else if (self.match(.kw_const)) {
                const const_decl = try self.parseConstDecl();
                const item = try self.astAllocator().create(TraitItem);
                item.* = .{
                    .kind = .{ .constant = const_decl },
                    .span = makeSpan(item_start, self.previousLocation()),
                };
                try items.append(item);
            } else {
                return self.errorAtCurrent("Expected 'fn' or 'const' in trait body");
            }

            // Allow optional comma/semicolon between methods
            _ = self.match(.comma) or self.match(.semicolon);
        }

        try self.expect(.rbrace, "'}' after trait body");

        const t = try self.astAllocator().create(TraitDecl);
        t.* = .{
            .name = name_ident,
            .generic_params = generic_params,
            .super_traits = try super_traits.toOwnedSlice(),
            .items = try items.toOwnedSlice(),
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return t;
    }

    /// Parse an impl block
    pub fn parseImpl(self: *Self) !*ImplBlock {
        const start_loc = self.currentLocation();
        const generic_params = try self.parseGenericParams();

        // Parse trait for Type OR just Type
        const first_type = try self.parseTypeExpr();

        var trait_type: ?*TypeExpr = null;
        var target_type: *TypeExpr = first_type;

        if (self.match(.kw_for)) {
            trait_type = first_type;
            target_type = try self.parseTypeExpr();
        }

        try self.expect(.lbrace, "'{' after impl target");

        var items = std.ArrayList(*ImplItem).init(self.astAllocator());
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const item_start = self.currentLocation();
            const visibility: Declaration.Visibility = if (self.match(.kw_private)) .private else .public;

            if (self.match(.kw_fn)) {
                const func = try self.parseFunction();
                const item = try self.astAllocator().create(ImplItem);
                item.* = .{
                    .kind = .{ .function = func },
                    .visibility = visibility,
                    .span = makeSpan(item_start, self.previousLocation()),
                };
                try items.append(item);
            } else if (self.match(.kw_const)) {
                const const_decl = try self.parseConstDecl();
                const item = try self.astAllocator().create(ImplItem);
                item.* = .{
                    .kind = .{ .constant = const_decl },
                    .visibility = visibility,
                    .span = makeSpan(item_start, self.previousLocation()),
                };
                try items.append(item);
            } else {
                return self.errorAtCurrent("Expected 'fn' or 'const' in impl block");
            }
        }

        try self.expect(.rbrace, "'}' after impl body");

        const impl = try self.astAllocator().create(ImplBlock);
        impl.* = .{
            .generic_params = generic_params,
            .trait_type = trait_type,
            .target_type = target_type,
            .where_clause = null,
            .items = try items.toOwnedSlice(),
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return impl;
    }

    /// Parse an import declaration
    fn parseImportDecl(self: *Self) !*ImportDecl {
        const start_loc = self.currentLocation();
        try self.expect(.kw_import, "'import'");

        var segments = std.ArrayList(Identifier).init(self.astAllocator());
        const first = try self.expectIdentifier("module path");
        try segments.append(makeIdentifier(first, self.previousLocation()));

        while (self.match(.colon_colon)) {
            if (self.check(.lbrace)) break; // import foo::bar::{...}
            const seg = try self.expectIdentifier("module path segment");
            try segments.append(makeIdentifier(seg, self.previousLocation()));
        }

        const path = Path{
            .segments = try segments.toOwnedSlice(),
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        var items: ?[]const ImportDecl.ImportItem = null;
        if (self.match(.lbrace)) {
            var import_items = std.ArrayList(ImportDecl.ImportItem).init(self.astAllocator());
            while (!self.check(.rbrace) and !self.isAtEnd()) {
                const item_name = try self.expectIdentifier("import item");
                const item_ident = makeIdentifier(item_name, self.previousLocation());
                const alias: ?Identifier = null;
                // Could add 'as' keyword handling here if needed
                try import_items.append(.{
                    .name = item_ident,
                    .alias = alias,
                });
                if (!self.match(.comma)) break;
            }
            try self.expect(.rbrace, "'}' after import items");
            items = try import_items.toOwnedSlice();
        }

        const imp = try self.astAllocator().create(ImportDecl);
        imp.* = .{
            .path = path,
            .items = items,
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return imp;
    }

    /// Parse a constant declaration
    fn parseConstDecl(self: *Self) !*ConstDecl {
        const start_loc = self.currentLocation();
        const name = try self.expectIdentifier("constant name");
        const name_ident = makeIdentifier(name, start_loc);

        var type_ann: ?*TypeExpr = null;
        if (self.match(.colon)) {
            type_ann = try self.parseTypeExpr();
        }

        var value: ?*Expr = null;
        if (self.match(.eq)) {
            value = try self.parseExpr();
        }

        const const_decl = try self.astAllocator().create(ConstDecl);
        const_decl.* = .{
            .name = name_ident,
            .type_expr = type_ann,
            .value = value,
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return const_decl;
    }

    // ========================================================================
    // Type Expression Parsing
    // ========================================================================

    /// Parse a type expression
    pub fn parseTypeExpr(self: *Self) anyerror!*TypeExpr {
        const start_loc = self.currentLocation();
        const type_expr = try self.astAllocator().create(TypeExpr);

        // Optional type: ?T
        if (self.match(.question)) {
            const inner = try self.parseTypeExpr();
            const opt_type = try self.astAllocator().create(OptionType);
            opt_type.* = .{
                .inner_type = inner,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            type_expr.* = .{
                .kind = .{ .option = opt_type },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return type_expr;
        }

        // Reference type: &T or &mut T
        if (self.match(.ampersand)) {
            const is_mut = self.match(.kw_mut);
            const inner = try self.parseTypeExpr();
            const ref_type = try self.astAllocator().create(ReferenceType);
            ref_type.* = .{
                .referenced_type = inner,
                .is_mut = is_mut,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            type_expr.* = .{
                .kind = .{ .reference = ref_type },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return type_expr;
        }

        // Function type: fn(T, U) -> V
        if (self.match(.kw_fn)) {
            try self.expect(.lparen, "'(' after 'fn' in type");
            var param_types = std.ArrayList(*TypeExpr).init(self.astAllocator());
            while (!self.check(.rparen) and !self.isAtEnd()) {
                const param_type = try self.parseTypeExpr();
                try param_types.append(param_type);
                if (!self.match(.comma)) break;
            }
            try self.expect(.rparen, "')' after function type parameters");

            var ret_type: *TypeExpr = undefined;
            if (self.match(.arrow)) {
                ret_type = try self.parseTypeExpr();
            } else {
                // Default void return
                ret_type = try self.astAllocator().create(TypeExpr);
                const named = try self.astAllocator().create(NamedType);
                const void_ident = makeIdentifier("void", start_loc);
                named.* = .{
                    .path = .{
                        .segments = try self.astAllocator().dupe(Identifier, &[_]Identifier{void_ident}),
                        .span = makeSpan(start_loc, start_loc),
                    },
                    .generic_args = null,
                    .span = makeSpan(start_loc, start_loc),
                };
                ret_type.* = .{
                    .kind = .{ .named = named },
                    .span = makeSpan(start_loc, start_loc),
                };
            }

            var effects: ?[]const *TypeExpr = null;
            if (self.match(.kw_with)) {
                try self.expect(.lbracket, "'[' after 'with' in type");
                var effect_list = std.ArrayList(*TypeExpr).init(self.astAllocator());
                while (!self.check(.rbracket) and !self.isAtEnd()) {
                    const effect = try self.parseTypeExpr();
                    try effect_list.append(effect);
                    if (!self.match(.comma)) break;
                }
                try self.expect(.rbracket, "']' after effects in type");
                effects = try effect_list.toOwnedSlice();
            }

            const func_type = try self.astAllocator().create(FunctionType);
            func_type.* = .{
                .params = try param_types.toOwnedSlice(),
                .return_type = ret_type,
                .effects = effects,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            type_expr.* = .{
                .kind = .{ .function = func_type },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return type_expr;
        }

        // Tuple type: (T, U, V)
        if (self.match(.lparen)) {
            var types = std.ArrayList(*TypeExpr).init(self.astAllocator());
            if (!self.check(.rparen)) {
                const first = try self.parseTypeExpr();
                try types.append(first);
                while (self.match(.comma)) {
                    const next_type = try self.parseTypeExpr();
                    try types.append(next_type);
                }
            }
            try self.expect(.rparen, "')' after tuple type");

            const tuple_type = try self.astAllocator().create(TupleType);
            tuple_type.* = .{
                .elements = try types.toOwnedSlice(),
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            type_expr.* = .{
                .kind = .{ .tuple = tuple_type },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return type_expr;
        }

        // Array type: [T] or [T; N]
        if (self.match(.lbracket)) {
            const elem_type = try self.parseTypeExpr();
            if (self.match(.semicolon)) {
                // Fixed-size array: [T; N]
                const size = try self.parseExpr();
                try self.expect(.rbracket, "']' after array type");

                const arr_type = try self.astAllocator().create(ArrayType);
                arr_type.* = .{
                    .element_type = elem_type,
                    .size = size,
                    .span = makeSpan(start_loc, self.previousLocation()),
                };
                type_expr.* = .{
                    .kind = .{ .array = arr_type },
                    .span = makeSpan(start_loc, self.previousLocation()),
                };
            } else {
                // Slice type: [T]
                try self.expect(.rbracket, "']' after slice type");

                const slice_type = try self.astAllocator().create(SliceType);
                slice_type.* = .{
                    .element_type = elem_type,
                    .span = makeSpan(start_loc, self.previousLocation()),
                };
                type_expr.* = .{
                    .kind = .{ .slice = slice_type },
                    .span = makeSpan(start_loc, self.previousLocation()),
                };
            }
            return type_expr;
        }

        // Inferred type: _
        if (self.match(.underscore)) {
            type_expr.* = .{
                .kind = .infer,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return type_expr;
        }

        // Named type or generic type
        const name = try self.expectIdentifier("type name");
        const name_ident = makeIdentifier(name, start_loc);

        var segments = std.ArrayList(Identifier).init(self.astAllocator());
        try segments.append(name_ident);

        // Handle paths: Foo::Bar::Baz
        while (self.match(.colon_colon)) {
            const seg = try self.expectIdentifier("type path segment");
            try segments.append(makeIdentifier(seg, self.previousLocation()));
        }

        const path = Path{
            .segments = try segments.toOwnedSlice(),
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        // Check for generic arguments
        var generic_args: ?[]const *TypeExpr = null;
        if (self.match(.lbracket)) {
            var args = std.ArrayList(*TypeExpr).init(self.astAllocator());
            while (!self.check(.rbracket) and !self.isAtEnd()) {
                const arg = try self.parseTypeExpr();
                try args.append(arg);
                if (!self.match(.comma)) break;
            }
            try self.expect(.rbracket, "']' after generic arguments");
            generic_args = try args.toOwnedSlice();
        }

        const named = try self.astAllocator().create(NamedType);
        named.* = .{
            .path = path,
            .generic_args = generic_args,
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        type_expr.* = .{
            .kind = .{ .named = named },
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        return type_expr;
    }

    // ========================================================================
    // Statement Parsing
    // ========================================================================

    /// Parse a statement
    pub fn parseStatement(self: *Self) anyerror!*Statement {
        const start_loc = self.currentLocation();

        if (self.match(.kw_let)) {
            return self.parseLetBinding(start_loc);
        }
        if (self.match(.kw_return)) {
            return self.parseReturn(start_loc);
        }
        if (self.match(.kw_break)) {
            const value = if (!self.check(.rbrace) and !self.check(.semicolon) and !self.isAtEnd())
                try self.parseExpr()
            else
                null;
            _ = self.match(.semicolon);

            const break_stmt = try self.astAllocator().create(BreakStmt);
            break_stmt.* = .{
                .label = null,
                .value = value,
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const stmt = try self.astAllocator().create(Statement);
            stmt.* = .{
                .kind = .{ .break_stmt = break_stmt },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return stmt;
        }
        if (self.match(.kw_continue)) {
            _ = self.match(.semicolon);

            const continue_stmt = try self.astAllocator().create(ContinueStmt);
            continue_stmt.* = .{
                .label = null,
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const stmt = try self.astAllocator().create(Statement);
            stmt.* = .{
                .kind = .{ .continue_stmt = continue_stmt },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return stmt;
        }
        if (self.match(.kw_while)) {
            return self.parseWhile(start_loc);
        }
        if (self.match(.kw_for)) {
            return self.parseFor(start_loc);
        }
        if (self.match(.kw_loop)) {
            return self.parseLoop(start_loc);
        }
        if (self.match(.kw_discard)) {
            const expr = try self.parseExpr();
            _ = self.match(.semicolon);

            const discard = try self.astAllocator().create(DiscardStmt);
            discard.* = .{
                .value = expr,
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const stmt = try self.astAllocator().create(Statement);
            stmt.* = .{
                .kind = .{ .discard = discard },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return stmt;
        }

        // Expression statement or assignment
        const expr = try self.parseExpr();

        // Check for assignment
        if (self.match(.eq)) {
            const value = try self.parseExpr();
            _ = self.match(.semicolon);

            const assignment = try self.astAllocator().create(Assignment);
            assignment.* = .{
                .target = expr,
                .op = .assign,
                .value = value,
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const stmt = try self.astAllocator().create(Statement);
            stmt.* = .{
                .kind = .{ .assignment = assignment },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return stmt;
        }

        // Check for compound assignment
        if (self.matchCompoundAssignment()) |op| {
            const value = try self.parseExpr();
            _ = self.match(.semicolon);

            const assignment = try self.astAllocator().create(Assignment);
            assignment.* = .{
                .target = expr,
                .op = op,
                .value = value,
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const stmt = try self.astAllocator().create(Statement);
            stmt.* = .{
                .kind = .{ .assignment = assignment },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return stmt;
        }

        _ = self.match(.semicolon);
        const stmt = try self.astAllocator().create(Statement);
        stmt.* = .{
            .kind = .{ .expression = expr },
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return stmt;
    }

    /// Parse let binding
    pub fn parseLetBinding(self: *Self, start_loc: SourceLocation) !*Statement {
        const is_mut = self.match(.kw_mut);
        const pattern = try self.parsePattern();

        var type_ann: ?*TypeExpr = null;
        if (self.match(.colon)) {
            type_ann = try self.parseTypeExpr();
        }

        var value: ?*Expr = null;
        if (self.match(.eq)) {
            value = try self.parseExpr();
        }

        _ = self.match(.semicolon);

        const let_binding = try self.astAllocator().create(LetBinding);
        let_binding.* = .{
            .pattern = pattern,
            .type_annotation = type_ann,
            .value = value,
            .is_mut = is_mut,
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        const stmt = try self.astAllocator().create(Statement);
        stmt.* = .{
            .kind = .{ .let_binding = let_binding },
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return stmt;
    }

    /// Parse return statement
    pub fn parseReturn(self: *Self, start_loc: SourceLocation) !*Statement {
        var value: ?*Expr = null;
        if (!self.check(.rbrace) and !self.check(.semicolon) and !self.isAtEnd()) {
            value = try self.parseExpr();
        }
        _ = self.match(.semicolon);

        const return_stmt = try self.astAllocator().create(ReturnStmt);
        return_stmt.* = .{
            .value = value,
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        const stmt = try self.astAllocator().create(Statement);
        stmt.* = .{
            .kind = .{ .return_stmt = return_stmt },
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return stmt;
    }

    /// Parse while loop
    pub fn parseWhile(self: *Self, start_loc: SourceLocation) !*Statement {
        // Use parseExprNoStruct to avoid `while a > b { ... }` being parsed as struct literal
        const condition = try self.parseExprNoStruct();
        const body = try self.parseBlockExpr();

        const while_loop = try self.astAllocator().create(WhileLoop);
        while_loop.* = .{
            .label = null,
            .condition = condition,
            .body = body,
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        const stmt = try self.astAllocator().create(Statement);
        stmt.* = .{
            .kind = .{ .while_loop = while_loop },
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return stmt;
    }

    /// Parse for loop
    pub fn parseFor(self: *Self, start_loc: SourceLocation) !*Statement {
        const pattern = try self.parsePattern();
        try self.expect(.kw_in, "'in' after for loop pattern");
        const iterable = try self.parseExpr();
        const body = try self.parseBlockExpr();

        const for_loop = try self.astAllocator().create(ForLoop);
        for_loop.* = .{
            .label = null,
            .pattern = pattern,
            .iterator = iterable,
            .body = body,
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        const stmt = try self.astAllocator().create(Statement);
        stmt.* = .{
            .kind = .{ .for_loop = for_loop },
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return stmt;
    }

    /// Parse infinite loop
    pub fn parseLoop(self: *Self, start_loc: SourceLocation) !*Statement {
        const body = try self.parseBlockExpr();

        const loop_stmt = try self.astAllocator().create(LoopStmt);
        loop_stmt.* = .{
            .label = null,
            .body = body,
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        const stmt = try self.astAllocator().create(Statement);
        stmt.* = .{
            .kind = .{ .loop_stmt = loop_stmt },
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return stmt;
    }

    /// Parse a block expression: { ... }
    pub fn parseBlockExpr(self: *Self) anyerror!*BlockExpr {
        const start_loc = self.currentLocation();
        try self.expect(.lbrace, "'{'");

        var statements = std.ArrayList(*Statement).init(self.astAllocator());
        var trailing_expr: ?*Expr = null;

        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const stmt = try self.parseStatement();

            // If it's an expression statement and we're at the end of the block,
            // it might be the trailing expression
            if (self.check(.rbrace)) {
                if (stmt.kind == .expression) {
                    trailing_expr = stmt.kind.expression;
                } else {
                    try statements.append(stmt);
                }
            } else {
                try statements.append(stmt);
            }
        }

        try self.expect(.rbrace, "'}'");

        const block = try self.astAllocator().create(BlockExpr);
        block.* = .{
            .statements = try statements.toOwnedSlice(),
            .result = trailing_expr,
            .span = makeSpan(start_loc, self.previousLocation()),
        };
        return block;
    }

    // ========================================================================
    // Expression Parsing (Pratt Parser)
    // ========================================================================

    /// Parse an expression (entry point)
    pub fn parseExpr(self: *Self) anyerror!*Expr {
        return self.parsePrecedenceImpl(.pipeline, true);
    }

    /// Parse an expression without allowing struct literals at the top level
    /// Used for match scrutinees where `match x { ... }` shouldn't parse `x { ... }` as struct literal
    fn parseExprNoStruct(self: *Self) anyerror!*Expr {
        return self.parsePrecedenceImpl(.pipeline, false);
    }

    /// Parse expression with given precedence level
    fn parsePrecedence(self: *Self, min_prec: Precedence) anyerror!*Expr {
        return self.parsePrecedenceImpl(min_prec, true);
    }

    /// Parse expression with given precedence level, with control over struct literal parsing
    fn parsePrecedenceImpl(self: *Self, min_prec: Precedence, allow_struct_lit: bool) anyerror!*Expr {
        var left = try self.parseUnaryImpl(allow_struct_lit);

        while (true) {
            const op_prec = self.getCurrentPrecedence();
            if (@intFromEnum(op_prec) < @intFromEnum(min_prec)) break;

            // Handle range operators specially
            if (self.check(.dot_dot) or self.check(.dot_dot_eq)) {
                const is_inclusive = self.check(.dot_dot_eq);
                _ = self.advance();
                var end_expr: ?*Expr = null;

                // Check if there's an end expression
                if (!self.check(.rbracket) and !self.check(.rparen) and !self.check(.rbrace) and
                    !self.check(.comma) and !self.check(.semicolon) and !self.isAtEnd())
                {
                    end_expr = try self.parsePrecedenceImpl(op_prec.next(), allow_struct_lit);
                }

                const range = try self.astAllocator().create(RangeExpr);
                range.* = .{
                    .start = left,
                    .end = end_expr,
                    .inclusive = is_inclusive,
                    .span = Span.merge(left.span, if (end_expr) |e| e.span else left.span),
                };

                const new_left = try self.astAllocator().create(Expr);
                new_left.* = .{
                    .kind = .{ .range = range },
                    .span = range.span,
                };
                left = new_left;
                continue;
            }

            // Handle pipeline operator
            if (self.check(.pipe_gt)) {
                _ = self.advance();
                const right = try self.parsePrecedenceImpl(op_prec.next(), allow_struct_lit);

                const pipeline = try self.astAllocator().create(PipelineExpr);
                pipeline.* = .{
                    .left = left,
                    .right = right,
                    .span = Span.merge(left.span, right.span),
                };

                const new_left = try self.astAllocator().create(Expr);
                new_left.* = .{
                    .kind = .{ .pipeline = pipeline },
                    .span = pipeline.span,
                };
                left = new_left;
                continue;
            }

            const op_token = self.peek();
            const op = tokenToBinaryOp(op_token.type) orelse break;
            _ = self.advance();

            // Handle right-associativity (none in current implementation, but structure supports it)
            const next_prec = op_prec.next();
            const right = try self.parsePrecedenceImpl(next_prec, allow_struct_lit);

            const binary = try self.astAllocator().create(BinaryExpr);
            binary.* = .{
                .left = left,
                .op = op,
                .right = right,
                .span = Span.merge(left.span, right.span),
            };

            const new_left = try self.astAllocator().create(Expr);
            new_left.* = .{
                .kind = .{ .binary = binary },
                .span = binary.span,
            };
            left = new_left;
        }

        return left;
    }

    /// Parse unary expression
    fn parseUnary(self: *Self) anyerror!*Expr {
        return self.parseUnaryImpl(true);
    }

    /// Parse unary expression with control over struct literal parsing
    fn parseUnaryImpl(self: *Self, allow_struct_lit: bool) anyerror!*Expr {
        const start_loc = self.currentLocation();

        // Prefix operators
        if (self.match(.minus)) {
            const operand = try self.parseUnaryImpl(allow_struct_lit);

            const unary = try self.astAllocator().create(UnaryExpr);
            unary.* = .{
                .op = .neg,
                .operand = operand,
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .unary = unary },
                .span = unary.span,
            };
            return expr;
        }
        if (self.match(.kw_not) or self.match(.bang)) {
            const operand = try self.parseUnaryImpl(allow_struct_lit);

            const unary = try self.astAllocator().create(UnaryExpr);
            unary.* = .{
                .op = .not,
                .operand = operand,
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .unary = unary },
                .span = unary.span,
            };
            return expr;
        }
        if (self.match(.tilde)) {
            const operand = try self.parseUnaryImpl(allow_struct_lit);

            const unary = try self.astAllocator().create(UnaryExpr);
            unary.* = .{
                .op = .bit_not,
                .operand = operand,
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .unary = unary },
                .span = unary.span,
            };
            return expr;
        }

        return self.parsePostfixImpl(allow_struct_lit);
    }

    /// Parse postfix expression (calls, field access, indexing, ?, ??)
    fn parsePostfix(self: *Self) anyerror!*Expr {
        return self.parsePostfixImpl(true);
    }

    /// Parse postfix expression with control over struct literal parsing
    fn parsePostfixImpl(self: *Self, allow_struct_lit: bool) anyerror!*Expr {
        var expr = try self.parsePrimaryImpl(allow_struct_lit);

        while (true) {
            if (self.match(.lparen)) {
                // Function call
                expr = try self.finishCall(expr);
            } else if (self.match(.lbracket)) {
                // Index access
                const index = try self.parseExpr();
                try self.expect(.rbracket, "']' after index");

                const index_access = try self.astAllocator().create(IndexAccess);
                index_access.* = .{
                    .object = expr,
                    .index = index,
                    .span = Span.merge(expr.span, makeSpan(self.previousLocation(), self.previousLocation())),
                };

                const new_expr = try self.astAllocator().create(Expr);
                new_expr.* = .{
                    .kind = .{ .index_access = index_access },
                    .span = index_access.span,
                };
                expr = new_expr;
            } else if (self.match(.dot)) {
                // Field access or method call
                const field_start = self.currentLocation();
                const field = try self.expectIdentifier("field name");
                const field_ident = makeIdentifier(field, field_start);

                if (self.match(.lparen)) {
                    // Method call
                    var args = std.ArrayList(*Expr).init(self.astAllocator());
                    if (!self.check(.rparen)) {
                        const first = try self.parseExpr();
                        try args.append(first);
                        while (self.match(.comma)) {
                            const arg = try self.parseExpr();
                            try args.append(arg);
                        }
                    }
                    try self.expect(.rparen, "')' after method arguments");

                    const method_call = try self.astAllocator().create(MethodCall);
                    method_call.* = .{
                        .object = expr,
                        .method = field_ident,
                        .generic_args = null,
                        .args = try args.toOwnedSlice(),
                        .span = Span.merge(expr.span, makeSpan(self.previousLocation(), self.previousLocation())),
                    };

                    const new_expr = try self.astAllocator().create(Expr);
                    new_expr.* = .{
                        .kind = .{ .method_call = method_call },
                        .span = method_call.span,
                    };
                    expr = new_expr;
                } else {
                    // Field access
                    const field_access = try self.astAllocator().create(FieldAccess);
                    field_access.* = .{
                        .object = expr,
                        .field = field_ident,
                        .span = Span.merge(expr.span, makeSpan(self.previousLocation(), self.previousLocation())),
                    };

                    const new_expr = try self.astAllocator().create(Expr);
                    new_expr.* = .{
                        .kind = .{ .field_access = field_access },
                        .span = field_access.span,
                    };
                    expr = new_expr;
                }
            } else if (self.match(.question)) {
                // Error propagation: expr?
                const error_prop = try self.astAllocator().create(ErrorPropagateExpr);
                error_prop.* = .{
                    .operand = expr,
                    .span = Span.merge(expr.span, makeSpan(self.previousLocation(), self.previousLocation())),
                };

                const new_expr = try self.astAllocator().create(Expr);
                new_expr.* = .{
                    .kind = .{ .error_propagate = error_prop },
                    .span = error_prop.span,
                };
                expr = new_expr;
            } else if (self.match(.question_question)) {
                // Null coalescing: expr ?? default
                const default = try self.parsePrecedence(.@"or");

                const coalesce = try self.astAllocator().create(CoalesceExpr);
                coalesce.* = .{
                    .left = expr,
                    .right = default,
                    .span = Span.merge(expr.span, default.span),
                };

                const new_expr = try self.astAllocator().create(Expr);
                new_expr.* = .{
                    .kind = .{ .coalesce = coalesce },
                    .span = coalesce.span,
                };
                expr = new_expr;
            } else {
                break;
            }
        }

        return expr;
    }

    /// Finish parsing a function call
    fn finishCall(self: *Self, callee: *Expr) !*Expr {
        var args = std.ArrayList(*FunctionCall.CallArg).init(self.astAllocator());

        if (!self.check(.rparen)) {
            while (true) {
                const arg_expr = try self.parseExpr();

                const call_arg = try self.astAllocator().create(FunctionCall.CallArg);
                call_arg.* = .{
                    .name = null,
                    .value = arg_expr,
                };
                try args.append(call_arg);

                if (!self.match(.comma)) break;
            }
        }

        try self.expect(.rparen, "')' after arguments");

        const call = try self.astAllocator().create(FunctionCall);
        call.* = .{
            .function = callee,
            .generic_args = null,
            .args = try args.toOwnedSlice(),
            .span = Span.merge(callee.span, makeSpan(self.previousLocation(), self.previousLocation())),
        };

        const expr = try self.astAllocator().create(Expr);
        expr.* = .{
            .kind = .{ .function_call = call },
            .span = call.span,
        };
        return expr;
    }

    /// Parse primary expression
    fn parsePrimary(self: *Self) anyerror!*Expr {
        return self.parsePrimaryImpl(true);
    }

    /// Parse primary expression with control over struct literal parsing
    fn parsePrimaryImpl(self: *Self, allow_struct_lit: bool) anyerror!*Expr {
        const start_loc = self.currentLocation();

        // Integer literal
        if (self.match(.integer)) {
            const lexeme = self.previous().lexeme;

            const lit = try self.astAllocator().create(Literal);
            lit.* = .{
                .kind = .{ .int = .{ .value = lexeme, .suffix = null } },
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .literal = lit },
                .span = lit.span,
            };
            return expr;
        }

        // Float literal
        if (self.match(.float)) {
            const lexeme = self.previous().lexeme;

            const lit = try self.astAllocator().create(Literal);
            lit.* = .{
                .kind = .{ .float = .{ .value = lexeme, .suffix = null } },
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .literal = lit },
                .span = lit.span,
            };
            return expr;
        }

        // String literal
        if (self.match(.string)) {
            const lexeme = self.previous().lexeme;
            // Strip surrounding quotes from "..."
            const value = if (lexeme.len >= 2) lexeme[1 .. lexeme.len - 1] else lexeme;

            const lit = try self.astAllocator().create(Literal);
            lit.* = .{
                .kind = .{ .string = .{ .value = value, .kind = .regular } },
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .literal = lit },
                .span = lit.span,
            };
            return expr;
        }

        // Raw string
        if (self.match(.raw_string)) {
            const lexeme = self.previous().lexeme;
            // Strip r"..." prefix and suffix
            const value = if (lexeme.len >= 3) lexeme[2 .. lexeme.len - 1] else lexeme;

            const lit = try self.astAllocator().create(Literal);
            lit.* = .{
                .kind = .{ .string = .{ .value = value, .kind = .raw } },
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .literal = lit },
                .span = lit.span,
            };
            return expr;
        }

        // Byte string
        if (self.match(.byte_string)) {
            const lexeme = self.previous().lexeme;
            // Strip b"..." prefix and suffix
            const value = if (lexeme.len >= 3) lexeme[2 .. lexeme.len - 1] else lexeme;

            const lit = try self.astAllocator().create(Literal);
            lit.* = .{
                .kind = .{ .string = .{ .value = value, .kind = .byte } },
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .literal = lit },
                .span = lit.span,
            };
            return expr;
        }

        // Character literal
        if (self.match(.char_literal)) {
            const lexeme = self.previous().lexeme;

            const lit = try self.astAllocator().create(Literal);
            lit.* = .{
                .kind = .{ .char = .{ .value = lexeme } },
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .literal = lit },
                .span = lit.span,
            };
            return expr;
        }

        // Boolean literals
        if (self.match(.kw_true)) {
            const lit = try self.astAllocator().create(Literal);
            lit.* = .{
                .kind = .{ .bool = true },
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .literal = lit },
                .span = lit.span,
            };
            return expr;
        }
        if (self.match(.kw_false)) {
            const lit = try self.astAllocator().create(Literal);
            lit.* = .{
                .kind = .{ .bool = false },
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .literal = lit },
                .span = lit.span,
            };
            return expr;
        }

        // If expression
        if (self.match(.kw_if)) {
            return self.parseIf(start_loc);
        }

        // Match expression
        if (self.match(.kw_match)) {
            return self.parseMatch(start_loc);
        }

        // Comptime expression
        if (self.match(.kw_comptime)) {
            const inner = try self.parseExpr();

            const comptime_expr = try self.astAllocator().create(ComptimeExpr);
            comptime_expr.* = .{
                .expr = inner,
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .comptime_expr = comptime_expr },
                .span = comptime_expr.span,
            };
            return expr;
        }

        // Block expression
        if (self.check(.lbrace)) {
            const block = try self.parseBlockExpr();
            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .block = block },
                .span = block.span,
            };
            return expr;
        }

        // Array literal
        if (self.match(.lbracket)) {
            var elements = std.ArrayList(*Expr).init(self.astAllocator());
            if (!self.check(.rbracket)) {
                const first = try self.parseExpr();

                // Check for repeat syntax: [value; count]
                if (self.match(.semicolon)) {
                    const count = try self.parseExpr();
                    try self.expect(.rbracket, "']' after array repeat");

                    const arr = try self.astAllocator().create(ArrayLiteral);
                    arr.* = .{
                        .kind = .{ .repeat = .{ .value = first, .count = count } },
                        .span = makeSpan(start_loc, self.previousLocation()),
                    };

                    const expr = try self.astAllocator().create(Expr);
                    expr.* = .{
                        .kind = .{ .array_literal = arr },
                        .span = arr.span,
                    };
                    return expr;
                }

                try elements.append(first);
                while (self.match(.comma)) {
                    if (self.check(.rbracket)) break; // trailing comma
                    const elem = try self.parseExpr();
                    try elements.append(elem);
                }
            }
            try self.expect(.rbracket, "']' after array elements");

            const arr = try self.astAllocator().create(ArrayLiteral);
            arr.* = .{
                .kind = .{ .elements = try elements.toOwnedSlice() },
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .array_literal = arr },
                .span = arr.span,
            };
            return expr;
        }

        // Lambda: |params| body
        if (self.match(.pipe)) {
            return self.parseLambda(start_loc);
        }

        // Grouped expression or tuple literal
        if (self.match(.lparen)) {
            if (self.check(.rparen)) {
                // Empty tuple
                _ = self.advance();

                const tuple = try self.astAllocator().create(TupleLiteral);
                tuple.* = .{
                    .elements = &[_]*Expr{},
                    .span = makeSpan(start_loc, self.previousLocation()),
                };

                const expr = try self.astAllocator().create(Expr);
                expr.* = .{
                    .kind = .{ .tuple_literal = tuple },
                    .span = tuple.span,
                };
                return expr;
            }

            const first = try self.parseExpr();

            if (self.match(.comma)) {
                // Tuple literal
                var elements = std.ArrayList(*Expr).init(self.astAllocator());
                try elements.append(first);
                if (!self.check(.rparen)) {
                    const second = try self.parseExpr();
                    try elements.append(second);
                    while (self.match(.comma)) {
                        if (self.check(.rparen)) break;
                        const elem = try self.parseExpr();
                        try elements.append(elem);
                    }
                }
                try self.expect(.rparen, "')' after tuple elements");

                const tuple = try self.astAllocator().create(TupleLiteral);
                tuple.* = .{
                    .elements = try elements.toOwnedSlice(),
                    .span = makeSpan(start_loc, self.previousLocation()),
                };

                const expr = try self.astAllocator().create(Expr);
                expr.* = .{
                    .kind = .{ .tuple_literal = tuple },
                    .span = tuple.span,
                };
                return expr;
            }

            try self.expect(.rparen, "')' after expression");
            const expr = try self.astAllocator().create(Expr);
            expr.* = .{
                .kind = .{ .grouped = first },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return expr;
        }

        // Identifier, path, or struct literal
        if (self.match(.identifier)) {
            const name = self.previous().lexeme;
            const name_ident = makeIdentifier(name, start_loc);

            // Build path if needed
            var segments = std.ArrayList(Identifier).init(self.astAllocator());
            try segments.append(name_ident);

            while (self.match(.colon_colon)) {
                const seg = try self.expectIdentifier("path segment");
                try segments.append(makeIdentifier(seg, self.previousLocation()));
            }

            // Check for struct literal: Name { ... }
            // Only allowed when allow_struct_lit is true (not in match scrutinee position)
            if (allow_struct_lit and self.check(.lbrace)) {
                const path = Path{
                    .segments = try segments.toOwnedSlice(),
                    .span = makeSpan(start_loc, self.previousLocation()),
                };
                return self.parseStructLiteral(start_loc, path);
            }

            // Just an identifier or path
            if (segments.items.len == 1) {
                const expr = try self.astAllocator().create(Expr);
                expr.* = .{
                    .kind = .{ .identifier = name_ident },
                    .span = makeSpan(start_loc, self.previousLocation()),
                };
                return expr;
            } else {
                const path = Path{
                    .segments = try segments.toOwnedSlice(),
                    .span = makeSpan(start_loc, self.previousLocation()),
                };
                const expr = try self.astAllocator().create(Expr);
                expr.* = .{
                    .kind = .{ .path = path },
                    .span = path.span,
                };
                return expr;
            }
        }

        return self.errorAtCurrent("Expected expression");
    }

    /// Parse if expression
    pub fn parseIf(self: *Self, start_loc: SourceLocation) !*Expr {
        // Use parseExprNoStruct to avoid `if a > b { ... }` being parsed as `if a > (b { ... })`
        const condition = try self.parseExprNoStruct();
        const then_branch = try self.parseBlockExpr();

        var else_branch: ?IfExpr.ElseBranch = null;
        if (self.match(.kw_else)) {
            if (self.match(.kw_if)) {
                // else if
                const else_if_expr = try self.parseIf(self.currentLocation());
                // Extract the IfExpr from the Expr
                if (else_if_expr.kind == .if_expr) {
                    else_branch = .{ .else_if = else_if_expr.kind.if_expr };
                }
            } else {
                // else block
                const else_block = try self.parseBlockExpr();
                else_branch = .{ .else_block = else_block };
            }
        }

        const if_expr = try self.astAllocator().create(IfExpr);
        if_expr.* = .{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        const expr = try self.astAllocator().create(Expr);
        expr.* = .{
            .kind = .{ .if_expr = if_expr },
            .span = if_expr.span,
        };
        return expr;
    }

    /// Parse match expression
    pub fn parseMatch(self: *Self, start_loc: SourceLocation) !*Expr {
        // Use parseExprNoStruct to avoid parsing `match foo { ... }` as `match (foo { ... })`
        const scrutinee = try self.parseExprNoStruct();
        try self.expect(.lbrace, "'{' after match scrutinee");

        var arms = std.ArrayList(*MatchArm).init(self.astAllocator());
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const arm_start = self.currentLocation();
            const pattern = try self.parsePattern();

            var guard: ?*Expr = null;
            if (self.match(.kw_if)) {
                guard = try self.parseExpr();
            }

            try self.expect(.fat_arrow, "'=>' after match pattern");
            const body_expr = try self.parseExpr();
            _ = self.match(.comma);

            const arm = try self.astAllocator().create(MatchArm);
            arm.* = .{
                .pattern = pattern,
                .guard = guard,
                .body = .{ .expression = body_expr },
                .span = makeSpan(arm_start, self.previousLocation()),
            };
            try arms.append(arm);
        }

        try self.expect(.rbrace, "'}' after match arms");

        const match_expr = try self.astAllocator().create(MatchExpr);
        match_expr.* = .{
            .scrutinee = scrutinee,
            .arms = try arms.toOwnedSlice(),
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        const expr = try self.astAllocator().create(Expr);
        expr.* = .{
            .kind = .{ .match_expr = match_expr },
            .span = match_expr.span,
        };
        return expr;
    }

    /// Parse lambda expression
    fn parseLambda(self: *Self, start_loc: SourceLocation) !*Expr {
        var params = std.ArrayList(*LambdaParam).init(self.astAllocator());

        if (!self.check(.pipe)) {
            while (true) {
                const param_start = self.currentLocation();
                const param_name = try self.expectIdentifier("parameter name");
                const param_ident = makeIdentifier(param_name, param_start);

                var type_ann: ?*TypeExpr = null;
                if (self.match(.colon)) {
                    type_ann = try self.parseTypeExpr();
                }

                const param = try self.astAllocator().create(LambdaParam);
                param.* = .{
                    .name = param_ident,
                    .type_expr = type_ann,
                    .span = makeSpan(param_start, self.previousLocation()),
                };
                try params.append(param);

                if (!self.match(.comma)) break;
            }
        }

        try self.expect(.pipe, "'|' after lambda parameters");

        var return_type: ?*TypeExpr = null;
        if (self.match(.arrow)) {
            return_type = try self.parseTypeExpr();
        }

        var body: LambdaExpr.LambdaBody = undefined;
        if (self.check(.lbrace)) {
            body = .{ .block = try self.parseBlockExpr() };
        } else {
            body = .{ .expression = try self.parseExpr() };
        }

        const lambda = try self.astAllocator().create(LambdaExpr);
        lambda.* = .{
            .params = try params.toOwnedSlice(),
            .return_type = return_type,
            .body = body,
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        const expr = try self.astAllocator().create(Expr);
        expr.* = .{
            .kind = .{ .lambda = lambda },
            .span = lambda.span,
        };
        return expr;
    }

    /// Parse struct literal
    fn parseStructLiteral(self: *Self, start_loc: SourceLocation, path: Path) !*Expr {
        try self.expect(.lbrace, "'{' for struct literal");

        var fields = std.ArrayList(*StructLiteral.FieldInit).init(self.astAllocator());
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const field_start = self.currentLocation();
            const field_name = try self.expectIdentifier("field name");
            const field_ident = makeIdentifier(field_name, field_start);

            try self.expect(.colon, "':' after field name");
            const field_value = try self.parseExpr();

            const field_init = try self.astAllocator().create(StructLiteral.FieldInit);
            field_init.* = .{
                .name = field_ident,
                .value = field_value,
                .span = makeSpan(field_start, self.previousLocation()),
            };
            try fields.append(field_init);

            if (!self.match(.comma)) {
                if (!self.check(.rbrace)) {
                    return self.errorAtCurrent("Expected ',' or '}' after field");
                }
            }
        }

        try self.expect(.rbrace, "'}' after struct fields");

        const struct_lit = try self.astAllocator().create(StructLiteral);
        struct_lit.* = .{
            .type_path = path,
            .fields = try fields.toOwnedSlice(),
            .spread = null,
            .span = makeSpan(start_loc, self.previousLocation()),
        };

        const expr = try self.astAllocator().create(Expr);
        expr.* = .{
            .kind = .{ .struct_literal = struct_lit },
            .span = struct_lit.span,
        };
        return expr;
    }

    // ========================================================================
    // Pattern Parsing
    // ========================================================================

    /// Parse a pattern
    pub fn parsePattern(self: *Self) anyerror!*Pattern {
        const start_loc = self.currentLocation();

        // Wildcard pattern
        if (self.match(.underscore)) {
            const pattern = try self.astAllocator().create(Pattern);
            pattern.* = .{
                .kind = .wildcard,
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return self.parsePatternSuffix(pattern);
        }

        // Literal patterns
        if (self.match(.integer) or self.match(.float) or self.match(.string) or
            self.match(.kw_true) or self.match(.kw_false))
        {
            // Rewind and parse as literal
            self.current -= 1;
            const lit_expr = try self.parsePrimary();

            const lit = switch (lit_expr.kind) {
                .literal => |l| l,
                else => return self.errorAtCurrent("Expected literal pattern"),
            };

            const pattern = try self.astAllocator().create(Pattern);
            pattern.* = .{
                .kind = .{ .literal = lit },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return self.parsePatternSuffix(pattern);
        }

        // Tuple pattern: (a, b, c)
        if (self.match(.lparen)) {
            var elements = std.ArrayList(*Pattern).init(self.astAllocator());
            if (!self.check(.rparen)) {
                const first = try self.parsePattern();
                try elements.append(first);
                while (self.match(.comma)) {
                    if (self.check(.rparen)) break;
                    const elem = try self.parsePattern();
                    try elements.append(elem);
                }
            }
            try self.expect(.rparen, "')' after tuple pattern");

            const tuple_pattern = try self.astAllocator().create(TuplePattern);
            tuple_pattern.* = .{
                .elements = try elements.toOwnedSlice(),
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const pattern = try self.astAllocator().create(Pattern);
            pattern.* = .{
                .kind = .{ .tuple = tuple_pattern },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return self.parsePatternSuffix(pattern);
        }

        // Slice pattern: [a, b, ..rest]
        if (self.match(.lbracket)) {
            var elements = std.ArrayList(*Pattern).init(self.astAllocator());
            var rest: ?SlicePattern.RestPattern = null;
            var rest_position: usize = 0;

            while (!self.check(.rbracket) and !self.isAtEnd()) {
                if (self.match(.dot_dot)) {
                    // Rest pattern
                    var binding: ?Identifier = null;
                    if (self.check(.identifier)) {
                        const rest_name = try self.expectIdentifier("rest binding name");
                        binding = makeIdentifier(rest_name, self.previousLocation());
                    }
                    rest = .{
                        .binding = binding,
                        .position = rest_position,
                    };
                    break;
                }
                const elem = try self.parsePattern();
                try elements.append(elem);
                rest_position += 1;
                if (!self.match(.comma)) break;
            }
            try self.expect(.rbracket, "']' after slice pattern");

            const slice_pattern = try self.astAllocator().create(SlicePattern);
            slice_pattern.* = .{
                .elements = try elements.toOwnedSlice(),
                .rest = rest,
                .span = makeSpan(start_loc, self.previousLocation()),
            };

            const pattern = try self.astAllocator().create(Pattern);
            pattern.* = .{
                .kind = .{ .slice = slice_pattern },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return self.parsePatternSuffix(pattern);
        }

        // Identifier, variant, or struct pattern
        if (self.match(.identifier)) {
            const name = self.previous().lexeme;
            const name_ident = makeIdentifier(name, start_loc);

            // Build path if needed
            var segments = std.ArrayList(Identifier).init(self.astAllocator());
            try segments.append(name_ident);

            while (self.match(.colon_colon)) {
                const seg = try self.expectIdentifier("path segment");
                try segments.append(makeIdentifier(seg, self.previousLocation()));
            }

            // Save all segments before toOwnedSlice moves them
            const all_segments = try self.astAllocator().dupe(Identifier, segments.items);
            const path: ?Path = if (all_segments.len > 1) Path{
                .segments = all_segments,
                .span = makeSpan(start_loc, self.previousLocation()),
            } else null;

            // Check for variant pattern: Some(x) or Shape::Circle(r)
            if (self.match(.lparen)) {
                var payload: EnumVariantPattern.Payload = .none;
                if (!self.check(.rparen)) {
                    var payload_patterns = std.ArrayList(*Pattern).init(self.astAllocator());
                    const first = try self.parsePattern();
                    try payload_patterns.append(first);
                    while (self.match(.comma)) {
                        if (self.check(.rparen)) break;
                        const p = try self.parsePattern();
                        try payload_patterns.append(p);
                    }
                    payload = .{ .tuple = try payload_patterns.toOwnedSlice() };
                }
                try self.expect(.rparen, "')' after variant payload");

                // For multi-segment paths, the last segment is the variant name
                // and the preceding segments form the type path
                var variant_name = name_ident;
                var type_path_val = path;
                if (all_segments.len > 1) {
                    variant_name = all_segments[all_segments.len - 1];
                    type_path_val = Path{
                        .segments = all_segments[0 .. all_segments.len - 1],
                        .span = makeSpan(start_loc, self.previousLocation()),
                    };
                }

                const variant_pattern = try self.astAllocator().create(EnumVariantPattern);
                variant_pattern.* = .{
                    .type_path = type_path_val,
                    .variant = variant_name,
                    .payload = payload,
                    .span = makeSpan(start_loc, self.previousLocation()),
                };

                const pattern = try self.astAllocator().create(Pattern);
                pattern.* = .{
                    .kind = .{ .enum_variant = variant_pattern },
                    .span = makeSpan(start_loc, self.previousLocation()),
                };
                return self.parsePatternSuffix(pattern);
            }

            // Check for struct pattern: Point { x, y }
            if (self.match(.lbrace)) {
                var fields = std.ArrayList(*FieldPattern).init(self.astAllocator());
                var has_rest = false;

                while (!self.check(.rbrace) and !self.isAtEnd()) {
                    if (self.match(.dot_dot)) {
                        has_rest = true;
                        break;
                    }

                    const field_start = self.currentLocation();
                    const field_name = try self.expectIdentifier("field name");
                    const field_ident = makeIdentifier(field_name, field_start);

                    var field_pattern: ?*Pattern = null;
                    if (self.match(.colon)) {
                        field_pattern = try self.parsePattern();
                    }

                    const fp = try self.astAllocator().create(FieldPattern);
                    fp.* = .{
                        .name = field_ident,
                        .pattern = field_pattern,
                        .span = makeSpan(field_start, self.previousLocation()),
                    };
                    try fields.append(fp);

                    if (!self.match(.comma)) break;
                }
                try self.expect(.rbrace, "'}' after struct pattern");

                const struct_pattern = try self.astAllocator().create(StructPattern);
                struct_pattern.* = .{
                    .type_path = path,
                    .fields = try fields.toOwnedSlice(),
                    .rest = has_rest,
                    .span = makeSpan(start_loc, self.previousLocation()),
                };

                const pattern = try self.astAllocator().create(Pattern);
                pattern.* = .{
                    .kind = .{ .struct_pattern = struct_pattern },
                    .span = makeSpan(start_loc, self.previousLocation()),
                };
                return self.parsePatternSuffix(pattern);
            }

            // Multi-segment path without payload is a unit enum variant pattern
            if (path != null) {
                const last_seg = all_segments[all_segments.len - 1];
                const type_path = Path{
                    .segments = all_segments[0 .. all_segments.len - 1],
                    .span = makeSpan(start_loc, self.previousLocation()),
                };

                const variant_pattern = try self.astAllocator().create(EnumVariantPattern);
                variant_pattern.* = .{
                    .type_path = type_path,
                    .variant = last_seg,
                    .payload = .none,
                    .span = makeSpan(start_loc, self.previousLocation()),
                };

                const pattern = try self.astAllocator().create(Pattern);
                pattern.* = .{
                    .kind = .{ .enum_variant = variant_pattern },
                    .span = makeSpan(start_loc, self.previousLocation()),
                };
                return self.parsePatternSuffix(pattern);
            }

            // Simple identifier binding
            const pattern = try self.astAllocator().create(Pattern);
            pattern.* = .{
                .kind = .{
                    .identifier = .{
                        .name = name_ident,
                        .is_mut = false,
                        .binding = null,
                    },
                },
                .span = makeSpan(start_loc, self.previousLocation()),
            };
            return self.parsePatternSuffix(pattern);
        }

        return self.errorAtCurrent("Expected pattern");
    }

    /// Parse pattern suffix (| for or-patterns)
    fn parsePatternSuffix(self: *Self, first: *Pattern) !*Pattern {
        if (!self.match(.pipe)) {
            return first;
        }

        var patterns = std.ArrayList(*Pattern).init(self.astAllocator());
        try patterns.append(first);

        while (true) {
            const next = try self.parsePattern();
            try patterns.append(next);
            if (!self.match(.pipe)) break;
        }

        const or_pattern = try self.astAllocator().create(OrPattern);
        or_pattern.* = .{
            .patterns = try patterns.toOwnedSlice(),
            .span = Span.merge(first.span, patterns.items[patterns.items.len - 1].span),
        };

        const pattern = try self.astAllocator().create(Pattern);
        pattern.* = .{
            .kind = .{ .or_pattern = or_pattern },
            .span = or_pattern.span,
        };
        return pattern;
    }

    // ========================================================================
    // Helper Functions
    // ========================================================================

    /// Get the precedence of the current token
    fn getCurrentPrecedence(self: *Self) Precedence {
        return switch (self.peek().type) {
            .pipe_gt => .pipeline,
            .kw_or => .@"or",
            .kw_and => .@"and",
            .eq_eq, .bang_eq => .equality,
            .lt, .gt, .lt_eq, .gt_eq => .comparison,
            .pipe => .bit_or,
            .caret => .bit_xor,
            .ampersand => .bit_and,
            .lt_lt, .gt_gt => .shift,
            .plus, .minus => .term,
            .star, .slash, .percent => .factor,
            .dot_dot, .dot_dot_eq => .comparison,
            else => .none,
        };
    }

    /// Convert token type to binary operator
    fn tokenToBinaryOp(token_type: TokenType) ?BinaryExpr.BinaryOp {
        return switch (token_type) {
            .plus => .add,
            .minus => .sub,
            .star => .mul,
            .slash => .div,
            .percent => .mod,
            .eq_eq => .eq,
            .bang_eq => .ne,
            .lt => .lt,
            .gt => .gt,
            .lt_eq => .le,
            .gt_eq => .ge,
            .kw_and => .@"and",
            .kw_or => .@"or",
            .ampersand => .bit_and,
            .pipe => .bit_or,
            .caret => .bit_xor,
            .lt_lt => .shl,
            .gt_gt => .shr,
            .kw_in => .in,
            else => null,
        };
    }

    /// Match compound assignment operator
    fn matchCompoundAssignment(self: *Self) ?Assignment.AssignOp {
        if (self.match(.plus_eq)) return .add_assign;
        if (self.match(.minus_eq)) return .sub_assign;
        if (self.match(.star_eq)) return .mul_assign;
        if (self.match(.slash_eq)) return .div_assign;
        if (self.match(.percent_eq)) return .mod_assign;
        if (self.match(.ampersand_eq)) return .bit_and_assign;
        if (self.match(.pipe_eq)) return .bit_or_assign;
        if (self.match(.caret_eq)) return .bit_xor_assign;
        if (self.match(.lt_lt_eq)) return .shl_assign;
        if (self.match(.gt_gt_eq)) return .shr_assign;
        return null;
    }

    /// Check if at end of tokens
    fn isAtEnd(self: *Self) bool {
        return self.peek().type == .eof;
    }

    /// Look at current token
    fn peek(self: *Self) Token {
        return self.tokens[self.current];
    }

    /// Look at previous token
    fn previous(self: *Self) Token {
        if (self.current == 0) {
            // Return EOF token at start if we haven't advanced yet
            return self.tokens[0];
        }
        return self.tokens[self.current - 1];
    }

    /// Advance to next token
    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return self.previous();
    }

    /// Check if current token matches type
    fn check(self: *Self, token_type: TokenType) bool {
        return self.peek().type == token_type;
    }

    /// Match and consume if current token matches type
    fn match(self: *Self, token_type: TokenType) bool {
        if (self.check(token_type)) {
            _ = self.advance();
            return true;
        }
        return false;
    }

    /// Expect a specific token type
    fn expect(self: *Self, token_type: TokenType, message: []const u8) !void {
        if (self.check(token_type)) {
            _ = self.advance();
            return;
        }
        return self.errorAtCurrent(message);
    }

    /// Expect an identifier
    fn expectIdentifier(self: *Self, context: []const u8) ![]const u8 {
        if (self.check(.identifier)) {
            const name = self.advance().lexeme;
            return name;
        }
        return self.errorExpecting("identifier", context);
    }

    /// Get current source location
    fn currentLocation(self: *Self) SourceLocation {
        return self.peek().location;
    }

    /// Get previous source location
    fn previousLocation(self: *Self) SourceLocation {
        return self.previous().location;
    }

    /// Report error at current token
    fn errorAtCurrent(self: *Self, message: []const u8) anyerror {
        const token = self.peek();
        const err = ParseError{
            .message = message,
            .location = token.location,
            .expected = null,
            .found = token.lexeme,
        };
        self.errors.append(err) catch {};
        self.panic_mode = true;
        return error.ParseError;
    }

    /// Report error expecting something
    fn errorExpecting(self: *Self, expected: []const u8, context: []const u8) anyerror {
        const token = self.peek();
        var buf: [256]u8 = undefined;
        const message = std.fmt.bufPrint(&buf, "Expected {s} for {s}", .{ expected, context }) catch "Parse error";
        const err = ParseError{
            .message = message,
            .location = token.location,
            .expected = expected,
            .found = token.lexeme,
        };
        self.errors.append(err) catch {};
        self.panic_mode = true;
        return error.ParseError;
    }

    /// Synchronize after error for recovery
    fn synchronize(self: *Self) void {
        self.panic_mode = false;

        while (!self.isAtEnd()) {
            // Stop at statement boundaries
            if (self.previous().type == .semicolon or self.previous().type == .rbrace) return;

            // Stop at declaration keywords
            switch (self.peek().type) {
                .kw_fn,
                .kw_struct,
                .kw_enum,
                .kw_trait,
                .kw_impl,
                .kw_import,
                .kw_let,
                .kw_const,
                .kw_if,
                .kw_while,
                .kw_for,
                .kw_loop,
                .kw_return,
                .kw_match,
                => return,
                else => {},
            }

            _ = self.advance();
        }
    }

    /// Check if there were any parse errors
    pub fn hasErrors(self: *Self) bool {
        return self.errors.items.len > 0;
    }

    /// Get all accumulated errors
    pub fn getErrors(self: *Self) []const ParseError {
        return self.errors.items;
    }
};

// ============================================================================
// Convenience Function
// ============================================================================

/// Parse source code into a SourceFile AST
pub fn parseSource(source: []const u8, allocator: Allocator) !*SourceFile {
    var lex = Lexer.init(source, allocator);
    defer lex.deinit();

    const tokens = try lex.scanAll();
    defer allocator.free(tokens);

    var parser = Parser.init(tokens, allocator);
    defer parser.deinit();

    return parser.parse();
}

// ============================================================================
// TESTS
// ============================================================================

const testing = std.testing;

/// Test context with arena allocator for AST nodes (avoids memory leaks in tests)
const TestContext = struct {
    arena: std.heap.ArenaAllocator,

    pub fn init() TestContext {
        return .{ .arena = std.heap.ArenaAllocator.init(testing.allocator) };
    }

    pub fn deinit(self: *TestContext) void {
        self.arena.deinit();
    }

    pub fn parse(self: *TestContext, source: []const u8) !*SourceFile {
        const alloc = self.arena.allocator();
        var lex = Lexer.init(source, alloc);
        const tokens = try lex.scanAll();
        var parser = Parser.init(tokens, alloc);
        return parser.parse();
    }
};

test "parse empty source" {
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse("");
    try testing.expectEqual(@as(usize, 0), source_file.declarations.len);
}

test "parse simple function" {
    const source =
        \\fn add(a: int, b: int) -> int {
        \\    return a + b
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    try testing.expectEqual(@as(usize, 1), source_file.declarations.len);

    const decl = source_file.declarations[0];
    try testing.expect(decl.kind == .function);

    const func = decl.kind.function;
    try testing.expectEqualStrings("add", func.name.name);
    try testing.expectEqual(@as(usize, 2), func.params.len);
}

test "parse function with generics" {
    const source =
        \\fn first[T](list: List[T]) -> Option[T] {
        \\    return list.get(0)
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    try testing.expectEqual(@as(usize, 1), source_file.declarations.len);

    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.generic_params != null);
    try testing.expectEqual(@as(usize, 1), func.generic_params.?.len);
    try testing.expectEqualStrings("T", func.generic_params.?[0].name.name);
}

test "parse function with effects" {
    const source =
        \\fn read_file(path: str) -> str with [IO, Error] {
        \\    return ""
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.effects != null);
    try testing.expectEqual(@as(usize, 2), func.effects.?.len);
}

test "parse function with contracts" {
    const source =
        \\fn divide(a: int, b: int) -> int
        \\    requires b != 0
        \\    ensures result >= 0
        \\{
        \\    return a / b
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    try testing.expectEqual(@as(usize, 1), source_file.declarations.len);

    const func = source_file.declarations[0].kind.function;
    try testing.expectEqualStrings("divide", func.name.name);
    try testing.expect(func.contracts != null);
    try testing.expectEqual(@as(usize, 1), func.contracts.?.requires.len);
    try testing.expectEqual(@as(usize, 1), func.contracts.?.ensures.len);
}

test "parse struct definition" {
    const source =
        \\struct Point {
        \\    x: int,
        \\    y: int,
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    try testing.expectEqual(@as(usize, 1), source_file.declarations.len);

    const s = source_file.declarations[0].kind.struct_def;
    try testing.expectEqualStrings("Point", s.name.name);
    try testing.expectEqual(@as(usize, 2), s.fields.len);
    try testing.expectEqualStrings("x", s.fields[0].name.name);
    try testing.expectEqualStrings("y", s.fields[1].name.name);
}

test "parse generic struct" {
    const source =
        \\struct Pair[A, B] {
        \\    first: A,
        \\    second: B,
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const s = source_file.declarations[0].kind.struct_def;
    try testing.expect(s.generic_params != null);
    try testing.expectEqual(@as(usize, 2), s.generic_params.?.len);
}

test "parse enum definition" {
    const source =
        \\enum Option[T] {
        \\    Some(T),
        \\    None,
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    try testing.expectEqual(@as(usize, 1), source_file.declarations.len);

    const e = source_file.declarations[0].kind.enum_def;
    try testing.expectEqualStrings("Option", e.name.name);
    try testing.expectEqual(@as(usize, 2), e.variants.len);
    try testing.expectEqualStrings("Some", e.variants[0].name.name);
    try testing.expectEqualStrings("None", e.variants[1].name.name);
}

test "parse trait definition" {
    const source =
        \\trait Iterator[T] {
        \\    fn next(self: &mut Self) -> Option[T]
        \\    fn has_next(self: &Self) -> bool
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const t = source_file.declarations[0].kind.trait_def;
    try testing.expectEqualStrings("Iterator", t.name.name);
    try testing.expectEqual(@as(usize, 2), t.items.len);
}

test "parse impl block" {
    const source =
        \\impl Display for Point {
        \\    fn display(self: &Self) -> str {
        \\        return "Point"
        \\    }
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const impl = source_file.declarations[0].kind.impl_block;
    try testing.expect(impl.trait_type != null);
    try testing.expectEqual(@as(usize, 1), impl.items.len);
}

test "parse let binding" {
    const source =
        \\fn main() {
        \\    let x = 42
        \\    let mut y: int = 10
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
    try testing.expectEqual(@as(usize, 2), func.body.?.block.statements.len);
}

test "parse if expression" {
    const source =
        \\fn test() -> int {
        \\    if x > 0 {
        \\        1
        \\    } else {
        \\        0
        \\    }
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse match expression" {
    const source =
        \\fn test(opt: Option[int]) -> int {
        \\    match opt {
        \\        Some(x) => x,
        \\        None => 0,
        \\    }
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse binary expressions with precedence" {
    const source =
        \\fn test() -> int {
        \\    1 + 2 * 3
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    const body = func.body.?.block;

    // Should be trailing expression
    try testing.expect(body.result != null);
    const expr = body.result.?;

    // Should be: 1 + (2 * 3), so top level is add
    try testing.expect(expr.kind == .binary);
    try testing.expect(expr.kind.binary.op == .add);

    // Right side should be multiplication
    const right = expr.kind.binary.right;
    try testing.expect(right.kind == .binary);
    try testing.expect(right.kind.binary.op == .mul);
}

test "parse pipeline expression" {
    const source =
        \\fn test() {
        \\    data |> parse |> validate
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse for loop" {
    const source =
        \\fn test() {
        \\    for i in 0..10 {
        \\        print(i)
        \\    }
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
    try testing.expect(func.body.?.block.statements.len > 0);
    try testing.expect(func.body.?.block.statements[0].kind == .for_loop);
}

test "parse while loop" {
    const source =
        \\fn test() {
        \\    while x > 0 {
        \\        x = x - 1
        \\    }
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
    try testing.expect(func.body.?.block.statements.len > 0);
    try testing.expect(func.body.?.block.statements[0].kind == .while_loop);
}

test "parse loop" {
    const source =
        \\fn test() {
        \\    loop {
        \\        break 42
        \\    }
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
    try testing.expect(func.body.?.block.statements.len > 0);
    try testing.expect(func.body.?.block.statements[0].kind == .loop_stmt);
}

test "parse array literal" {
    const source =
        \\fn test() {
        \\    let arr = [1, 2, 3]
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse lambda expression" {
    const source =
        \\fn test() {
        \\    let f = |x, y| x + y
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse struct literal" {
    const source =
        \\fn test() {
        \\    let p = Point { x: 1, y: 2 }
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse complex nested expression" {
    const source =
        \\fn test() {
        \\    foo(bar.baz[0]).method(1 + 2, true)? ?? default
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse unary expressions" {
    const source =
        \\fn test() {
        \\    let a = -x
        \\    let b = not y
        \\    let c = !z
        \\    let d = ~bits
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 4), func.body.?.block.statements.len);
}

test "parse pattern matching patterns" {
    const source =
        \\fn test(val: Value) {
        \\    match val {
        \\        Point { x, y } => use(x, y),
        \\        Some(inner) => use(inner),
        \\        (a, b, c) => use(a, b, c),
        \\        [first, second, ..rest] => use(first),
        \\        _ => default(),
        \\    }
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse private declarations" {
    const source =
        \\private fn internal() {}
        \\private struct Hidden {}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    try testing.expectEqual(@as(usize, 2), source_file.declarations.len);
    try testing.expect(source_file.declarations[0].visibility == .private);
    try testing.expect(source_file.declarations[1].visibility == .private);
}

test "parse import statement" {
    const source =
        \\import std::io::{read, write}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    try testing.expectEqual(@as(usize, 1), source_file.imports.len);
    const imp = source_file.imports[0];
    try testing.expectEqual(@as(usize, 2), imp.path.segments.len);
    try testing.expectEqualStrings("std", imp.path.segments[0].name);
    try testing.expectEqualStrings("io", imp.path.segments[1].name);
}

test "parse const declaration" {
    const source =
        \\const PI: float = 3.14159
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    try testing.expectEqual(@as(usize, 1), source_file.declarations.len);
    try testing.expect(source_file.declarations[0].kind == .constant);
    try testing.expectEqualStrings("PI", source_file.declarations[0].kind.constant.name.name);
}

test "parse tuple type and literal" {
    const source =
        \\fn test() -> (int, str, bool) {
        \\    (42, "hello", true)
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.return_type != null);
    try testing.expect(func.return_type.?.kind == .tuple);
}

test "parse method call chain" {
    const source =
        \\fn test() {
        \\    obj.first().second().third()
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse compound assignment" {
    const source =
        \\fn test() {
        \\    x += 1
        \\    y -= 2
        \\    z *= 3
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 3), func.body.?.block.statements.len);
    try testing.expect(func.body.?.block.statements[0].kind == .assignment);
}

test "parse logical operators" {
    const source =
        \\fn test() {
        \\    a and b or c
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
    // 'or' has lower precedence than 'and', so: (a and b) or c
    const expr = func.body.?.block.result.?;
    try testing.expect(expr.kind == .binary);
    try testing.expect(expr.kind.binary.op == .@"or");
}

test "parse comparison chain" {
    const source =
        \\fn test() {
        \\    x == y and y < z
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse else if chain" {
    const source =
        \\fn test(x: int) -> str {
        \\    if x < 0 {
        \\        "negative"
        \\    } else if x == 0 {
        \\        "zero"
        \\    } else {
        \\        "positive"
        \\    }
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse bitwise operators" {
    const source =
        \\fn test() {
        \\    let a = x & y
        \\    let b = x | y
        \\    let c = x << 2
        \\    let d = x >> 2
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 4), func.body.?.block.statements.len);
}

test "parse type expressions" {
    const source =
        \\fn test(
        \\    a: int,
        \\    b: ?str,
        \\    c: &int,
        \\    d: &mut int,
        \\    e: List[int],
        \\    f: Map[str, int],
        \\    g: fn(int) -> bool,
        \\    h: [int; 10],
        \\) {}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 8), func.params.len);
}

test "parse trait with super traits" {
    const source =
        \\trait Ord: Eq + PartialOrd {
        \\    fn cmp(self: &Self, other: &Self) -> Ordering
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const t = source_file.declarations[0].kind.trait_def;
    try testing.expectEqual(@as(usize, 2), t.super_traits.len);
}

test "parse match with guards" {
    const source =
        \\fn test(x: int) -> str {
        \\    match x {
        \\        n if n < 0 => "negative",
        \\        0 => "zero",
        \\        n if n > 100 => "large",
        \\        _ => "normal",
        \\    }
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse range expressions" {
    const source =
        \\fn test() {
        \\    let a = 0..10
        \\    let b = 0..=10
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
    try testing.expectEqual(@as(usize, 2), func.body.?.block.statements.len);
}

test "parse array repeat syntax" {
    const source =
        \\fn test() {
        \\    let zeros = [0; 100]
        \\}
    ;
    var ctx = TestContext.init();
    defer ctx.deinit();
    const source_file = try ctx.parse(source);
    const func = source_file.declarations[0].kind.function;
    try testing.expect(func.body != null);
}
