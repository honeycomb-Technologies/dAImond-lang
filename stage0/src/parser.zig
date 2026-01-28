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

// ============================================================================
// AST Node Definitions (inline until ast.zig is created)
// ============================================================================

/// Unique identifier for AST nodes
pub const NodeId = u32;

/// Span in source code
pub const Span = struct {
    start: SourceLocation,
    end: SourceLocation,

    pub fn merge(a: Span, b: Span) Span {
        return .{
            .start = if (a.start.offset < b.start.offset) a.start else b.start,
            .end = if (a.end.offset > b.end.offset) a.end else b.end,
        };
    }
};

/// Binary operators
pub const BinaryOp = enum {
    // Arithmetic
    add,
    sub,
    mul,
    div,
    mod,
    power,
    // Comparison
    eq,
    neq,
    lt,
    gt,
    lte,
    gte,
    // Logical
    @"and",
    @"or",
    // Bitwise
    bit_and,
    bit_or,
    bit_xor,
    shl,
    shr,
    // Pipeline
    pipeline,
    // Range
    range,
    range_inclusive,
};

/// Unary operators
pub const UnaryOp = enum {
    neg,
    not,
    bit_not,
    try_op, // ?
    unwrap, // ??
};

/// Pattern kinds for pattern matching
pub const PatternKind = union(enum) {
    /// Wildcard pattern: _
    wildcard,
    /// Identifier binding: x
    identifier: []const u8,
    /// Literal pattern: 42, "hello", true
    literal: *const Expr,
    /// Tuple pattern: (a, b, c)
    tuple: []const *Pattern,
    /// Struct pattern: Point { x, y }
    @"struct": struct {
        name: []const u8,
        fields: []const FieldPattern,
    },
    /// Variant pattern: Some(x), None
    variant: struct {
        name: []const u8,
        payload: ?*Pattern,
    },
    /// Array/slice pattern: [first, second, ..rest]
    array: struct {
        elements: []const *Pattern,
        rest: ?[]const u8, // rest binding name
    },
    /// Or pattern: a | b
    @"or": []const *Pattern,
    /// Guard pattern: pattern if condition
    guard: struct {
        pattern: *Pattern,
        condition: *Expr,
    },
    /// Range pattern: 1..10
    range: struct {
        start: ?*Expr,
        end: ?*Expr,
        inclusive: bool,
    },
};

/// Field pattern for struct destructuring
pub const FieldPattern = struct {
    name: []const u8,
    pattern: ?*Pattern, // null means bind to same name
};

/// Pattern node
pub const Pattern = struct {
    kind: PatternKind,
    span: Span,
};

/// Type expression kinds
pub const TypeExprKind = union(enum) {
    /// Simple type name: int, String
    name: []const u8,
    /// Generic type: List[T], Map[K, V]
    generic: struct {
        name: []const u8,
        args: []const *TypeExpr,
    },
    /// Function type: fn(int, int) -> int
    function: struct {
        params: []const *TypeExpr,
        ret: *TypeExpr,
        effects: []const []const u8,
    },
    /// Tuple type: (int, String, bool)
    tuple: []const *TypeExpr,
    /// Array type: [int]
    array: struct {
        element: *TypeExpr,
        size: ?*Expr,
    },
    /// Optional type: ?int
    optional: *TypeExpr,
    /// Reference type: &T, &mut T
    reference: struct {
        inner: *TypeExpr,
        mutable: bool,
    },
    /// Inferred type: _
    inferred,
};

/// Type expression node
pub const TypeExpr = struct {
    kind: TypeExprKind,
    span: Span,
};

/// Expression kinds
pub const ExprKind = union(enum) {
    /// Integer literal: 42
    int_literal: i128,
    /// Float literal: 3.14
    float_literal: f64,
    /// String literal: "hello"
    string_literal: []const u8,
    /// Character literal: 'a'
    char_literal: u8,
    /// Boolean literal: true, false
    bool_literal: bool,
    /// Identifier: foo
    identifier: []const u8,
    /// Binary operation: a + b
    binary: struct {
        op: BinaryOp,
        left: *Expr,
        right: *Expr,
    },
    /// Unary operation: -x, not x
    unary: struct {
        op: UnaryOp,
        operand: *Expr,
    },
    /// Function call: foo(a, b)
    call: struct {
        callee: *Expr,
        args: []const *Expr,
    },
    /// Method call: obj.method(args)
    method_call: struct {
        receiver: *Expr,
        method: []const u8,
        args: []const *Expr,
    },
    /// Field access: obj.field
    field_access: struct {
        object: *Expr,
        field: []const u8,
    },
    /// Index access: arr[i]
    index: struct {
        object: *Expr,
        index: *Expr,
    },
    /// Array literal: [1, 2, 3]
    array_literal: []const *Expr,
    /// Tuple literal: (1, "hello", true)
    tuple_literal: []const *Expr,
    /// Struct literal: Point { x: 1, y: 2 }
    struct_literal: struct {
        name: ?[]const u8,
        fields: []const FieldInit,
    },
    /// Block expression: { ... }
    block: *Block,
    /// If expression: if cond { ... } else { ... }
    @"if": struct {
        condition: *Expr,
        then_branch: *Block,
        else_branch: ?*Expr,
    },
    /// Match expression
    match: struct {
        scrutinee: *Expr,
        arms: []const MatchArm,
    },
    /// Lambda: |x, y| x + y
    lambda: struct {
        params: []const LambdaParam,
        body: *Expr,
    },
    /// Return expression
    @"return": ?*Expr,
    /// Break expression
    @"break": ?*Expr,
    /// Continue expression
    @"continue",
    /// Comptime expression
    comptime_expr: *Expr,
    /// Error propagation: expr?
    try_expr: *Expr,
    /// Null coalescing: expr ?? default
    coalesce: struct {
        value: *Expr,
        default: *Expr,
    },
    /// Grouped expression (parentheses): (expr)
    grouped: *Expr,
};

/// Field initialization in struct literal
pub const FieldInit = struct {
    name: []const u8,
    value: *Expr,
};

/// Match arm
pub const MatchArm = struct {
    pattern: *Pattern,
    guard: ?*Expr,
    body: *Expr,
};

/// Lambda parameter
pub const LambdaParam = struct {
    name: []const u8,
    type_annotation: ?*TypeExpr,
};

/// Expression node
pub const Expr = struct {
    kind: ExprKind,
    span: Span,
};

/// Statement kinds
pub const StmtKind = union(enum) {
    /// Expression statement
    expr: *Expr,
    /// Let binding: let x = value, let mut x: Type = value
    let_binding: struct {
        pattern: *Pattern,
        type_annotation: ?*TypeExpr,
        value: ?*Expr,
        mutable: bool,
    },
    /// Assignment: x = value
    assignment: struct {
        target: *Expr,
        value: *Expr,
    },
    /// Compound assignment: x += value
    compound_assignment: struct {
        target: *Expr,
        op: BinaryOp,
        value: *Expr,
    },
    /// While loop
    while_loop: struct {
        condition: *Expr,
        body: *Block,
    },
    /// For loop
    for_loop: struct {
        pattern: *Pattern,
        iterable: *Expr,
        body: *Block,
    },
    /// Loop (infinite)
    loop: *Block,
    /// Return statement
    @"return": ?*Expr,
    /// Break statement
    @"break": ?*Expr,
    /// Continue statement
    @"continue",
    /// Discard statement: discard expr
    discard: *Expr,
    /// Expect statement: expect condition
    expect: *Expr,
};

/// Statement node
pub const Stmt = struct {
    kind: StmtKind,
    span: Span,
};

/// Block of statements with optional trailing expression
pub const Block = struct {
    statements: []const *Stmt,
    expr: ?*Expr, // trailing expression (block value)
    span: Span,
};

/// Function parameter
pub const Param = struct {
    name: []const u8,
    type_annotation: *TypeExpr,
    default_value: ?*Expr,
    mutable: bool,
};

/// Generic parameter
pub const GenericParam = struct {
    name: []const u8,
    bounds: []const *TypeExpr,
};

/// Contract clause (requires/ensures)
pub const Contract = struct {
    kind: enum { requires, ensures },
    condition: *Expr,
    message: ?[]const u8,
};

/// Function definition
pub const Function = struct {
    name: []const u8,
    generic_params: []const GenericParam,
    params: []const Param,
    return_type: ?*TypeExpr,
    effects: []const []const u8,
    contracts: []const Contract,
    body: ?*Block,
    is_private: bool,
    span: Span,
};

/// Struct field
pub const StructField = struct {
    name: []const u8,
    type_annotation: *TypeExpr,
    default_value: ?*Expr,
    is_private: bool,
};

/// Struct definition
pub const Struct = struct {
    name: []const u8,
    generic_params: []const GenericParam,
    fields: []const StructField,
    is_private: bool,
    span: Span,
};

/// Enum variant
pub const EnumVariant = struct {
    name: []const u8,
    fields: ?[]const StructField, // for tuple/struct variants
    discriminant: ?*Expr,
};

/// Enum definition
pub const Enum = struct {
    name: []const u8,
    generic_params: []const GenericParam,
    variants: []const EnumVariant,
    is_private: bool,
    span: Span,
};

/// Trait method signature
pub const TraitMethod = struct {
    name: []const u8,
    generic_params: []const GenericParam,
    params: []const Param,
    return_type: ?*TypeExpr,
    effects: []const []const u8,
    default_impl: ?*Block,
};

/// Trait definition
pub const Trait = struct {
    name: []const u8,
    generic_params: []const GenericParam,
    super_traits: []const *TypeExpr,
    methods: []const TraitMethod,
    is_private: bool,
    span: Span,
};

/// Impl block
pub const Impl = struct {
    generic_params: []const GenericParam,
    trait_type: ?*TypeExpr, // null for inherent impl
    target_type: *TypeExpr,
    methods: []const *Function,
    span: Span,
};

/// Import item
pub const ImportItem = struct {
    name: []const u8,
    alias: ?[]const u8,
};

/// Import declaration
pub const Import = struct {
    path: []const []const u8,
    items: ?[]const ImportItem, // null means import all (*)
    span: Span,
};

/// Top-level declaration kinds
pub const DeclKind = union(enum) {
    function: *Function,
    @"struct": *Struct,
    @"enum": *Enum,
    trait: *Trait,
    impl: *Impl,
    import: *Import,
    constant: struct {
        name: []const u8,
        type_annotation: ?*TypeExpr,
        value: *Expr,
        is_private: bool,
    },
};

/// Top-level declaration
pub const Decl = struct {
    kind: DeclKind,
    span: Span,
};

/// Module/Program AST root
pub const Module = struct {
    name: ?[]const u8,
    declarations: []const *Decl,
    span: Span,
};

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
    power = 12, // ^ (right associative)
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

    /// Clean up parser resources
    pub fn deinit(self: *Self) void {
        self.errors.deinit();
    }

    // ========================================================================
    // Entry Point
    // ========================================================================

    /// Parse the entire source into a Module AST
    pub fn parse(self: *Self) !*Module {
        var declarations = std.ArrayList(*Decl).init(self.allocator);
        errdefer declarations.deinit();

        const start_loc = self.currentLocation();

        while (!self.isAtEnd()) {
            if (self.parseDeclaration()) |decl| {
                try declarations.append(decl);
            } else |_| {
                self.synchronize();
            }
        }

        const module = try self.allocator.create(Module);
        module.* = .{
            .name = null,
            .declarations = try declarations.toOwnedSlice(),
            .span = .{
                .start = start_loc,
                .end = self.previousLocation(),
            },
        };
        return module;
    }

    // ========================================================================
    // Declaration Parsing
    // ========================================================================

    /// Parse a top-level declaration
    pub fn parseDeclaration(self: *Self) !*Decl {
        const start_loc = self.currentLocation();
        const is_private = self.match(.kw_private);

        const decl = try self.allocator.create(Decl);

        if (self.match(.kw_fn)) {
            decl.* = .{
                .kind = .{ .function = try self.parseFunction(is_private) },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
        } else if (self.match(.kw_struct)) {
            decl.* = .{
                .kind = .{ .@"struct" = try self.parseStruct(is_private) },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
        } else if (self.match(.kw_enum)) {
            decl.* = .{
                .kind = .{ .@"enum" = try self.parseEnum(is_private) },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
        } else if (self.match(.kw_trait)) {
            decl.* = .{
                .kind = .{ .trait = try self.parseTrait(is_private) },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
        } else if (self.match(.kw_impl)) {
            decl.* = .{
                .kind = .{ .impl = try self.parseImpl() },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
        } else if (self.match(.kw_import)) {
            decl.* = .{
                .kind = .{ .import = try self.parseImport() },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
        } else if (self.match(.kw_const)) {
            const name = try self.expectIdentifier("constant name");
            const type_ann = if (self.match(.colon)) try self.parseTypeExpr() else null;
            try self.expect(.eq, "'=' after constant name");
            const value = try self.parseExpr();
            decl.* = .{
                .kind = .{
                    .constant = .{
                        .name = name,
                        .type_annotation = type_ann,
                        .value = value,
                        .is_private = is_private,
                    },
                },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
        } else {
            return self.errorAtCurrent("Expected declaration (fn, struct, enum, trait, impl, import, or const)");
        }

        return decl;
    }

    /// Parse a function definition
    pub fn parseFunction(self: *Self, is_private: bool) !*Function {
        const start_loc = self.currentLocation();
        const name = try self.expectIdentifier("function name");

        // Generic parameters
        const generic_params = try self.parseGenericParams();

        // Parameters
        try self.expect(.lparen, "'(' after function name");
        const params = try self.parseFunctionParams();
        try self.expect(.rparen, "')' after parameters");

        // Return type
        var return_type: ?*TypeExpr = null;
        if (self.match(.arrow)) {
            return_type = try self.parseTypeExpr();
        }

        // Effects
        var effects = std.ArrayList([]const u8).init(self.allocator);
        if (self.match(.kw_with)) {
            try self.expect(.lbracket, "'[' after 'with'");
            while (!self.check(.rbracket) and !self.isAtEnd()) {
                const effect = try self.expectIdentifier("effect name");
                try effects.append(effect);
                if (!self.match(.comma)) break;
            }
            try self.expect(.rbracket, "']' after effects");
        }

        // Contracts
        var contracts = std.ArrayList(Contract).init(self.allocator);
        while (self.match(.kw_requires) or self.match(.kw_ensures)) {
            const kind: @TypeOf(contracts.items[0].kind) = if (self.previous().type == .kw_requires) .requires else .ensures;
            const condition = try self.parseExpr();
            var message: ?[]const u8 = null;
            if (self.match(.comma)) {
                if (self.match(.string)) {
                    message = self.previous().lexeme;
                }
            }
            try contracts.append(.{
                .kind = kind,
                .condition = condition,
                .message = message,
            });
        }

        // Body (optional for trait method signatures)
        var body: ?*Block = null;
        if (self.check(.lbrace)) {
            body = try self.parseBlock();
        }

        const func = try self.allocator.create(Function);
        func.* = .{
            .name = name,
            .generic_params = try generic_params.toOwnedSlice(),
            .params = try params.toOwnedSlice(),
            .return_type = return_type,
            .effects = try effects.toOwnedSlice(),
            .contracts = try contracts.toOwnedSlice(),
            .body = body,
            .is_private = is_private,
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return func;
    }

    /// Parse generic parameters: [T, U: Trait]
    fn parseGenericParams(self: *Self) !std.ArrayList(GenericParam) {
        var params = std.ArrayList(GenericParam).init(self.allocator);
        if (!self.match(.lbracket)) return params;

        while (!self.check(.rbracket) and !self.isAtEnd()) {
            const name = try self.expectIdentifier("generic parameter name");
            var bounds = std.ArrayList(*TypeExpr).init(self.allocator);

            if (self.match(.colon)) {
                // Parse trait bounds
                const bound = try self.parseTypeExpr();
                try bounds.append(bound);
                while (self.match(.plus)) {
                    const next_bound = try self.parseTypeExpr();
                    try bounds.append(next_bound);
                }
            }

            try params.append(.{
                .name = name,
                .bounds = try bounds.toOwnedSlice(),
            });

            if (!self.match(.comma)) break;
        }

        try self.expect(.rbracket, "']' after generic parameters");
        return params;
    }

    /// Parse function parameters
    fn parseFunctionParams(self: *Self) !std.ArrayList(Param) {
        var params = std.ArrayList(Param).init(self.allocator);

        while (!self.check(.rparen) and !self.isAtEnd()) {
            const mutable = self.match(.kw_mut);
            const name = try self.expectIdentifier("parameter name");
            try self.expect(.colon, "':' after parameter name");
            const type_ann = try self.parseTypeExpr();

            var default_val: ?*Expr = null;
            if (self.match(.eq)) {
                default_val = try self.parseExpr();
            }

            try params.append(.{
                .name = name,
                .type_annotation = type_ann,
                .default_value = default_val,
                .mutable = mutable,
            });

            if (!self.match(.comma)) break;
        }

        return params;
    }

    /// Parse a struct definition
    pub fn parseStruct(self: *Self, is_private: bool) !*Struct {
        const start_loc = self.currentLocation();
        const name = try self.expectIdentifier("struct name");
        const generic_params = try self.parseGenericParams();

        try self.expect(.lbrace, "'{' after struct name");

        var fields = std.ArrayList(StructField).init(self.allocator);
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const field_private = self.match(.kw_private);
            const field_name = try self.expectIdentifier("field name");
            try self.expect(.colon, "':' after field name");
            const field_type = try self.parseTypeExpr();

            var default_val: ?*Expr = null;
            if (self.match(.eq)) {
                default_val = try self.parseExpr();
            }

            try fields.append(.{
                .name = field_name,
                .type_annotation = field_type,
                .default_value = default_val,
                .is_private = field_private,
            });

            if (!self.match(.comma)) {
                // Allow trailing comma or no comma before }
                if (!self.check(.rbrace)) {
                    return self.errorAtCurrent("Expected ',' or '}' after field");
                }
            }
        }

        try self.expect(.rbrace, "'}' after struct fields");

        const s = try self.allocator.create(Struct);
        s.* = .{
            .name = name,
            .generic_params = try generic_params.toOwnedSlice(),
            .fields = try fields.toOwnedSlice(),
            .is_private = is_private,
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return s;
    }

    /// Parse an enum definition
    pub fn parseEnum(self: *Self, is_private: bool) !*Enum {
        const start_loc = self.currentLocation();
        const name = try self.expectIdentifier("enum name");
        const generic_params = try self.parseGenericParams();

        try self.expect(.lbrace, "'{' after enum name");

        var variants = std.ArrayList(EnumVariant).init(self.allocator);
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const variant_name = try self.expectIdentifier("variant name");

            var variant_fields: ?[]const StructField = null;
            var discriminant: ?*Expr = null;

            if (self.match(.lparen)) {
                // Tuple-style variant: Some(T)
                var fields = std.ArrayList(StructField).init(self.allocator);
                var idx: usize = 0;
                while (!self.check(.rparen) and !self.isAtEnd()) {
                    const field_type = try self.parseTypeExpr();
                    var field_name_buf: [32]u8 = undefined;
                    const field_name = std.fmt.bufPrint(&field_name_buf, "_{}", .{idx}) catch "field";
                    try fields.append(.{
                        .name = try self.allocator.dupe(u8, field_name),
                        .type_annotation = field_type,
                        .default_value = null,
                        .is_private = false,
                    });
                    idx += 1;
                    if (!self.match(.comma)) break;
                }
                try self.expect(.rparen, "')' after variant fields");
                variant_fields = try fields.toOwnedSlice();
            } else if (self.match(.lbrace)) {
                // Struct-style variant
                var fields = std.ArrayList(StructField).init(self.allocator);
                while (!self.check(.rbrace) and !self.isAtEnd()) {
                    const field_name = try self.expectIdentifier("field name");
                    try self.expect(.colon, "':' after field name");
                    const field_type = try self.parseTypeExpr();
                    try fields.append(.{
                        .name = field_name,
                        .type_annotation = field_type,
                        .default_value = null,
                        .is_private = false,
                    });
                    if (!self.match(.comma)) break;
                }
                try self.expect(.rbrace, "'}' after variant fields");
                variant_fields = try fields.toOwnedSlice();
            }

            if (self.match(.eq)) {
                discriminant = try self.parseExpr();
            }

            try variants.append(.{
                .name = variant_name,
                .fields = variant_fields,
                .discriminant = discriminant,
            });

            if (!self.match(.comma)) {
                if (!self.check(.rbrace)) {
                    return self.errorAtCurrent("Expected ',' or '}' after variant");
                }
            }
        }

        try self.expect(.rbrace, "'}' after enum variants");

        const e = try self.allocator.create(Enum);
        e.* = .{
            .name = name,
            .generic_params = try generic_params.toOwnedSlice(),
            .variants = try variants.toOwnedSlice(),
            .is_private = is_private,
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return e;
    }

    /// Parse a trait definition
    pub fn parseTrait(self: *Self, is_private: bool) !*Trait {
        const start_loc = self.currentLocation();
        const name = try self.expectIdentifier("trait name");
        const generic_params = try self.parseGenericParams();

        // Super traits: trait Child: Parent1 + Parent2
        var super_traits = std.ArrayList(*TypeExpr).init(self.allocator);
        if (self.match(.colon)) {
            const first = try self.parseTypeExpr();
            try super_traits.append(first);
            while (self.match(.plus)) {
                const next = try self.parseTypeExpr();
                try super_traits.append(next);
            }
        }

        try self.expect(.lbrace, "'{' after trait name");

        var methods = std.ArrayList(TraitMethod).init(self.allocator);
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            try self.expect(.kw_fn, "'fn' for trait method");
            const method_name = try self.expectIdentifier("method name");
            const method_generics = try self.parseGenericParams();

            try self.expect(.lparen, "'(' after method name");
            const params = try self.parseFunctionParams();
            try self.expect(.rparen, "')' after parameters");

            var return_type: ?*TypeExpr = null;
            if (self.match(.arrow)) {
                return_type = try self.parseTypeExpr();
            }

            var effects = std.ArrayList([]const u8).init(self.allocator);
            if (self.match(.kw_with)) {
                try self.expect(.lbracket, "'[' after 'with'");
                while (!self.check(.rbracket) and !self.isAtEnd()) {
                    const effect = try self.expectIdentifier("effect name");
                    try effects.append(effect);
                    if (!self.match(.comma)) break;
                }
                try self.expect(.rbracket, "']' after effects");
            }

            var default_impl: ?*Block = null;
            if (self.check(.lbrace)) {
                default_impl = try self.parseBlock();
            }

            try methods.append(.{
                .name = method_name,
                .generic_params = try method_generics.toOwnedSlice(),
                .params = try params.toOwnedSlice(),
                .return_type = return_type,
                .effects = try effects.toOwnedSlice(),
                .default_impl = default_impl,
            });

            // Allow optional comma/semicolon between methods
            _ = self.match(.comma) or self.match(.semicolon);
        }

        try self.expect(.rbrace, "'}' after trait body");

        const t = try self.allocator.create(Trait);
        t.* = .{
            .name = name,
            .generic_params = try generic_params.toOwnedSlice(),
            .super_traits = try super_traits.toOwnedSlice(),
            .methods = try methods.toOwnedSlice(),
            .is_private = is_private,
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return t;
    }

    /// Parse an impl block
    pub fn parseImpl(self: *Self) !*Impl {
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

        var methods = std.ArrayList(*Function).init(self.allocator);
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const is_private = self.match(.kw_private);
            try self.expect(.kw_fn, "'fn' for impl method");
            const func = try self.parseFunction(is_private);
            try methods.append(func);
        }

        try self.expect(.rbrace, "'}' after impl body");

        const impl = try self.allocator.create(Impl);
        impl.* = .{
            .generic_params = try generic_params.toOwnedSlice(),
            .trait_type = trait_type,
            .target_type = target_type,
            .methods = try methods.toOwnedSlice(),
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return impl;
    }

    /// Parse an import declaration
    fn parseImport(self: *Self) !*Import {
        const start_loc = self.currentLocation();

        var path = std.ArrayList([]const u8).init(self.allocator);
        const first = try self.expectIdentifier("module path");
        try path.append(first);

        while (self.match(.colon_colon)) {
            const next = try self.expectIdentifier("module path segment");
            try path.append(next);
        }

        var items: ?[]const ImportItem = null;
        if (self.match(.lbrace)) {
            var import_items = std.ArrayList(ImportItem).init(self.allocator);
            while (!self.check(.rbrace) and !self.isAtEnd()) {
                const item_name = try self.expectIdentifier("import item");
                var alias: ?[]const u8 = null;
                // Could add 'as' keyword handling here
                try import_items.append(.{
                    .name = item_name,
                    .alias = alias,
                });
                if (!self.match(.comma)) break;
            }
            try self.expect(.rbrace, "'}' after import items");
            items = try import_items.toOwnedSlice();
        }

        const imp = try self.allocator.create(Import);
        imp.* = .{
            .path = try path.toOwnedSlice(),
            .items = items,
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return imp;
    }

    // ========================================================================
    // Type Expression Parsing
    // ========================================================================

    /// Parse a type expression
    pub fn parseTypeExpr(self: *Self) anyerror!*TypeExpr {
        const start_loc = self.currentLocation();
        var type_expr = try self.allocator.create(TypeExpr);

        // Optional type: ?T
        if (self.match(.question)) {
            const inner = try self.parseTypeExpr();
            type_expr.* = .{
                .kind = .{ .optional = inner },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return type_expr;
        }

        // Reference type: &T or &mut T
        if (self.match(.ampersand)) {
            const mutable = self.match(.kw_mut);
            const inner = try self.parseTypeExpr();
            type_expr.* = .{
                .kind = .{ .reference = .{ .inner = inner, .mutable = mutable } },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return type_expr;
        }

        // Function type: fn(T, U) -> V
        if (self.match(.kw_fn)) {
            try self.expect(.lparen, "'(' after 'fn' in type");
            var param_types = std.ArrayList(*TypeExpr).init(self.allocator);
            while (!self.check(.rparen) and !self.isAtEnd()) {
                const param_type = try self.parseTypeExpr();
                try param_types.append(param_type);
                if (!self.match(.comma)) break;
            }
            try self.expect(.rparen, "')' after function type parameters");

            var ret_type = try self.allocator.create(TypeExpr);
            ret_type.* = .{
                .kind = .{ .name = "void" },
                .span = .{ .start = start_loc, .end = start_loc },
            };
            if (self.match(.arrow)) {
                ret_type = try self.parseTypeExpr();
            }

            var effects = std.ArrayList([]const u8).init(self.allocator);
            if (self.match(.kw_with)) {
                try self.expect(.lbracket, "'[' after 'with' in type");
                while (!self.check(.rbracket) and !self.isAtEnd()) {
                    const effect = try self.expectIdentifier("effect name");
                    try effects.append(effect);
                    if (!self.match(.comma)) break;
                }
                try self.expect(.rbracket, "']' after effects in type");
            }

            type_expr.* = .{
                .kind = .{
                    .function = .{
                        .params = try param_types.toOwnedSlice(),
                        .ret = ret_type,
                        .effects = try effects.toOwnedSlice(),
                    },
                },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return type_expr;
        }

        // Tuple type: (T, U, V)
        if (self.match(.lparen)) {
            var types = std.ArrayList(*TypeExpr).init(self.allocator);
            if (!self.check(.rparen)) {
                const first = try self.parseTypeExpr();
                try types.append(first);
                while (self.match(.comma)) {
                    const next = try self.parseTypeExpr();
                    try types.append(next);
                }
            }
            try self.expect(.rparen, "')' after tuple type");
            type_expr.* = .{
                .kind = .{ .tuple = try types.toOwnedSlice() },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return type_expr;
        }

        // Array type: [T] or [T; N]
        if (self.match(.lbracket)) {
            const elem_type = try self.parseTypeExpr();
            var size: ?*Expr = null;
            if (self.match(.semicolon)) {
                size = try self.parseExpr();
            }
            try self.expect(.rbracket, "']' after array type");
            type_expr.* = .{
                .kind = .{ .array = .{ .element = elem_type, .size = size } },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return type_expr;
        }

        // Inferred type: _
        if (self.match(.underscore)) {
            type_expr.* = .{
                .kind = .inferred,
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return type_expr;
        }

        // Named type or generic type
        const name = try self.expectIdentifier("type name");

        // Check for generic arguments
        if (self.match(.lbracket)) {
            var args = std.ArrayList(*TypeExpr).init(self.allocator);
            while (!self.check(.rbracket) and !self.isAtEnd()) {
                const arg = try self.parseTypeExpr();
                try args.append(arg);
                if (!self.match(.comma)) break;
            }
            try self.expect(.rbracket, "']' after generic arguments");
            type_expr.* = .{
                .kind = .{
                    .generic = .{
                        .name = name,
                        .args = try args.toOwnedSlice(),
                    },
                },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
        } else {
            type_expr.* = .{
                .kind = .{ .name = name },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
        }

        return type_expr;
    }

    // ========================================================================
    // Statement Parsing
    // ========================================================================

    /// Parse a statement
    pub fn parseStatement(self: *Self) anyerror!*Stmt {
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
            const stmt = try self.allocator.create(Stmt);
            stmt.* = .{
                .kind = .{ .@"break" = value },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return stmt;
        }
        if (self.match(.kw_continue)) {
            _ = self.match(.semicolon);
            const stmt = try self.allocator.create(Stmt);
            stmt.* = .{
                .kind = .@"continue",
                .span = .{ .start = start_loc, .end = self.previousLocation() },
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
            const stmt = try self.allocator.create(Stmt);
            stmt.* = .{
                .kind = .{ .discard = expr },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return stmt;
        }
        if (self.match(.kw_expect)) {
            const expr = try self.parseExpr();
            _ = self.match(.semicolon);
            const stmt = try self.allocator.create(Stmt);
            stmt.* = .{
                .kind = .{ .expect = expr },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return stmt;
        }

        // Expression statement or assignment
        const expr = try self.parseExpr();

        // Check for assignment
        if (self.match(.eq)) {
            const value = try self.parseExpr();
            _ = self.match(.semicolon);
            const stmt = try self.allocator.create(Stmt);
            stmt.* = .{
                .kind = .{ .assignment = .{ .target = expr, .value = value } },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return stmt;
        }

        // Check for compound assignment
        if (self.matchCompoundAssignment()) |op| {
            const value = try self.parseExpr();
            _ = self.match(.semicolon);
            const stmt = try self.allocator.create(Stmt);
            stmt.* = .{
                .kind = .{ .compound_assignment = .{ .target = expr, .op = op, .value = value } },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return stmt;
        }

        _ = self.match(.semicolon);
        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{
            .kind = .{ .expr = expr },
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return stmt;
    }

    /// Parse let binding
    pub fn parseLetBinding(self: *Self, start_loc: SourceLocation) !*Stmt {
        const mutable = self.match(.kw_mut);
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

        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{
            .kind = .{
                .let_binding = .{
                    .pattern = pattern,
                    .type_annotation = type_ann,
                    .value = value,
                    .mutable = mutable,
                },
            },
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return stmt;
    }

    /// Parse return statement
    pub fn parseReturn(self: *Self, start_loc: SourceLocation) !*Stmt {
        var value: ?*Expr = null;
        if (!self.check(.rbrace) and !self.check(.semicolon) and !self.isAtEnd()) {
            value = try self.parseExpr();
        }
        _ = self.match(.semicolon);

        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{
            .kind = .{ .@"return" = value },
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return stmt;
    }

    /// Parse while loop
    pub fn parseWhile(self: *Self, start_loc: SourceLocation) !*Stmt {
        const condition = try self.parseExpr();
        const body = try self.parseBlock();

        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{
            .kind = .{ .while_loop = .{ .condition = condition, .body = body } },
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return stmt;
    }

    /// Parse for loop
    pub fn parseFor(self: *Self, start_loc: SourceLocation) !*Stmt {
        const pattern = try self.parsePattern();
        try self.expect(.kw_in, "'in' after for loop pattern");
        const iterable = try self.parseExpr();
        const body = try self.parseBlock();

        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{
            .kind = .{ .for_loop = .{ .pattern = pattern, .iterable = iterable, .body = body } },
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return stmt;
    }

    /// Parse infinite loop
    pub fn parseLoop(self: *Self, start_loc: SourceLocation) !*Stmt {
        const body = try self.parseBlock();

        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{
            .kind = .{ .loop = body },
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return stmt;
    }

    /// Parse a block: { ... }
    pub fn parseBlock(self: *Self) anyerror!*Block {
        const start_loc = self.currentLocation();
        try self.expect(.lbrace, "'{'");

        var statements = std.ArrayList(*Stmt).init(self.allocator);
        var trailing_expr: ?*Expr = null;

        while (!self.check(.rbrace) and !self.isAtEnd()) {
            // Check if this could be a trailing expression (no semicolon needed)
            const stmt = try self.parseStatement();

            // If it's an expression statement and we're at the end of the block,
            // it might be the trailing expression
            if (self.check(.rbrace)) {
                if (stmt.kind == .expr) {
                    trailing_expr = stmt.kind.expr;
                } else {
                    try statements.append(stmt);
                }
            } else {
                try statements.append(stmt);
            }
        }

        try self.expect(.rbrace, "'}'");

        const block = try self.allocator.create(Block);
        block.* = .{
            .statements = try statements.toOwnedSlice(),
            .expr = trailing_expr,
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return block;
    }

    // ========================================================================
    // Expression Parsing (Pratt Parser)
    // ========================================================================

    /// Parse an expression (entry point)
    pub fn parseExpr(self: *Self) anyerror!*Expr {
        return self.parsePrecedence(.pipeline);
    }

    /// Parse expression with given precedence level
    fn parsePrecedence(self: *Self, min_prec: Precedence) anyerror!*Expr {
        var left = try self.parseUnary();

        while (true) {
            const op_prec = self.getCurrentPrecedence();
            if (@intFromEnum(op_prec) < @intFromEnum(min_prec)) break;

            const op_token = self.advance();
            const op = tokenToBinaryOp(op_token.type) orelse break;

            // Handle right-associativity for power operator
            const next_prec = if (op == .power) op_prec else op_prec.next();

            const right = try self.parsePrecedence(next_prec);

            const new_left = try self.allocator.create(Expr);
            new_left.* = .{
                .kind = .{
                    .binary = .{
                        .op = op,
                        .left = left,
                        .right = right,
                    },
                },
                .span = Span.merge(left.span, right.span),
            };
            left = new_left;
        }

        return left;
    }

    /// Parse unary expression
    fn parseUnary(self: *Self) anyerror!*Expr {
        const start_loc = self.currentLocation();

        // Prefix operators
        if (self.match(.minus)) {
            const operand = try self.parseUnary();
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .unary = .{ .op = .neg, .operand = operand } },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }
        if (self.match(.kw_not) or self.match(.bang)) {
            const operand = try self.parseUnary();
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .unary = .{ .op = .not, .operand = operand } },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }
        if (self.match(.tilde)) {
            const operand = try self.parseUnary();
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .unary = .{ .op = .bit_not, .operand = operand } },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        return self.parsePostfix();
    }

    /// Parse postfix expression (calls, field access, indexing, ?, ??)
    fn parsePostfix(self: *Self) anyerror!*Expr {
        var expr = try self.parsePrimary();

        while (true) {
            if (self.match(.lparen)) {
                // Function call
                expr = try self.finishCall(expr);
            } else if (self.match(.lbracket)) {
                // Index access
                const index = try self.parseExpr();
                try self.expect(.rbracket, "']' after index");
                const new_expr = try self.allocator.create(Expr);
                new_expr.* = .{
                    .kind = .{ .index = .{ .object = expr, .index = index } },
                    .span = Span.merge(expr.span, .{ .start = self.previousLocation(), .end = self.previousLocation() }),
                };
                expr = new_expr;
            } else if (self.match(.dot)) {
                // Field access or method call
                const field = try self.expectIdentifier("field name");
                if (self.match(.lparen)) {
                    // Method call
                    var args = std.ArrayList(*Expr).init(self.allocator);
                    if (!self.check(.rparen)) {
                        const first = try self.parseExpr();
                        try args.append(first);
                        while (self.match(.comma)) {
                            const arg = try self.parseExpr();
                            try args.append(arg);
                        }
                    }
                    try self.expect(.rparen, "')' after method arguments");
                    const new_expr = try self.allocator.create(Expr);
                    new_expr.* = .{
                        .kind = .{
                            .method_call = .{
                                .receiver = expr,
                                .method = field,
                                .args = try args.toOwnedSlice(),
                            },
                        },
                        .span = Span.merge(expr.span, .{ .start = self.previousLocation(), .end = self.previousLocation() }),
                    };
                    expr = new_expr;
                } else {
                    // Field access
                    const new_expr = try self.allocator.create(Expr);
                    new_expr.* = .{
                        .kind = .{ .field_access = .{ .object = expr, .field = field } },
                        .span = Span.merge(expr.span, .{ .start = self.previousLocation(), .end = self.previousLocation() }),
                    };
                    expr = new_expr;
                }
            } else if (self.match(.question)) {
                // Try operator (error propagation)
                const new_expr = try self.allocator.create(Expr);
                new_expr.* = .{
                    .kind = .{ .try_expr = expr },
                    .span = Span.merge(expr.span, .{ .start = self.previousLocation(), .end = self.previousLocation() }),
                };
                expr = new_expr;
            } else if (self.match(.question_question)) {
                // Null coalescing
                const default = try self.parsePrecedence(.@"or");
                const new_expr = try self.allocator.create(Expr);
                new_expr.* = .{
                    .kind = .{ .coalesce = .{ .value = expr, .default = default } },
                    .span = Span.merge(expr.span, default.span),
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
        var args = std.ArrayList(*Expr).init(self.allocator);

        if (!self.check(.rparen)) {
            const first = try self.parseExpr();
            try args.append(first);
            while (self.match(.comma)) {
                const arg = try self.parseExpr();
                try args.append(arg);
            }
        }

        try self.expect(.rparen, "')' after arguments");

        const expr = try self.allocator.create(Expr);
        expr.* = .{
            .kind = .{
                .call = .{
                    .callee = callee,
                    .args = try args.toOwnedSlice(),
                },
            },
            .span = Span.merge(callee.span, .{ .start = self.previousLocation(), .end = self.previousLocation() }),
        };
        return expr;
    }

    /// Parse primary expression
    fn parsePrimary(self: *Self) anyerror!*Expr {
        const start_loc = self.currentLocation();

        // Integer literal
        if (self.match(.integer)) {
            const lexeme = self.previous().lexeme;
            // Remove underscores and parse
            var clean = std.ArrayList(u8).init(self.allocator);
            defer clean.deinit();
            for (lexeme) |c| {
                if (c != '_') try clean.append(c);
            }
            const value = std.fmt.parseInt(i128, clean.items, 10) catch 0;
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .int_literal = value },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        // Float literal
        if (self.match(.float)) {
            const lexeme = self.previous().lexeme;
            var clean = std.ArrayList(u8).init(self.allocator);
            defer clean.deinit();
            for (lexeme) |c| {
                if (c != '_') try clean.append(c);
            }
            const value = std.fmt.parseFloat(f64, clean.items) catch 0.0;
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .float_literal = value },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        // String literal
        if (self.match(.string) or self.match(.raw_string) or self.match(.byte_string)) {
            const lexeme = self.previous().lexeme;
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .string_literal = lexeme },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        // Character literal
        if (self.match(.char_literal)) {
            const lexeme = self.previous().lexeme;
            const value = if (lexeme.len >= 2) lexeme[1] else 0;
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .char_literal = value },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        // Boolean literals
        if (self.match(.kw_true)) {
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .bool_literal = true },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }
        if (self.match(.kw_false)) {
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .bool_literal = false },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
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

        // Return expression
        if (self.match(.kw_return)) {
            var value: ?*Expr = null;
            if (!self.check(.rbrace) and !self.check(.semicolon) and !self.isAtEnd()) {
                value = try self.parseExpr();
            }
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .@"return" = value },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        // Break expression
        if (self.match(.kw_break)) {
            var value: ?*Expr = null;
            if (!self.check(.rbrace) and !self.check(.semicolon) and !self.isAtEnd()) {
                value = try self.parseExpr();
            }
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .@"break" = value },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        // Continue expression
        if (self.match(.kw_continue)) {
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .@"continue",
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        // Comptime expression
        if (self.match(.kw_comptime)) {
            const inner = try self.parseExpr();
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .comptime_expr = inner },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        // Block expression
        if (self.check(.lbrace)) {
            const block = try self.parseBlock();
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .block = block },
                .span = block.span,
            };
            return expr;
        }

        // Array literal
        if (self.match(.lbracket)) {
            var elements = std.ArrayList(*Expr).init(self.allocator);
            if (!self.check(.rbracket)) {
                const first = try self.parseExpr();
                try elements.append(first);
                while (self.match(.comma)) {
                    if (self.check(.rbracket)) break; // trailing comma
                    const elem = try self.parseExpr();
                    try elements.append(elem);
                }
            }
            try self.expect(.rbracket, "']' after array elements");
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .array_literal = try elements.toOwnedSlice() },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
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
                const expr = try self.allocator.create(Expr);
                expr.* = .{
                    .kind = .{ .tuple_literal = &[_]*Expr{} },
                    .span = .{ .start = start_loc, .end = self.previousLocation() },
                };
                return expr;
            }

            const first = try self.parseExpr();

            if (self.match(.comma)) {
                // Tuple literal
                var elements = std.ArrayList(*Expr).init(self.allocator);
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
                const expr = try self.allocator.create(Expr);
                expr.* = .{
                    .kind = .{ .tuple_literal = try elements.toOwnedSlice() },
                    .span = .{ .start = start_loc, .end = self.previousLocation() },
                };
                return expr;
            }

            try self.expect(.rparen, "')' after expression");
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .grouped = first },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        // Identifier or struct literal
        if (self.match(.identifier)) {
            const name = self.previous().lexeme;

            // Check for struct literal: Name { ... }
            if (self.check(.lbrace)) {
                return self.parseStructLiteral(start_loc, name);
            }

            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .kind = .{ .identifier = name },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return expr;
        }

        return self.errorAtCurrent("Expected expression");
    }

    /// Parse if expression
    pub fn parseIf(self: *Self, start_loc: SourceLocation) !*Expr {
        const condition = try self.parseExpr();
        const then_branch = try self.parseBlock();

        var else_branch: ?*Expr = null;
        if (self.match(.kw_else)) {
            if (self.match(.kw_if)) {
                // else if
                else_branch = try self.parseIf(self.currentLocation());
            } else {
                // else block
                const else_block = try self.parseBlock();
                const else_expr = try self.allocator.create(Expr);
                else_expr.* = .{
                    .kind = .{ .block = else_block },
                    .span = else_block.span,
                };
                else_branch = else_expr;
            }
        }

        const expr = try self.allocator.create(Expr);
        expr.* = .{
            .kind = .{
                .@"if" = .{
                    .condition = condition,
                    .then_branch = then_branch,
                    .else_branch = else_branch,
                },
            },
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return expr;
    }

    /// Parse match expression
    pub fn parseMatch(self: *Self, start_loc: SourceLocation) !*Expr {
        const scrutinee = try self.parseExpr();
        try self.expect(.lbrace, "'{' after match scrutinee");

        var arms = std.ArrayList(MatchArm).init(self.allocator);
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const pattern = try self.parsePattern();

            var guard: ?*Expr = null;
            if (self.match(.kw_if)) {
                guard = try self.parseExpr();
            }

            try self.expect(.fat_arrow, "'=>' after match pattern");
            const body = try self.parseExpr();
            _ = self.match(.comma);

            try arms.append(.{
                .pattern = pattern,
                .guard = guard,
                .body = body,
            });
        }

        try self.expect(.rbrace, "'}' after match arms");

        const expr = try self.allocator.create(Expr);
        expr.* = .{
            .kind = .{
                .match = .{
                    .scrutinee = scrutinee,
                    .arms = try arms.toOwnedSlice(),
                },
            },
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return expr;
    }

    /// Parse lambda expression
    fn parseLambda(self: *Self, start_loc: SourceLocation) !*Expr {
        var params = std.ArrayList(LambdaParam).init(self.allocator);

        if (!self.check(.pipe)) {
            while (true) {
                const param_name = try self.expectIdentifier("parameter name");
                var type_ann: ?*TypeExpr = null;
                if (self.match(.colon)) {
                    type_ann = try self.parseTypeExpr();
                }
                try params.append(.{
                    .name = param_name,
                    .type_annotation = type_ann,
                });
                if (!self.match(.comma)) break;
            }
        }

        try self.expect(.pipe, "'|' after lambda parameters");

        const body = try self.parseExpr();

        const expr = try self.allocator.create(Expr);
        expr.* = .{
            .kind = .{
                .lambda = .{
                    .params = try params.toOwnedSlice(),
                    .body = body,
                },
            },
            .span = .{ .start = start_loc, .end = self.previousLocation() },
        };
        return expr;
    }

    /// Parse struct literal
    fn parseStructLiteral(self: *Self, start_loc: SourceLocation, name: []const u8) !*Expr {
        try self.expect(.lbrace, "'{' for struct literal");

        var fields = std.ArrayList(FieldInit).init(self.allocator);
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const field_name = try self.expectIdentifier("field name");
            try self.expect(.colon, "':' after field name");
            const field_value = try self.parseExpr();
            try fields.append(.{
                .name = field_name,
                .value = field_value,
            });
            if (!self.match(.comma)) {
                if (!self.check(.rbrace)) {
                    return self.errorAtCurrent("Expected ',' or '}' after field");
                }
            }
        }

        try self.expect(.rbrace, "'}' after struct fields");

        const expr = try self.allocator.create(Expr);
        expr.* = .{
            .kind = .{
                .struct_literal = .{
                    .name = name,
                    .fields = try fields.toOwnedSlice(),
                },
            },
            .span = .{ .start = start_loc, .end = self.previousLocation() },
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
            const pattern = try self.allocator.create(Pattern);
            pattern.* = .{
                .kind = .wildcard,
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return pattern;
        }

        // Literal patterns
        if (self.match(.integer) or self.match(.float) or self.match(.string) or
            self.match(.kw_true) or self.match(.kw_false))
        {
            // Rewind and parse as expression
            self.current -= 1;
            const expr = try self.parsePrimary();
            const pattern = try self.allocator.create(Pattern);
            pattern.* = .{
                .kind = .{ .literal = expr },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return pattern;
        }

        // Tuple pattern: (a, b, c)
        if (self.match(.lparen)) {
            var elements = std.ArrayList(*Pattern).init(self.allocator);
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
            const pattern = try self.allocator.create(Pattern);
            pattern.* = .{
                .kind = .{ .tuple = try elements.toOwnedSlice() },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return pattern;
        }

        // Array pattern: [a, b, ..rest]
        if (self.match(.lbracket)) {
            var elements = std.ArrayList(*Pattern).init(self.allocator);
            var rest: ?[]const u8 = null;

            while (!self.check(.rbracket) and !self.isAtEnd()) {
                if (self.match(.dot_dot)) {
                    // Rest pattern
                    rest = try self.expectIdentifier("rest binding name");
                    break;
                }
                const elem = try self.parsePattern();
                try elements.append(elem);
                if (!self.match(.comma)) break;
            }
            try self.expect(.rbracket, "']' after array pattern");
            const pattern = try self.allocator.create(Pattern);
            pattern.* = .{
                .kind = .{
                    .array = .{
                        .elements = try elements.toOwnedSlice(),
                        .rest = rest,
                    },
                },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
            };
            return pattern;
        }

        // Identifier or variant pattern
        if (self.match(.identifier)) {
            const name = self.previous().lexeme;

            // Check for variant pattern: Some(x) or None
            if (self.match(.lparen)) {
                var payload: ?*Pattern = null;
                if (!self.check(.rparen)) {
                    payload = try self.parsePattern();
                }
                try self.expect(.rparen, "')' after variant payload");
                const pattern = try self.allocator.create(Pattern);
                pattern.* = .{
                    .kind = .{ .variant = .{ .name = name, .payload = payload } },
                    .span = .{ .start = start_loc, .end = self.previousLocation() },
                };
                return self.parsePatternSuffix(pattern);
            }

            // Check for struct pattern: Point { x, y }
            if (self.match(.lbrace)) {
                var fields = std.ArrayList(FieldPattern).init(self.allocator);
                while (!self.check(.rbrace) and !self.isAtEnd()) {
                    const field_name = try self.expectIdentifier("field name");
                    var field_pattern: ?*Pattern = null;
                    if (self.match(.colon)) {
                        field_pattern = try self.parsePattern();
                    }
                    try fields.append(.{
                        .name = field_name,
                        .pattern = field_pattern,
                    });
                    if (!self.match(.comma)) break;
                }
                try self.expect(.rbrace, "'}' after struct pattern");
                const pattern = try self.allocator.create(Pattern);
                pattern.* = .{
                    .kind = .{ .@"struct" = .{ .name = name, .fields = try fields.toOwnedSlice() } },
                    .span = .{ .start = start_loc, .end = self.previousLocation() },
                };
                return self.parsePatternSuffix(pattern);
            }

            // Simple identifier binding
            const pattern = try self.allocator.create(Pattern);
            pattern.* = .{
                .kind = .{ .identifier = name },
                .span = .{ .start = start_loc, .end = self.previousLocation() },
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

        var patterns = std.ArrayList(*Pattern).init(self.allocator);
        try patterns.append(first);

        while (true) {
            const next = try self.parsePattern();
            try patterns.append(next);
            if (!self.match(.pipe)) break;
        }

        const pattern = try self.allocator.create(Pattern);
        pattern.* = .{
            .kind = .{ .@"or" = try patterns.toOwnedSlice() },
            .span = Span.merge(first.span, patterns.items[patterns.items.len - 1].span),
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
            .dot_dot, .dot_dot_eq => .comparison, // Range as comparison precedence
            else => .none,
        };
    }

    /// Convert token type to binary operator
    fn tokenToBinaryOp(token_type: TokenType) ?BinaryOp {
        return switch (token_type) {
            .plus => .add,
            .minus => .sub,
            .star => .mul,
            .slash => .div,
            .percent => .mod,
            .caret => .power,
            .eq_eq => .eq,
            .bang_eq => .neq,
            .lt => .lt,
            .gt => .gt,
            .lt_eq => .lte,
            .gt_eq => .gte,
            .kw_and => .@"and",
            .kw_or => .@"or",
            .ampersand => .bit_and,
            .pipe => .bit_or,
            .lt_lt => .shl,
            .gt_gt => .shr,
            .pipe_gt => .pipeline,
            .dot_dot => .range,
            .dot_dot_eq => .range_inclusive,
            else => null,
        };
    }

    /// Match compound assignment operator
    fn matchCompoundAssignment(self: *Self) ?BinaryOp {
        if (self.match(.plus_eq)) return .add;
        if (self.match(.minus_eq)) return .sub;
        if (self.match(.star_eq)) return .mul;
        if (self.match(.slash_eq)) return .div;
        if (self.match(.percent_eq)) return .mod;
        if (self.match(.ampersand_eq)) return .bit_and;
        if (self.match(.pipe_eq)) return .bit_or;
        if (self.match(.caret_eq)) return .power;
        if (self.match(.lt_lt_eq)) return .shl;
        if (self.match(.gt_gt_eq)) return .shr;
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

/// Parse source code into a Module AST
pub fn parseSource(source: []const u8, allocator: Allocator) !*Module {
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

fn testParse(source: []const u8) !*Module {
    return parseSource(source, testing.allocator);
}

fn expectNoErrors(parser: *Parser) !void {
    if (parser.hasErrors()) {
        for (parser.getErrors()) |err| {
            std.debug.print("Parse error: {}\n", .{err});
        }
        return error.UnexpectedErrors;
    }
}

test "parse empty source" {
    const module = try testParse("");
    try testing.expectEqual(@as(usize, 0), module.declarations.len);
}

test "parse simple function" {
    const source =
        \\fn add(a: int, b: int) -> int {
        \\    return a + b
        \\}
    ;
    const module = try testParse(source);
    try testing.expectEqual(@as(usize, 1), module.declarations.len);

    const decl = module.declarations[0];
    try testing.expect(decl.kind == .function);

    const func = decl.kind.function;
    try testing.expectEqualStrings("add", func.name);
    try testing.expectEqual(@as(usize, 2), func.params.len);
}

test "parse function with generics" {
    const source =
        \\fn first[T](list: List[T]) -> Option[T] {
        \\    return list.get(0)
        \\}
    ;
    const module = try testParse(source);
    try testing.expectEqual(@as(usize, 1), module.declarations.len);

    const func = module.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 1), func.generic_params.len);
    try testing.expectEqualStrings("T", func.generic_params[0].name);
}

test "parse function with effects" {
    const source =
        \\fn read_file(path: str) -> str with [IO, Error] {
        \\    return ""
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 2), func.effects.len);
    try testing.expectEqualStrings("IO", func.effects[0]);
    try testing.expectEqualStrings("Error", func.effects[1]);
}

test "parse function with contracts" {
    const source =
        \\fn divide(a: int, b: int) -> int
        \\    requires b != 0
        \\    ensures result * b == a
        \\{
        \\    return a / b
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 2), func.contracts.len);
    try testing.expect(func.contracts[0].kind == .requires);
    try testing.expect(func.contracts[1].kind == .ensures);
}

test "parse struct definition" {
    const source =
        \\struct Point {
        \\    x: int,
        \\    y: int,
        \\}
    ;
    const module = try testParse(source);
    try testing.expectEqual(@as(usize, 1), module.declarations.len);

    const s = module.declarations[0].kind.@"struct";
    try testing.expectEqualStrings("Point", s.name);
    try testing.expectEqual(@as(usize, 2), s.fields.len);
    try testing.expectEqualStrings("x", s.fields[0].name);
    try testing.expectEqualStrings("y", s.fields[1].name);
}

test "parse generic struct" {
    const source =
        \\struct Pair[A, B] {
        \\    first: A,
        \\    second: B,
        \\}
    ;
    const module = try testParse(source);
    const s = module.declarations[0].kind.@"struct";
    try testing.expectEqual(@as(usize, 2), s.generic_params.len);
}

test "parse enum definition" {
    const source =
        \\enum Option[T] {
        \\    Some(T),
        \\    None,
        \\}
    ;
    const module = try testParse(source);
    try testing.expectEqual(@as(usize, 1), module.declarations.len);

    const e = module.declarations[0].kind.@"enum";
    try testing.expectEqualStrings("Option", e.name);
    try testing.expectEqual(@as(usize, 2), e.variants.len);
    try testing.expectEqualStrings("Some", e.variants[0].name);
    try testing.expectEqualStrings("None", e.variants[1].name);
}

test "parse trait definition" {
    const source =
        \\trait Iterator[T] {
        \\    fn next(self: &mut Self) -> Option[T]
        \\    fn has_next(self: &Self) -> bool
        \\}
    ;
    const module = try testParse(source);
    const t = module.declarations[0].kind.trait;
    try testing.expectEqualStrings("Iterator", t.name);
    try testing.expectEqual(@as(usize, 2), t.methods.len);
}

test "parse impl block" {
    const source =
        \\impl Display for Point {
        \\    fn display(self: &Self) -> str {
        \\        return "Point"
        \\    }
        \\}
    ;
    const module = try testParse(source);
    const impl = module.declarations[0].kind.impl;
    try testing.expect(impl.trait_type != null);
    try testing.expectEqual(@as(usize, 1), impl.methods.len);
}

test "parse let binding" {
    const source =
        \\fn main() {
        \\    let x = 42
        \\    let mut y: int = 10
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
    try testing.expectEqual(@as(usize, 2), func.body.?.statements.len);
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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse binary expressions with precedence" {
    const source =
        \\fn test() -> int {
        \\    1 + 2 * 3
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    const body = func.body.?;

    // Should be trailing expression
    try testing.expect(body.expr != null);
    const expr = body.expr.?;

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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
    try testing.expect(func.body.?.statements.len > 0);
    try testing.expect(func.body.?.statements[0].kind == .for_loop);
}

test "parse while loop" {
    const source =
        \\fn test() {
        \\    while x > 0 {
        \\        x = x - 1
        \\    }
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
    try testing.expect(func.body.?.statements.len > 0);
    try testing.expect(func.body.?.statements[0].kind == .while_loop);
}

test "parse loop" {
    const source =
        \\fn test() {
        \\    loop {
        \\        break 42
        \\    }
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
    try testing.expect(func.body.?.statements.len > 0);
    try testing.expect(func.body.?.statements[0].kind == .loop);
}

test "parse array literal" {
    const source =
        \\fn test() {
        \\    let arr = [1, 2, 3]
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse lambda expression" {
    const source =
        \\fn test() {
        \\    let f = |x, y| x + y
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse struct literal" {
    const source =
        \\fn test() {
        \\    let p = Point { x: 1, y: 2 }
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse complex nested expression" {
    const source =
        \\fn test() {
        \\    foo(bar.baz[0]).method(1 + 2, true)?.unwrap() ?? default
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 4), func.body.?.statements.len);
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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
}

test "parse private declarations" {
    const source =
        \\private fn internal() {}
        \\private struct Hidden {}
    ;
    const module = try testParse(source);
    try testing.expectEqual(@as(usize, 2), module.declarations.len);
    try testing.expect(module.declarations[0].kind.function.is_private);
    try testing.expect(module.declarations[1].kind.@"struct".is_private);
}

test "parse import statement" {
    const source =
        \\import std::io::{read, write}
    ;
    const module = try testParse(source);
    try testing.expectEqual(@as(usize, 1), module.declarations.len);
    const imp = module.declarations[0].kind.import;
    try testing.expectEqual(@as(usize, 2), imp.path.len);
    try testing.expectEqualStrings("std", imp.path[0]);
    try testing.expectEqualStrings("io", imp.path[1]);
}

test "parse const declaration" {
    const source =
        \\const PI: float = 3.14159
    ;
    const module = try testParse(source);
    try testing.expectEqual(@as(usize, 1), module.declarations.len);
    try testing.expect(module.declarations[0].kind == .constant);
    try testing.expectEqualStrings("PI", module.declarations[0].kind.constant.name);
}

test "parse tuple type and literal" {
    const source =
        \\fn test() -> (int, str, bool) {
        \\    (42, "hello", true)
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.return_type != null);
    try testing.expect(func.return_type.?.kind == .tuple);
}

test "parse method call chain" {
    const source =
        \\fn test() {
        \\    obj.first().second().third()
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 3), func.body.?.statements.len);
    try testing.expect(func.body.?.statements[0].kind == .compound_assignment);
}

test "parse logical operators" {
    const source =
        \\fn test() {
        \\    a and b or c
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
    // 'or' has lower precedence than 'and', so: (a and b) or c
    const expr = func.body.?.expr.?;
    try testing.expect(expr.kind == .binary);
    try testing.expect(expr.kind.binary.op == .@"or");
}

test "parse comparison chain" {
    const source =
        \\fn test() {
        \\    x == y and y < z
        \\}
    ;
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 4), func.body.?.statements.len);
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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expectEqual(@as(usize, 8), func.params.len);
}

test "parse trait with super traits" {
    const source =
        \\trait Ord: Eq + PartialOrd {
        \\    fn cmp(self: &Self, other: &Self) -> Ordering
        \\}
    ;
    const module = try testParse(source);
    const t = module.declarations[0].kind.trait;
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
    const module = try testParse(source);
    const func = module.declarations[0].kind.function;
    try testing.expect(func.body != null);
}
