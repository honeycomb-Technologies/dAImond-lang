//! dAImond Abstract Syntax Tree
//!
//! This module defines the AST node types for the dAImond programming language.
//! The AST represents the syntactic structure of dAImond programs after parsing.
//!
//! Design principles:
//! - Use tagged unions (union(enum)) for variant types
//! - Include source location tracking via Span
//! - Support all dAImond language constructs
//! - Enable easy traversal and transformation

const std = @import("std");
const Allocator = std.mem.Allocator;

// ============================================================================
// Source Location
// ============================================================================

/// Represents a span of source code (start to end position)
pub const Span = struct {
    start: Position,
    end: Position,

    pub const Position = struct {
        line: usize,
        column: usize,
        offset: usize, // byte offset from start of source
    };

    /// Create a span from start and end positions
    pub fn init(start: Position, end: Position) Span {
        return .{ .start = start, .end = end };
    }

    /// Create a zero-length span at a position (for synthetic nodes)
    pub fn point(pos: Position) Span {
        return .{ .start = pos, .end = pos };
    }

    /// Create a span covering two other spans
    pub fn merge(a: Span, b: Span) Span {
        return .{
            .start = if (a.start.offset < b.start.offset) a.start else b.start,
            .end = if (a.end.offset > b.end.offset) a.end else b.end,
        };
    }

    /// A dummy span for testing or synthetic nodes
    pub const dummy: Span = .{
        .start = .{ .line = 0, .column = 0, .offset = 0 },
        .end = .{ .line = 0, .column = 0, .offset = 0 },
    };
};

// ============================================================================
// Identifiers and Names
// ============================================================================

/// An identifier with source location
pub const Identifier = struct {
    name: []const u8,
    span: Span,
};

/// A path like `std::io::print` or `module::Type`
pub const Path = struct {
    segments: []const Identifier,
    span: Span,
};

// ============================================================================
// Top-Level Declarations
// ============================================================================

/// A complete source file / module
pub const SourceFile = struct {
    module_decl: ?*ModuleDecl,
    imports: []const *ImportDecl,
    declarations: []const *Declaration,
    span: Span,
};

/// Module declaration: `module name`
pub const ModuleDecl = struct {
    name: Path,
    span: Span,
};

/// Import declaration: `import path { items }` or `import path`
pub const ImportDecl = struct {
    path: Path,
    items: ?[]const ImportItem,
    span: Span,

    pub const ImportItem = struct {
        name: Identifier,
        alias: ?Identifier, // for `import x { foo as bar }`
    };
};

/// A top-level declaration
pub const Declaration = struct {
    kind: Kind,
    visibility: Visibility,
    span: Span,

    pub const Visibility = enum {
        public,
        private,
    };

    pub const Kind = union(enum) {
        function: *FunctionDecl,
        struct_def: *StructDecl,
        enum_def: *EnumDecl,
        trait_def: *TraitDecl,
        impl_block: *ImplBlock,
        constant: *ConstDecl,
    };
};

/// Function declaration
pub const FunctionDecl = struct {
    name: Identifier,
    generic_params: ?[]const *GenericParam,
    params: []const *FunctionParam,
    return_type: ?*TypeExpr,
    effects: ?[]const *TypeExpr, // `with [Effect1, Effect2]`
    contracts: ?Contracts,
    body: ?FunctionBody,
    is_comptime: bool,
    is_extern: bool,
    is_async: bool,
    span: Span,

    pub const FunctionBody = union(enum) {
        block: *BlockExpr,
        expression: *Expr, // `fn foo() -> int = 42`
    };

    pub const Contracts = struct {
        requires: []const *Expr, // preconditions
        ensures: []const *Expr, // postconditions
    };
};

/// Function parameter
pub const FunctionParam = struct {
    name: Identifier,
    type_expr: *TypeExpr,
    default_value: ?*Expr,
    is_mut: bool,
    span: Span,
};

/// Generic type parameter: `T`, `T: Trait`, `T: Trait1 + Trait2`
pub const GenericParam = struct {
    name: Identifier,
    bounds: []const *TypeExpr, // trait bounds
    default: ?*TypeExpr,
    span: Span,
};

/// Struct declaration
pub const StructDecl = struct {
    name: Identifier,
    generic_params: ?[]const *GenericParam,
    fields: []const *StructField,
    span: Span,
};

/// Struct field
pub const StructField = struct {
    name: Identifier,
    type_expr: *TypeExpr,
    default_value: ?*Expr,
    visibility: Declaration.Visibility,
    span: Span,
};

/// Enum declaration (algebraic data type)
pub const EnumDecl = struct {
    name: Identifier,
    generic_params: ?[]const *GenericParam,
    variants: []const *EnumVariant,
    span: Span,
};

/// Enum variant
pub const EnumVariant = struct {
    name: Identifier,
    payload: Payload,
    span: Span,

    pub const Payload = union(enum) {
        none, // `None`
        tuple: []const *TypeExpr, // `Some(T)` or `Pair(A, B)`
        struct_fields: []const *StructField, // `Variant { field: Type }`
    };
};

/// Trait declaration
pub const TraitDecl = struct {
    name: Identifier,
    generic_params: ?[]const *GenericParam,
    super_traits: []const *TypeExpr, // trait bounds this extends
    items: []const *TraitItem,
    span: Span,
};

/// Item within a trait
pub const TraitItem = struct {
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        function: *FunctionDecl, // may have no body (abstract)
        type_alias: *TypeAliasDecl,
        constant: *ConstDecl,
    };
};

/// Type alias declaration
pub const TypeAliasDecl = struct {
    name: Identifier,
    generic_params: ?[]const *GenericParam,
    type_expr: ?*TypeExpr, // null for abstract associated types
    span: Span,
};

/// Implementation block
pub const ImplBlock = struct {
    generic_params: ?[]const *GenericParam,
    trait_type: ?*TypeExpr, // null for inherent impls
    target_type: *TypeExpr,
    where_clause: ?[]const *WherePredicate,
    items: []const *ImplItem,
    span: Span,
};

/// Where clause predicate: `where T: Trait`
pub const WherePredicate = struct {
    type_param: *TypeExpr,
    bounds: []const *TypeExpr,
    span: Span,
};

/// Item within an impl block
pub const ImplItem = struct {
    kind: Kind,
    visibility: Declaration.Visibility,
    span: Span,

    pub const Kind = union(enum) {
        function: *FunctionDecl,
        type_alias: *TypeAliasDecl,
        constant: *ConstDecl,
    };
};

/// Constant declaration
pub const ConstDecl = struct {
    name: Identifier,
    type_expr: ?*TypeExpr,
    value: ?*Expr, // null for trait associated constants
    span: Span,
};

// ============================================================================
// Statements
// ============================================================================

/// A statement
pub const Statement = struct {
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        let_binding: *LetBinding,
        return_stmt: *ReturnStmt,
        if_stmt: *IfExpr, // if can be statement or expression
        match_stmt: *MatchExpr,
        for_loop: *ForLoop,
        while_loop: *WhileLoop,
        loop_stmt: *LoopStmt,
        break_stmt: *BreakStmt,
        continue_stmt: *ContinueStmt,
        region_block: *RegionBlock,
        expression: *Expr, // expression statement
        assignment: *Assignment,
        discard: *DiscardStmt,
    };
};

/// Let binding: `let x = value`, `let mut x: Type = value`
pub const LetBinding = struct {
    pattern: *Pattern,
    type_annotation: ?*TypeExpr,
    value: ?*Expr,
    is_mut: bool,
    span: Span,
};

/// Assignment: `x = value`, `x.field = value`, `x[i] = value`
pub const Assignment = struct {
    target: *Expr,
    op: AssignOp,
    value: *Expr,
    span: Span,

    pub const AssignOp = enum {
        assign, // =
        add_assign, // +=
        sub_assign, // -=
        mul_assign, // *=
        div_assign, // /=
        mod_assign, // %=
        bit_and_assign, // &=
        bit_or_assign, // |=
        bit_xor_assign, // ^=
        shl_assign, // <<=
        shr_assign, // >>=
    };
};

/// Return statement: `return` or `return value`
pub const ReturnStmt = struct {
    value: ?*Expr,
    span: Span,
};

/// For loop: `for item in collection { }` or `for i in 0..10 { }`
pub const ForLoop = struct {
    label: ?Identifier,
    pattern: *Pattern,
    iterator: *Expr,
    body: *BlockExpr,
    span: Span,
};

/// While loop: `while condition { }`
pub const WhileLoop = struct {
    label: ?Identifier,
    condition: *Expr,
    body: *BlockExpr,
    span: Span,
};

/// Infinite loop: `loop { }`
pub const LoopStmt = struct {
    label: ?Identifier,
    body: *BlockExpr,
    span: Span,
};

/// Break statement: `break` or `break 'label` or `break value`
pub const BreakStmt = struct {
    label: ?Identifier,
    value: ?*Expr,
    span: Span,
};

/// Continue statement: `continue` or `continue 'label`
pub const ContinueStmt = struct {
    label: ?Identifier,
    span: Span,
};

/// Region block: `region name { }`
pub const RegionBlock = struct {
    name: Identifier,
    body: *BlockExpr,
    span: Span,
};

/// Discard statement: `discard expr`
pub const DiscardStmt = struct {
    value: *Expr,
    span: Span,
};

// ============================================================================
// Expressions
// ============================================================================

/// An expression
pub const Expr = struct {
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        // Literals
        literal: *Literal,

        // Identifiers and paths
        identifier: Identifier,
        path: Path,

        // Operators
        binary: *BinaryExpr,
        unary: *UnaryExpr,

        // Access
        field_access: *FieldAccess,
        index_access: *IndexAccess,
        method_call: *MethodCall,

        // Calls and construction
        function_call: *FunctionCall,
        struct_literal: *StructLiteral,
        enum_literal: *EnumLiteral,
        array_literal: *ArrayLiteral,
        tuple_literal: *TupleLiteral,

        // Control flow (expressions)
        if_expr: *IfExpr,
        match_expr: *MatchExpr,
        block: *BlockExpr,

        // Functions
        lambda: *LambdaExpr,

        // Special operators
        pipeline: *PipelineExpr,
        error_propagate: *ErrorPropagateExpr,
        coalesce: *CoalesceExpr,
        range: *RangeExpr,

        // Type operations
        cast: *CastExpr,
        type_check: *TypeCheckExpr,

        // String interpolation
        string_interpolation: *StringInterpolation,

        // Special
        grouped: *Expr, // parenthesized expression
        comptime_expr: *ComptimeExpr,
        await_expr: *AwaitExpr,
    };
};

/// Literal values
pub const Literal = struct {
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        int: IntLiteral,
        float: FloatLiteral,
        string: StringLiteral,
        char: CharLiteral,
        bool: bool,
        null_lit, // for nullable types
    };

    pub const IntLiteral = struct {
        value: []const u8, // raw text, can be parsed later
        suffix: ?[]const u8, // type suffix like i32, u64
    };

    pub const FloatLiteral = struct {
        value: []const u8,
        suffix: ?[]const u8,
    };

    pub const StringLiteral = struct {
        value: []const u8,
        kind: StringKind,

        pub const StringKind = enum {
            regular,
            raw, // r"..."
            byte, // b"..."
        };
    };

    pub const CharLiteral = struct {
        value: []const u8,
    };
};

/// Binary expression: `a + b`, `a and b`, `a == b`
pub const BinaryExpr = struct {
    left: *Expr,
    op: BinaryOp,
    right: *Expr,
    span: Span,

    pub const BinaryOp = enum {
        // Arithmetic
        add, // +
        sub, // -
        mul, // *
        div, // /
        mod, // %

        // Comparison
        eq, // ==
        ne, // !=
        lt, // <
        le, // <=
        gt, // >
        ge, // >=

        // Logical
        @"and", // and
        @"or", // or

        // Bitwise
        bit_and, // &
        bit_or, // |
        bit_xor, // ^
        shl, // <<
        shr, // >>

        // Membership
        in, // in
    };
};

/// Unary expression: `-x`, `not x`, `!x`
pub const UnaryExpr = struct {
    op: UnaryOp,
    operand: *Expr,
    span: Span,

    pub const UnaryOp = enum {
        neg, // -
        not, // not
        bit_not, // ~
        deref, // * (dereference)
        ref, // & (reference)
    };
};

/// Field access: `expr.field`
pub const FieldAccess = struct {
    object: *Expr,
    field: Identifier,
    span: Span,
};

/// Index access: `expr[index]`
pub const IndexAccess = struct {
    object: *Expr,
    index: *Expr,
    span: Span,
};

/// Method call: `expr.method(args)`
pub const MethodCall = struct {
    object: *Expr,
    method: Identifier,
    generic_args: ?[]const *TypeExpr,
    args: []const *Expr,
    span: Span,
};

/// Function call: `func(args)` or `func[T](args)`
pub const FunctionCall = struct {
    function: *Expr,
    generic_args: ?[]const *TypeExpr,
    args: []const *CallArg,
    span: Span,

    pub const CallArg = struct {
        name: ?Identifier, // for named arguments
        value: *Expr,
    };
};

/// Struct literal: `Point { x: 1, y: 2 }`
pub const StructLiteral = struct {
    type_path: ?Path,
    fields: []const *FieldInit,
    spread: ?*Expr, // `..other` for struct update syntax
    span: Span,

    pub const FieldInit = struct {
        name: Identifier,
        value: *Expr,
        span: Span,
    };
};

/// Enum variant construction: `Some(value)` or `Option::Some(value)`
pub const EnumLiteral = struct {
    type_path: ?Path,
    variant: Identifier,
    payload: Payload,
    span: Span,

    pub const Payload = union(enum) {
        none,
        tuple: []const *Expr,
        struct_fields: []const *StructLiteral.FieldInit,
    };
};

/// Array literal: `[1, 2, 3]` or `[0; 100]`
pub const ArrayLiteral = struct {
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        elements: []const *Expr,
        repeat: struct {
            value: *Expr,
            count: *Expr,
        },
    };
};

/// Tuple literal: `(a, b, c)`
pub const TupleLiteral = struct {
    elements: []const *Expr,
    span: Span,
};

/// If expression: `if cond { } else { }`
pub const IfExpr = struct {
    condition: *Expr,
    then_branch: *BlockExpr,
    else_branch: ?ElseBranch,
    span: Span,

    pub const ElseBranch = union(enum) {
        else_block: *BlockExpr,
        else_if: *IfExpr,
    };
};

/// Match expression
pub const MatchExpr = struct {
    scrutinee: *Expr,
    arms: []const *MatchArm,
    span: Span,
};

/// Match arm: `pattern => expression` or `pattern if guard => expression`
pub const MatchArm = struct {
    pattern: *Pattern,
    guard: ?*Expr,
    body: MatchBody,
    span: Span,

    pub const MatchBody = union(enum) {
        expression: *Expr,
        block: *BlockExpr,
    };
};

/// Block expression: `{ statements; expr }`
pub const BlockExpr = struct {
    statements: []const *Statement,
    result: ?*Expr, // final expression (no semicolon)
    span: Span,
};

/// Lambda expression: `|args| expr` or `|args| { body }`
pub const LambdaExpr = struct {
    params: []const *LambdaParam,
    return_type: ?*TypeExpr,
    body: LambdaBody,
    span: Span,

    pub const LambdaBody = union(enum) {
        expression: *Expr,
        block: *BlockExpr,
    };
};

/// Lambda parameter
pub const LambdaParam = struct {
    name: Identifier,
    type_expr: ?*TypeExpr,
    span: Span,
};

/// Pipeline expression: `expr |> func`
pub const PipelineExpr = struct {
    left: *Expr,
    right: *Expr,
    span: Span,
};

/// Error propagation: `expr?`
pub const ErrorPropagateExpr = struct {
    operand: *Expr,
    span: Span,
};

/// Await expression: `await expr`
pub const AwaitExpr = struct {
    operand: *Expr,
    span: Span,
};

/// Null coalescing: `expr ?? default`
pub const CoalesceExpr = struct {
    left: *Expr,
    right: *Expr,
    span: Span,
};

/// Range expression: `start..end` or `start..=end`
pub const RangeExpr = struct {
    start: ?*Expr,
    end: ?*Expr,
    inclusive: bool, // true for ..=
    span: Span,
};

/// Type cast: `expr as Type`
pub const CastExpr = struct {
    expr: *Expr,
    target_type: *TypeExpr,
    span: Span,
};

/// Type check: `expr is Type`
pub const TypeCheckExpr = struct {
    expr: *Expr,
    checked_type: *TypeExpr,
    span: Span,
};

/// Comptime expression: `comptime { expr }`
pub const ComptimeExpr = struct {
    expr: *Expr,
    span: Span,
};

/// String interpolation expression: f"prefix {expr} suffix"
/// Represented as alternating literal strings and expressions
pub const StringInterpolation = struct {
    /// Interleaved parts: literal strings and embedded expressions
    parts: []InterpolPart,
    span: Span,

    pub const InterpolPart = union(enum) {
        literal: []const u8,
        expr: *Expr,
    };
};

// ============================================================================
// Patterns
// ============================================================================

/// A pattern for matching
pub const Pattern = struct {
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        literal: *Literal,
        identifier: IdentifierPattern,
        wildcard, // _
        tuple: *TuplePattern,
        struct_pattern: *StructPattern,
        enum_variant: *EnumVariantPattern,
        slice: *SlicePattern,
        range: *RangePattern,
        or_pattern: *OrPattern,
        ref_pattern: *RefPattern,
    };

    pub const IdentifierPattern = struct {
        name: Identifier,
        is_mut: bool,
        binding: ?*Pattern, // for `name @ pattern`
    };
};

/// Tuple pattern: `(a, b, c)`
pub const TuplePattern = struct {
    elements: []const *Pattern,
    span: Span,
};

/// Struct pattern: `Point { x, y }` or `Point { x: a, y: b }`
pub const StructPattern = struct {
    type_path: ?Path,
    fields: []const *FieldPattern,
    rest: bool, // true if has `..` to ignore remaining fields
    span: Span,
};

/// Field pattern within struct pattern
pub const FieldPattern = struct {
    name: Identifier,
    pattern: ?*Pattern, // null means bind to field name
    span: Span,
};

/// Enum variant pattern: `Some(x)` or `None`
pub const EnumVariantPattern = struct {
    type_path: ?Path,
    variant: Identifier,
    payload: Payload,
    span: Span,

    pub const Payload = union(enum) {
        none,
        tuple: []const *Pattern,
        struct_fields: []const *FieldPattern,
    };
};

/// Slice pattern: `[first, second, ..rest]`
pub const SlicePattern = struct {
    elements: []const *Pattern,
    rest: ?RestPattern,
    span: Span,

    pub const RestPattern = struct {
        binding: ?Identifier,
        position: usize, // index where .. appears
    };
};

/// Range pattern: `1..10` or `'a'..='z'`
pub const RangePattern = struct {
    start: ?*Expr,
    end: ?*Expr,
    inclusive: bool,
    span: Span,
};

/// Or pattern: `A | B | C`
pub const OrPattern = struct {
    patterns: []const *Pattern,
    span: Span,
};

/// Reference pattern: `&x` or `&mut x`
pub const RefPattern = struct {
    pattern: *Pattern,
    is_mut: bool,
    span: Span,
};

// ============================================================================
// Type Expressions
// ============================================================================

/// A type expression
pub const TypeExpr = struct {
    kind: Kind,
    span: Span,

    pub const Kind = union(enum) {
        named: *NamedType,
        function: *FunctionType,
        array: *ArrayType,
        slice: *SliceType,
        pointer: *PointerType,
        reference: *ReferenceType,
        tuple: *TupleType,
        option: *OptionType, // T? sugar
        result: *ResultType, // T! sugar or Result[T, E]
        trait_object: *TraitObjectType, // dyn Trait
        infer, // _ for type inference
        never, // ! for never type
        self_type, // Self in traits
    };
};

/// Named type with optional generic arguments: `List[T]`, `HashMap[K, V]`
pub const NamedType = struct {
    path: Path,
    generic_args: ?[]const *TypeExpr,
    span: Span,
};

/// Dynamic trait object type: `dyn Trait`
pub const TraitObjectType = struct {
    trait_type: *TypeExpr, // The trait (named type)
    span: Span,
};

/// Function type: `fn(int, int) -> int` or `fn(int) -> int with [IO]`
pub const FunctionType = struct {
    params: []const *TypeExpr,
    return_type: *TypeExpr,
    effects: ?[]const *TypeExpr,
    span: Span,
};

/// Array type with fixed size: `[int; 10]`
pub const ArrayType = struct {
    element_type: *TypeExpr,
    size: *Expr, // compile-time expression for size
    span: Span,
};

/// Slice type: `[int]` or `[]int`
pub const SliceType = struct {
    element_type: *TypeExpr,
    span: Span,
};

/// Pointer type: `*T` or `*mut T`
pub const PointerType = struct {
    pointee_type: *TypeExpr,
    is_mut: bool,
    span: Span,
};

/// Reference type: `&T` or `&mut T`
pub const ReferenceType = struct {
    referenced_type: *TypeExpr,
    is_mut: bool,
    span: Span,
};

/// Tuple type: `(int, string, bool)`
pub const TupleType = struct {
    elements: []const *TypeExpr,
    span: Span,
};

/// Option type sugar: `T?` equivalent to `Option[T]`
pub const OptionType = struct {
    inner_type: *TypeExpr,
    span: Span,
};

/// Result type sugar: `T!` or `T!E` equivalent to `Result[T, E]`
pub const ResultType = struct {
    ok_type: *TypeExpr,
    err_type: ?*TypeExpr, // null means default error type
    span: Span,
};

// ============================================================================
// Helper Functions
// ============================================================================

/// Create an identifier
pub fn makeIdentifier(name: []const u8, span: Span) Identifier {
    return .{ .name = name, .span = span };
}

/// Create a path from a single identifier
pub fn pathFromIdentifier(allocator: Allocator, ident: Identifier) !Path {
    const segments = try allocator.alloc(Identifier, 1);
    segments[0] = ident;
    return Path{ .segments = segments, .span = ident.span };
}

// ============================================================================
// Visitor Interface (for AST traversal)
// ============================================================================

/// Generic visitor interface for AST traversal
pub fn Visitor(comptime Context: type, comptime Error: type) type {
    return struct {
        const Self = @This();

        context: Context,

        // Declaration visitors
        visitSourceFile: ?*const fn (*Self, *SourceFile) Error!void = null,
        visitDeclaration: ?*const fn (*Self, *Declaration) Error!void = null,
        visitFunctionDecl: ?*const fn (*Self, *FunctionDecl) Error!void = null,
        visitStructDecl: ?*const fn (*Self, *StructDecl) Error!void = null,
        visitEnumDecl: ?*const fn (*Self, *EnumDecl) Error!void = null,
        visitTraitDecl: ?*const fn (*Self, *TraitDecl) Error!void = null,
        visitImplBlock: ?*const fn (*Self, *ImplBlock) Error!void = null,

        // Statement visitors
        visitStatement: ?*const fn (*Self, *Statement) Error!void = null,
        visitLetBinding: ?*const fn (*Self, *LetBinding) Error!void = null,

        // Expression visitors
        visitExpr: ?*const fn (*Self, *Expr) Error!void = null,
        visitBinaryExpr: ?*const fn (*Self, *BinaryExpr) Error!void = null,
        visitUnaryExpr: ?*const fn (*Self, *UnaryExpr) Error!void = null,
        visitFunctionCall: ?*const fn (*Self, *FunctionCall) Error!void = null,
        visitMatchExpr: ?*const fn (*Self, *MatchExpr) Error!void = null,
        visitIfExpr: ?*const fn (*Self, *IfExpr) Error!void = null,
        visitBlockExpr: ?*const fn (*Self, *BlockExpr) Error!void = null,

        // Pattern visitors
        visitPattern: ?*const fn (*Self, *Pattern) Error!void = null,

        // Type visitors
        visitTypeExpr: ?*const fn (*Self, *TypeExpr) Error!void = null,
    };
}

// ============================================================================
// Debug / Pretty Printing
// ============================================================================

/// Format helpers for debugging AST nodes
pub const Formatter = struct {
    /// Format a span for display
    pub fn formatSpan(span: Span, writer: anytype) !void {
        try writer.print("{d}:{d}-{d}:{d}", .{
            span.start.line,
            span.start.column,
            span.end.line,
            span.end.column,
        });
    }

    /// Format a path for display
    pub fn formatPath(path: Path, writer: anytype) !void {
        for (path.segments, 0..) |segment, i| {
            if (i > 0) try writer.writeAll("::");
            try writer.writeAll(segment.name);
        }
    }

    /// Format a binary operator
    pub fn formatBinaryOp(op: BinaryExpr.BinaryOp) []const u8 {
        return switch (op) {
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",
            .eq => "==",
            .ne => "!=",
            .lt => "<",
            .le => "<=",
            .gt => ">",
            .ge => ">=",
            .@"and" => "and",
            .@"or" => "or",
            .bit_and => "&",
            .bit_or => "|",
            .bit_xor => "^",
            .shl => "<<",
            .shr => ">>",
            .in => "in",
        };
    }

    /// Format a unary operator
    pub fn formatUnaryOp(op: UnaryExpr.UnaryOp) []const u8 {
        return switch (op) {
            .neg => "-",
            .not => "not",
            .bit_not => "~",
            .deref => "*",
            .ref => "&",
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "Span creation and merging" {
    const pos1 = Span.Position{ .line = 1, .column = 1, .offset = 0 };
    const pos2 = Span.Position{ .line = 1, .column = 10, .offset = 9 };
    const pos3 = Span.Position{ .line = 2, .column = 5, .offset = 20 };
    const pos4 = Span.Position{ .line = 2, .column = 15, .offset = 30 };

    const span1 = Span.init(pos1, pos2);
    const span2 = Span.init(pos3, pos4);
    const merged = Span.merge(span1, span2);

    try testing.expectEqual(@as(usize, 1), merged.start.line);
    try testing.expectEqual(@as(usize, 1), merged.start.column);
    try testing.expectEqual(@as(usize, 2), merged.end.line);
    try testing.expectEqual(@as(usize, 15), merged.end.column);
}

test "Span.point creates zero-width span" {
    const pos = Span.Position{ .line = 5, .column = 10, .offset = 50 };
    const span = Span.point(pos);

    try testing.expectEqual(span.start.line, span.end.line);
    try testing.expectEqual(span.start.column, span.end.column);
    try testing.expectEqual(span.start.offset, span.end.offset);
}

test "makeIdentifier creates identifier" {
    const ident = makeIdentifier("test_name", Span.dummy);
    try testing.expectEqualStrings("test_name", ident.name);
}

test "pathFromIdentifier creates single-segment path" {
    const ident = makeIdentifier("module", Span.dummy);
    const path = try pathFromIdentifier(testing.allocator, ident);
    defer testing.allocator.free(path.segments);

    try testing.expectEqual(@as(usize, 1), path.segments.len);
    try testing.expectEqualStrings("module", path.segments[0].name);
}

test "BinaryOp formatting" {
    try testing.expectEqualStrings("+", Formatter.formatBinaryOp(.add));
    try testing.expectEqualStrings("==", Formatter.formatBinaryOp(.eq));
    try testing.expectEqualStrings("and", Formatter.formatBinaryOp(.@"and"));
    try testing.expectEqualStrings("<<", Formatter.formatBinaryOp(.shl));
}

test "UnaryOp formatting" {
    try testing.expectEqualStrings("-", Formatter.formatUnaryOp(.neg));
    try testing.expectEqualStrings("not", Formatter.formatUnaryOp(.not));
    try testing.expectEqualStrings("~", Formatter.formatUnaryOp(.bit_not));
}

test "Declaration.Visibility enum values" {
    const vis = Declaration.Visibility.public;
    try testing.expect(vis == .public);
    try testing.expect(vis != .private);
}

test "Literal.Kind union is tagged" {
    const int_lit = Literal.Kind{ .int = .{ .value = "42", .suffix = null } };
    const bool_lit = Literal.Kind{ .bool = true };

    try testing.expect(int_lit == .int);
    try testing.expect(bool_lit == .bool);
}

test "Expression kind union is tagged" {
    const expr_kind = Expr.Kind{ .identifier = makeIdentifier("x", Span.dummy) };
    try testing.expect(expr_kind == .identifier);
}

test "Pattern kind union is tagged" {
    const pattern_kind = Pattern.Kind.wildcard;
    try testing.expect(pattern_kind == .wildcard);
}

test "TypeExpr kind includes all expected variants" {
    // Test that we can create each variant
    const infer_type = TypeExpr.Kind.infer;
    const never_type = TypeExpr.Kind.never;
    const self_type = TypeExpr.Kind.self_type;

    try testing.expect(infer_type == .infer);
    try testing.expect(never_type == .never);
    try testing.expect(self_type == .self_type);
}

test "Assignment operators" {
    const ops = [_]Assignment.AssignOp{
        .assign,
        .add_assign,
        .sub_assign,
        .mul_assign,
        .div_assign,
        .mod_assign,
        .bit_and_assign,
        .bit_or_assign,
        .bit_xor_assign,
        .shl_assign,
        .shr_assign,
    };

    try testing.expectEqual(@as(usize, 11), ops.len);
}
