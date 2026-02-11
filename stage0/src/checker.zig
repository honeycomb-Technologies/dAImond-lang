//! dAImond Type Checker
//!
//! This module implements the type checking and inference system for the dAImond
//! programming language. It performs semantic analysis on the AST, including:
//!
//! - Type inference using a Hindley-Milner based algorithm
//! - Type unification with constraint solving
//! - Scope and symbol management
//! - Effect checking and propagation
//! - Contract validation
//! - Pattern exhaustiveness checking

const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const types = @import("types.zig");
const errors = @import("errors.zig");
const lexer = @import("lexer.zig");

// Re-export commonly used types
pub const TypeId = types.TypeId;
pub const Type = types.Type;
pub const TypeContext = types.TypeContext;
pub const EffectSet = types.EffectSet;
pub const EffectKind = types.EffectKind;
pub const DiagnosticBag = errors.DiagnosticBag;
pub const ErrorCode = errors.ErrorCode;
pub const Diagnostic = errors.Diagnostic;
pub const SourceLocation = lexer.SourceLocation;

// ============================================================================
// SYMBOLS AND SCOPES
// ============================================================================

/// Symbol kinds in the symbol table
pub const SymbolKind = enum {
    variable,
    function,
    type_def,
    type_alias,
    type_param,
    module,
    constant,
    enum_variant,
    trait_def,
    impl_block,
};

/// A symbol in the symbol table
pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind,
    type_id: TypeId,
    is_mutable: bool,
    is_public: bool,
    /// For functions: the declared effects
    effects: ?EffectSet,
    /// For generic symbols: type parameters
    type_params: ?[]const []const u8,
    /// Source location of definition
    def_location: ?ast.Span,
    /// Whether this symbol has been used
    used: bool,

    pub fn init(name: []const u8, kind: SymbolKind, type_id: TypeId) Symbol {
        return .{
            .name = name,
            .kind = kind,
            .type_id = type_id,
            .is_mutable = false,
            .is_public = false,
            .effects = null,
            .type_params = null,
            .def_location = null,
            .used = false,
        };
    }

    pub fn withMutable(self: Symbol, is_mutable: bool) Symbol {
        var copy = self;
        copy.is_mutable = is_mutable;
        return copy;
    }

    pub fn withPublic(self: Symbol, is_public: bool) Symbol {
        var copy = self;
        copy.is_public = is_public;
        return copy;
    }

    pub fn withEffects(self: Symbol, effect_set: EffectSet) Symbol {
        var copy = self;
        copy.effects = effect_set;
        return copy;
    }

    pub fn withLocation(self: Symbol, span: ast.Span) Symbol {
        var copy = self;
        copy.def_location = span;
        return copy;
    }
};

/// A lexical scope containing symbols
pub const Scope = struct {
    symbols: std.StringHashMap(Symbol),
    parent: ?*Scope,
    /// Scope kind for special handling
    kind: ScopeKind,
    /// For loop scopes: the label if any
    label: ?[]const u8,
    /// The expected return type for function scopes
    return_type: ?TypeId,
    /// Effects allowed in this scope
    allowed_effects: ?EffectSet,

    pub const ScopeKind = enum {
        global,
        module,
        function,
        block,
        loop,
        match_arm,
        impl_block,
        trait_def,
    };

    pub fn init(allocator: Allocator, parent: ?*Scope, kind: ScopeKind) Scope {
        return .{
            .symbols = std.StringHashMap(Symbol).init(allocator),
            .parent = parent,
            .kind = kind,
            .label = null,
            .return_type = if (parent) |p| p.return_type else null,
            .allowed_effects = if (parent) |p| p.allowed_effects else null,
        };
    }

    pub fn deinit(self: *Scope) void {
        self.symbols.deinit();
    }

    /// Define a symbol in this scope
    pub fn define(self: *Scope, symbol: Symbol) !void {
        try self.symbols.put(symbol.name, symbol);
    }

    /// Look up a symbol in this scope only
    pub fn lookupLocal(self: *Scope, name: []const u8) ?Symbol {
        return self.symbols.get(name);
    }

    /// Look up a symbol in this scope and all parent scopes
    pub fn lookup(self: *Scope, name: []const u8) ?Symbol {
        if (self.symbols.get(name)) |sym| {
            return sym;
        }
        if (self.parent) |parent| {
            return parent.lookup(name);
        }
        return null;
    }

    /// Mark a symbol as used
    pub fn markUsed(self: *Scope, name: []const u8) void {
        if (self.symbols.getPtr(name)) |sym| {
            sym.used = true;
        } else if (self.parent) |parent| {
            parent.markUsed(name);
        }
    }

    /// Check if we're inside a loop (for break/continue validation)
    pub fn isInLoop(self: *Scope) bool {
        if (self.kind == .loop) return true;
        if (self.parent) |parent| return parent.isInLoop();
        return false;
    }

    /// Find the enclosing function scope
    pub fn enclosingFunction(self: *Scope) ?*Scope {
        if (self.kind == .function) return self;
        if (self.parent) |parent| return parent.enclosingFunction();
        return null;
    }
};

/// Environment managing all scopes
pub const Environment = struct {
    allocator: Allocator,
    scopes: std.ArrayList(*Scope),
    current: *Scope,
    global: *Scope,

    const Self = @This();

    pub fn init(allocator: Allocator) !Self {
        var scopes = std.ArrayList(*Scope).init(allocator);

        const global = try allocator.create(Scope);
        global.* = Scope.init(allocator, null, .global);

        try scopes.append(global);

        return .{
            .allocator = allocator,
            .scopes = scopes,
            .current = global,
            .global = global,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.scopes.items) |scope| {
            scope.deinit();
            self.allocator.destroy(scope);
        }
        self.scopes.deinit();
    }

    /// Enter a new scope
    pub fn pushScope(self: *Self, kind: Scope.ScopeKind) !void {
        const scope = try self.allocator.create(Scope);
        scope.* = Scope.init(self.allocator, self.current, kind);
        try self.scopes.append(scope);
        self.current = scope;
    }

    /// Leave the current scope
    pub fn popScope(self: *Self) void {
        if (self.current.parent) |parent| {
            self.current = parent;
        }
    }

    /// Define a symbol in the current scope
    pub fn define(self: *Self, symbol: Symbol) !void {
        try self.current.define(symbol);
    }

    /// Look up a symbol
    pub fn lookup(self: *Self, name: []const u8) ?Symbol {
        return self.current.lookup(name);
    }

    /// Look up only in current scope
    pub fn lookupLocal(self: *Self, name: []const u8) ?Symbol {
        return self.current.lookupLocal(name);
    }

    /// Define in global scope
    pub fn defineGlobal(self: *Self, symbol: Symbol) !void {
        try self.global.define(symbol);
    }
};

// ============================================================================
// TYPE INFERENCE - UNION-FIND AND CONSTRAINTS
// ============================================================================

/// Type variable for inference
pub const InferenceVar = struct {
    id: u32,
    /// Resolved type (null if not yet resolved)
    resolved: ?TypeId,
    /// Bounds/constraints on this variable
    bounds: std.ArrayList(TypeId),

    pub fn init(allocator: Allocator, id: u32) InferenceVar {
        return .{
            .id = id,
            .resolved = null,
            .bounds = std.ArrayList(TypeId).init(allocator),
        };
    }

    pub fn deinit(self: *InferenceVar) void {
        self.bounds.deinit();
    }
};

/// Constraint kinds for type inference
pub const ConstraintKind = enum {
    /// Two types must be equal
    equal,
    /// First type must be subtype of second
    subtype,
    /// Type must implement trait
    implements,
    /// Type must have field
    has_field,
    /// Type must be callable with given args
    callable,
};

/// A type constraint to be solved
pub const Constraint = struct {
    kind: ConstraintKind,
    left: TypeId,
    right: TypeId,
    /// Additional data for complex constraints
    extra: ?[]const u8,
    /// Location for error reporting
    location: ?ast.Span,

    pub fn equal(left: TypeId, right: TypeId, location: ?ast.Span) Constraint {
        return .{
            .kind = .equal,
            .left = left,
            .right = right,
            .extra = null,
            .location = location,
        };
    }

    pub fn subtype(sub: TypeId, super: TypeId, location: ?ast.Span) Constraint {
        return .{
            .kind = .subtype,
            .left = sub,
            .right = super,
            .extra = null,
            .location = location,
        };
    }

    pub fn implements(type_id: TypeId, trait_id: TypeId, location: ?ast.Span) Constraint {
        return .{
            .kind = .implements,
            .left = type_id,
            .right = trait_id,
            .extra = null,
            .location = location,
        };
    }

    pub fn hasField(type_id: TypeId, field_name: []const u8, field_type: TypeId, location: ?ast.Span) Constraint {
        return .{
            .kind = .has_field,
            .left = type_id,
            .right = field_type,
            .extra = field_name,
            .location = location,
        };
    }
};

/// Union-Find structure for type unification
pub const UnionFind = struct {
    allocator: Allocator,
    /// Parent pointers (type var id -> parent type var id or self)
    parent: std.AutoHashMap(u32, u32),
    /// Rank for union by rank optimization
    rank: std.AutoHashMap(u32, u32),
    /// Resolved types for each equivalence class representative
    resolved: std.AutoHashMap(u32, TypeId),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .parent = std.AutoHashMap(u32, u32).init(allocator),
            .rank = std.AutoHashMap(u32, u32).init(allocator),
            .resolved = std.AutoHashMap(u32, TypeId).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.parent.deinit();
        self.rank.deinit();
        self.resolved.deinit();
    }

    /// Make a new set for a type variable
    pub fn makeSet(self: *Self, var_id: u32) !void {
        try self.parent.put(var_id, var_id);
        try self.rank.put(var_id, 0);
    }

    /// Find the representative of a set (with path compression)
    pub fn find(self: *Self, var_id: u32) u32 {
        const p = self.parent.get(var_id) orelse var_id;
        if (p != var_id) {
            const root = self.find(p);
            self.parent.put(var_id, root) catch {};
            return root;
        }
        return var_id;
    }

    /// Union two sets (by rank)
    pub fn unite(self: *Self, a: u32, b: u32) !void {
        const root_a = self.find(a);
        const root_b = self.find(b);

        if (root_a == root_b) return;

        const rank_a = self.rank.get(root_a) orelse 0;
        const rank_b = self.rank.get(root_b) orelse 0;

        if (rank_a < rank_b) {
            try self.parent.put(root_a, root_b);
            // Propagate resolved type
            if (self.resolved.get(root_a)) |t| {
                try self.resolved.put(root_b, t);
            }
        } else if (rank_a > rank_b) {
            try self.parent.put(root_b, root_a);
            if (self.resolved.get(root_b)) |t| {
                try self.resolved.put(root_a, t);
            }
        } else {
            try self.parent.put(root_b, root_a);
            try self.rank.put(root_a, rank_a + 1);
            if (self.resolved.get(root_b)) |t| {
                try self.resolved.put(root_a, t);
            }
        }
    }

    /// Set the resolved type for a type variable
    pub fn setResolved(self: *Self, var_id: u32, type_id: TypeId) !void {
        const root = self.find(var_id);
        try self.resolved.put(root, type_id);
    }

    /// Get the resolved type for a type variable
    pub fn getResolved(self: *Self, var_id: u32) ?TypeId {
        const root = self.find(var_id);
        return self.resolved.get(root);
    }
};

// ============================================================================
// TYPE CHECKER
// ============================================================================

/// The main type checker
pub const TypeChecker = struct {
    allocator: Allocator,
    /// Type context for managing types
    type_ctx: *TypeContext,
    /// Symbol environment
    env: Environment,
    /// Diagnostic bag for errors
    diagnostics: DiagnosticBag,
    /// Union-find for type unification
    union_find: UnionFind,
    /// Pending constraints to solve
    constraints: std.ArrayList(Constraint),
    /// Counter for generating fresh type variables
    next_type_var: u32,
    /// Current effects being tracked
    current_effects: EffectSet,
    /// Source code for error messages
    source: ?[]const u8,
    /// Source file name
    source_file: ?[]const u8,
    /// Names of functions that have generic parameters
    generic_function_names: std.StringHashMap(void),

    const Self = @This();

    /// Initialize a new type checker
    pub fn init(allocator: Allocator) !Self {
        const type_ctx = try allocator.create(TypeContext);
        type_ctx.* = TypeContext.init(allocator);

        var checker = Self{
            .allocator = allocator,
            .type_ctx = type_ctx,
            .env = try Environment.init(allocator),
            .diagnostics = DiagnosticBag.init(allocator),
            .union_find = UnionFind.init(allocator),
            .constraints = std.ArrayList(Constraint).init(allocator),
            .next_type_var = 0,
            .current_effects = EffectSet.pure,
            .source = null,
            .source_file = null,
            .generic_function_names = std.StringHashMap(void).init(allocator),
        };

        // Register built-in types and functions
        try checker.registerBuiltins();

        return checker;
    }

    /// Clean up resources
    pub fn deinit(self: *Self) void {
        self.env.deinit();
        self.diagnostics.deinit();
        self.union_find.deinit();
        self.constraints.deinit();
        self.generic_function_names.deinit();
        self.type_ctx.deinit();
        self.allocator.destroy(self.type_ctx);
    }

    /// Set source code for error messages
    pub fn setSource(self: *Self, source: []const u8) void {
        self.source = source;
    }

    /// Set source file name
    pub fn setSourceFile(self: *Self, file: []const u8) void {
        self.source_file = file;
    }

    /// Register built-in types and functions
    fn registerBuiltins(self: *Self) !void {
        // Built-in primitive types are already registered in TypeContext

        // Register type aliases
        const int_id = self.type_ctx.lookup("int") orelse try self.type_ctx.intern(.{ .int = .i64 });
        const uint_id = self.type_ctx.lookup("uint") orelse try self.type_ctx.intern(.{ .int = .u64 });
        const float_id = self.type_ctx.lookup("float") orelse try self.type_ctx.intern(.{ .float = .f64 });
        const bool_id = try self.type_ctx.intern(.bool);
        const str_id = try self.type_ctx.intern(.str);
        const unit_id = try self.type_ctx.intern(.unit);

        // Define primitive types in global scope
        try self.env.defineGlobal(Symbol.init("int", .type_def, int_id));
        try self.env.defineGlobal(Symbol.init("uint", .type_def, uint_id));
        try self.env.defineGlobal(Symbol.init("i8", .type_def, try self.type_ctx.intern(.{ .int = .i8 })));
        try self.env.defineGlobal(Symbol.init("i16", .type_def, try self.type_ctx.intern(.{ .int = .i16 })));
        try self.env.defineGlobal(Symbol.init("i32", .type_def, try self.type_ctx.intern(.{ .int = .i32 })));
        try self.env.defineGlobal(Symbol.init("i64", .type_def, try self.type_ctx.intern(.{ .int = .i64 })));
        try self.env.defineGlobal(Symbol.init("i128", .type_def, try self.type_ctx.intern(.{ .int = .i128 })));
        try self.env.defineGlobal(Symbol.init("u8", .type_def, try self.type_ctx.intern(.{ .int = .u8 })));
        try self.env.defineGlobal(Symbol.init("u16", .type_def, try self.type_ctx.intern(.{ .int = .u16 })));
        try self.env.defineGlobal(Symbol.init("u32", .type_def, try self.type_ctx.intern(.{ .int = .u32 })));
        try self.env.defineGlobal(Symbol.init("u64", .type_def, try self.type_ctx.intern(.{ .int = .u64 })));
        try self.env.defineGlobal(Symbol.init("u128", .type_def, try self.type_ctx.intern(.{ .int = .u128 })));
        try self.env.defineGlobal(Symbol.init("f32", .type_def, try self.type_ctx.intern(.{ .float = .f32 })));
        try self.env.defineGlobal(Symbol.init("f64", .type_def, try self.type_ctx.intern(.{ .float = .f64 })));
        try self.env.defineGlobal(Symbol.init("float", .type_def, float_id));
        try self.env.defineGlobal(Symbol.init("bool", .type_def, bool_id));
        try self.env.defineGlobal(Symbol.init("str", .type_def, str_id));
        try self.env.defineGlobal(Symbol.init("string", .type_def, str_id)); // alias for str
        try self.env.defineGlobal(Symbol.init("byte", .type_def, try self.type_ctx.intern(.{ .int = .u8 })));
        try self.env.defineGlobal(Symbol.init("char", .type_def, try self.type_ctx.intern(.char)));

        // Register SIMD vector types
        try self.env.defineGlobal(Symbol.init("f32x4", .type_def, try self.type_ctx.intern(.{ .simd = .f32x4 })));
        try self.env.defineGlobal(Symbol.init("f32x8", .type_def, try self.type_ctx.intern(.{ .simd = .f32x8 })));
        try self.env.defineGlobal(Symbol.init("f64x2", .type_def, try self.type_ctx.intern(.{ .simd = .f64x2 })));
        try self.env.defineGlobal(Symbol.init("f64x4", .type_def, try self.type_ctx.intern(.{ .simd = .f64x4 })));
        try self.env.defineGlobal(Symbol.init("i32x4", .type_def, try self.type_ctx.intern(.{ .simd = .i32x4 })));
        try self.env.defineGlobal(Symbol.init("i32x8", .type_def, try self.type_ctx.intern(.{ .simd = .i32x8 })));
        try self.env.defineGlobal(Symbol.init("i64x2", .type_def, try self.type_ctx.intern(.{ .simd = .i64x2 })));
        try self.env.defineGlobal(Symbol.init("i64x4", .type_def, try self.type_ctx.intern(.{ .simd = .i64x4 })));

        // Register generic container types (List, Vec, Box, Map)
        // These are mapped to monomorphized C types by codegen
        try self.env.defineGlobal(Symbol.init("List", .type_def, try self.freshTypeVar()));
        try self.env.defineGlobal(Symbol.init("Vec", .type_def, try self.freshTypeVar()));
        try self.env.defineGlobal(Symbol.init("Box", .type_def, try self.freshTypeVar()));
        try self.env.defineGlobal(Symbol.init("Map", .type_def, try self.freshTypeVar()));
        try self.env.defineGlobal(Symbol.init("HashMap", .type_def, try self.freshTypeVar()));
        try self.env.defineGlobal(Symbol.init("String", .type_def, str_id));

        // Register Option, Result, and Future generic types
        try self.env.defineGlobal(Symbol.init("Option", .type_def, try self.freshTypeVar()));
        try self.env.defineGlobal(Symbol.init("Result", .type_def, try self.freshTypeVar()));
        try self.env.defineGlobal(Symbol.init("Future", .type_def, try self.freshTypeVar()));

        // Register Option/Result constructors as builtins
        // Some(x) - creates an Option with a value
        const some_fn_type = try self.type_ctx.makeFunction(
            &[_]TypeId{try self.freshTypeVar()},
            try self.freshTypeVar(),
            EffectSet.init(self.allocator),
        );
        try self.env.defineGlobal(Symbol.init("Some", .function, some_fn_type));

        // None - a value (no-argument constructor)
        try self.env.defineGlobal(Symbol.init("None", .variable, try self.freshTypeVar()));

        // Ok(x) - creates a Result with success value
        const ok_fn_type = try self.type_ctx.makeFunction(
            &[_]TypeId{try self.freshTypeVar()},
            try self.freshTypeVar(),
            EffectSet.init(self.allocator),
        );
        try self.env.defineGlobal(Symbol.init("Ok", .function, ok_fn_type));

        // Err(x) - creates a Result with error value
        const err_fn_type = try self.type_ctx.makeFunction(
            &[_]TypeId{try self.freshTypeVar()},
            try self.freshTypeVar(),
            EffectSet.init(self.allocator),
        );
        try self.env.defineGlobal(Symbol.init("Err", .function, err_fn_type));

        // Register built-in functions
        // print function with IO effect
        var print_effects = EffectSet.init(self.allocator);
        print_effects.addBuiltin(.io);
        print_effects.addBuiltin(.console);
        const print_fn_type = try self.type_ctx.makeFunction(
            &[_]TypeId{str_id},
            unit_id,
            print_effects,
        );
        try self.env.defineGlobal(Symbol.init("print", .function, print_fn_type).withEffects(print_effects));

        // println function
        const println_fn_type = try self.type_ctx.makeFunction(
            &[_]TypeId{str_id},
            unit_id,
            print_effects,
        );
        try self.env.defineGlobal(Symbol.init("println", .function, println_fn_type).withEffects(print_effects));
    }

    // ========================================================================
    // TYPE VARIABLE MANAGEMENT
    // ========================================================================

    /// Create a fresh type variable
    pub fn freshTypeVar(self: *Self) !TypeId {
        const var_id = self.next_type_var;
        self.next_type_var += 1;

        try self.union_find.makeSet(var_id);

        return try self.type_ctx.intern(.{
            .type_var = .{
                .id = var_id,
                .name = null,
                .bounds = &[_]types.TraitBound{},
                .resolved = null,
            },
        });
    }

    /// Create a named type variable
    pub fn namedTypeVar(self: *Self, name: []const u8) !TypeId {
        const var_id = self.next_type_var;
        self.next_type_var += 1;

        try self.union_find.makeSet(var_id);

        return try self.type_ctx.intern(.{
            .type_var = .{
                .id = var_id,
                .name = name,
                .bounds = &[_]types.TraitBound{},
                .resolved = null,
            },
        });
    }

    // ========================================================================
    // CONSTRAINT GENERATION AND SOLVING
    // ========================================================================

    /// Add a constraint
    pub fn addConstraint(self: *Self, constraint: Constraint) !void {
        try self.constraints.append(constraint);
    }

    /// Solve all pending constraints
    pub fn solveConstraints(self: *Self) !void {
        while (self.constraints.items.len > 0) {
            const constraint = self.constraints.orderedRemove(0);
            try self.solveConstraint(constraint);
        }
    }

    /// Solve a single constraint
    fn solveConstraint(self: *Self, constraint: Constraint) !void {
        switch (constraint.kind) {
            .equal => try self.unify(constraint.left, constraint.right, constraint.location),
            .subtype => try self.checkSubtype(constraint.left, constraint.right, constraint.location),
            .implements => try self.checkImplements(constraint.left, constraint.right, constraint.location),
            .has_field => {
                if (constraint.extra) |field_name| {
                    try self.checkHasField(constraint.left, field_name, constraint.right, constraint.location);
                }
            },
            .callable => try self.checkCallable(constraint.left, constraint.right, constraint.location),
        }
    }

    // ========================================================================
    // UNIFICATION
    // ========================================================================

    /// Unify two types
    pub fn unify(self: *Self, a: TypeId, b: TypeId, location: ?ast.Span) !void {
        const resolved_a = self.resolveType(a);
        const resolved_b = self.resolveType(b);

        if (resolved_a == resolved_b) return;

        const type_a = self.type_ctx.get(resolved_a);
        const type_b = self.type_ctx.get(resolved_b);

        if (type_a == null or type_b == null) {
            try self.reportTypeMismatch(resolved_a, resolved_b, location);
            return;
        }

        const ta = type_a.?;
        const tb = type_b.?;

        // Handle type variables
        if (ta == .type_var) {
            try self.bindTypeVar(ta.type_var.id, resolved_b);
            return;
        }
        if (tb == .type_var) {
            try self.bindTypeVar(tb.type_var.id, resolved_a);
            return;
        }

        // Structural unification
        const tag_a = std.meta.activeTag(ta);
        const tag_b = std.meta.activeTag(tb);

        if (tag_a != tag_b) {
            // Check for compatible coercions
            if (types.canCoerce(self.type_ctx, resolved_a, resolved_b)) {
                return;
            }
            try self.reportTypeMismatch(resolved_a, resolved_b, location);
            return;
        }

        switch (ta) {
            .int => |int_a| {
                if (tb.int != int_a) {
                    try self.reportTypeMismatch(resolved_a, resolved_b, location);
                }
            },
            .float => |float_a| {
                if (tb.float != float_a) {
                    try self.reportTypeMismatch(resolved_a, resolved_b, location);
                }
            },
            .option => |inner_a| {
                try self.unify(inner_a, tb.option, location);
            },
            .result => |res_a| {
                try self.unify(res_a.ok_type, tb.result.ok_type, location);
                try self.unify(res_a.err_type, tb.result.err_type, location);
            },
            .array => |arr_a| {
                if (arr_a.size != tb.array.size) {
                    try self.reportTypeMismatch(resolved_a, resolved_b, location);
                    return;
                }
                try self.unify(arr_a.element_type, tb.array.element_type, location);
            },
            .slice => |slice_a| {
                try self.unify(slice_a.element_type, tb.slice.element_type, location);
            },
            .tuple => |tup_a| {
                if (tup_a.element_types.len != tb.tuple.element_types.len) {
                    try self.reportTypeMismatch(resolved_a, resolved_b, location);
                    return;
                }
                for (tup_a.element_types, tb.tuple.element_types) |elem_a, elem_b| {
                    try self.unify(elem_a, elem_b, location);
                }
            },
            .function => |fn_a| {
                if (fn_a.param_types.len != tb.function.param_types.len) {
                    try self.reportTypeMismatch(resolved_a, resolved_b, location);
                    return;
                }
                for (fn_a.param_types, tb.function.param_types) |param_a, param_b| {
                    try self.unify(param_a, param_b, location);
                }
                try self.unify(fn_a.return_type, tb.function.return_type, location);
            },
            .ref => |ref_a| {
                try self.unify(ref_a.pointee, tb.ref.pointee, location);
            },
            .box => |box_a| {
                try self.unify(box_a, tb.box, location);
            },
            else => {
                // For simple types, they should be equal already
                if (resolved_a != resolved_b) {
                    try self.reportTypeMismatch(resolved_a, resolved_b, location);
                }
            },
        }
    }

    /// Bind a type variable to a type
    fn bindTypeVar(self: *Self, var_id: u32, type_id: TypeId) !void {
        // Occurs check
        if (self.occursIn(var_id, type_id)) {
            try self.reportError(.cyclic_type, "cyclic type detected", null);
            return;
        }
        try self.union_find.setResolved(var_id, type_id);
    }

    /// Check if a type variable occurs in a type (for occurs check)
    /// Box types are excluded from the occurs check because Box provides heap indirection
    /// that makes recursive types representable (finite size on the stack).
    fn occursIn(self: *Self, var_id: u32, type_id: TypeId) bool {
        const resolved = self.resolveType(type_id);
        const typ = self.type_ctx.get(resolved) orelse return false;

        return switch (typ) {
            .type_var => |tv| tv.id == var_id,
            .option => |inner| self.occursIn(var_id, inner),
            .result => |res| self.occursIn(var_id, res.ok_type) or self.occursIn(var_id, res.err_type),
            // Box provides heap indirection - don't check through it for recursive types
            .box => false,
            .array => |arr| self.occursIn(var_id, arr.element_type),
            .slice => |s| self.occursIn(var_id, s.element_type),
            .tuple => |tup| {
                for (tup.element_types) |elem| {
                    if (self.occursIn(var_id, elem)) return true;
                }
                return false;
            },
            .function => |func| {
                for (func.param_types) |param| {
                    if (self.occursIn(var_id, param)) return true;
                }
                return self.occursIn(var_id, func.return_type);
            },
            .ref => |r| self.occursIn(var_id, r.pointee),
            else => false,
        };
    }

    /// Resolve a type, following type variable bindings
    pub fn resolveType(self: *Self, type_id: TypeId) TypeId {
        const typ = self.type_ctx.get(type_id) orelse return type_id;

        if (typ == .type_var) {
            if (self.union_find.getResolved(typ.type_var.id)) |resolved| {
                return self.resolveType(resolved);
            }
        }

        return type_id;
    }

    // ========================================================================
    // SUBTYPE AND CONSTRAINT CHECKING
    // ========================================================================

    fn checkSubtype(self: *Self, sub: TypeId, super: TypeId, location: ?ast.Span) !void {
        const resolved_sub = self.resolveType(sub);
        const resolved_super = self.resolveType(super);

        if (!types.isCompatible(self.type_ctx, resolved_sub, resolved_super)) {
            // For Box and other compound types, try structural unification which
            // resolves type_vars inside compound types (e.g., Box(type_var) vs Box(struct))
            self.unify(resolved_sub, resolved_super, location) catch {
                // Unification failed - the types are genuinely incompatible
                // (reportTypeMismatch was already called by unify)
            };
        }
    }

    fn checkImplements(self: *Self, type_id: TypeId, trait_id: TypeId, location: ?ast.Span) !void {
        if (!types.implementsTrait(self.type_ctx, type_id, trait_id)) {
            try self.reportError(.constraint_not_satisfied, "type does not implement required trait", location);
        }
    }

    fn checkHasField(self: *Self, type_id: TypeId, field_name: []const u8, expected_type: TypeId, location: ?ast.Span) !void {
        const resolved = self.resolveType(type_id);
        const typ = self.type_ctx.get(resolved) orelse {
            try self.reportError(.undefined_field, "cannot access field on unknown type", location);
            return;
        };

        switch (typ) {
            .@"struct" => |s| {
                if (s.getField(field_name)) |field| {
                    try self.unify(field.type_id, expected_type, location);
                } else {
                    try self.reportError(.undefined_field, "struct has no such field", location);
                }
            },
            .tuple => |t| {
                // Parse field name as index
                const index = std.fmt.parseInt(usize, field_name, 10) catch {
                    try self.reportError(.undefined_field, "tuple fields must be numeric indices", location);
                    return;
                };
                if (index >= t.element_types.len) {
                    try self.reportError(.undefined_field, "tuple index out of bounds", location);
                    return;
                }
                try self.unify(t.element_types[index], expected_type, location);
            },
            else => {
                try self.reportError(.expected_struct_type, "field access requires struct or tuple type", location);
            },
        }
    }

    fn checkCallable(self: *Self, fn_type: TypeId, return_type: TypeId, location: ?ast.Span) !void {
        const resolved = self.resolveType(fn_type);
        const typ = self.type_ctx.get(resolved) orelse {
            try self.reportError(.expected_function_type, "expected callable type", location);
            return;
        };

        if (typ != .function) {
            try self.reportError(.expected_function_type, "expected function type", location);
            return;
        }

        try self.unify(typ.function.return_type, return_type, location);
    }

    // ========================================================================
    // EXPRESSION TYPE CHECKING
    // ========================================================================

    /// Infer the type of an expression
    pub fn inferExpr(self: *Self, expr: *const ast.Expr) anyerror!TypeId {
        return switch (expr.kind) {
            .literal => |lit| self.inferLiteral(lit),
            .identifier => |ident| self.inferIdentifier(ident, expr.span),
            .path => |path| self.inferPath(path),
            .binary => |bin| self.inferBinary(bin),
            .unary => |un| self.inferUnary(un),
            .field_access => |fa| self.inferFieldAccess(fa),
            .index_access => |ia| self.inferIndexAccess(ia),
            .method_call => |mc| self.inferMethodCall(mc),
            .function_call => |fc| self.inferFunctionCall(fc),
            .struct_literal => |sl| self.inferStructLiteral(sl),
            .enum_literal => |el| self.inferEnumLiteral(el),
            .array_literal => |al| self.inferArrayLiteral(al),
            .tuple_literal => |tl| self.inferTupleLiteral(tl),
            .if_expr => |ie| self.inferIfExpr(ie),
            .match_expr => |me| self.inferMatchExpr(me),
            .block => |blk| self.inferBlock(blk),
            .lambda => |lam| self.inferLambda(lam),
            .pipeline => |pipe| self.inferPipeline(pipe),
            .error_propagate => |ep| self.inferErrorPropagate(ep),
            .coalesce => |coal| self.inferCoalesce(coal),
            .range => |rng| self.inferRange(rng),
            .cast => |cast| self.inferCast(cast),
            .type_check => |tc| self.inferTypeCheck(tc),
            .string_interpolation => |si| self.inferStringInterpolation(si),
            .grouped => |inner| self.inferExpr(inner),
            .comptime_expr => |ce| self.inferExpr(ce.expr),
            .await_expr => |aw| self.inferAwaitExpr(aw),
        };
    }

    fn inferLiteral(self: *Self, lit: *const ast.Literal) anyerror!TypeId {
        return switch (lit.kind) {
            .int => |int_lit| {
                if (int_lit.suffix) |suffix| {
                    return self.resolveTypeName(suffix) catch try self.type_ctx.intern(.{ .int = .i64 });
                }
                return try self.type_ctx.intern(.{ .int = .i64 });
            },
            .float => |float_lit| {
                if (float_lit.suffix) |suffix| {
                    return self.resolveTypeName(suffix) catch try self.type_ctx.intern(.{ .float = .f64 });
                }
                return try self.type_ctx.intern(.{ .float = .f64 });
            },
            .string => try self.type_ctx.intern(.str),
            .char => try self.type_ctx.intern(.char),
            .bool => try self.type_ctx.intern(.bool),
            .null_lit => {
                // null has type Option[?T] where T is inferred
                const inner = try self.freshTypeVar();
                return try self.type_ctx.makeOption(inner);
            },
        };
    }

    fn inferIdentifier(self: *Self, ident: ast.Identifier, span: ast.Span) anyerror!TypeId {
        if (self.env.lookup(ident.name)) |symbol| {
            self.env.current.markUsed(ident.name);
            return symbol.type_id;
        }

        try self.reportError(.undefined_variable, "undefined variable", span);
        return try self.type_ctx.intern(.error_type);
    }

    fn inferPath(self: *Self, path: ast.Path) anyerror!TypeId {
        // For now, just look up the first segment
        if (path.segments.len > 0) {
            if (self.env.lookup(path.segments[0].name)) |symbol| {
                return symbol.type_id;
            }
        }

        try self.reportError(.undefined_variable, "undefined path", path.span);
        return try self.type_ctx.intern(.error_type);
    }

    fn inferBinary(self: *Self, bin: *const ast.BinaryExpr) anyerror!TypeId {
        const left_type = try self.inferExpr(bin.left);
        const right_type = try self.inferExpr(bin.right);

        return switch (bin.op) {
            // Arithmetic operators
            .add, .sub, .mul, .div, .mod => {
                const common = types.commonType(self.type_ctx, left_type, right_type);
                if (common) |t| {
                    return t;
                }
                try self.reportError(.incompatible_types, "incompatible types for arithmetic operation", bin.span);
                return try self.type_ctx.intern(.error_type);
            },

            // Comparison operators
            .eq, .ne, .lt, .le, .gt, .ge => {
                try self.addConstraint(Constraint.equal(left_type, right_type, bin.span));
                return try self.type_ctx.intern(.bool);
            },

            // Logical operators
            .@"and", .@"or" => {
                const bool_type = try self.type_ctx.intern(.bool);
                try self.unify(left_type, bool_type, bin.span);
                try self.unify(right_type, bool_type, bin.span);
                return bool_type;
            },

            // Bitwise operators
            .bit_and, .bit_or, .bit_xor, .shl, .shr => {
                if (!types.isIntegral(self.type_ctx, left_type)) {
                    try self.reportError(.expected_numeric_type, "bitwise operator requires integer type", bin.span);
                }
                return left_type;
            },

            // Membership
            .in => {
                return try self.type_ctx.intern(.bool);
            },
        };
    }

    fn inferUnary(self: *Self, un: *const ast.UnaryExpr) anyerror!TypeId {
        const operand_type = try self.inferExpr(un.operand);

        return switch (un.op) {
            .neg => {
                if (!types.isNumeric(self.type_ctx, operand_type)) {
                    try self.reportError(.expected_numeric_type, "negation requires numeric type", un.span);
                }
                return operand_type;
            },
            .not => {
                const bool_type = try self.type_ctx.intern(.bool);
                try self.unify(operand_type, bool_type, un.span);
                return bool_type;
            },
            .bit_not => {
                if (!types.isIntegral(self.type_ctx, operand_type)) {
                    try self.reportError(.expected_numeric_type, "bitwise not requires integer type", un.span);
                }
                return operand_type;
            },
            .deref => {
                const resolved = self.resolveType(operand_type);
                const typ = self.type_ctx.get(resolved) orelse {
                    try self.reportError(.type_mismatch, "cannot dereference unknown type", un.span);
                    return try self.type_ctx.intern(.error_type);
                };
                if (typ == .ref) {
                    return typ.ref.pointee;
                }
                if (typ == .box) {
                    return typ.box;
                }
                if (typ == .type_var) {
                    // Type variable (e.g., from generic container elements) - allow deref
                    return try self.freshTypeVar();
                }
                try self.reportError(.type_mismatch, "cannot dereference non-reference type", un.span);
                return try self.type_ctx.intern(.error_type);
            },
            .ref => {
                return try self.type_ctx.makeRef(operand_type, false, null);
            },
        };
    }

    fn inferFieldAccess(self: *Self, fa: *const ast.FieldAccess) anyerror!TypeId {
        const object_type = try self.inferExpr(fa.object);
        const resolved = self.resolveType(object_type);
        const typ = self.type_ctx.get(resolved) orelse {
            // Unknown type - allow field access (codegen will handle it)
            return try self.freshTypeVar();
        };

        // Auto-dereference through references and Box types for field access
        const derefed_typ = if (typ == .ref) blk: {
            const pointee = self.resolveType(typ.ref.pointee);
            break :blk self.type_ctx.get(pointee) orelse {
                return try self.freshTypeVar();
            };
        } else if (typ == .box) blk: {
            const inner = self.resolveType(typ.box);
            break :blk self.type_ctx.get(inner) orelse {
                return try self.freshTypeVar();
            };
        } else typ;

        switch (derefed_typ) {
            .@"struct" => |s| {
                if (s.getField(fa.field.name)) |field| {
                    return field.type_id;
                }
                try self.reportError(.undefined_field, "struct has no such field", fa.span);
                return try self.type_ctx.intern(.error_type);
            },
            .tuple => |t| {
                const index = std.fmt.parseInt(usize, fa.field.name, 10) catch {
                    try self.reportError(.undefined_field, "tuple fields must be numeric", fa.span);
                    return try self.type_ctx.intern(.error_type);
                };
                if (index >= t.element_types.len) {
                    try self.reportError(.undefined_field, "tuple index out of bounds", fa.span);
                    return try self.type_ctx.intern(.error_type);
                }
                return t.element_types[index];
            },
            .type_var => {
                // Type variable (e.g., from generic container elements) - allow field access
                return try self.freshTypeVar();
            },
            .option => |opt| {
                // Option[T] fields: has_value (bool), value (T)
                if (std.mem.eql(u8, fa.field.name, "has_value")) {
                    return try self.type_ctx.intern(.bool);
                } else if (std.mem.eql(u8, fa.field.name, "value")) {
                    return self.resolveType(opt);
                }
                try self.reportError(.undefined_field, "Option has no such field", fa.span);
                return try self.type_ctx.intern(.error_type);
            },
            .result => |res| {
                // Result[T, E] fields: is_ok (bool), ok (T), err (E)
                if (std.mem.eql(u8, fa.field.name, "is_ok")) {
                    return try self.type_ctx.intern(.bool);
                } else if (std.mem.eql(u8, fa.field.name, "ok")) {
                    return self.resolveType(res.ok_type);
                } else if (std.mem.eql(u8, fa.field.name, "err")) {
                    return self.resolveType(res.err_type);
                }
                try self.reportError(.undefined_field, "Result has no such field", fa.span);
                return try self.type_ctx.intern(.error_type);
            },
            else => {
                try self.reportError(.expected_struct_type, "field access requires struct type", fa.span);
                return try self.type_ctx.intern(.error_type);
            },
        }
    }

    fn inferIndexAccess(self: *Self, ia: *const ast.IndexAccess) anyerror!TypeId {
        const object_type = try self.inferExpr(ia.object);
        const index_type = try self.inferExpr(ia.index);
        const resolved = self.resolveType(object_type);
        const typ = self.type_ctx.get(resolved) orelse {
            // Unknown type (e.g., List[T]) - allow indexing, return fresh type var
            return try self.freshTypeVar();
        };

        // Check index is integer
        if (!types.isIntegral(self.type_ctx, index_type)) {
            try self.reportError(.expected_numeric_type, "array index must be integer", ia.span);
        }

        switch (typ) {
            .array => |arr| return arr.element_type,
            .slice => |s| return s.element_type,
            .str => return try self.type_ctx.intern(.char),
            .type_var => {
                // Type variable (e.g., from List[T]) - allow indexing
                return try self.freshTypeVar();
            },
            else => {
                try self.reportError(.expected_array_type, "indexing requires array or slice type", ia.span);
                return try self.type_ctx.intern(.error_type);
            },
        }
    }

    fn inferMethodCall(self: *Self, mc: *const ast.MethodCall) anyerror!TypeId {
        const object_type = try self.inferExpr(mc.object);
        _ = object_type;

        // Infer argument types
        for (mc.args) |arg| {
            _ = try self.inferExpr(arg);
        }

        // For now, create fresh return type
        // Full implementation would look up method on type
        const return_type = try self.freshTypeVar();

        return return_type;
    }

    fn inferFunctionCall(self: *Self, fc: *const ast.FunctionCall) anyerror!TypeId {
        // Special handling for built-in functions that accept any type
        if (fc.function.kind == .identifier) {
            const name = fc.function.kind.identifier.name;
            // Builtins returning void/unit
            if (std.mem.eql(u8, name, "print") or std.mem.eql(u8, name, "println") or
                std.mem.eql(u8, name, "assert") or std.mem.eql(u8, name, "assert_eq") or std.mem.eql(u8, name, "panic") or
                std.mem.eql(u8, name, "eprint") or std.mem.eql(u8, name, "eprintln") or
                std.mem.eql(u8, name, "exit") or std.mem.eql(u8, name, "file_write") or
                std.mem.eql(u8, name, "file_append") or
                std.mem.eql(u8, name, "fs_write") or std.mem.eql(u8, name, "fs_append") or
                std.mem.eql(u8, name, "tcp_close") or
                std.mem.eql(u8, name, "thread_join") or
                std.mem.eql(u8, name, "mutex_lock") or std.mem.eql(u8, name, "mutex_unlock") or
                std.mem.eql(u8, name, "mutex_destroy") or
                std.mem.eql(u8, name, "sleep_ms") or
                std.mem.eql(u8, name, "flush") or
                std.mem.eql(u8, name, "mem_sweep"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                // Effect tracking for builtins
                if (std.mem.eql(u8, name, "print") or std.mem.eql(u8, name, "println") or
                    std.mem.eql(u8, name, "eprint") or std.mem.eql(u8, name, "eprintln"))
                {
                    self.current_effects.addBuiltin(.io);
                    self.current_effects.addBuiltin(.console);
                } else if (std.mem.eql(u8, name, "panic") or std.mem.eql(u8, name, "exit")) {
                    self.current_effects.addBuiltin(.io);
                    self.current_effects.addBuiltin(.diverge);
                } else if (std.mem.eql(u8, name, "file_write") or std.mem.eql(u8, name, "file_append") or
                    std.mem.eql(u8, name, "fs_write") or std.mem.eql(u8, name, "fs_append"))
                {
                    self.current_effects.addBuiltin(.file_system);
                } else if (std.mem.eql(u8, name, "sleep_ms") or std.mem.eql(u8, name, "flush")) {
                    self.current_effects.addBuiltin(.io);
                }
                return try self.type_ctx.intern(.unit);
            }
            // Builtins returning int
            if (std.mem.eql(u8, name, "len") or std.mem.eql(u8, name, "parse_int") or
                std.mem.eql(u8, name, "string_to_int") or
                std.mem.eql(u8, name, "string_find") or std.mem.eql(u8, name, "args_len") or
                std.mem.eql(u8, name, "system") or
                std.mem.eql(u8, name, "fs_mkdir") or std.mem.eql(u8, name, "fs_remove") or
                std.mem.eql(u8, name, "fs_rename") or std.mem.eql(u8, name, "fs_rmdir") or
                std.mem.eql(u8, name, "tcp_listen") or std.mem.eql(u8, name, "tcp_accept") or
                std.mem.eql(u8, name, "tcp_connect") or std.mem.eql(u8, name, "tcp_write") or
                std.mem.eql(u8, name, "thread_spawn") or std.mem.eql(u8, name, "mutex_new") or
                std.mem.eql(u8, name, "read_key") or std.mem.eql(u8, name, "read_key_nb") or
                std.mem.eql(u8, name, "term_width") or std.mem.eql(u8, name, "term_height") or
                std.mem.eql(u8, name, "time_ms") or std.mem.eql(u8, name, "mem_mark"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                // Effect tracking
                if (std.mem.eql(u8, name, "system")) {
                    self.current_effects.addBuiltin(.io);
                } else if (std.mem.eql(u8, name, "args_len")) {
                    self.current_effects.addBuiltin(.io);
                } else if (std.mem.eql(u8, name, "fs_mkdir") or std.mem.eql(u8, name, "fs_remove") or
                    std.mem.eql(u8, name, "fs_rename") or std.mem.eql(u8, name, "fs_rmdir"))
                {
                    self.current_effects.addBuiltin(.file_system);
                } else if (std.mem.eql(u8, name, "read_key") or std.mem.eql(u8, name, "read_key_nb")) {
                    self.current_effects.addBuiltin(.io);
                    self.current_effects.addBuiltin(.console);
                } else if (std.mem.eql(u8, name, "term_width") or std.mem.eql(u8, name, "term_height") or
                    std.mem.eql(u8, name, "time_ms"))
                {
                    self.current_effects.addBuiltin(.io);
                }
                return try self.type_ctx.intern(.{ .int = .i64 });
            }
            // Builtins returning float
            if (std.mem.eql(u8, name, "parse_float"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                return try self.type_ctx.intern(.{ .float = .f64 });
            }
            // Builtins returning string
            if (std.mem.eql(u8, name, "int_to_string") or std.mem.eql(u8, name, "bool_to_string") or
                std.mem.eql(u8, name, "float_to_string") or std.mem.eql(u8, name, "substr") or
                std.mem.eql(u8, name, "char_to_string") or std.mem.eql(u8, name, "char_at") or
                std.mem.eql(u8, name, "file_read") or
                std.mem.eql(u8, name, "args_get") or
                std.mem.eql(u8, name, "string_trim") or std.mem.eql(u8, name, "string_replace") or
                std.mem.eql(u8, name, "string_to_upper") or std.mem.eql(u8, name, "string_to_lower") or
                std.mem.eql(u8, name, "path_dirname") or std.mem.eql(u8, name, "path_basename") or
                std.mem.eql(u8, name, "path_extension") or std.mem.eql(u8, name, "path_stem") or
                std.mem.eql(u8, name, "path_join") or std.mem.eql(u8, name, "read_line") or std.mem.eql(u8, name, "chr") or
                std.mem.eql(u8, name, "fs_readdir") or std.mem.eql(u8, name, "fs_getcwd") or
                std.mem.eql(u8, name, "env_get") or
                std.mem.eql(u8, name, "tcp_read"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                // Effect tracking
                if (std.mem.eql(u8, name, "file_read") or std.mem.eql(u8, name, "fs_readdir") or
                    std.mem.eql(u8, name, "fs_getcwd"))
                {
                    self.current_effects.addBuiltin(.file_system);
                } else if (std.mem.eql(u8, name, "env_get")) {
                    self.current_effects.addBuiltin(.io);
                } else if (std.mem.eql(u8, name, "read_line")) {
                    self.current_effects.addBuiltin(.io);
                    self.current_effects.addBuiltin(.console);
                } else if (std.mem.eql(u8, name, "args_get")) {
                    self.current_effects.addBuiltin(.io);
                }
                return try self.type_ctx.intern(.str);
            }
            // string_split returns a fresh type var (codegen handles the split_result struct)
            if (std.mem.eql(u8, name, "string_split"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                return try self.freshTypeVar();
            }
            // Builtins returning bool
            if (std.mem.eql(u8, name, "is_alpha") or std.mem.eql(u8, name, "is_digit") or
                std.mem.eql(u8, name, "is_alnum") or std.mem.eql(u8, name, "is_whitespace") or
                std.mem.eql(u8, name, "string_contains") or std.mem.eql(u8, name, "starts_with") or
                std.mem.eql(u8, name, "ends_with") or std.mem.eql(u8, name, "file_exists") or
                std.mem.eql(u8, name, "fs_exists"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                // Effect tracking
                if (std.mem.eql(u8, name, "file_exists") or std.mem.eql(u8, name, "fs_exists")) {
                    self.current_effects.addBuiltin(.file_system);
                }
                return try self.type_ctx.intern(.bool);
            }
            // List_new returns a fresh type var (its type is inferred from usage context)
            if (std.mem.eql(u8, name, "List_new"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                return try self.freshTypeVar();
            }
            // Box_new(value) returns Box[T] where T is inferred from the argument
            if (std.mem.eql(u8, name, "Box_new"))
            {
                if (fc.args.len > 0) {
                    const inner_type = try self.inferExpr(fc.args[0].value);
                    return try self.type_ctx.makeBox(inner_type);
                }
                return try self.freshTypeVar();
            }
            // Box_null() returns a fresh type var (type inferred from context)
            if (std.mem.eql(u8, name, "Box_null"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                return try self.freshTypeVar();
            }
            // Map_new() returns a fresh type var (type inferred from let binding)
            if (std.mem.eql(u8, name, "Map_new"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                return try self.freshTypeVar();
            }
            // SIMD arithmetic builtins: return same type as first argument
            if (std.mem.eql(u8, name, "simd_add") or std.mem.eql(u8, name, "simd_sub") or
                std.mem.eql(u8, name, "simd_mul") or std.mem.eql(u8, name, "simd_div"))
            {
                var first_type: ?TypeId = null;
                for (fc.args, 0..) |arg, i| {
                    const t = try self.inferExpr(arg.value);
                    if (i == 0) first_type = t;
                }
                return first_type orelse try self.freshTypeVar();
            }
            // SIMD extract: returns scalar (use fresh type var, C handles the actual type)
            if (std.mem.eql(u8, name, "simd_extract"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                return try self.freshTypeVar();
            }
            // SIMD splat/set builtins: simd_splat_f32x4(val) -> f32x4, etc.
            if (std.mem.startsWith(u8, name, "simd_splat_") or std.mem.startsWith(u8, name, "simd_set_"))
            {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                // Extract the SIMD type suffix (e.g., "f32x4" from "simd_splat_f32x4")
                const suffix = if (std.mem.startsWith(u8, name, "simd_splat_"))
                    name[11..]
                else
                    name[9..];
                // Look up the SIMD type by name
                if (self.type_ctx.type_names.get(suffix)) |simd_type_id| {
                    return simd_type_id;
                }
                return try self.freshTypeVar();
            }

            // Implicit generic function calls: identity(42), identity("hello")
            // If the function is known to be generic but called without explicit type args,
            // treat it the same as an explicit generic call: infer arg types and return fresh type var.
            // The codegen handles monomorphization from concrete argument types.
            if (fc.generic_args == null or fc.generic_args.?.len == 0) {
                if (self.generic_function_names.contains(name)) {
                    for (fc.args) |arg| {
                        _ = try self.inferExpr(arg.value);
                    }
                    return try self.freshTypeVar();
                }
            }

            // Generic function calls: identity[int](42)
            // If the call has explicit generic args, infer argument types and
            // return a fresh type variable (codegen handles monomorphization)
            if (fc.generic_args != null and fc.generic_args.?.len > 0) {
                for (fc.args) |arg| {
                    _ = try self.inferExpr(arg.value);
                }
                return try self.freshTypeVar();
            }
        }

        // Special handling for enum variant constructors: Option::Some(42)
        if (fc.function.kind == .path) {
            const path = fc.function.kind.path;
            if (path.segments.len >= 2) {
                if (self.env.lookup(path.segments[0].name)) |symbol| {
                    const resolved_type = self.type_ctx.get(symbol.type_id);
                    if (resolved_type) |rt| {
                        if (rt == .@"enum") {
                            // This is an enum variant constructor call
                            // Infer argument types but don't constrain them
                            for (fc.args) |arg| {
                                _ = try self.inferExpr(arg.value);
                            }
                            return symbol.type_id;
                        }
                    }
                }
            }
        }

        const func_type = try self.inferExpr(fc.function);
        const resolved = self.resolveType(func_type);
        const typ = self.type_ctx.get(resolved) orelse {
            try self.reportError(.expected_function_type, "cannot call unknown type", fc.span);
            return try self.type_ctx.intern(.error_type);
        };

        if (typ != .function) {
            try self.reportError(.expected_function_type, "expected function type", fc.span);
            return try self.type_ctx.intern(.error_type);
        }

        const fn_type = typ.function;

        // Check argument count
        if (fc.args.len != fn_type.param_types.len) {
            try self.reportError(.type_parameter_count_mismatch, "wrong number of arguments", fc.span);
        }

        // Check argument types
        const min_args = @min(fc.args.len, fn_type.param_types.len);
        for (fc.args[0..min_args], fn_type.param_types[0..min_args]) |arg, param_type| {
            const arg_type = try self.inferExpr(arg.value);
            try self.addConstraint(Constraint.subtype(arg_type, param_type, fc.span));
        }

        // Propagate effects
        self.current_effects = self.current_effects.unionWith(fn_type.effects);

        return fn_type.return_type;
    }

    fn inferStructLiteral(self: *Self, sl: *const ast.StructLiteral) anyerror!TypeId {
        if (sl.type_path) |path| {
            if (path.segments.len > 0) {
                if (self.env.lookup(path.segments[0].name)) |symbol| {
                    // Verify fields
                    const struct_type = self.type_ctx.get(symbol.type_id);
                    if (struct_type) |st| {
                        if (st == .@"struct") {
                            for (sl.fields) |field| {
                                const field_type = try self.inferExpr(field.value);
                                if (st.@"struct".getField(field.name.name)) |sf| {
                                    try self.unify(field_type, sf.type_id, field.span);
                                } else {
                                    try self.reportError(.undefined_field, "unknown field", field.span);
                                }
                            }
                        }
                    }
                    return symbol.type_id;
                }
            }
        }

        // Anonymous struct
        var field_types = std.ArrayList(types.StructField).init(self.allocator);
        defer field_types.deinit();

        for (sl.fields) |field| {
            const field_type = try self.inferExpr(field.value);
            try field_types.append(.{
                .name = field.name.name,
                .type_id = field_type,
                .is_mutable = false,
                .default_value = null,
                .offset = null,
            });
        }

        return try self.type_ctx.intern(.{
            .@"struct" = .{
                .name = null,
                .fields = field_types.items,
                .type_params = &[_][]const u8{},
                .is_packed = false,
                .size = null,
                .alignment = null,
            },
        });
    }

    fn inferEnumLiteral(self: *Self, el: *const ast.EnumLiteral) anyerror!TypeId {
        if (el.type_path) |path| {
            if (path.segments.len > 0) {
                if (self.env.lookup(path.segments[0].name)) |symbol| {
                    return symbol.type_id;
                }
            }
        }

        try self.reportError(.undefined_type, "unknown enum type", el.span);
        return try self.type_ctx.intern(.error_type);
    }

    fn inferArrayLiteral(self: *Self, al: *const ast.ArrayLiteral) anyerror!TypeId {
        switch (al.kind) {
            .elements => |elements| {
                if (elements.len == 0) {
                    const elem_type = try self.freshTypeVar();
                    return try self.type_ctx.makeArray(elem_type, 0);
                }

                const first_type = try self.inferExpr(elements[0]);
                for (elements[1..]) |elem| {
                    const elem_type = try self.inferExpr(elem);
                    try self.unify(first_type, elem_type, al.span);
                }

                return try self.type_ctx.makeArray(first_type, elements.len);
            },
            .repeat => |rep| {
                const val_type = try self.inferExpr(rep.value);
                _ = try self.inferExpr(rep.count);
                const size = evalComptimeUsize(rep.count) orelse 0;
                return try self.type_ctx.makeArray(val_type, size);
            },
        }
    }

    fn inferTupleLiteral(self: *Self, tl: *const ast.TupleLiteral) anyerror!TypeId {
        var elem_types = std.ArrayList(TypeId).init(self.allocator);
        defer elem_types.deinit();

        for (tl.elements) |elem| {
            const elem_type = try self.inferExpr(elem);
            try elem_types.append(elem_type);
        }

        return try self.type_ctx.makeTuple(elem_types.items);
    }

    fn inferIfExpr(self: *Self, ie: *const ast.IfExpr) anyerror!TypeId {
        const cond_type = try self.inferExpr(ie.condition);
        const bool_type = try self.type_ctx.intern(.bool);
        try self.unify(cond_type, bool_type, ie.span);

        const then_type = try self.inferBlock(ie.then_branch);

        if (ie.else_branch) |else_br| {
            const else_type = switch (else_br) {
                .else_block => |blk| try self.inferBlock(blk),
                .else_if => |elif| try self.inferIfExpr(elif),
            };

            // Both branches must have compatible types
            if (types.commonType(self.type_ctx, then_type, else_type)) |common| {
                return common;
            }
            try self.reportTypeMismatch(then_type, else_type, ie.span);
            return then_type;
        }

        // If without else has unit type
        return try self.type_ctx.intern(.unit);
    }

    fn inferMatchExpr(self: *Self, me: *const ast.MatchExpr) anyerror!TypeId {
        const scrutinee_type = try self.inferExpr(me.scrutinee);

        if (me.arms.len == 0) {
            try self.reportError(.empty_match, "match expression has no arms", me.span);
            return try self.type_ctx.intern(.error_type);
        }

        // Check all arms and collect their types
        var arm_type: ?TypeId = null;

        for (me.arms) |arm| {
            // Each arm gets its own scope for pattern bindings
            try self.env.pushScope(.block);

            try self.checkPattern(arm.pattern, scrutinee_type);
            // Register pattern bindings in scope for guard and body
            try self.bindPattern(arm.pattern, scrutinee_type, false);

            if (arm.guard) |guard| {
                const guard_type = try self.inferExpr(guard);
                const bool_type = try self.type_ctx.intern(.bool);
                try self.unify(guard_type, bool_type, arm.span);
            }

            const body_type = switch (arm.body) {
                .expression => |expr| try self.inferExpr(expr),
                .block => |blk| try self.inferBlock(blk),
            };

            self.env.popScope();

            if (arm_type) |at| {
                try self.unify(at, body_type, arm.span);
            } else {
                arm_type = body_type;
            }
        }

        // Check exhaustiveness
        try self.checkExhaustiveness(scrutinee_type, me.arms, me.span);

        return arm_type orelse try self.type_ctx.intern(.unit);
    }

    fn inferBlock(self: *Self, blk: *const ast.BlockExpr) anyerror!TypeId {
        try self.env.pushScope(.block);
        defer self.env.popScope();

        for (blk.statements) |stmt| {
            try self.checkStatement(stmt);
        }

        if (blk.result) |result| {
            return try self.inferExpr(result);
        }

        // If the last statement is a return, the block diverges (never type)
        if (blk.statements.len > 0) {
            const last = blk.statements[blk.statements.len - 1];
            if (last.kind == .return_stmt) {
                return try self.type_ctx.intern(.never);
            }
        }

        return try self.type_ctx.intern(.unit);
    }

    fn inferLambda(self: *Self, lam: *const ast.LambdaExpr) anyerror!TypeId {
        try self.env.pushScope(.function);
        defer self.env.popScope();

        var param_types = std.ArrayList(TypeId).init(self.allocator);
        defer param_types.deinit();

        for (lam.params) |param| {
            const param_type = if (param.type_expr) |te|
                try self.resolveTypeExpr(te)
            else
                try self.freshTypeVar();

            try param_types.append(param_type);
            try self.env.define(Symbol.init(param.name.name, .variable, param_type));
        }

        // If the lambda has an explicit return type annotation, resolve it and
        // set it on the function scope so that return statements inside the
        // block body can be properly type-checked against it.
        const declared_return_type = if (lam.return_type) |rt|
            try self.resolveTypeExpr(rt)
        else
            null;

        if (declared_return_type) |rt| {
            self.env.current.return_type = rt;
        }

        const body_type = switch (lam.body) {
            .expression => |expr| try self.inferExpr(expr),
            .block => |blk| try self.inferBlock(blk),
        };

        const return_type = declared_return_type orelse body_type;

        // Only unify body_type with return_type if the body has a result expression.
        // For block bodies with return statements (body_type == unit), the return
        // type is determined by the explicit annotation or the return statements.
        const unit_type = try self.type_ctx.intern(.unit);
        if (body_type != unit_type or declared_return_type == null) {
            try self.unify(body_type, return_type, lam.span);
        }

        return try self.type_ctx.makeFunction(param_types.items, return_type, EffectSet.pure);
    }

    fn inferPipeline(self: *Self, pipe: *const ast.PipelineExpr) anyerror!TypeId {
        // Pipeline desugars: x |> f => f(x), x |> f(a,b) => f(x,a,b)
        // We construct a virtual function call and type-check it normally.
        switch (pipe.right.kind) {
            .function_call => |fc| {
                // x |> f(args...) => f(x, args...)
                // Build new args array with pipe.left prepended
                const new_args = try self.allocator.alloc(*ast.FunctionCall.CallArg, fc.args.len + 1);
                const left_arg = try self.allocator.create(ast.FunctionCall.CallArg);
                left_arg.* = .{ .name = null, .value = pipe.left };
                new_args[0] = left_arg;
                for (fc.args, 0..) |arg, i| {
                    new_args[i + 1] = arg;
                }
                const virtual_call = try self.allocator.create(ast.FunctionCall);
                virtual_call.* = .{
                    .function = fc.function,
                    .generic_args = fc.generic_args,
                    .args = new_args,
                    .span = pipe.span,
                };
                return self.inferFunctionCall(virtual_call);
            },
            .identifier => {
                // x |> f => f(x)
                const left_arg = try self.allocator.create(ast.FunctionCall.CallArg);
                left_arg.* = .{ .name = null, .value = pipe.left };
                const new_args = try self.allocator.alloc(*ast.FunctionCall.CallArg, 1);
                new_args[0] = left_arg;
                const virtual_call = try self.allocator.create(ast.FunctionCall);
                virtual_call.* = .{
                    .function = pipe.right,
                    .generic_args = null,
                    .args = new_args,
                    .span = pipe.span,
                };
                return self.inferFunctionCall(virtual_call);
            },
            else => {
                // Fallback: try original approach
                _ = try self.inferExpr(pipe.left);
                const right_type = try self.inferExpr(pipe.right);
                const resolved_right = self.resolveType(right_type);
                const typ = self.type_ctx.get(resolved_right) orelse {
                    try self.reportError(.expected_function_type, "pipeline requires function on right", pipe.span);
                    return try self.type_ctx.intern(.error_type);
                };
                if (typ != .function) {
                    try self.reportError(.expected_function_type, "pipeline requires function", pipe.span);
                    return try self.type_ctx.intern(.error_type);
                }
                return typ.function.return_type;
            },
        }
    }

    fn inferErrorPropagate(self: *Self, ep: *const ast.ErrorPropagateExpr) anyerror!TypeId {
        const operand_type = try self.inferExpr(ep.operand);
        const resolved = self.resolveType(operand_type);
        const typ = self.type_ctx.get(resolved) orelse {
            try self.reportError(.expected_result_type, "? operator requires Result type", ep.span);
            return try self.type_ctx.intern(.error_type);
        };

        if (typ == .result) {
            return typ.result.ok_type;
        } else if (typ == .option) {
            return typ.option;
        }

        try self.reportError(.expected_result_type, "? operator requires Result or Option type", ep.span);
        return try self.type_ctx.intern(.error_type);
    }

    fn inferAwaitExpr(self: *Self, aw: *const ast.AwaitExpr) anyerror!TypeId {
        const operand_type = try self.inferExpr(aw.operand);
        const resolved = self.resolveType(operand_type);
        const typ = self.type_ctx.get(resolved) orelse {
            try self.reportError(.expected_result_type, "await requires Future type", aw.span);
            return try self.type_ctx.intern(.error_type);
        };

        if (typ == .future) {
            return typ.future;
        }

        try self.reportError(.expected_result_type, "await requires Future type", aw.span);
        return try self.type_ctx.intern(.error_type);
    }

    fn inferCoalesce(self: *Self, coal: *const ast.CoalesceExpr) anyerror!TypeId {
        const left_type = try self.inferExpr(coal.left);
        const right_type = try self.inferExpr(coal.right);

        const resolved = self.resolveType(left_type);
        const typ = self.type_ctx.get(resolved) orelse {
            try self.reportError(.expected_option_type, "?? operator requires Option type", coal.span);
            return try self.type_ctx.intern(.error_type);
        };

        if (typ == .option) {
            try self.unify(typ.option, right_type, coal.span);
            return typ.option;
        }

        try self.reportError(.expected_option_type, "?? operator requires Option type", coal.span);
        return try self.type_ctx.intern(.error_type);
    }

    fn inferRange(self: *Self, rng: *const ast.RangeExpr) anyerror!TypeId {
        var elem_type: TypeId = try self.type_ctx.intern(.{ .int = .i64 });

        if (rng.start) |start| {
            elem_type = try self.inferExpr(start);
        }
        if (rng.end) |end| {
            const end_type = try self.inferExpr(end);
            try self.unify(elem_type, end_type, rng.span);
        }

        // Range[T] type - for now just return the element type
        return elem_type;
    }

    fn inferCast(self: *Self, cast: *const ast.CastExpr) anyerror!TypeId {
        _ = try self.inferExpr(cast.expr);
        return try self.resolveTypeExpr(cast.target_type);
    }

    fn inferTypeCheck(self: *Self, tc: *const ast.TypeCheckExpr) anyerror!TypeId {
        _ = try self.inferExpr(tc.expr);
        _ = try self.resolveTypeExpr(tc.checked_type);
        return try self.type_ctx.intern(.bool);
    }

    fn inferStringInterpolation(self: *Self, si: *const ast.StringInterpolation) anyerror!TypeId {
        // Type-check all expression parts; the result is always string
        for (si.parts) |part| {
            switch (part) {
                .expr => |expr| {
                    _ = try self.inferExpr(expr);
                },
                .literal => {},
            }
        }
        return try self.type_ctx.intern(.str);
    }

    // ========================================================================
    // STATEMENT CHECKING
    // ========================================================================

    pub fn checkStatement(self: *Self, stmt: *const ast.Statement) !void {
        switch (stmt.kind) {
            .let_binding => |lb| try self.checkLetBinding(lb),
            .return_stmt => |rs| try self.checkReturn(rs),
            .if_stmt => |ifs| _ = try self.inferIfExpr(ifs),
            .match_stmt => |ms| _ = try self.inferMatchExpr(ms),
            .for_loop => |fl| try self.checkForLoop(fl),
            .while_loop => |wl| try self.checkWhileLoop(wl),
            .loop_stmt => |ls| try self.checkLoop(ls),
            .break_stmt => |bs| try self.checkBreak(bs),
            .continue_stmt => |cs| try self.checkContinue(cs),
            .region_block => |rb| try self.checkRegionBlock(rb),
            .expression => |expr| _ = try self.inferExpr(expr),
            .assignment => |assign| try self.checkAssignment(assign),
            .discard => |disc| _ = try self.inferExpr(disc.value),
        }
    }

    fn checkLetBinding(self: *Self, lb: *const ast.LetBinding) !void {
        const declared_type = if (lb.type_annotation) |ta|
            try self.resolveTypeExpr(ta)
        else
            null;

        // If there's a type annotation and the value is an unsuffixed numeric literal,
        // use the declared type directly (allows `let x: i32 = 42`)
        const value_type = if (lb.value) |val| blk: {
            if (declared_type) |dt| {
                if (self.isUnsuffixedNumericLiteral(val, dt)) {
                    break :blk dt;
                }
            }
            break :blk try self.inferExpr(val);
        } else try self.freshTypeVar();

        const final_type = declared_type orelse value_type;

        if (lb.value != null and declared_type != null) {
            // Skip unification for dyn Trait assignments — any concrete type
            // that implements the trait is valid (checked at codegen time)
            const is_dyn = if (lb.type_annotation) |ta| ta.kind == .trait_object else false;
            if (!is_dyn) {
                try self.unify(value_type, final_type, lb.span);
            }
        }

        // Bind pattern
        try self.bindPattern(lb.pattern, final_type, lb.is_mut);
    }

    /// Check if an expression is an unsuffixed integer or float literal that
    /// can adopt the given target type.
    fn isUnsuffixedNumericLiteral(self: *Self, expr: *const ast.Expr, target_type: TypeId) bool {
        switch (expr.kind) {
            .literal => |lit| {
                switch (lit.kind) {
                    .int => |int_lit| {
                        if (int_lit.suffix != null) return false;
                        // Target must be an integer type
                        if (self.type_ctx.get(target_type)) |t| {
                            return std.meta.activeTag(t) == .int;
                        }
                        return false;
                    },
                    .float => |float_lit| {
                        if (float_lit.suffix != null) return false;
                        // Target must be a float type
                        if (self.type_ctx.get(target_type)) |t| {
                            return std.meta.activeTag(t) == .float;
                        }
                        return false;
                    },
                    else => return false,
                }
            },
            .unary => |un| {
                // Handle negative literals: -42
                if (un.op == .neg) {
                    return self.isUnsuffixedNumericLiteral(un.operand, target_type);
                }
                return false;
            },
            else => return false,
        }
    }

    fn checkReturn(self: *Self, rs: *const ast.ReturnStmt) !void {
        const fn_scope = self.env.current.enclosingFunction();
        if (fn_scope == null) {
            try self.reportError(.unexpected_token, "return outside function", rs.span);
            return;
        }

        const return_type = if (rs.value) |val|
            try self.inferExpr(val)
        else
            try self.type_ctx.intern(.unit);

        if (fn_scope.?.return_type) |expected| {
            try self.unify(return_type, expected, rs.span);
        }
    }

    fn checkForLoop(self: *Self, fl: *const ast.ForLoop) !void {
        try self.env.pushScope(.loop);
        defer self.env.popScope();

        if (fl.label) |lbl| {
            self.env.current.label = lbl.name;
        }

        const iter_type = try self.inferExpr(fl.iterator);
        const elem_type = try self.getIteratorElementType(iter_type, fl.span);

        try self.bindPattern(fl.pattern, elem_type, false);
        _ = try self.inferBlock(fl.body);
    }

    fn checkWhileLoop(self: *Self, wl: *const ast.WhileLoop) !void {
        try self.env.pushScope(.loop);
        defer self.env.popScope();

        if (wl.label) |lbl| {
            self.env.current.label = lbl.name;
        }

        const cond_type = try self.inferExpr(wl.condition);
        const bool_type = try self.type_ctx.intern(.bool);
        try self.unify(cond_type, bool_type, wl.span);

        _ = try self.inferBlock(wl.body);
    }

    fn checkLoop(self: *Self, ls: *const ast.LoopStmt) !void {
        try self.env.pushScope(.loop);
        defer self.env.popScope();

        if (ls.label) |lbl| {
            self.env.current.label = lbl.name;
        }

        _ = try self.inferBlock(ls.body);
    }

    fn checkBreak(self: *Self, bs: *const ast.BreakStmt) !void {
        if (!self.env.current.isInLoop()) {
            try self.reportError(.unexpected_token, "break outside loop", bs.span);
        }

        if (bs.value) |val| {
            _ = try self.inferExpr(val);
        }
    }

    fn checkContinue(self: *Self, cs: *const ast.ContinueStmt) !void {
        if (!self.env.current.isInLoop()) {
            try self.reportError(.unexpected_token, "continue outside loop", cs.span);
        }
    }

    fn checkRegionBlock(self: *Self, rb: *const ast.RegionBlock) !void {
        try self.env.pushScope(.block);
        defer self.env.popScope();

        // Register the region
        const region_type = try self.type_ctx.intern(.{
            .region = .{
                .name = rb.name.name,
                .parent = null,
                .is_static = false,
            },
        });
        try self.env.define(Symbol.init(rb.name.name, .type_def, region_type));

        _ = try self.inferBlock(rb.body);
    }

    fn checkAssignment(self: *Self, assign: *const ast.Assignment) !void {
        const target_type = try self.inferExpr(assign.target);
        const value_type = try self.inferExpr(assign.value);

        // Check mutability
        if (assign.target.kind == .identifier) {
            const name = assign.target.kind.identifier.name;
            if (self.env.lookup(name)) |symbol| {
                if (!symbol.is_mutable) {
                    try self.reportError(.reassign_immutable, "cannot assign to immutable variable", assign.span);
                }
            }
        }

        // Check type compatibility based on operator
        switch (assign.op) {
            .assign => {
                try self.unify(value_type, target_type, assign.span);
            },
            .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => {
                if (!types.isNumeric(self.type_ctx, target_type)) {
                    try self.reportError(.expected_numeric_type, "compound assignment requires numeric type", assign.span);
                }
                try self.unify(value_type, target_type, assign.span);
            },
            .bit_and_assign, .bit_or_assign, .bit_xor_assign, .shl_assign, .shr_assign => {
                if (!types.isIntegral(self.type_ctx, target_type)) {
                    try self.reportError(.expected_numeric_type, "bitwise assignment requires integer type", assign.span);
                }
                try self.unify(value_type, target_type, assign.span);
            },
        }
    }

    // ========================================================================
    // PATTERN CHECKING
    // ========================================================================

    fn checkPattern(self: *Self, pattern: *const ast.Pattern, expected_type: TypeId) !void {
        // Auto-deref Box types: match on the inner type
        const resolved_expected = self.resolveType(expected_type);
        if (self.type_ctx.get(resolved_expected)) |t| {
            if (t == .box) {
                return self.checkPattern(pattern, t.box);
            }
        }
        switch (pattern.kind) {
            .literal => |lit| {
                const lit_type = try self.inferLiteral(lit);
                try self.unify(lit_type, expected_type, pattern.span);
            },
            .identifier => {
                // Identifier patterns match any type
            },
            .wildcard => {
                // Wildcard matches any type
            },
            .tuple => |tup| {
                const resolved = self.resolveType(expected_type);
                const typ = self.type_ctx.get(resolved);
                if (typ != null and typ.? == .tuple) {
                    const tuple = typ.?.tuple;
                    if (tup.elements.len != tuple.element_types.len) {
                        try self.reportError(.type_mismatch, "tuple pattern has wrong number of elements", pattern.span);
                    }
                    const min_len = @min(tup.elements.len, tuple.element_types.len);
                    for (tup.elements[0..min_len], tuple.element_types[0..min_len]) |elem, elem_type| {
                        try self.checkPattern(elem, elem_type);
                    }
                }
            },
            .struct_pattern => |sp| {
                const resolved = self.resolveType(expected_type);
                const typ = self.type_ctx.get(resolved);
                if (typ != null and typ.? == .@"struct") {
                    const struct_type = typ.?.@"struct";
                    for (sp.fields) |field| {
                        if (struct_type.getField(field.name.name)) |sf| {
                            if (field.pattern) |fp| {
                                try self.checkPattern(fp, sf.type_id);
                            }
                        } else {
                            try self.reportError(.undefined_field, "unknown field in pattern", field.span);
                        }
                    }
                }
            },
            .enum_variant => |ev| {
                const resolved = self.resolveType(expected_type);
                const typ = self.type_ctx.get(resolved);
                if (typ != null and typ.? == .@"enum") {
                    if (typ.?.@"enum".getVariant(ev.variant.name) == null) {
                        try self.reportError(.undefined_variant, "unknown enum variant", pattern.span);
                    }
                }
            },
            .or_pattern => |orp| {
                for (orp.patterns) |p| {
                    try self.checkPattern(p, expected_type);
                }
            },
            .range => |rp| {
                if (rp.start) |start| {
                    const start_type = try self.inferExpr(start);
                    try self.unify(start_type, expected_type, pattern.span);
                }
                if (rp.end) |end| {
                    const end_type = try self.inferExpr(end);
                    try self.unify(end_type, expected_type, pattern.span);
                }
            },
            .slice => |sp| {
                const resolved = self.resolveType(expected_type);
                const typ = self.type_ctx.get(resolved);
                if (typ != null) {
                    const elem_type = switch (typ.?) {
                        .array => |arr| arr.element_type,
                        .slice => |s| s.element_type,
                        else => try self.freshTypeVar(),
                    };
                    for (sp.elements) |elem| {
                        try self.checkPattern(elem, elem_type);
                    }
                }
            },
            .ref_pattern => |rp| {
                const resolved = self.resolveType(expected_type);
                const typ = self.type_ctx.get(resolved);
                if (typ != null and typ.? == .ref) {
                    try self.checkPattern(rp.pattern, typ.?.ref.pointee);
                }
            },
        }
    }

    fn bindPattern(self: *Self, pattern: *const ast.Pattern, typ: TypeId, is_mut: bool) !void {
        // Auto-deref Box types for enum variant/struct/tuple patterns (match destructuring)
        // but NOT for simple identifier bindings (let a: Box[T] should keep the Box type)
        if (pattern.kind != .identifier and pattern.kind != .wildcard) {
            const resolved_box = self.resolveType(typ);
            if (self.type_ctx.get(resolved_box)) |bt| {
                if (bt == .box) {
                    return self.bindPattern(pattern, bt.box, is_mut);
                }
            }
        }
        switch (pattern.kind) {
            .identifier => |ident| {
                try self.env.define(Symbol.init(ident.name.name, .variable, typ).withMutable(is_mut or ident.is_mut));
            },
            .wildcard => {},
            .tuple => |tup| {
                const resolved = self.resolveType(typ);
                const t = self.type_ctx.get(resolved);
                if (t != null and t.? == .tuple) {
                    const min_len = @min(tup.elements.len, t.?.tuple.element_types.len);
                    for (tup.elements[0..min_len], t.?.tuple.element_types[0..min_len]) |elem, elem_type| {
                        try self.bindPattern(elem, elem_type, is_mut);
                    }
                }
            },
            .struct_pattern => |sp| {
                const resolved = self.resolveType(typ);
                const t = self.type_ctx.get(resolved);
                if (t != null and t.? == .@"struct") {
                    for (sp.fields) |field| {
                        if (t.?.@"struct".getField(field.name.name)) |sf| {
                            if (field.pattern) |fp| {
                                try self.bindPattern(fp, sf.type_id, is_mut);
                            } else {
                                try self.env.define(Symbol.init(field.name.name, .variable, sf.type_id).withMutable(is_mut));
                            }
                        }
                    }
                }
            },
            .enum_variant => |ev| {
                // Look up payload types from the enum definition
                const resolved_enum = self.resolveType(typ);
                const enum_typ = self.type_ctx.get(resolved_enum);
                const variant_info = if (enum_typ != null and enum_typ.? == .@"enum")
                    enum_typ.?.@"enum".getVariant(ev.variant.name)
                else
                    null;

                switch (ev.payload) {
                    .none => {},
                    .tuple => |patterns| {
                        for (patterns, 0..) |p, idx| {
                            const payload_type = if (variant_info != null and idx < variant_info.?.payload_types.len)
                                variant_info.?.payload_types[idx]
                            else
                                try self.freshTypeVar();
                            try self.bindPattern(p, payload_type, is_mut);
                        }
                    },
                    .struct_fields => |fields| {
                        for (fields) |field| {
                            if (field.pattern) |fp| {
                                try self.bindPattern(fp, try self.freshTypeVar(), is_mut);
                            }
                        }
                    },
                }
            },
            .or_pattern => |orp| {
                // All alternatives must bind same variables
                if (orp.patterns.len > 0) {
                    try self.bindPattern(orp.patterns[0], typ, is_mut);
                }
            },
            .ref_pattern => |rp| {
                const resolved = self.resolveType(typ);
                const t = self.type_ctx.get(resolved);
                if (t != null and t.? == .ref) {
                    try self.bindPattern(rp.pattern, t.?.ref.pointee, is_mut);
                }
            },
            else => {},
        }
    }

    fn checkExhaustiveness(self: *Self, scrutinee_type: TypeId, arms: []const *ast.MatchArm, span: ast.Span) !void {
        const resolved = self.resolveType(scrutinee_type);
        const typ = self.type_ctx.get(resolved) orelse return;

        // Auto-deref Box types for exhaustiveness checking
        if (typ == .box) {
            const inner_resolved = self.resolveType(typ.box);
            return self.checkExhaustiveness(inner_resolved, arms, span);
        }

        switch (typ) {
            .@"enum" => |e| {
                // Check for wildcard/identifier patterns that cover all variants
                var has_catchall = false;
                for (arms) |arm| {
                    if (arm.pattern.kind == .wildcard or arm.pattern.kind == .identifier) {
                        has_catchall = true;
                        break;
                    }
                }

                if (!has_catchall) {
                    var covered = std.StringHashMap(void).init(self.allocator);
                    defer covered.deinit();

                    for (arms) |arm| {
                        try self.collectCoveredVariants(arm.pattern, &covered);
                    }

                    for (e.variants) |variant| {
                        if (!covered.contains(variant.name)) {
                            const msg = try std.fmt.allocPrint(self.allocator, "non-exhaustive pattern: missing variant '{s}'", .{variant.name});
                            try self.reportError(.missing_match_arm, msg, span);
                            return;
                        }
                    }
                }
            },
            .bool => {
                var has_true = false;
                var has_false = false;
                var has_wildcard = false;

                for (arms) |arm| {
                    switch (arm.pattern.kind) {
                        .wildcard => has_wildcard = true,
                        .literal => |lit| {
                            if (lit.kind == .bool) {
                                if (lit.kind.bool) has_true = true else has_false = true;
                            }
                        },
                        .identifier => has_wildcard = true,
                        else => {},
                    }
                }

                if (!has_wildcard and !(has_true and has_false)) {
                    try self.reportError(.missing_match_arm, "non-exhaustive pattern", span);
                }
            },
            .result => {
                // Result[T, E] is exhaustive if Ok + Err are covered (or wildcard)
                var has_ok = false;
                var has_err = false;
                var has_wildcard = false;
                for (arms) |arm| {
                    switch (arm.pattern.kind) {
                        .wildcard, .identifier => has_wildcard = true,
                        .enum_variant => |ev| {
                            if (std.mem.eql(u8, ev.variant.name, "Ok")) has_ok = true;
                            if (std.mem.eql(u8, ev.variant.name, "Err")) has_err = true;
                        },
                        else => {},
                    }
                }
                if (!has_wildcard and !(has_ok and has_err)) {
                    try self.reportError(.missing_match_arm, "non-exhaustive pattern: add a wildcard", span);
                }
            },
            .option => {
                // Option[T] is exhaustive if Some + None are covered (or wildcard)
                var has_some = false;
                var has_none = false;
                var has_wildcard = false;
                for (arms) |arm| {
                    switch (arm.pattern.kind) {
                        .wildcard, .identifier => has_wildcard = true,
                        .enum_variant => |ev| {
                            if (std.mem.eql(u8, ev.variant.name, "Some")) has_some = true;
                            if (std.mem.eql(u8, ev.variant.name, "None")) has_none = true;
                        },
                        else => {},
                    }
                }
                if (!has_wildcard and !(has_some and has_none)) {
                    try self.reportError(.missing_match_arm, "non-exhaustive pattern: add a wildcard", span);
                }
            },
            else => {
                // For other types, check for wildcard
                var has_wildcard = false;
                for (arms) |arm| {
                    if (arm.pattern.kind == .wildcard or arm.pattern.kind == .identifier) {
                        has_wildcard = true;
                        break;
                    }
                }
                if (!has_wildcard) {
                    try self.reportError(.missing_match_arm, "non-exhaustive pattern: add a wildcard", span);
                }
            },
        }
    }

    fn collectCoveredVariants(self: *Self, pattern: *const ast.Pattern, covered: *std.StringHashMap(void)) !void {
        switch (pattern.kind) {
            .wildcard, .identifier => {
                // Wildcard covers all
                covered.clearRetainingCapacity();
            },
            .enum_variant => |ev| {
                try covered.put(ev.variant.name, {});
            },
            .or_pattern => |orp| {
                for (orp.patterns) |p| {
                    try self.collectCoveredVariants(p, covered);
                }
            },
            else => {},
        }
    }

    // ========================================================================
    // DECLARATION CHECKING
    // ========================================================================

    pub fn checkDeclaration(self: *Self, decl: *const ast.Declaration) !void {
        switch (decl.kind) {
            .function => |func| try self.checkFunction(func, decl.visibility == .public),
            // Structs, enums, and traits are already checked in the first pass
            .struct_def => {},
            .enum_def => {},
            .trait_def => {},
            .impl_block => |ib| try self.checkImpl(ib),
            .constant => |cd| try self.checkConst(cd, decl.visibility == .public),
        }
    }

    /// Pre-register a function's type signature without checking its body.
    /// This enables forward function references across the module.
    fn registerFunctionSignature(self: *Self, func: *const ast.FunctionDecl, is_public: bool) !void {
        // Skip if already registered (e.g. duplicate function name - error reported later)
        if (self.env.lookupLocal(func.name.name)) |_| {
            return;
        }

        // For generic functions, register generic type params in a temporary scope
        const has_generics = func.generic_params != null and func.generic_params.?.len > 0;
        if (has_generics) {
            try self.generic_function_names.put(func.name.name, {});
            try self.env.pushScope(.function);
            for (func.generic_params.?) |gp| {
                try self.env.define(Symbol.init(gp.name.name, .type_param, try self.freshTypeVar()));
            }
        }

        // Build function type from signature
        var param_types = std.ArrayList(TypeId).init(self.allocator);
        defer param_types.deinit();

        for (func.params) |param| {
            const param_type = try self.resolveTypeExpr(param.type_expr);
            try param_types.append(param_type);
        }

        const inner_return_type = if (func.return_type) |rt|
            try self.resolveTypeExpr(rt)
        else
            try self.type_ctx.intern(.unit);

        // For async functions, external return type is Future[T]
        const return_type = if (func.is_async)
            try self.type_ctx.makeFuture(inner_return_type)
        else
            inner_return_type;

        var effects = EffectSet.pure;
        if (func.effects) |effs| {
            for (effs) |eff| {
                try self.addEffectFromTypeExpr(eff, &effects);
            }
        }

        if (func.is_async) {
            effects.addBuiltin(.async_effect);
        }

        if (has_generics) {
            self.env.popScope();
        }

        const fn_type = try self.type_ctx.makeFunction(param_types.items, return_type, effects);
        try self.env.define(Symbol.init(func.name.name, .function, fn_type)
            .withPublic(is_public)
            .withEffects(effects)
            .withLocation(func.span));
    }

    fn checkFunction(self: *Self, func: *const ast.FunctionDecl, is_public: bool) !void {
        // For generic functions, register generic type params in a temporary scope for type resolution
        const has_generics = func.generic_params != null and func.generic_params.?.len > 0;
        if (has_generics) {
            try self.env.pushScope(.function);
            for (func.generic_params.?) |gp| {
                try self.env.define(Symbol.init(gp.name.name, .type_param, try self.freshTypeVar()));
            }
        }

        // Build function type from signature
        var param_types = std.ArrayList(TypeId).init(self.allocator);
        defer param_types.deinit();

        for (func.params) |param| {
            const param_type = try self.resolveTypeExpr(param.type_expr);
            try param_types.append(param_type);
        }

        // Process return type
        const inner_return_type = if (func.return_type) |rt|
            try self.resolveTypeExpr(rt)
        else
            try self.type_ctx.intern(.unit);

        // For async functions, the external return type is Future[T],
        // but the body type-checks against the inner T
        const return_type = if (func.is_async)
            try self.type_ctx.makeFuture(inner_return_type)
        else
            inner_return_type;

        // Process effects
        var effects = EffectSet.pure;
        if (func.effects) |effs| {
            for (effs) |eff| {
                try self.addEffectFromTypeExpr(eff, &effects);
            }
        }

        // Auto-add async effect for async functions
        if (func.is_async) {
            effects.addBuiltin(.async_effect);
        }

        if (has_generics) {
            self.env.popScope();
        }

        // Register function if not already pre-registered from pass 1.5
        if (self.env.lookupLocal(func.name.name)) |existing| {
            if (existing.kind != .function) {
                try self.reportError(.duplicate_definition, "duplicate function definition", func.span);
                return;
            }
            // Already pre-registered, proceed to check body
        } else {
            // Not pre-registered (e.g. impl method), register now
            const fn_type = try self.type_ctx.makeFunction(param_types.items, return_type, effects);
            try self.env.define(Symbol.init(func.name.name, .function, fn_type)
                .withPublic(is_public)
                .withEffects(effects)
                .withLocation(func.span));
        }

        // Now push a function scope for the body
        try self.env.pushScope(.function);
        defer self.env.popScope();

        // Register generic parameters in function scope
        if (func.generic_params) |gps| {
            for (gps) |gp| {
                const type_var = try self.namedTypeVar(gp.name.name);
                try self.env.define(Symbol.init(gp.name.name, .type_param, type_var));
            }
        }

        // Define parameters in function scope
        for (func.params) |param| {
            const param_type = try self.resolveTypeExpr(param.type_expr);
            try self.env.define(Symbol.init(param.name.name, .variable, param_type).withMutable(param.is_mut));
        }

        // For async functions, the body checks against inner_return_type (T, not Future[T])
        const body_return_type = if (func.is_async) inner_return_type else return_type;
        self.env.current.return_type = body_return_type;
        self.env.current.allowed_effects = effects;

        // Check contracts
        if (func.contracts) |contracts| {
            for (contracts.requires) |req| {
                const req_type = try self.inferExpr(req);
                const bool_type = try self.type_ctx.intern(.bool);
                try self.unify(req_type, bool_type, func.span);
            }
        }

        // Check body
        if (func.body) |body| {
            // Save and reset effect tracking for this function
            const saved_effects = self.current_effects;
            self.current_effects = EffectSet.pure;
            defer self.current_effects = saved_effects;

            switch (body) {
                .block => |blk| {
                    const body_type = try self.inferBlock(blk);
                    // Only unify body type with return type if the block has a
                    // result expression (tail expression). Blocks with explicit
                    // return statements already check return types in checkReturn.
                    if (blk.result != null) {
                        try self.unify(body_type, body_return_type, func.span);
                    }
                },
                .expression => |expr| {
                    const body_type = try self.inferExpr(expr);
                    try self.unify(body_type, body_return_type, func.span);
                },
            }

            // Phase B Full: await is now allowed in all control flow positions
            // (The linear async restriction has been removed)

            // Effect enforcement: if the function declares effects (with [...]),
            // verify the observed effects don't exceed the declared set.
            if (func.effects) |_| {
                if (!self.current_effects.isSubsetOf(effects)) {
                    try self.reportError(
                        .unhandled_effect,
                        "function uses effects not declared in its 'with' clause",
                        func.span,
                    );
                }
            }

            // Check ensures contracts
            if (func.contracts) |contracts| {
                for (contracts.ensures) |ens| {
                    const ens_type = try self.inferExpr(ens);
                    const bool_type = try self.type_ctx.intern(.bool);
                    try self.unify(ens_type, bool_type, func.span);
                }
            }
        }
    }

    // ========================================================================
    // LINEAR ASYNC VALIDATION
    // ========================================================================

    /// Validate that an async function body only uses await in linear positions
    /// (not inside if/while/for/loop/match branches).
    fn validateLinearAsync(self: *Self, body: *const ast.BlockExpr, span: ast.Span) !void {
        for (body.statements) |stmt| {
            switch (stmt.kind) {
                .if_stmt => |if_expr| {
                    if (ifExprContainsAwait(if_expr)) {
                        try self.reportError(.type_mismatch, "await is not allowed inside if/else branches (linear async only)", span);
                        return;
                    }
                },
                .while_loop => |wl| {
                    if (blockContainsAwait(wl.body)) {
                        try self.reportError(.type_mismatch, "await is not allowed inside while loops (linear async only)", span);
                        return;
                    }
                },
                .for_loop => |fl| {
                    if (blockContainsAwait(fl.body)) {
                        try self.reportError(.type_mismatch, "await is not allowed inside for loops (linear async only)", span);
                        return;
                    }
                },
                .loop_stmt => |ls| {
                    if (blockContainsAwait(ls.body)) {
                        try self.reportError(.type_mismatch, "await is not allowed inside loop blocks (linear async only)", span);
                        return;
                    }
                },
                .match_stmt => |me| {
                    if (matchContainsAwait(me)) {
                        try self.reportError(.type_mismatch, "await is not allowed inside match arms (linear async only)", span);
                        return;
                    }
                },
                else => {},
            }
        }
        // Also check result expression for control flow containing await
        if (body.result) |result| {
            switch (result.kind) {
                .if_expr => |ie| {
                    if (ifExprContainsAwait(ie)) {
                        try self.reportError(.type_mismatch, "await is not allowed inside if/else branches (linear async only)", span);
                    }
                },
                .match_expr => |me| {
                    if (matchContainsAwait(me)) {
                        try self.reportError(.type_mismatch, "await is not allowed inside match arms (linear async only)", span);
                    }
                },
                else => {},
            }
        }
    }

    fn ifExprContainsAwait(ie: *const ast.IfExpr) bool {
        if (blockContainsAwait(ie.then_branch)) return true;
        if (ie.else_branch) |eb| {
            switch (eb) {
                .else_block => |b| {
                    if (blockContainsAwait(b)) return true;
                },
                .else_if => |ei| {
                    if (ifExprContainsAwait(ei)) return true;
                },
            }
        }
        return false;
    }

    fn matchContainsAwait(me: *const ast.MatchExpr) bool {
        for (me.arms) |arm| {
            switch (arm.body) {
                .block => |b| {
                    if (blockContainsAwait(b)) return true;
                },
                .expression => |e| {
                    if (exprContainsAwait(e)) return true;
                },
            }
        }
        return false;
    }

    fn blockContainsAwait(block: *const ast.BlockExpr) bool {
        for (block.statements) |stmt| {
            if (stmtContainsAwait(stmt)) return true;
        }
        if (block.result) |result| {
            if (exprContainsAwait(result)) return true;
        }
        return false;
    }

    fn stmtContainsAwait(stmt: *const ast.Statement) bool {
        return switch (stmt.kind) {
            .let_binding => |lb| if (lb.value) |v| exprContainsAwait(v) else false,
            .return_stmt => |rs| if (rs.value) |v| exprContainsAwait(v) else false,
            .expression => |e| exprContainsAwait(e),
            .assignment => |a| exprContainsAwait(a.value) or exprContainsAwait(a.target),
            .if_stmt => |ie| exprContainsAwait(ie.condition) or blockContainsAwait(ie.then_branch) or
                (if (ie.else_branch) |eb| switch (eb) {
                .else_block => |b| blockContainsAwait(b),
                .else_if => |ei| ifExprContainsAwait(ei),
            } else false),
            .while_loop => |wl| exprContainsAwait(wl.condition) or blockContainsAwait(wl.body),
            .for_loop => |fl| exprContainsAwait(fl.iterator) or blockContainsAwait(fl.body),
            .loop_stmt => |ls| blockContainsAwait(ls.body),
            .match_stmt => |me| exprContainsAwait(me.scrutinee) or matchContainsAwait(me),
            .region_block => |rb| blockContainsAwait(rb.body),
            .discard => |d| exprContainsAwait(d.value),
            .break_stmt => |bs| if (bs.value) |v| exprContainsAwait(v) else false,
            .continue_stmt => false,
        };
    }

    fn exprContainsAwait(expr: *const ast.Expr) bool {
        return switch (expr.kind) {
            .await_expr => true,
            .binary => |b| exprContainsAwait(b.left) or exprContainsAwait(b.right),
            .unary => |u| exprContainsAwait(u.operand),
            .function_call => |fc| blk: {
                if (exprContainsAwait(fc.function)) break :blk true;
                for (fc.args) |arg| {
                    if (exprContainsAwait(arg.value)) break :blk true;
                }
                break :blk false;
            },
            .method_call => |mc| blk: {
                if (exprContainsAwait(mc.object)) break :blk true;
                for (mc.args) |arg| {
                    if (exprContainsAwait(arg)) break :blk true;
                }
                break :blk false;
            },
            .field_access => |fa| exprContainsAwait(fa.object),
            .index_access => |ia| exprContainsAwait(ia.object) or exprContainsAwait(ia.index),
            .if_expr => |ie| exprContainsAwait(ie.condition) or blockContainsAwait(ie.then_branch) or
                (if (ie.else_branch) |eb| switch (eb) {
                .else_block => |b| blockContainsAwait(b),
                .else_if => |ei| ifExprContainsAwait(ei),
            } else false),
            .match_expr => |me| exprContainsAwait(me.scrutinee) or matchContainsAwait(me),
            .block => |b| blockContainsAwait(b),
            .pipeline => |p| exprContainsAwait(p.left) or exprContainsAwait(p.right),
            .error_propagate => |e| exprContainsAwait(e.operand),
            .coalesce => |c| exprContainsAwait(c.left) or exprContainsAwait(c.right),
            .cast => |c| exprContainsAwait(c.expr),
            .type_check => |tc| exprContainsAwait(tc.expr),
            .grouped => |g| exprContainsAwait(g),
            .comptime_expr => |c| exprContainsAwait(c.expr),
            .string_interpolation => |si| blk: {
                for (si.parts) |part| {
                    switch (part) {
                        .expr => |e| {
                            if (exprContainsAwait(e)) break :blk true;
                        },
                        .literal => {},
                    }
                }
                break :blk false;
            },
            .struct_literal => |sl| blk: {
                for (sl.fields) |f| {
                    if (exprContainsAwait(f.value)) break :blk true;
                }
                if (sl.spread) |s| {
                    if (exprContainsAwait(s)) break :blk true;
                }
                break :blk false;
            },
            .array_literal => |al| switch (al.kind) {
                .elements => |elems| blk: {
                    for (elems) |e| {
                        if (exprContainsAwait(e)) break :blk true;
                    }
                    break :blk false;
                },
                .repeat => |rep| exprContainsAwait(rep.value) or exprContainsAwait(rep.count),
            },
            .enum_literal => |el| switch (el.payload) {
                .none => false,
                .tuple => |t| blk: {
                    for (t) |e| {
                        if (exprContainsAwait(e)) break :blk true;
                    }
                    break :blk false;
                },
                .struct_fields => |sfs| blk: {
                    for (sfs) |f| {
                        if (exprContainsAwait(f.value)) break :blk true;
                    }
                    break :blk false;
                },
            },
            .tuple_literal => |tl| blk: {
                for (tl.elements) |e| {
                    if (exprContainsAwait(e)) break :blk true;
                }
                break :blk false;
            },
            .range => |r| (if (r.start) |s| exprContainsAwait(s) else false) or
                (if (r.end) |e| exprContainsAwait(e) else false),
            .lambda => false,
            .literal, .identifier, .path => false,
        };
    }

    fn checkStruct(self: *Self, sd: *const ast.StructDecl, is_public: bool) !void {
        // Save placeholder type_id for unification after defining the real type
        var placeholder_type_id: ?TypeId = null;
        if (self.env.lookupLocal(sd.name.name)) |existing| {
            // Allow overwriting forward-declared placeholder (type_var from Pass 0)
            const existing_type = self.type_ctx.get(existing.type_id);
            if (existing_type == null or existing_type.? != .type_var) {
                try self.reportError(.duplicate_definition, "duplicate struct definition", sd.span);
                return;
            }
            placeholder_type_id = existing.type_id;
        }

        var fields = std.ArrayList(types.StructField).init(self.allocator);

        var type_params = std.ArrayList([]const u8).init(self.allocator);

        // Register generic parameters
        if (sd.generic_params) |gps| {
            for (gps) |gp| {
                try type_params.append(gp.name.name);
            }
        }

        // Process fields
        for (sd.fields) |field| {
            const field_type = try self.resolveTypeExpr(field.type_expr);
            try fields.append(.{
                .name = field.name.name,
                .type_id = field_type,
                .is_mutable = field.visibility == .public,
                .default_value = null,
                .offset = null,
            });
        }

        // Use toOwnedSlice to produce a stable slice that outlives the ArrayList
        const owned_fields = try fields.toOwnedSlice();
        const owned_type_params = try type_params.toOwnedSlice();

        const struct_type = try self.type_ctx.intern(.{
            .@"struct" = .{
                .name = sd.name.name,
                .fields = owned_fields,
                .type_params = owned_type_params,
                .is_packed = false,
                .size = null,
                .alignment = null,
            },
        });

        // Unify placeholder type_var with the real struct type so forward references resolve
        if (placeholder_type_id) |ph| {
            self.unify(ph, struct_type, sd.span) catch {};
        }

        try self.env.define(Symbol.init(sd.name.name, .type_def, struct_type)
            .withPublic(is_public)
            .withLocation(sd.span));
    }

    fn checkEnum(self: *Self, ed: *const ast.EnumDecl, is_public: bool) !void {
        var placeholder_type_id: ?TypeId = null;
        if (self.env.lookupLocal(ed.name.name)) |existing| {
            // Allow overwriting forward-declared placeholder (type_var from Pass 0)
            const existing_type = self.type_ctx.get(existing.type_id);
            if (existing_type == null or existing_type.? != .type_var) {
                try self.reportError(.duplicate_definition, "duplicate enum definition", ed.span);
                return;
            }
            placeholder_type_id = existing.type_id;
        }

        var variants = std.ArrayList(types.EnumVariant).init(self.allocator);

        var type_params = std.ArrayList([]const u8).init(self.allocator);

        if (ed.generic_params) |gps| {
            for (gps) |gp| {
                try type_params.append(gp.name.name);
            }
        }

        for (ed.variants) |variant| {
            var payload_types = std.ArrayList(TypeId).init(self.allocator);

            switch (variant.payload) {
                .none => {},
                .tuple => |types_list| {
                    for (types_list) |te| {
                        const t = try self.resolveTypeExpr(te);
                        try payload_types.append(t);
                    }
                },
                .struct_fields => |fields| {
                    for (fields) |field| {
                        const t = try self.resolveTypeExpr(field.type_expr);
                        try payload_types.append(t);
                    }
                },
            }

            // Use toOwnedSlice so the payload_types slice outlives the ArrayList
            const owned_payload = try payload_types.toOwnedSlice();

            try variants.append(.{
                .name = variant.name.name,
                .payload_types = owned_payload,
                .tag_value = null,
            });
        }

        const owned_variants = try variants.toOwnedSlice();
        const owned_type_params = try type_params.toOwnedSlice();

        const enum_type = try self.type_ctx.intern(.{
            .@"enum" = .{
                .name = ed.name.name,
                .variants = owned_variants,
                .type_params = owned_type_params,
                .tag_type = .u32,
            },
        });

        // Unify placeholder type_var with the real enum type so forward references resolve
        if (placeholder_type_id) |ph| {
            self.unify(ph, enum_type, ed.span) catch {};
        }

        try self.env.define(Symbol.init(ed.name.name, .type_def, enum_type)
            .withPublic(is_public)
            .withLocation(ed.span));

        // Also define variant constructors
        for (ed.variants) |variant| {
            try self.env.define(Symbol.init(variant.name.name, .enum_variant, enum_type)
                .withLocation(variant.span));
        }
    }

    fn checkTrait(self: *Self, td: *const ast.TraitDecl, is_public: bool) !void {
        if (self.env.lookupLocal(td.name.name)) |_| {
            try self.reportError(.duplicate_definition, "duplicate trait definition", td.span);
            return;
        }

        try self.env.pushScope(.trait_def);
        defer self.env.popScope();

        // Register Self type
        const self_type = try self.freshTypeVar();
        try self.env.define(Symbol.init("Self", .type_param, self_type));

        // Process trait items
        for (td.items) |item| {
            switch (item.kind) {
                .function => |func| try self.checkFunction(func, true),
                .type_alias => |ta| {
                    const alias_type = if (ta.type_expr) |te|
                        try self.resolveTypeExpr(te)
                    else
                        try self.freshTypeVar();
                    try self.env.define(Symbol.init(ta.name.name, .type_alias, alias_type));
                },
                .constant => |cd| try self.checkConst(cd, true),
            }
        }

        const trait_type = try self.type_ctx.intern(.{
            .trait_def = .{
                .name = td.name.name,
                .type_params = &[_][]const u8{},
                .methods = &[_]types.TraitMethod{},
                .super_traits = &[_]TypeId{},
                .associated_types = &[_][]const u8{},
            },
        });

        self.env.popScope();
        try self.env.define(Symbol.init(td.name.name, .trait_def, trait_type)
            .withPublic(is_public)
            .withLocation(td.span));
        try self.env.pushScope(.trait_def);
    }

    fn checkImpl(self: *Self, ib: *const ast.ImplBlock) !void {
        try self.env.pushScope(.impl_block);
        defer self.env.popScope();

        const target_type = try self.resolveTypeExpr(ib.target_type);

        // Register Self
        try self.env.define(Symbol.init("Self", .type_param, target_type));

        // Pre-register all method signatures (enables forward references within impl)
        for (ib.items) |item| {
            switch (item.kind) {
                .function => |func| try self.registerFunctionSignature(func, item.visibility == .public),
                else => {},
            }
        }

        // Check all items
        for (ib.items) |item| {
            switch (item.kind) {
                .function => |func| try self.checkFunction(func, item.visibility == .public),
                .type_alias => |ta| {
                    const alias_type = if (ta.type_expr) |te|
                        try self.resolveTypeExpr(te)
                    else
                        try self.freshTypeVar();
                    try self.env.define(Symbol.init(ta.name.name, .type_alias, alias_type));
                },
                .constant => |cd| try self.checkConst(cd, item.visibility == .public),
            }
        }
    }

    fn checkConst(self: *Self, cd: *const ast.ConstDecl, is_public: bool) !void {
        if (self.env.lookupLocal(cd.name.name)) |_| {
            try self.reportError(.duplicate_definition, "duplicate constant definition", cd.span);
            return;
        }

        const value_type = if (cd.value) |val|
            try self.inferExpr(val)
        else
            try self.freshTypeVar();

        const declared_type = if (cd.type_expr) |te|
            try self.resolveTypeExpr(te)
        else
            value_type;

        if (cd.value != null and cd.type_expr != null) {
            try self.unify(value_type, declared_type, cd.span);
        }

        try self.env.define(Symbol.init(cd.name.name, .constant, declared_type)
            .withPublic(is_public)
            .withLocation(cd.span));
    }

    // ========================================================================
    // TYPE RESOLUTION
    // ========================================================================

    /// Evaluate a comptime expression to a usize (for array sizes).
    /// Returns null if the expression cannot be statically evaluated.
    fn evalComptimeUsize(expr: *const ast.Expr) ?usize {
        switch (expr.kind) {
            .literal => |lit| {
                switch (lit.kind) {
                    .int => |int_lit| {
                        const val = std.fmt.parseInt(i64, int_lit.value, 10) catch return null;
                        if (val >= 0) return @intCast(@as(u64, @intCast(val)));
                        return null;
                    },
                    else => return null,
                }
            },
            .comptime_expr => |ce| return evalComptimeUsize(ce.expr),
            else => return null,
        }
    }

    pub fn resolveTypeExpr(self: *Self, te: *const ast.TypeExpr) !TypeId {
        return switch (te.kind) {
            .named => |named| {
                const name = named.path.segments[0].name;
                // Special handling for Box[T] -> creates a proper .box type
                if (std.mem.eql(u8, name, "Box")) {
                    if (named.generic_args) |args| {
                        if (args.len > 0) {
                            const inner = try self.resolveTypeExpr(args[0]);
                            return try self.type_ctx.makeBox(inner);
                        }
                    }
                    return try self.freshTypeVar();
                }
                // Special handling for Option[T] -> creates a proper .option type
                if (std.mem.eql(u8, name, "Option")) {
                    if (named.generic_args) |args| {
                        if (args.len > 0) {
                            const inner = try self.resolveTypeExpr(args[0]);
                            return try self.type_ctx.makeOption(inner);
                        }
                    }
                    return try self.freshTypeVar();
                }
                // Special handling for Future[T] -> creates a proper .future type
                if (std.mem.eql(u8, name, "Future")) {
                    if (named.generic_args) |args| {
                        if (args.len > 0) {
                            const inner = try self.resolveTypeExpr(args[0]);
                            return try self.type_ctx.makeFuture(inner);
                        }
                    }
                    return try self.freshTypeVar();
                }
                // Special handling for Result[T, E] -> creates a proper .result type
                if (std.mem.eql(u8, name, "Result")) {
                    if (named.generic_args) |args| {
                        if (args.len > 0) {
                            const ok_type = try self.resolveTypeExpr(args[0]);
                            const err_type = if (args.len > 1)
                                try self.resolveTypeExpr(args[1])
                            else
                                try self.type_ctx.intern(.str);
                            return try self.type_ctx.makeResult(ok_type, err_type);
                        }
                    }
                    return try self.freshTypeVar();
                }
                if (self.env.lookup(name)) |symbol| {
                    if (named.generic_args) |args| {
                        return try self.instantiateGeneric(symbol.type_id, args);
                    }
                    return symbol.type_id;
                }
                try self.reportError(.undefined_type, "undefined type", te.span);
                return try self.type_ctx.intern(.error_type);
            },
            .function => |func| {
                var param_types = std.ArrayList(TypeId).init(self.allocator);
                defer param_types.deinit();

                for (func.params) |param| {
                    try param_types.append(try self.resolveTypeExpr(param));
                }

                const return_type = try self.resolveTypeExpr(func.return_type);

                var effects = EffectSet.pure;
                if (func.effects) |effs| {
                    for (effs) |eff| {
                        try self.addEffectFromTypeExpr(eff, &effects);
                    }
                }

                return try self.type_ctx.makeFunction(param_types.items, return_type, effects);
            },
            .array => |arr| {
                const elem_type = try self.resolveTypeExpr(arr.element_type);
                const size = evalComptimeUsize(arr.size) orelse 0;
                return try self.type_ctx.makeArray(elem_type, size);
            },
            .slice => |s| {
                const elem_type = try self.resolveTypeExpr(s.element_type);
                return try self.type_ctx.makeSlice(elem_type, false);
            },
            .pointer => |ptr| {
                const pointee = try self.resolveTypeExpr(ptr.pointee_type);
                return try self.type_ctx.makeRef(pointee, ptr.is_mut, null);
            },
            .reference => |ref| {
                const referenced = try self.resolveTypeExpr(ref.referenced_type);
                return try self.type_ctx.makeRef(referenced, ref.is_mut, null);
            },
            .tuple => |tup| {
                var elem_types = std.ArrayList(TypeId).init(self.allocator);
                defer elem_types.deinit();

                for (tup.elements) |elem| {
                    try elem_types.append(try self.resolveTypeExpr(elem));
                }

                return try self.type_ctx.makeTuple(elem_types.items);
            },
            .option => |opt| {
                const inner = try self.resolveTypeExpr(opt.inner_type);
                return try self.type_ctx.makeOption(inner);
            },
            .result => |res| {
                const ok = try self.resolveTypeExpr(res.ok_type);
                const err = if (res.err_type) |et|
                    try self.resolveTypeExpr(et)
                else
                    try self.type_ctx.intern(.str);
                return try self.type_ctx.makeResult(ok, err);
            },
            .trait_object => |to| {
                // Resolve the trait type, then create a trait_object type
                const trait_type = try self.resolveTypeExpr(to.trait_type);
                const trait_ids = try self.allocator.alloc(TypeId, 1);
                trait_ids[0] = trait_type;
                return try self.type_ctx.intern(.{ .trait_object = trait_ids });
            },
            .infer => try self.freshTypeVar(),
            .never => try self.type_ctx.intern(.never),
            .self_type => {
                if (self.env.lookup("Self")) |sym| {
                    return sym.type_id;
                }
                try self.reportError(.undefined_type, "Self used outside of trait/impl", te.span);
                return try self.type_ctx.intern(.error_type);
            },
        };
    }

    fn resolveTypeName(self: *Self, name: []const u8) !TypeId {
        if (self.env.lookup(name)) |symbol| {
            return symbol.type_id;
        }
        return error.UndefinedType;
    }

    fn instantiateGeneric(self: *Self, base_type: TypeId, args: []const *ast.TypeExpr) anyerror!TypeId {
        var type_args = std.ArrayList(TypeId).init(self.allocator);
        defer type_args.deinit();

        for (args) |arg| {
            try type_args.append(try self.resolveTypeExpr(arg));
        }

        return types.instantiateGeneric(self.type_ctx, base_type, type_args.items) catch base_type;
    }

    fn addEffectFromTypeExpr(self: *Self, te: *const ast.TypeExpr, effects: *EffectSet) !void {
        if (te.kind == .named) {
            const name = te.kind.named.path.segments[0].name;
            if (std.mem.eql(u8, name, "IO")) {
                effects.addBuiltin(.io);
            } else if (std.mem.eql(u8, name, "Console")) {
                effects.addBuiltin(.console);
            } else if (std.mem.eql(u8, name, "Network")) {
                effects.addBuiltin(.network);
            } else if (std.mem.eql(u8, name, "FileSystem")) {
                effects.addBuiltin(.file_system);
            } else if (std.mem.eql(u8, name, "Random")) {
                effects.addBuiltin(.random);
            } else if (std.mem.eql(u8, name, "Time")) {
                effects.addBuiltin(.time);
            } else if (std.mem.eql(u8, name, "Memory")) {
                effects.addBuiltin(.memory);
            } else if (std.mem.eql(u8, name, "Async")) {
                effects.addBuiltin(.async_effect);
            } else if (std.mem.eql(u8, name, "Exception")) {
                effects.addBuiltin(.exception);
            } else if (std.mem.eql(u8, name, "State")) {
                effects.addBuiltin(.state);
            } else {
                // Custom effect — track it
                if (effects.allocator == null) {
                    effects.allocator = self.allocator;
                }
                var list = std.ArrayList([]const u8).init(effects.allocator.?);
                for (effects.custom_effects) |c| {
                    list.append(c) catch {};
                }
                // Don't add duplicates
                var already = false;
                for (effects.custom_effects) |c| {
                    if (std.mem.eql(u8, c, name)) {
                        already = true;
                        break;
                    }
                }
                if (!already) {
                    list.append(name) catch {};
                }
                effects.custom_effects = list.toOwnedSlice() catch effects.custom_effects;
            }
        }
    }

    fn getIteratorElementType(self: *Self, iter_type: TypeId, span: ast.Span) !TypeId {
        const resolved = self.resolveType(iter_type);
        const typ = self.type_ctx.get(resolved) orelse {
            return try self.freshTypeVar();
        };

        return switch (typ) {
            .array => |arr| arr.element_type,
            .slice => |s| s.element_type,
            .str => try self.type_ctx.intern(.char),
            // Ranges return the element type directly (e.g., i64 for 0..5),
            // so iterating over a range of int/float yields that same type.
            .int, .float => resolved,
            // Type variables (e.g., from List[T]) - allow iteration
            .type_var => try self.freshTypeVar(),
            else => {
                try self.reportError(.type_mismatch, "expected iterable type", span);
                return try self.freshTypeVar();
            },
        };
    }

    // ========================================================================
    // ERROR REPORTING
    // ========================================================================

    fn reportError(self: *Self, code: ErrorCode, message: []const u8, span: ?ast.Span) !void {
        const loc = if (span) |s| SourceLocation{
            .line = s.start.line,
            .column = s.start.column,
            .offset = s.start.offset,
        } else SourceLocation{
            .line = 0,
            .column = 0,
            .offset = 0,
        };

        try self.diagnostics.add(.{
            .code = code,
            .severity = .@"error",
            .message = message,
            .location = loc,
            .source_file = self.source_file,
            .source_line = if (self.source) |src| errors.extractLine(src, loc.line) else null,
        });
    }

    fn reportTypeMismatch(self: *Self, expected: TypeId, found: TypeId, span: ?ast.Span) !void {
        _ = expected;
        _ = found;
        try self.reportError(.type_mismatch, "type mismatch", span);
    }

    // ========================================================================
    // PUBLIC API
    // ========================================================================

    /// Check an entire source file
    pub fn checkSourceFile(self: *Self, source_file: *const ast.SourceFile) !void {
        // Process imports
        for (source_file.imports) |import_decl| {
            try self.processImport(import_decl);
        }

        // Pass 0: pre-register type names so recursive types (e.g., enum Foo { Bar(Box[Foo]) }) work
        for (source_file.declarations) |decl| {
            switch (decl.kind) {
                .struct_def => |sd| {
                    if (self.env.lookupLocal(sd.name.name) == null) {
                        try self.env.define(Symbol.init(sd.name.name, .type_def, try self.freshTypeVar())
                            .withLocation(sd.span));
                    }
                },
                .enum_def => |ed| {
                    if (self.env.lookupLocal(ed.name.name) == null) {
                        try self.env.define(Symbol.init(ed.name.name, .type_def, try self.freshTypeVar())
                            .withLocation(ed.span));
                    }
                },
                else => {},
            }
        }

        // First pass: register all type definitions
        for (source_file.declarations) |decl| {
            switch (decl.kind) {
                .struct_def => |sd| try self.checkStruct(sd, decl.visibility == .public),
                .enum_def => |ed| try self.checkEnum(ed, decl.visibility == .public),
                .trait_def => |td| try self.checkTrait(td, decl.visibility == .public),
                else => {},
            }
        }

        // Pass 1.5: register all top-level function signatures (enables forward references)
        for (source_file.declarations) |decl| {
            switch (decl.kind) {
                .function => |func| try self.registerFunctionSignature(func, decl.visibility == .public),
                else => {},
            }
        }

        // Second pass: check all declarations
        for (source_file.declarations) |decl| {
            try self.checkDeclaration(decl);
        }

        // Solve remaining constraints
        try self.solveConstraints();
    }

    fn processImport(self: *Self, import_decl: *const ast.ImportDecl) !void {
        // For now, just mark as processed
        _ = import_decl;
        _ = self;
    }

    /// Check if there are any errors
    pub fn hasErrors(self: *Self) bool {
        return self.diagnostics.hasErrors();
    }

    /// Get all diagnostics
    pub fn getDiagnostics(self: *Self) []const Diagnostic {
        return self.diagnostics.items();
    }

    /// Get the type context for use by codegen
    pub fn getTypeContext(self: *Self) ?*TypeContext {
        return self.type_ctx;
    }

    /// Get the inferred type of an expression (after solving constraints)
    pub fn getExprType(self: *Self, expr: *const ast.Expr) !TypeId {
        const raw_type = try self.inferExpr(expr);
        try self.solveConstraints();
        return self.resolveType(raw_type);
    }
};

// ============================================================================
// TESTS
// ============================================================================

const testing = std.testing;

test "type checker initialization" {
    var checker = try TypeChecker.init(testing.allocator);
    defer checker.deinit();

    // Built-in types should be registered
    try testing.expect(checker.env.lookup("int") != null);
    try testing.expect(checker.env.lookup("bool") != null);
    try testing.expect(checker.env.lookup("str") != null);
}

test "fresh type variable creation" {
    var checker = try TypeChecker.init(testing.allocator);
    defer checker.deinit();

    const tv1 = try checker.freshTypeVar();
    const tv2 = try checker.freshTypeVar();

    try testing.expect(tv1 != tv2);
}

test "type unification - same types" {
    var checker = try TypeChecker.init(testing.allocator);
    defer checker.deinit();

    const int_type = try checker.type_ctx.intern(.{ .int = .i64 });
    try checker.unify(int_type, int_type, null);
    try testing.expect(!checker.hasErrors());
}

test "type unification - type variable" {
    var checker = try TypeChecker.init(testing.allocator);
    defer checker.deinit();

    const tv = try checker.freshTypeVar();
    const int_type = try checker.type_ctx.intern(.{ .int = .i64 });

    try checker.unify(tv, int_type, null);

    const resolved = checker.resolveType(tv);
    try testing.expectEqual(int_type, resolved);
}

test "type unification - incompatible types" {
    var checker = try TypeChecker.init(testing.allocator);
    defer checker.deinit();

    const int_type = try checker.type_ctx.intern(.{ .int = .i64 });
    const bool_type = try checker.type_ctx.intern(.bool);

    try checker.unify(int_type, bool_type, null);

    try testing.expect(checker.hasErrors());
}

test "constraint solving" {
    var checker = try TypeChecker.init(testing.allocator);
    defer checker.deinit();

    const tv = try checker.freshTypeVar();
    const int_type = try checker.type_ctx.intern(.{ .int = .i64 });

    try checker.addConstraint(Constraint.equal(tv, int_type, null));
    try checker.solveConstraints();

    const resolved = checker.resolveType(tv);
    try testing.expectEqual(int_type, resolved);
}

test "scope management" {
    var checker = try TypeChecker.init(testing.allocator);
    defer checker.deinit();

    const int_type = try checker.type_ctx.intern(.{ .int = .i64 });

    try checker.env.define(Symbol.init("x", .variable, int_type));
    try testing.expect(checker.env.lookup("x") != null);

    try checker.env.pushScope(.block);
    try testing.expect(checker.env.lookup("x") != null); // visible from parent

    try checker.env.define(Symbol.init("y", .variable, int_type));
    try testing.expect(checker.env.lookup("y") != null);

    checker.env.popScope();
    try testing.expect(checker.env.lookup("y") == null); // out of scope
    try testing.expect(checker.env.lookup("x") != null); // still visible
}

test "union-find operations" {
    var uf = UnionFind.init(testing.allocator);
    defer uf.deinit();

    try uf.makeSet(0);
    try uf.makeSet(1);
    try uf.makeSet(2);

    try uf.unite(0, 1);
    try testing.expectEqual(uf.find(0), uf.find(1));
    try testing.expect(uf.find(0) != uf.find(2));

    try uf.unite(1, 2);
    try testing.expectEqual(uf.find(0), uf.find(2));
}

test "effect set operations" {
    var effects = EffectSet.init(testing.allocator);
    defer effects.deinit();

    try testing.expect(effects.isPure());

    effects.addBuiltin(.io);
    try testing.expect(!effects.isPure());
    try testing.expect(effects.hasIO());

    effects.addBuiltin(.network);
    try testing.expect(effects.hasBuiltin(.network));
}

test "symbol creation with fluent API" {
    const int_type: TypeId = 42;

    const sym = Symbol.init("test", .variable, int_type)
        .withMutable(true)
        .withPublic(true);

    try testing.expectEqualStrings("test", sym.name);
    try testing.expect(sym.is_mutable);
    try testing.expect(sym.is_public);
    try testing.expectEqual(int_type, sym.type_id);
}

test "scope kind detection" {
    var checker = try TypeChecker.init(testing.allocator);
    defer checker.deinit();

    try testing.expect(!checker.env.current.isInLoop());

    try checker.env.pushScope(.loop);
    try testing.expect(checker.env.current.isInLoop());

    try checker.env.pushScope(.block);
    try testing.expect(checker.env.current.isInLoop()); // nested in loop

    checker.env.popScope();
    checker.env.popScope();
    try testing.expect(!checker.env.current.isInLoop());
}

test "enclosing function detection" {
    var checker = try TypeChecker.init(testing.allocator);
    defer checker.deinit();

    try testing.expect(checker.env.current.enclosingFunction() == null);

    try checker.env.pushScope(.function);
    try testing.expect(checker.env.current.enclosingFunction() != null);

    try checker.env.pushScope(.block);
    try testing.expect(checker.env.current.enclosingFunction() != null);
}
