//! dAImond Type System
//!
//! This module implements the type system foundations for the dAImond programming language.
//! It provides representations for all types, type checking utilities, effect types,
//! and trait/interface support.

const std = @import("std");
const Allocator = std.mem.Allocator;

// ============================================================================
// TYPE IDS AND INTERNING
// ============================================================================

/// Unique identifier for interned types
pub const TypeId = u32;

/// Sentinel value for invalid/unresolved types
pub const INVALID_TYPE_ID: TypeId = std.math.maxInt(TypeId);

// ============================================================================
// PRIMITIVE TYPES
// ============================================================================

/// Integer type variants with their bit widths
pub const IntType = enum {
    i8,
    i16,
    i32,
    i64, // 'int' is an alias for i64
    i128,
    u8, // 'byte' is an alias for u8
    u16,
    u32,
    u64, // 'uint' is an alias for u64
    u128,

    /// Get the bit width of this integer type
    pub fn bitWidth(self: IntType) u8 {
        return switch (self) {
            .i8, .u8 => 8,
            .i16, .u16 => 16,
            .i32, .u32 => 32,
            .i64, .u64 => 64,
            .i128, .u128 => 128,
        };
    }

    /// Check if this is a signed integer type
    pub fn isSigned(self: IntType) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .i128 => true,
            .u8, .u16, .u32, .u64, .u128 => false,
        };
    }

    /// Get the maximum value for this integer type
    pub fn maxValue(self: IntType) u128 {
        return switch (self) {
            .i8 => @as(u128, std.math.maxInt(i8)),
            .i16 => @as(u128, std.math.maxInt(i16)),
            .i32 => @as(u128, std.math.maxInt(i32)),
            .i64 => @as(u128, std.math.maxInt(i64)),
            .i128 => @as(u128, std.math.maxInt(i128)),
            .u8 => @as(u128, std.math.maxInt(u8)),
            .u16 => @as(u128, std.math.maxInt(u16)),
            .u32 => @as(u128, std.math.maxInt(u32)),
            .u64 => @as(u128, std.math.maxInt(u64)),
            .u128 => std.math.maxInt(u128),
        };
    }

    /// Get a string representation of the type
    pub fn name(self: IntType) []const u8 {
        return @tagName(self);
    }
};

/// Float type variants
pub const FloatType = enum {
    f32,
    f64, // 'float' is an alias for f64

    /// Get the bit width of this float type
    pub fn bitWidth(self: FloatType) u8 {
        return switch (self) {
            .f32 => 32,
            .f64 => 64,
        };
    }

    /// Get a string representation of the type
    pub fn name(self: FloatType) []const u8 {
        return @tagName(self);
    }
};

// ============================================================================
// EFFECT TYPES
// ============================================================================

/// Built-in effect types
pub const EffectKind = enum {
    io, // General I/O operations
    console, // Console read/write
    network, // Network operations
    file_system, // File system operations
    random, // Random number generation
    time, // Time/clock operations
    memory, // Dynamic memory allocation
    diverge, // Non-termination (infinite loops, panic)
    async_effect, // Async/await operations
    exception, // Exception handling
    state, // Mutable state
    custom, // User-defined effects

    pub fn name(self: EffectKind) []const u8 {
        return switch (self) {
            .io => "IO",
            .console => "Console",
            .network => "Network",
            .file_system => "FileSystem",
            .random => "Random",
            .time => "Time",
            .memory => "Memory",
            .diverge => "Diverge",
            .async_effect => "Async",
            .exception => "Exception",
            .state => "State",
            .custom => "Custom",
        };
    }
};

/// Represents a set of effects
pub const EffectSet = struct {
    /// Bitset for built-in effects
    builtin_effects: u16,
    /// Custom effect identifiers
    custom_effects: []const []const u8,
    /// Allocator for custom effects
    allocator: ?Allocator,

    const Self = @This();

    /// Empty effect set (pure function)
    pub const pure = Self{
        .builtin_effects = 0,
        .custom_effects = &[_][]const u8{},
        .allocator = null,
    };

    /// Create an effect set with given allocator
    pub fn init(allocator: Allocator) Self {
        return .{
            .builtin_effects = 0,
            .custom_effects = &[_][]const u8{},
            .allocator = allocator,
        };
    }

    /// Free resources
    pub fn deinit(self: *Self) void {
        if (self.allocator) |alloc| {
            if (self.custom_effects.len > 0) {
                alloc.free(self.custom_effects);
            }
        }
    }

    /// Add a built-in effect
    pub fn addBuiltin(self: *Self, effect: EffectKind) void {
        const bit = @as(u16, 1) << @intFromEnum(effect);
        self.builtin_effects |= bit;
    }

    /// Check if a built-in effect is present
    pub fn hasBuiltin(self: Self, effect: EffectKind) bool {
        const bit = @as(u16, 1) << @intFromEnum(effect);
        return (self.builtin_effects & bit) != 0;
    }

    /// Check if this set has IO effect
    pub fn hasIO(self: Self) bool {
        return self.hasBuiltin(.io);
    }

    /// Check if this is a pure effect set (no effects)
    pub fn isPure(self: Self) bool {
        return self.builtin_effects == 0 and self.custom_effects.len == 0;
    }

    /// Union of two effect sets
    pub fn unionWith(self: Self, other: Self) Self {
        // Note: This doesn't merge custom effects properly, would need allocator
        return .{
            .builtin_effects = self.builtin_effects | other.builtin_effects,
            .custom_effects = if (self.custom_effects.len > 0) self.custom_effects else other.custom_effects,
            .allocator = self.allocator orelse other.allocator,
        };
    }

    /// Check if self is a subset of other (for effect compatibility)
    pub fn isSubsetOf(self: Self, other: Self) bool {
        // All of self's effects must be in other
        if ((self.builtin_effects & ~other.builtin_effects) != 0) {
            return false;
        }
        // TODO: Check custom effects
        return true;
    }

    /// Check if two effect sets are equal
    pub fn eql(self: Self, other: Self) bool {
        return self.builtin_effects == other.builtin_effects;
        // TODO: Compare custom effects
    }

    /// Format effect set for display
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        if (self.isPure()) {
            try writer.writeAll("Pure");
            return;
        }

        try writer.writeAll("[");
        var first = true;
        inline for (std.meta.fields(EffectKind)) |field| {
            const effect: EffectKind = @enumFromInt(field.value);
            if (self.hasBuiltin(effect)) {
                if (!first) try writer.writeAll(", ");
                try writer.writeAll(effect.name());
                first = false;
            }
        }
        try writer.writeAll("]");
    }
};

// ============================================================================
// TRAIT/INTERFACE TYPES
// ============================================================================

/// Represents a trait method signature
pub const TraitMethod = struct {
    name: []const u8,
    params: []const TypeId,
    return_type: TypeId,
    effects: EffectSet,
    has_default_impl: bool,
};

/// Represents a trait definition
pub const TraitDef = struct {
    name: []const u8,
    type_params: []const []const u8,
    methods: []const TraitMethod,
    super_traits: []const TypeId, // Traits this trait extends
    associated_types: []const []const u8,
};

/// Represents a trait bound on a generic type parameter
pub const TraitBound = struct {
    trait_id: TypeId,
    type_args: []const TypeId, // For parameterized traits like Iterator[T]
};

/// Represents a trait implementation
pub const TraitImpl = struct {
    trait_id: TypeId,
    implementing_type: TypeId,
    type_args: []const TypeId, // Concrete types for trait parameters
    method_impls: []const MethodImpl,
};

/// Represents a method implementation
pub const MethodImpl = struct {
    method_name: []const u8,
    // In a real implementation, this would reference the AST/IR
    impl_id: u32,
};

// ============================================================================
// COMPOSITE TYPE COMPONENTS
// ============================================================================

/// Represents a field in a struct
pub const StructField = struct {
    name: []const u8,
    type_id: TypeId,
    is_mutable: bool,
    default_value: ?*const anyopaque, // Optional default value (placeholder)
    offset: ?usize, // Byte offset (computed during layout)
};

/// Represents a struct type
pub const StructType = struct {
    name: ?[]const u8, // Named struct or anonymous
    fields: []const StructField,
    type_params: []const []const u8, // Generic parameters if any
    is_packed: bool,
    size: ?usize, // Computed size in bytes
    alignment: ?usize, // Computed alignment

    /// Find a field by name
    pub fn getField(self: StructType, field_name: []const u8) ?StructField {
        for (self.fields) |field| {
            if (std.mem.eql(u8, field.name, field_name)) {
                return field;
            }
        }
        return null;
    }

    /// Get field index by name
    pub fn getFieldIndex(self: StructType, field_name: []const u8) ?usize {
        for (self.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, field_name)) {
                return i;
            }
        }
        return null;
    }
};

/// Represents a variant in an enum (algebraic data type)
pub const EnumVariant = struct {
    name: []const u8,
    payload_types: []const TypeId, // Empty for unit variants
    tag_value: ?i64, // Explicit discriminant if specified
};

/// Represents an enum type (sum type / algebraic data type)
pub const EnumType = struct {
    name: []const u8,
    variants: []const EnumVariant,
    type_params: []const []const u8,
    tag_type: ?IntType, // Type used for discriminant

    /// Find a variant by name
    pub fn getVariant(self: EnumType, variant_name: []const u8) ?EnumVariant {
        for (self.variants) |variant| {
            if (std.mem.eql(u8, variant.name, variant_name)) {
                return variant;
            }
        }
        return null;
    }

    /// Get variant index by name
    pub fn getVariantIndex(self: EnumType, variant_name: []const u8) ?usize {
        for (self.variants, 0..) |variant, i| {
            if (std.mem.eql(u8, variant.name, variant_name)) {
                return i;
            }
        }
        return null;
    }
};

/// Represents a function type
pub const FunctionType = struct {
    param_types: []const TypeId,
    param_names: ?[]const []const u8, // Optional parameter names
    return_type: TypeId,
    effects: EffectSet,
    is_variadic: bool,
    type_params: []const []const u8, // Generic parameters

    /// Check if function is pure (no effects)
    pub fn isPure(self: FunctionType) bool {
        return self.effects.isPure();
    }
};

/// Represents an array type [T; N]
pub const ArrayType = struct {
    element_type: TypeId,
    size: usize,
};

/// Represents a slice type [T]
pub const SliceType = struct {
    element_type: TypeId,
    is_mutable: bool,
};

/// Represents a tuple type (T1, T2, ...)
pub const TupleType = struct {
    element_types: []const TypeId,

    /// Get the arity (number of elements) of the tuple
    pub fn arity(self: TupleType) usize {
        return self.element_types.len;
    }
};

/// Represents a generic type parameter
pub const TypeParam = struct {
    name: []const u8,
    bounds: []const TraitBound,
    default_type: ?TypeId,
};

/// Represents an instantiated generic type
pub const GenericInstance = struct {
    base_type: TypeId, // The generic type definition
    type_args: []const TypeId, // Concrete type arguments
};

/// Represents a reference/pointer type
pub const RefType = struct {
    pointee: TypeId,
    is_mutable: bool,
    region: ?TypeId, // Region for memory management
};

/// Represents a region type for memory management
pub const RegionType = struct {
    name: []const u8,
    parent: ?TypeId, // Parent region if nested
    is_static: bool, // Static lifetime
};

// ============================================================================
// TYPE VARIABLE (FOR TYPE INFERENCE)
// ============================================================================

/// Represents a type variable for type inference
pub const TypeVar = struct {
    id: u32,
    name: ?[]const u8, // Optional name for error messages
    bounds: []const TraitBound,
    /// The resolved type (null if not yet resolved)
    resolved: ?TypeId,
};

// ============================================================================
// MAIN TYPE REPRESENTATION
// ============================================================================

/// The main type union representing all dAImond types
pub const Type = union(enum) {
    // Primitive types
    int: IntType,
    float: FloatType,
    bool,
    char, // Unicode scalar (4 bytes)
    str, // String type
    unit, // () / void

    // Composite types
    @"struct": StructType,
    @"enum": EnumType,
    tuple: TupleType,
    array: ArrayType,
    slice: SliceType,
    function: FunctionType,

    // Generic types
    type_param: TypeParam,
    generic_instance: GenericInstance,

    // Reference types
    ref: RefType,
    region: RegionType,

    // Special built-in types
    option: TypeId, // Option[T] - wraps the inner type
    result: struct { ok_type: TypeId, err_type: TypeId }, // Result[T, E]
    box: TypeId, // Box[T] - heap allocated

    // Trait types
    trait_def: TraitDef,
    trait_object: []const TypeId, // Dynamic trait object (dyn Trait)

    // Type inference
    type_var: TypeVar,

    // Special
    never, // Bottom type (never returns)
    any, // Top type (accepts anything) - mostly for error recovery
    unknown, // Placeholder for unresolved types
    error_type, // Represents a type error

    const Self = @This();

    /// Check if this is a numeric type
    pub fn isNumeric(self: Self) bool {
        return switch (self) {
            .int, .float => true,
            else => false,
        };
    }

    /// Check if this is an integral (integer) type
    pub fn isIntegral(self: Self) bool {
        return self == .int;
    }

    /// Check if this is a floating-point type
    pub fn isFloating(self: Self) bool {
        return self == .float;
    }

    /// Check if this is a primitive type
    pub fn isPrimitive(self: Self) bool {
        return switch (self) {
            .int, .float, .bool, .char, .str, .unit => true,
            else => false,
        };
    }

    /// Check if this is a reference type
    pub fn isReference(self: Self) bool {
        return self == .ref;
    }

    /// Check if this is a composite type
    pub fn isComposite(self: Self) bool {
        return switch (self) {
            .@"struct", .@"enum", .tuple, .array, .slice => true,
            else => false,
        };
    }

    /// Check if this type can be copied implicitly (Copy semantics)
    pub fn isCopy(self: Self) bool {
        return switch (self) {
            .int, .float, .bool, .char, .unit => true,
            .array => false, // Would need to check element type
            .tuple => false, // Would need to check element types
            else => false,
        };
    }

    /// Get the size of this type in bytes (if known statically)
    pub fn sizeOf(self: Self) ?usize {
        return switch (self) {
            .int => |i| @as(usize, i.bitWidth()) / 8,
            .float => |f| @as(usize, f.bitWidth()) / 8,
            .bool => 1,
            .char => 4, // Unicode scalar
            .unit => 0,
            .array => |arr| {
                _ = arr;
                return null; // Would need element size
            },
            .@"struct" => |s| s.size,
            else => null,
        };
    }

    /// Get the alignment of this type (if known statically)
    pub fn alignOf(self: Self) ?usize {
        return switch (self) {
            .int => |i| @as(usize, i.bitWidth()) / 8,
            .float => |f| @as(usize, f.bitWidth()) / 8,
            .bool => 1,
            .char => 4,
            .unit => 1,
            .@"struct" => |s| s.alignment,
            else => null,
        };
    }

    /// Format type for display
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .int => |i| try writer.writeAll(i.name()),
            .float => |f| try writer.writeAll(f.name()),
            .bool => try writer.writeAll("bool"),
            .char => try writer.writeAll("char"),
            .str => try writer.writeAll("str"),
            .unit => try writer.writeAll("()"),
            .@"struct" => |s| {
                if (s.name) |name| {
                    try writer.writeAll(name);
                } else {
                    try writer.writeAll("struct { ... }");
                }
            },
            .@"enum" => |e| try writer.writeAll(e.name),
            .tuple => |t| {
                try writer.writeAll("(");
                for (t.element_types, 0..) |_, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("T{}", .{i});
                }
                try writer.writeAll(")");
            },
            .array => |a| try writer.print("[T; {}]", .{a.size}),
            .slice => |s| {
                if (s.is_mutable) {
                    try writer.writeAll("[mut T]");
                } else {
                    try writer.writeAll("[T]");
                }
            },
            .function => |f| {
                try writer.writeAll("fn(");
                for (f.param_types, 0..) |_, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("T{}", .{i});
                }
                try writer.writeAll(") -> R");
                if (!f.effects.isPure()) {
                    try writer.writeAll(" with ");
                    try f.effects.format("", .{}, writer);
                }
            },
            .type_param => |p| try writer.writeAll(p.name),
            .generic_instance => try writer.writeAll("Generic[...]"),
            .ref => |r| {
                if (r.is_mutable) {
                    try writer.writeAll("&mut T");
                } else {
                    try writer.writeAll("&T");
                }
            },
            .region => |reg| try writer.print("region '{s}", .{reg.name}),
            .option => try writer.writeAll("Option[T]"),
            .result => try writer.writeAll("Result[T, E]"),
            .box => try writer.writeAll("Box[T]"),
            .trait_def => |t| try writer.print("trait {s}", .{t.name}),
            .trait_object => try writer.writeAll("dyn Trait"),
            .type_var => |v| {
                if (v.name) |name| {
                    try writer.print("?{s}", .{name});
                } else {
                    try writer.print("?T{}", .{v.id});
                }
            },
            .never => try writer.writeAll("!"),
            .any => try writer.writeAll("any"),
            .unknown => try writer.writeAll("???"),
            .error_type => try writer.writeAll("<error>"),
        }
    }
};

// ============================================================================
// TYPE CONTEXT / ENVIRONMENT
// ============================================================================

/// Type environment for managing types during compilation
pub const TypeContext = struct {
    allocator: Allocator,
    /// Arena for type internal data (param arrays, element arrays, etc.)
    arena: std.heap.ArenaAllocator,
    /// Interned types storage
    types: std.ArrayList(Type),
    /// Name to type ID mapping
    type_names: std.StringHashMap(TypeId),
    /// Type variable counter for inference
    next_type_var: u32,
    /// Trait implementations registry
    trait_impls: std.ArrayList(TraitImpl),
    /// Substitution map for generic instantiation
    substitutions: std.AutoHashMap(TypeId, TypeId),

    const Self = @This();

    /// Initialize a new type context
    pub fn init(allocator: Allocator) Self {
        var ctx = Self{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .types = std.ArrayList(Type).init(allocator),
            .type_names = std.StringHashMap(TypeId).init(allocator),
            .next_type_var = 0,
            .trait_impls = std.ArrayList(TraitImpl).init(allocator),
            .substitutions = std.AutoHashMap(TypeId, TypeId).init(allocator),
        };

        // Register built-in types
        ctx.registerBuiltins() catch {};

        return ctx;
    }

    /// Clean up resources - arena frees all type internal data
    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        self.types.deinit();
        self.type_names.deinit();
        self.trait_impls.deinit();
        self.substitutions.deinit();
    }

    /// Get arena allocator for type internal data
    fn typeAllocator(self: *Self) Allocator {
        return self.arena.allocator();
    }

    /// Register built-in primitive types
    fn registerBuiltins(self: *Self) !void {
        // Integer types
        _ = try self.intern(.{ .int = .i8 });
        _ = try self.intern(.{ .int = .i16 });
        _ = try self.intern(.{ .int = .i32 });
        _ = try self.intern(.{ .int = .i64 });
        _ = try self.intern(.{ .int = .i128 });
        _ = try self.intern(.{ .int = .u8 });
        _ = try self.intern(.{ .int = .u16 });
        _ = try self.intern(.{ .int = .u32 });
        _ = try self.intern(.{ .int = .u64 });
        _ = try self.intern(.{ .int = .u128 });

        // Float types
        _ = try self.intern(.{ .float = .f32 });
        _ = try self.intern(.{ .float = .f64 });

        // Other primitives
        _ = try self.intern(.bool);
        _ = try self.intern(.char);
        _ = try self.intern(.str);
        _ = try self.intern(.unit);
        _ = try self.intern(.never);

        // Register type aliases
        try self.type_names.put("int", try self.intern(.{ .int = .i64 }));
        try self.type_names.put("uint", try self.intern(.{ .int = .u64 }));
        try self.type_names.put("byte", try self.intern(.{ .int = .u8 }));
        try self.type_names.put("float", try self.intern(.{ .float = .f64 }));
    }

    /// Intern a type and return its ID
    pub fn intern(self: *Self, typ: Type) !TypeId {
        // Check if already exists (simple deduplication)
        for (self.types.items, 0..) |existing, i| {
            if (typeEqlSimple(existing, typ)) {
                return @intCast(i);
            }
        }

        // Add new type
        const id: TypeId = @intCast(self.types.items.len);
        try self.types.append(typ);
        return id;
    }

    /// Get a type by its ID
    pub fn get(self: *Self, id: TypeId) ?Type {
        if (id >= self.types.items.len) return null;
        return self.types.items[id];
    }

    /// Lookup a type by name
    pub fn lookup(self: *Self, name: []const u8) ?TypeId {
        return self.type_names.get(name);
    }

    /// Register a named type
    pub fn registerNamed(self: *Self, name: []const u8, id: TypeId) !void {
        try self.type_names.put(name, id);
    }

    /// Create a fresh type variable for inference
    pub fn freshTypeVar(self: *Self) !TypeId {
        const var_id = self.next_type_var;
        self.next_type_var += 1;

        return try self.intern(.{
            .type_var = .{
                .id = var_id,
                .name = null,
                .bounds = &[_]TraitBound{},
                .resolved = null,
            },
        });
    }

    /// Create an Option[T] type
    pub fn makeOption(self: *Self, inner: TypeId) !TypeId {
        return try self.intern(.{ .option = inner });
    }

    /// Create a Result[T, E] type
    pub fn makeResult(self: *Self, ok_type: TypeId, err_type: TypeId) !TypeId {
        return try self.intern(.{ .result = .{ .ok_type = ok_type, .err_type = err_type } });
    }

    /// Create a Box[T] type
    pub fn makeBox(self: *Self, inner: TypeId) !TypeId {
        return try self.intern(.{ .box = inner });
    }

    /// Create an array type [T; N]
    pub fn makeArray(self: *Self, element: TypeId, size: usize) !TypeId {
        return try self.intern(.{ .array = .{ .element_type = element, .size = size } });
    }

    /// Create a slice type [T]
    pub fn makeSlice(self: *Self, element: TypeId, is_mutable: bool) !TypeId {
        return try self.intern(.{ .slice = .{ .element_type = element, .is_mutable = is_mutable } });
    }

    /// Create a tuple type
    pub fn makeTuple(self: *Self, elements: []const TypeId) !TypeId {
        // Need to copy the slice using arena
        const copied = try self.typeAllocator().dupe(TypeId, elements);
        return try self.intern(.{ .tuple = .{ .element_types = copied } });
    }

    /// Create a reference type
    pub fn makeRef(self: *Self, pointee: TypeId, is_mutable: bool, region: ?TypeId) !TypeId {
        return try self.intern(.{ .ref = .{
            .pointee = pointee,
            .is_mutable = is_mutable,
            .region = region,
        } });
    }

    /// Create a function type
    pub fn makeFunction(
        self: *Self,
        params: []const TypeId,
        return_type: TypeId,
        effects: EffectSet,
    ) !TypeId {
        const copied_params = try self.typeAllocator().dupe(TypeId, params);
        return try self.intern(.{
            .function = .{
                .param_types = copied_params,
                .param_names = null,
                .return_type = return_type,
                .effects = effects,
                .is_variadic = false,
                .type_params = &[_][]const u8{},
            },
        });
    }

    /// Apply substitution to a type (for generic instantiation)
    pub fn substitute(self: *Self, type_id: TypeId, subs: *const std.AutoHashMap(TypeId, TypeId)) !TypeId {
        if (subs.get(type_id)) |replacement| {
            return replacement;
        }

        const typ = self.get(type_id) orelse return type_id;

        return switch (typ) {
            .option => |inner| try self.makeOption(try self.substitute(inner, subs)),
            .result => |r| try self.makeResult(
                try self.substitute(r.ok_type, subs),
                try self.substitute(r.err_type, subs),
            ),
            .box => |inner| try self.makeBox(try self.substitute(inner, subs)),
            .array => |a| try self.makeArray(try self.substitute(a.element_type, subs), a.size),
            .slice => |s| try self.makeSlice(try self.substitute(s.element_type, subs), s.is_mutable),
            .tuple => |t| {
                var new_elements = try self.typeAllocator().alloc(TypeId, t.element_types.len);
                for (t.element_types, 0..) |elem, i| {
                    new_elements[i] = try self.substitute(elem, subs);
                }
                return try self.intern(.{ .tuple = .{ .element_types = new_elements } });
            },
            .ref => |r| try self.makeRef(
                try self.substitute(r.pointee, subs),
                r.is_mutable,
                if (r.region) |reg| try self.substitute(reg, subs) else null,
            ),
            .function => |f| {
                var new_params = try self.typeAllocator().alloc(TypeId, f.param_types.len);
                for (f.param_types, 0..) |param, i| {
                    new_params[i] = try self.substitute(param, subs);
                }
                const new_return = try self.substitute(f.return_type, subs);
                return try self.intern(.{
                    .function = .{
                        .param_types = new_params,
                        .param_names = f.param_names,
                        .return_type = new_return,
                        .effects = f.effects,
                        .is_variadic = f.is_variadic,
                        .type_params = f.type_params,
                    },
                });
            },
            else => type_id,
        };
    }

    /// Register a trait implementation
    pub fn registerTraitImpl(self: *Self, impl: TraitImpl) !void {
        try self.trait_impls.append(impl);
    }

    /// Find implementations of a trait for a type
    pub fn findTraitImpls(self: *Self, trait_id: TypeId, type_id: TypeId) []const TraitImpl {
        var results = std.ArrayList(TraitImpl).init(self.typeAllocator());
        defer results.deinit();

        for (self.trait_impls.items) |impl| {
            if (impl.trait_id == trait_id and impl.implementing_type == type_id) {
                results.append(impl) catch {};
            }
        }

        return results.toOwnedSlice() catch &[_]TraitImpl{};
    }
};

// ============================================================================
// TYPE UTILITIES AND HELPER FUNCTIONS
// ============================================================================

/// Simple type equality (tag comparison only)
fn typeEqlSimple(a: Type, b: Type) bool {
    const TagType = @typeInfo(Type).Union.tag_type.?;
    const tag_a: TagType = a;
    const tag_b: TagType = b;

    if (tag_a != tag_b) return false;

    return switch (a) {
        .int => |i| b.int == i,
        .float => |f| b.float == f,
        .bool, .char, .str, .unit, .never, .any, .unknown, .error_type => true,
        .option => |o| b.option == o,
        .result => |r| b.result.ok_type == r.ok_type and b.result.err_type == r.err_type,
        .box => |bx| b.box == bx,
        .array => |arr| b.array.element_type == arr.element_type and b.array.size == arr.size,
        .slice => |s| b.slice.element_type == s.element_type and b.slice.is_mutable == s.is_mutable,
        else => false, // Complex types need deeper comparison
    };
}

/// Check if type is numeric
pub fn isNumeric(ctx: *TypeContext, type_id: TypeId) bool {
    const typ = ctx.get(type_id) orelse return false;
    return typ.isNumeric();
}

/// Check if type is an integer
pub fn isIntegral(ctx: *TypeContext, type_id: TypeId) bool {
    const typ = ctx.get(type_id) orelse return false;
    return typ.isIntegral();
}

/// Check if type is a float
pub fn isFloating(ctx: *TypeContext, type_id: TypeId) bool {
    const typ = ctx.get(type_id) orelse return false;
    return typ.isFloating();
}

/// Check if implicit coercion is allowed from one type to another
pub fn canCoerce(ctx: *TypeContext, from_id: TypeId, to_id: TypeId) bool {
    if (from_id == to_id) return true;

    const from = ctx.get(from_id) orelse return false;
    const to = ctx.get(to_id) orelse return false;

    // Same type categories
    const from_tag = std.meta.activeTag(from);
    const to_tag = std.meta.activeTag(to);

    // Integer widening
    if (from_tag == .int and to_tag == .int) {
        const from_int = from.int;
        const to_int = to.int;

        // Same signedness, wider target
        if (from_int.isSigned() == to_int.isSigned()) {
            return to_int.bitWidth() >= from_int.bitWidth();
        }

        // Unsigned to signed if target is wider
        if (!from_int.isSigned() and to_int.isSigned()) {
            return to_int.bitWidth() > from_int.bitWidth();
        }

        return false;
    }

    // Float widening
    if (from_tag == .float and to_tag == .float) {
        return to.float.bitWidth() >= from.float.bitWidth();
    }

    // Integer to float (with potential precision loss)
    if (from_tag == .int and to_tag == .float) {
        const from_int = from.int;
        const to_float = to.float;
        // Allow if float has enough mantissa bits
        const mantissa_bits: u8 = if (to_float == .f32) 24 else 53;
        return from_int.bitWidth() <= mantissa_bits;
    }

    // T to Option[T]
    if (to_tag == .option) {
        return from_id == to.option;
    }

    // Array to slice
    if (from_tag == .array and to_tag == .slice) {
        return from.array.element_type == to.slice.element_type;
    }

    // Mutable reference to immutable reference
    if (from_tag == .ref and to_tag == .ref) {
        return from.ref.pointee == to.ref.pointee and
            (from.ref.is_mutable or !to.ref.is_mutable);
    }

    // never coerces to anything
    if (from_tag == .never) return true;

    // anything coerces to any
    if (to_tag == .any) return true;

    return false;
}

/// Find the common supertype of two types
pub fn commonType(ctx: *TypeContext, a_id: TypeId, b_id: TypeId) ?TypeId {
    if (a_id == b_id) return a_id;

    const a = ctx.get(a_id) orelse return null;
    const b = ctx.get(b_id) orelse return null;

    const a_tag = std.meta.activeTag(a);
    const b_tag = std.meta.activeTag(b);

    // Type variables are compatible with any concrete type
    if (a_tag == .type_var) return b_id;
    if (b_tag == .type_var) return a_id;

    // Both integers - find wider type
    if (a_tag == .int and b_tag == .int) {
        const a_int = a.int;
        const b_int = b.int;

        // Same signedness - use wider
        if (a_int.isSigned() == b_int.isSigned()) {
            return if (a_int.bitWidth() >= b_int.bitWidth()) a_id else b_id;
        }

        // Mixed signedness - promote to signed if possible
        const unsigned = if (a_int.isSigned()) b_int else a_int;
        const signed = if (a_int.isSigned()) a_int else b_int;

        // Need a signed type that can hold unsigned max
        if (signed.bitWidth() > unsigned.bitWidth()) {
            return if (a_int.isSigned()) a_id else b_id;
        }

        // Promote to next wider signed type
        return switch (unsigned.bitWidth()) {
            8 => ctx.lookup("i16"),
            16 => ctx.lookup("i32"),
            32 => ctx.lookup("i64"),
            64 => ctx.lookup("i128"),
            else => null,
        };
    }

    // Both floats - use wider
    if (a_tag == .float and b_tag == .float) {
        return if (a.float.bitWidth() >= b.float.bitWidth()) a_id else b_id;
    }

    // Int and float - promote to float
    if (a_tag == .int and b_tag == .float) return b_id;
    if (a_tag == .float and b_tag == .int) return a_id;

    // Option types - find common inner type
    if (a_tag == .option and b_tag == .option) {
        if (commonType(ctx, a.option, b.option)) |common_inner| {
            return ctx.makeOption(common_inner) catch null;
        }
    }

    // never is bottom type
    if (a_tag == .never) return b_id;
    if (b_tag == .never) return a_id;

    return null;
}

/// Instantiate a generic type with concrete type arguments
pub fn instantiateGeneric(
    ctx: *TypeContext,
    generic_id: TypeId,
    type_args: []const TypeId,
) !TypeId {
    const generic = ctx.get(generic_id) orelse return error.InvalidType;

    // Build substitution map from type parameters to arguments
    var subs = std.AutoHashMap(TypeId, TypeId).init(ctx.typeAllocator());
    defer subs.deinit();

    switch (generic) {
        .@"struct" => |s| {
            if (type_args.len != s.type_params.len) return error.WrongNumberOfTypeArgs;
            for (s.type_params, 0..) |param_name, i| {
                if (ctx.lookup(param_name)) |param_id| {
                    try subs.put(param_id, type_args[i]);
                }
            }
        },
        .@"enum" => |e| {
            if (type_args.len != e.type_params.len) return error.WrongNumberOfTypeArgs;
            for (e.type_params, 0..) |param_name, i| {
                if (ctx.lookup(param_name)) |param_id| {
                    try subs.put(param_id, type_args[i]);
                }
            }
        },
        .function => |f| {
            if (type_args.len != f.type_params.len) return error.WrongNumberOfTypeArgs;
            for (f.type_params, 0..) |param_name, i| {
                if (ctx.lookup(param_name)) |param_id| {
                    try subs.put(param_id, type_args[i]);
                }
            }
        },
        else => return error.NotGenericType,
    }

    // Create instance
    return try ctx.intern(.{
        .generic_instance = .{
            .base_type = generic_id,
            .type_args = try ctx.typeAllocator().dupe(TypeId, type_args),
        },
    });
}

/// Check if two types are equal
pub fn typeEql(ctx: *TypeContext, a_id: TypeId, b_id: TypeId) bool {
    if (a_id == b_id) return true;

    const a = ctx.get(a_id) orelse return false;
    const b = ctx.get(b_id) orelse return false;

    return typeEqlSimple(a, b);
}

/// Check if type A is compatible with type B (assignable)
pub fn isCompatible(ctx: *TypeContext, source_id: TypeId, target_id: TypeId) bool {
    if (typeEql(ctx, source_id, target_id)) return true;
    return canCoerce(ctx, source_id, target_id);
}

/// Check if a type implements a trait
pub fn implementsTrait(ctx: *TypeContext, type_id: TypeId, trait_id: TypeId) bool {
    const impls = ctx.findTraitImpls(trait_id, type_id);
    return impls.len > 0;
}

/// Check effect compatibility (required effects must be subset of provided)
pub fn effectsCompatible(required: EffectSet, provided: EffectSet) bool {
    return required.isSubsetOf(provided);
}

// ============================================================================
// TYPE DISPLAY UTILITIES
// ============================================================================

/// Format a type ID for display
pub fn formatType(ctx: *TypeContext, type_id: TypeId, writer: anytype) !void {
    if (ctx.get(type_id)) |typ| {
        try typ.format("", .{}, writer);
    } else {
        try writer.print("<invalid type {}>", .{type_id});
    }
}

/// Get a string representation of a type
pub fn typeName(ctx: *TypeContext, type_id: TypeId, allocator: Allocator) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    try formatType(ctx, type_id, buf.writer());
    return buf.toOwnedSlice();
}

// ============================================================================
// TESTS
// ============================================================================

const testing = std.testing;

test "type context initialization" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    // Built-in types should be registered
    try testing.expect(ctx.lookup("int") != null);
    try testing.expect(ctx.lookup("uint") != null);
    try testing.expect(ctx.lookup("byte") != null);
    try testing.expect(ctx.lookup("float") != null);
}

test "integer type properties" {
    try testing.expectEqual(@as(u8, 8), IntType.i8.bitWidth());
    try testing.expectEqual(@as(u8, 64), IntType.i64.bitWidth());
    try testing.expectEqual(@as(u8, 128), IntType.u128.bitWidth());

    try testing.expect(IntType.i32.isSigned());
    try testing.expect(!IntType.u32.isSigned());
}

test "float type properties" {
    try testing.expectEqual(@as(u8, 32), FloatType.f32.bitWidth());
    try testing.expectEqual(@as(u8, 64), FloatType.f64.bitWidth());
}

test "type numeric checks" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const f64_id = try ctx.intern(.{ .float = .f64 });
    const bool_id = try ctx.intern(.bool);

    try testing.expect(isNumeric(&ctx, i32_id));
    try testing.expect(isNumeric(&ctx, f64_id));
    try testing.expect(!isNumeric(&ctx, bool_id));

    try testing.expect(isIntegral(&ctx, i32_id));
    try testing.expect(!isIntegral(&ctx, f64_id));

    try testing.expect(isFloating(&ctx, f64_id));
    try testing.expect(!isFloating(&ctx, i32_id));
}

test "type coercion - integer widening" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i8_id = try ctx.intern(.{ .int = .i8 });
    const i16_id = try ctx.intern(.{ .int = .i16 });
    const i32_id = try ctx.intern(.{ .int = .i32 });
    const i64_id = try ctx.intern(.{ .int = .i64 });

    // Same type
    try testing.expect(canCoerce(&ctx, i32_id, i32_id));

    // Widening allowed
    try testing.expect(canCoerce(&ctx, i8_id, i16_id));
    try testing.expect(canCoerce(&ctx, i8_id, i32_id));
    try testing.expect(canCoerce(&ctx, i16_id, i64_id));

    // Narrowing not allowed
    try testing.expect(!canCoerce(&ctx, i64_id, i32_id));
    try testing.expect(!canCoerce(&ctx, i32_id, i8_id));
}

test "type coercion - signed/unsigned" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const u8_id = try ctx.intern(.{ .int = .u8 });
    const u16_id = try ctx.intern(.{ .int = .u16 });
    const i16_id = try ctx.intern(.{ .int = .i16 });
    const i32_id = try ctx.intern(.{ .int = .i32 });

    // Unsigned to wider signed allowed
    try testing.expect(canCoerce(&ctx, u8_id, i16_id));
    try testing.expect(canCoerce(&ctx, u16_id, i32_id));

    // Signed to unsigned not allowed implicitly
    try testing.expect(!canCoerce(&ctx, i16_id, u16_id));
}

test "type coercion - float widening" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const f32_id = try ctx.intern(.{ .float = .f32 });
    const f64_id = try ctx.intern(.{ .float = .f64 });

    try testing.expect(canCoerce(&ctx, f32_id, f64_id));
    try testing.expect(!canCoerce(&ctx, f64_id, f32_id));
}

test "type coercion - int to float" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const f64_id = try ctx.intern(.{ .float = .f64 });

    try testing.expect(canCoerce(&ctx, i32_id, f64_id));
}

test "type coercion - to option" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const opt_i32 = try ctx.makeOption(i32_id);

    try testing.expect(canCoerce(&ctx, i32_id, opt_i32));
}

test "type coercion - never type" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const never_id = try ctx.intern(.never);
    const i32_id = try ctx.intern(.{ .int = .i32 });
    const str_id = try ctx.intern(.str);

    // never coerces to anything
    try testing.expect(canCoerce(&ctx, never_id, i32_id));
    try testing.expect(canCoerce(&ctx, never_id, str_id));
}

test "common type - integers" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i16_id = try ctx.intern(.{ .int = .i16 });
    const i32_id = try ctx.intern(.{ .int = .i32 });

    const common = commonType(&ctx, i16_id, i32_id);
    try testing.expect(common != null);
    try testing.expectEqual(i32_id, common.?);
}

test "common type - floats" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const f32_id = try ctx.intern(.{ .float = .f32 });
    const f64_id = try ctx.intern(.{ .float = .f64 });

    const common = commonType(&ctx, f32_id, f64_id);
    try testing.expect(common != null);
    try testing.expectEqual(f64_id, common.?);
}

test "common type - int and float" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const f64_id = try ctx.intern(.{ .float = .f64 });

    const common = commonType(&ctx, i32_id, f64_id);
    try testing.expect(common != null);
    try testing.expectEqual(f64_id, common.?);
}

test "common type - with never" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const never_id = try ctx.intern(.never);
    const i32_id = try ctx.intern(.{ .int = .i32 });

    const common = commonType(&ctx, never_id, i32_id);
    try testing.expect(common != null);
    try testing.expectEqual(i32_id, common.?);
}

test "make composite types" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const str_id = try ctx.intern(.str);

    // Option
    const opt_i32 = try ctx.makeOption(i32_id);
    const opt_type = ctx.get(opt_i32).?;
    try testing.expectEqual(Type{ .option = i32_id }, opt_type);

    // Result
    const result_type = try ctx.makeResult(i32_id, str_id);
    const res = ctx.get(result_type).?;
    try testing.expectEqual(i32_id, res.result.ok_type);
    try testing.expectEqual(str_id, res.result.err_type);

    // Box
    const box_i32 = try ctx.makeBox(i32_id);
    const box_type = ctx.get(box_i32).?;
    try testing.expectEqual(Type{ .box = i32_id }, box_type);

    // Array
    const arr_type = try ctx.makeArray(i32_id, 10);
    const arr = ctx.get(arr_type).?;
    try testing.expectEqual(i32_id, arr.array.element_type);
    try testing.expectEqual(@as(usize, 10), arr.array.size);

    // Slice
    const slice_type = try ctx.makeSlice(i32_id, false);
    const slc = ctx.get(slice_type).?;
    try testing.expectEqual(i32_id, slc.slice.element_type);
    try testing.expect(!slc.slice.is_mutable);
}

test "make tuple type" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const str_id = try ctx.intern(.str);
    const bool_id = try ctx.intern(.bool);

    const tuple_id = try ctx.makeTuple(&[_]TypeId{ i32_id, str_id, bool_id });
    const tuple = ctx.get(tuple_id).?;

    try testing.expectEqual(@as(usize, 3), tuple.tuple.arity());
    try testing.expectEqual(i32_id, tuple.tuple.element_types[0]);
    try testing.expectEqual(str_id, tuple.tuple.element_types[1]);
    try testing.expectEqual(bool_id, tuple.tuple.element_types[2]);
}

test "make function type" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const bool_id = try ctx.intern(.bool);

    var effects = EffectSet.init(testing.allocator);
    defer effects.deinit();
    effects.addBuiltin(.io);

    const fn_type = try ctx.makeFunction(&[_]TypeId{ i32_id, i32_id }, bool_id, effects);
    const func = ctx.get(fn_type).?;

    try testing.expectEqual(@as(usize, 2), func.function.param_types.len);
    try testing.expectEqual(bool_id, func.function.return_type);
    try testing.expect(func.function.effects.hasIO());
    try testing.expect(!func.function.isPure());
}

test "make reference type" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });

    const ref_type = try ctx.makeRef(i32_id, false, null);
    const ref = ctx.get(ref_type).?;

    try testing.expectEqual(i32_id, ref.ref.pointee);
    try testing.expect(!ref.ref.is_mutable);

    const mut_ref_type = try ctx.makeRef(i32_id, true, null);
    const mut_ref = ctx.get(mut_ref_type).?;
    try testing.expect(mut_ref.ref.is_mutable);
}

test "fresh type variable" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const tv1 = try ctx.freshTypeVar();
    const tv2 = try ctx.freshTypeVar();

    try testing.expect(tv1 != tv2);

    const var1 = ctx.get(tv1).?;
    const var2 = ctx.get(tv2).?;

    try testing.expectEqual(@as(u32, 0), var1.type_var.id);
    try testing.expectEqual(@as(u32, 1), var2.type_var.id);
}

test "effect set operations" {
    var effects = EffectSet.init(testing.allocator);
    defer effects.deinit();

    try testing.expect(effects.isPure());

    effects.addBuiltin(.io);
    try testing.expect(!effects.isPure());
    try testing.expect(effects.hasIO());
    try testing.expect(effects.hasBuiltin(.io));
    try testing.expect(!effects.hasBuiltin(.network));

    effects.addBuiltin(.network);
    try testing.expect(effects.hasBuiltin(.network));
}

test "effect set subset check" {
    var set1 = EffectSet.init(testing.allocator);
    defer set1.deinit();
    set1.addBuiltin(.io);

    var set2 = EffectSet.init(testing.allocator);
    defer set2.deinit();
    set2.addBuiltin(.io);
    set2.addBuiltin(.network);

    // set1 is subset of set2
    try testing.expect(set1.isSubsetOf(set2));
    // set2 is not subset of set1
    try testing.expect(!set2.isSubsetOf(set1));

    // Pure is subset of everything
    try testing.expect(EffectSet.pure.isSubsetOf(set1));
    try testing.expect(EffectSet.pure.isSubsetOf(set2));
}

test "effect set union" {
    var set1 = EffectSet.init(testing.allocator);
    defer set1.deinit();
    set1.addBuiltin(.io);

    var set2 = EffectSet.init(testing.allocator);
    defer set2.deinit();
    set2.addBuiltin(.network);

    const combined = set1.unionWith(set2);
    try testing.expect(combined.hasBuiltin(.io));
    try testing.expect(combined.hasBuiltin(.network));
}

test "struct type field access" {
    const fields = [_]StructField{
        .{ .name = "x", .type_id = 0, .is_mutable = false, .default_value = null, .offset = null },
        .{ .name = "y", .type_id = 0, .is_mutable = false, .default_value = null, .offset = null },
        .{ .name = "z", .type_id = 0, .is_mutable = true, .default_value = null, .offset = null },
    };

    const struct_type = StructType{
        .name = "Point",
        .fields = &fields,
        .type_params = &[_][]const u8{},
        .is_packed = false,
        .size = null,
        .alignment = null,
    };

    try testing.expect(struct_type.getField("x") != null);
    try testing.expect(struct_type.getField("y") != null);
    try testing.expect(struct_type.getField("z") != null);
    try testing.expect(struct_type.getField("w") == null);

    try testing.expectEqual(@as(?usize, 0), struct_type.getFieldIndex("x"));
    try testing.expectEqual(@as(?usize, 2), struct_type.getFieldIndex("z"));
}

test "enum type variant access" {
    const variants = [_]EnumVariant{
        .{ .name = "None", .payload_types = &[_]TypeId{}, .tag_value = null },
        .{ .name = "Some", .payload_types = &[_]TypeId{0}, .tag_value = null },
    };

    const enum_type = EnumType{
        .name = "Option",
        .variants = &variants,
        .type_params = &[_][]const u8{"T"},
        .tag_type = .u8,
    };

    try testing.expect(enum_type.getVariant("None") != null);
    try testing.expect(enum_type.getVariant("Some") != null);
    try testing.expect(enum_type.getVariant("Err") == null);

    try testing.expectEqual(@as(?usize, 0), enum_type.getVariantIndex("None"));
    try testing.expectEqual(@as(?usize, 1), enum_type.getVariantIndex("Some"));
}

test "type equality" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_a = try ctx.intern(.{ .int = .i32 });
    const i32_b = try ctx.intern(.{ .int = .i32 });
    const i64_id = try ctx.intern(.{ .int = .i64 });

    try testing.expect(typeEql(&ctx, i32_a, i32_b));
    try testing.expect(!typeEql(&ctx, i32_a, i64_id));
}

test "type compatibility" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const i64_id = try ctx.intern(.{ .int = .i64 });

    // Same type is compatible
    try testing.expect(isCompatible(&ctx, i32_id, i32_id));

    // Widening is compatible
    try testing.expect(isCompatible(&ctx, i32_id, i64_id));

    // Narrowing is not compatible
    try testing.expect(!isCompatible(&ctx, i64_id, i32_id));
}

test "type size and alignment" {
    const i32_type = Type{ .int = .i32 };
    try testing.expectEqual(@as(?usize, 4), i32_type.sizeOf());
    try testing.expectEqual(@as(?usize, 4), i32_type.alignOf());

    const i64_type = Type{ .int = .i64 };
    try testing.expectEqual(@as(?usize, 8), i64_type.sizeOf());
    try testing.expectEqual(@as(?usize, 8), i64_type.alignOf());

    const char_type: Type = .char;
    try testing.expectEqual(@as(?usize, 4), char_type.sizeOf());

    const bool_type: Type = .bool;
    try testing.expectEqual(@as(?usize, 1), bool_type.sizeOf());

    const unit_type: Type = .unit;
    try testing.expectEqual(@as(?usize, 0), unit_type.sizeOf());
}

test "type primitive checks" {
    const i32_type = Type{ .int = .i32 };
    try testing.expect(i32_type.isPrimitive());
    try testing.expect(i32_type.isNumeric());
    try testing.expect(i32_type.isIntegral());
    try testing.expect(!i32_type.isFloating());

    const f64_type = Type{ .float = .f64 };
    try testing.expect(f64_type.isPrimitive());
    try testing.expect(f64_type.isNumeric());
    try testing.expect(!f64_type.isIntegral());
    try testing.expect(f64_type.isFloating());

    const bool_type: Type = .bool;
    try testing.expect(bool_type.isPrimitive());
    try testing.expect(!bool_type.isNumeric());
}

test "type copy semantics" {
    const i32_type = Type{ .int = .i32 };
    try testing.expect(i32_type.isCopy());

    const bool_type: Type = .bool;
    try testing.expect(bool_type.isCopy());

    const str_type: Type = .str;
    try testing.expect(!str_type.isCopy());
}

test "effects compatibility" {
    var required = EffectSet.init(testing.allocator);
    defer required.deinit();
    required.addBuiltin(.io);

    var provided = EffectSet.init(testing.allocator);
    defer provided.deinit();
    provided.addBuiltin(.io);
    provided.addBuiltin(.network);

    // Required effects are subset of provided
    try testing.expect(effectsCompatible(required, provided));

    // But not the other way around
    try testing.expect(!effectsCompatible(provided, required));

    // Pure requires no effects
    try testing.expect(effectsCompatible(EffectSet.pure, required));
}

test "array to slice coercion" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const arr_type = try ctx.makeArray(i32_id, 10);
    const slice_type = try ctx.makeSlice(i32_id, false);

    try testing.expect(canCoerce(&ctx, arr_type, slice_type));
}

test "reference coercion" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const mut_ref = try ctx.makeRef(i32_id, true, null);
    const immut_ref = try ctx.makeRef(i32_id, false, null);

    // Mutable to immutable is allowed
    try testing.expect(canCoerce(&ctx, mut_ref, immut_ref));

    // Immutable to mutable is not allowed
    try testing.expect(!canCoerce(&ctx, immut_ref, mut_ref));
}

test "type formatting" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const i32_id = try ctx.intern(.{ .int = .i32 });
    const name = try typeName(&ctx, i32_id, testing.allocator);
    defer testing.allocator.free(name);

    try testing.expectEqualStrings("i32", name);
}

test "trait bound representation" {
    const bound = TraitBound{
        .trait_id = 42,
        .type_args = &[_]TypeId{},
    };

    try testing.expectEqual(@as(TypeId, 42), bound.trait_id);
}

test "trait impl registration and lookup" {
    var ctx = TypeContext.init(testing.allocator);
    defer ctx.deinit();

    const trait_impl = TraitImpl{
        .trait_id = 10,
        .implementing_type = 20,
        .type_args = &[_]TypeId{},
        .method_impls = &[_]MethodImpl{},
    };

    try ctx.registerTraitImpl(trait_impl);

    const impls = ctx.findTraitImpls(10, 20);
    try testing.expectEqual(@as(usize, 1), impls.len);
}
