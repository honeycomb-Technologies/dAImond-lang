//! dAImond IR Generation
//!
//! Lowers the typed AST into dAImond IR (SSA form).
//! This is the bridge between the frontend (shared with Stage 0)
//! and the LLVM backend.

const std = @import("std");
const ir = @import("ir.zig");
const frontend = @import("frontend");
const ast = frontend.ast;

const Allocator = std.mem.Allocator;

const StructInfo = struct {
    name: []const u8,
    fields: []const FieldInfo,
    ir_type: *const ir.IRType,
};

const FieldInfo = struct {
    name: []const u8,
    ty: *const ir.IRType,
};

const EnumInfo = struct {
    name: []const u8,
    variants: []const EnumVariantInfo,
    ir_type: *const ir.IRType,
};

const EnumVariantInfo = struct {
    name: []const u8,
    tag: i64,
    payload_types: ?[]const *const ir.IRType,
};

/// Element type category for typed list operations
const ListElemKind = enum { int64, float64, string, other };

/// Map key/value type combinations for typed map operations
const MapKind = enum { string_int, int_string };

pub const IRGenerator = struct {
    allocator: Allocator,
    module: ir.Module,
    builder: ir.IRBuilder,
    /// Map from variable name to alloca value (instruction_ref)
    variable_map: std.StringHashMap(ir.Value),
    /// Map from variable name to its IR type
    variable_types: std.StringHashMap(*const ir.IRType),
    /// Struct definitions: name -> StructInfo
    struct_defs: std.StringHashMap(StructInfo),
    /// Track which variables are lists and their element type category
    list_elem_kinds: std.StringHashMap(ListElemKind),
    /// Track the actual IR element type for struct-typed lists (variable name -> struct IRType)
    list_elem_types: std.StringHashMap(*const ir.IRType),
    /// Generic function declarations (name -> AST node)
    generic_fn_decls: std.StringHashMap(*const ast.FunctionDecl),
    /// Generated monomorphizations (mangled name -> void)
    mono_generated: std.StringHashMap(void),
    /// Type parameter substitutions (for monomorphization)
    type_substitutions: std.StringHashMap(*const ir.IRType),
    /// Current function being generated
    current_function: ?*ir.Function = null,
    /// Counter for generating unique block labels
    label_counter: u32 = 0,
    /// Break/continue targets for loop control flow
    break_target: ?[]const u8 = null,
    continue_target: ?[]const u8 = null,
    /// Current function's return type (for Ok/Err result type context)
    current_return_type: ?*const ir.IRType = null,
    /// Counter for generating unique lambda names
    lambda_counter: u32 = 0,
    /// Track which variables hold lambda function pointers (var name -> lambda fn name)
    fn_ptr_vars: std.StringHashMap([]const u8),
    /// Track lambda captures: lambda function name -> list of captured variable names
    lambda_captures: std.StringHashMap([]const []const u8),
    /// Current impl block target type name (e.g., "Point" when inside `impl Point { ... }`)
    current_impl_type: ?[]const u8 = null,
    /// Map from method name to struct type name (e.g., "sum" -> "Point")
    method_struct_map: std.StringHashMap([]const u8),
    /// Map from variable name to its struct type name (e.g., "p" -> "Point")
    var_struct_types: std.StringHashMap([]const u8),
    /// Source file declarations (stored for comptime function resolution)
    comptime_declarations: ?[]const *ast.Declaration = null,
    /// Track which functions have `mut` parameters (fn name -> bitmask of mut param indices)
    /// Each entry is a list of booleans indicating which params are mut (by-ref).
    mut_param_fns: std.StringHashMap([]const bool),
    /// Enum definitions: enum name -> EnumInfo
    enum_defs: std.StringHashMap(EnumInfo),
    /// Track which variables hold enum values and their enum type name
    var_enum_types: std.StringHashMap([]const u8),
    /// Track which variables are maps and their map kind
    map_var_kinds: std.StringHashMap(MapKind),
    /// Track which variables are Box[T] and their inner struct type
    box_inner_types: std.StringHashMap(*const ir.IRType),
    /// Track which variables are dyn Trait objects: var name -> concrete struct type name
    dyn_var_concrete: std.StringHashMap([]const u8),
    /// Track which variables are dyn Trait objects: var name -> trait name
    dyn_var_traits: std.StringHashMap([]const u8),
    /// Track trait impl relationships: "StructName:TraitName" -> list of method names
    trait_impl_methods: std.StringHashMap([]const []const u8),
    /// Track user-declared extern functions (not runtime builtins): name -> void
    user_extern_fns: std.StringHashMap(void),
    /// Track list-typed struct fields: "StructName.field_name" -> ListElemKind
    field_list_elem_kinds: std.StringHashMap(ListElemKind),
    /// Track list-typed struct fields element types: "StructName.field_name" -> struct IRType (for .other kind)
    field_list_elem_types: std.StringHashMap(*const ir.IRType),

    pub fn init(allocator: Allocator) IRGenerator {
        return .{
            .allocator = allocator,
            .module = ir.Module.init(allocator),
            .builder = ir.IRBuilder.init(allocator),
            .variable_map = std.StringHashMap(ir.Value).init(allocator),
            .variable_types = std.StringHashMap(*const ir.IRType).init(allocator),
            .struct_defs = std.StringHashMap(StructInfo).init(allocator),
            .list_elem_kinds = std.StringHashMap(ListElemKind).init(allocator),
            .list_elem_types = std.StringHashMap(*const ir.IRType).init(allocator),
            .generic_fn_decls = std.StringHashMap(*const ast.FunctionDecl).init(allocator),
            .mono_generated = std.StringHashMap(void).init(allocator),
            .type_substitutions = std.StringHashMap(*const ir.IRType).init(allocator),
            .fn_ptr_vars = std.StringHashMap([]const u8).init(allocator),
            .lambda_captures = std.StringHashMap([]const []const u8).init(allocator),
            .method_struct_map = std.StringHashMap([]const u8).init(allocator),
            .var_struct_types = std.StringHashMap([]const u8).init(allocator),
            .mut_param_fns = std.StringHashMap([]const bool).init(allocator),
            .enum_defs = std.StringHashMap(EnumInfo).init(allocator),
            .var_enum_types = std.StringHashMap([]const u8).init(allocator),
            .map_var_kinds = std.StringHashMap(MapKind).init(allocator),
            .box_inner_types = std.StringHashMap(*const ir.IRType).init(allocator),
            .dyn_var_concrete = std.StringHashMap([]const u8).init(allocator),
            .dyn_var_traits = std.StringHashMap([]const u8).init(allocator),
            .trait_impl_methods = std.StringHashMap([]const []const u8).init(allocator),
            .user_extern_fns = std.StringHashMap(void).init(allocator),
            .field_list_elem_kinds = std.StringHashMap(ListElemKind).init(allocator),
            .field_list_elem_types = std.StringHashMap(*const ir.IRType).init(allocator),
        };
    }

    pub fn deinit(self: *IRGenerator) void {
        self.module.deinit();
        self.builder.deinit();
        self.variable_map.deinit();
        self.variable_types.deinit();
        self.struct_defs.deinit();
        self.list_elem_kinds.deinit();
        self.list_elem_types.deinit();
        self.generic_fn_decls.deinit();
        self.mono_generated.deinit();
        self.type_substitutions.deinit();
        self.fn_ptr_vars.deinit();
        self.lambda_captures.deinit();
        self.method_struct_map.deinit();
        self.var_struct_types.deinit();
        self.mut_param_fns.deinit();
        self.enum_defs.deinit();
        self.var_enum_types.deinit();
        self.box_inner_types.deinit();
        self.dyn_var_concrete.deinit();
        self.dyn_var_traits.deinit();
        self.trait_impl_methods.deinit();
        self.user_extern_fns.deinit();
        self.field_list_elem_kinds.deinit();
        self.field_list_elem_types.deinit();
    }

    pub fn getModule(self: *IRGenerator) *ir.Module {
        return &self.module;
    }

    fn nextLabel(self: *IRGenerator, prefix: []const u8) ![]const u8 {
        const label = try std.fmt.allocPrint(self.allocator, "{s}_{}", .{ prefix, self.label_counter });
        self.label_counter += 1;
        return label;
    }

    fn defaultValueForType(self: *IRGenerator, ty: *const ir.IRType) ir.Value {
        _ = self;
        return switch (ty.*) {
            .i8_type, .i16_type, .i32_type, .i64_type,
            .u8_type, .u16_type, .u32_type, .u64_type,
            => .{ .const_int = 0 },
            .f32_type, .f64_type => .{ .const_float = 0.0 },
            .bool_type => .{ .const_bool = false },
            .string_type => .{ .const_string = "" },
            else => .{ .const_int = 0 },
        };
    }

    /// Extract the target type name from an impl block's target_type expression
    fn getImplTargetName(_: *IRGenerator, type_expr: *const ast.TypeExpr) ?[]const u8 {
        return switch (type_expr.kind) {
            .named => |named| if (named.path.segments.len > 0) named.path.segments[0].name else null,
            else => null,
        };
    }

    /// Extract the trait name from a trait type expression (e.g., `Display` from `impl Display for Point`)
    fn extractTraitName(_: *IRGenerator, type_expr: *const ast.TypeExpr) ?[]const u8 {
        return switch (type_expr.kind) {
            .named => |named| if (named.path.segments.len > 0) named.path.segments[0].name else null,
            else => null,
        };
    }

    // ========================================================================
    // Type Mapping
    // ========================================================================

    fn isFloatIRType(ty: *const ir.IRType) bool {
        return ty.* == .f32_type or ty.* == .f64_type;
    }

    /// Map an AST type expression to an IR type
    fn mapTypeExpr(self: *IRGenerator, type_expr: *const ast.TypeExpr) anyerror!*const ir.IRType {
        return switch (type_expr.kind) {
            .named => |named| self.mapNamedType(named),
            .never => self.builder.allocType(.never_type),
            .infer => self.builder.allocType(.i64_type), // default to i64
            .array => |arr| blk: {
                const elem_ty = try self.mapTypeExpr(arr.element_type);
                break :blk self.builder.allocType(.{ .array = .{ .elem = elem_ty, .size = 0 } });
            },
            .option => |opt| blk: {
                const inner = try self.mapTypeExpr(opt.inner_type);
                break :blk self.builder.allocType(.{ .option_type = inner });
            },
            .result => |res| blk: {
                const ok_ty = try self.mapTypeExpr(res.ok_type);
                const err_ty = if (res.err_type) |et| try self.mapTypeExpr(et) else try self.builder.allocType(.string_type);
                break :blk self.builder.allocType(.{ .result_type = .{ .ok = ok_ty, .err = err_ty } });
            },
            .function => |func_ty| blk: {
                var param_types = std.ArrayList(*const ir.IRType).init(self.allocator);
                defer param_types.deinit();
                for (func_ty.params) |p| {
                    try param_types.append(try self.mapTypeExpr(p));
                }
                const ret_ty = try self.mapTypeExpr(func_ty.return_type);
                const params_slice = try self.allocator.dupe(*const ir.IRType, param_types.items);
                break :blk self.builder.allocType(.{ .fn_type = .{ .params = params_slice, .ret = ret_ty } });
            },
            .pointer => self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) }),
            .reference => |ref_ty| blk: {
                const inner = try self.mapTypeExpr(ref_ty.referenced_type);
                break :blk self.builder.allocType(.{ .ptr = inner });
            },
            .slice => |sl| blk: {
                const inner = try self.mapTypeExpr(sl.element_type);
                _ = inner;
                break :blk self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
            },
            .tuple => self.builder.allocType(.void_type),
            .trait_object => self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) }),
            .self_type => blk: {
                // Resolve Self to the current impl target struct type
                if (self.current_impl_type) |impl_name| {
                    if (self.struct_defs.get(impl_name)) |sd| {
                        break :blk sd.ir_type;
                    }
                }
                break :blk self.builder.allocType(.i64_type);
            },
        };
    }

    fn mapNamedType(self: *IRGenerator, named: *const ast.NamedType) anyerror!*const ir.IRType {
        if (named.path.segments.len == 1) {
            const name = named.path.segments[0].name;
            // Check type parameter substitutions first (for monomorphization)
            if (self.type_substitutions.get(name)) |substituted| {
                return substituted;
            }
            // Resolve Self to the current impl target struct type
            if (std.mem.eql(u8, name, "Self")) {
                if (self.current_impl_type) |impl_name| {
                    if (self.struct_defs.get(impl_name)) |sd| {
                        return sd.ir_type;
                    }
                }
            }
            // Basic types
            if (std.mem.eql(u8, name, "int")) return self.builder.allocType(.i64_type);
            if (std.mem.eql(u8, name, "i8")) return self.builder.allocType(.i8_type);
            if (std.mem.eql(u8, name, "i16")) return self.builder.allocType(.i16_type);
            if (std.mem.eql(u8, name, "i32")) return self.builder.allocType(.i32_type);
            if (std.mem.eql(u8, name, "i64")) return self.builder.allocType(.i64_type);
            if (std.mem.eql(u8, name, "u8")) return self.builder.allocType(.u8_type);
            if (std.mem.eql(u8, name, "u16")) return self.builder.allocType(.u16_type);
            if (std.mem.eql(u8, name, "u32")) return self.builder.allocType(.u32_type);
            if (std.mem.eql(u8, name, "u64")) return self.builder.allocType(.u64_type);
            if (std.mem.eql(u8, name, "f32")) return self.builder.allocType(.f32_type);
            if (std.mem.eql(u8, name, "f64")) return self.builder.allocType(.f64_type);
            if (std.mem.eql(u8, name, "float")) return self.builder.allocType(.f64_type);
            if (std.mem.eql(u8, name, "bool")) return self.builder.allocType(.bool_type);
            if (std.mem.eql(u8, name, "string")) return self.builder.allocType(.string_type);
            if (std.mem.eql(u8, name, "void")) return self.builder.allocType(.void_type);

            // SIMD vector types
            if (std.mem.eql(u8, name, "f32x4")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .f32_elem, .lanes = 4 } });
            if (std.mem.eql(u8, name, "f32x8")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .f32_elem, .lanes = 8 } });
            if (std.mem.eql(u8, name, "f64x2")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .f64_elem, .lanes = 2 } });
            if (std.mem.eql(u8, name, "f64x4")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .f64_elem, .lanes = 4 } });
            if (std.mem.eql(u8, name, "i32x4")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .i32_elem, .lanes = 4 } });
            if (std.mem.eql(u8, name, "i32x8")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .i32_elem, .lanes = 8 } });
            if (std.mem.eql(u8, name, "i64x2")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .i64_elem, .lanes = 2 } });
            if (std.mem.eql(u8, name, "i64x4")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .i64_elem, .lanes = 4 } });

            // User-defined struct types
            if (self.struct_defs.get(name)) |sd| {
                return sd.ir_type;
            }

            // User-defined enum types
            if (self.enum_defs.get(name)) |ed| {
                return ed.ir_type;
            }

            // Generic types
            if (std.mem.eql(u8, name, "Option")) {
                if (named.generic_args) |args| {
                    if (args.len > 0) {
                        const inner = try self.mapTypeExpr(args[0]);
                        return self.builder.allocType(.{ .option_type = inner });
                    }
                }
                return self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
            }
            // Box[T] is a pointer type (heap-allocated)
            if (std.mem.eql(u8, name, "Box")) {
                return self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
            }
            if (std.mem.eql(u8, name, "Result")) {
                if (named.generic_args) |args| {
                    if (args.len >= 2) {
                        const ok_ty = try self.mapTypeExpr(args[0]);
                        const err_ty = try self.mapTypeExpr(args[1]);
                        return self.builder.allocType(.{ .result_type = .{ .ok = ok_ty, .err = err_ty } });
                    }
                }
                return self.builder.allocType(.i64_type);
            }
            if (std.mem.eql(u8, name, "Future")) {
                if (named.generic_args) |args| {
                    if (args.len > 0) {
                        const inner = try self.mapTypeExpr(args[0]);
                        return self.builder.allocType(.{ .future_type = inner });
                    }
                }
                return self.builder.allocType(.i64_type);
            }
            if (std.mem.eql(u8, name, "List")) {
                // Determine the list element kind to select the right list struct type
                const lek = self.resolveListElemKind(named);
                const list_struct_name: []const u8 = switch (lek) {
                    .int64 => "dm_list_int64",
                    .float64 => "dm_list_double",
                    .string => "dm_list_dm_string",
                    .other => "dm_list_generic",
                };
                // Check if this list struct type is already registered
                if (self.module.struct_defs.get(list_struct_name)) |existing_ty| {
                    return existing_ty;
                }
                // Create and register the list struct type
                const ptr_field_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                const i64_field_ty = try self.builder.allocType(.i64_type);
                const list_fields = try self.allocator.dupe(ir.Field, &.{
                    .{ .name = "data", .ty = ptr_field_ty },
                    .{ .name = "len", .ty = i64_field_ty },
                    .{ .name = "capacity", .ty = i64_field_ty },
                });
                const list_ty = try self.builder.allocType(.{ .struct_type = .{
                    .name = list_struct_name,
                    .fields = list_fields,
                } });
                try self.module.struct_defs.put(list_struct_name, list_ty);
                return list_ty;
            }
            if (std.mem.eql(u8, name, "Map")) {
                return self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
            }
        }
        // Default: treat as opaque pointer for unknown types
        return self.builder.allocType(.i64_type);
    }

    // ========================================================================
    // Top-level Generation
    // ========================================================================

    /// Generate IR from a parsed+checked AST
    pub fn generate(self: *IRGenerator, source_file: *const ast.SourceFile) !void {
        // Store declarations for comptime function resolution
        self.comptime_declarations = source_file.declarations;

        // Phase 0: Register struct and enum definitions
        for (source_file.declarations) |decl| {
            switch (decl.kind) {
                .struct_def => |sd| try self.registerStruct(sd),
                .enum_def => |ed| try self.registerEnum(ed),
                else => {},
            }
        }

        // Phase 0.5: Collect trait impl relationships for dyn dispatch
        for (source_file.declarations) |decl| {
            switch (decl.kind) {
                .impl_block => |impl_blk| {
                    if (impl_blk.trait_type) |trait_type_expr| {
                        // This is `impl TraitName for StructName { ... }`
                        const trait_name = self.extractTraitName(trait_type_expr);
                        const impl_type_name = self.getImplTargetName(impl_blk.target_type);
                        if (trait_name != null and impl_type_name != null) {
                            var method_names = std.ArrayList([]const u8).init(self.allocator);
                            for (impl_blk.items) |item| {
                                switch (item.kind) {
                                    .function => |func_decl| {
                                        try method_names.append(func_decl.name.name);
                                    },
                                    else => {},
                                }
                            }
                            const key = try std.fmt.allocPrint(self.allocator, "{s}:{s}", .{ impl_type_name.?, trait_name.? });
                            try self.trait_impl_methods.put(key, try self.allocator.dupe([]const u8, method_names.items));
                        }
                    }
                },
                else => {},
            }
        }

        // Phase 1: Declare all extern functions needed by builtins
        try self.declareRuntimeFunctions();

        // Phase 2: Prescan and declare all functions (including impl methods)
        for (source_file.declarations) |decl| {
            switch (decl.kind) {
                .function => |func_decl| {
                    try self.declareFunction(func_decl);
                },
                .constant => |const_decl| {
                    try self.generateConstant(const_decl);
                },
                .impl_block => |impl_blk| {
                    const impl_type_name = self.getImplTargetName(impl_blk.target_type);
                    self.current_impl_type = impl_type_name;
                    for (impl_blk.items) |item| {
                        switch (item.kind) {
                            .function => |func_decl| {
                                if (impl_type_name) |tname| {
                                    const mangled = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ tname, func_decl.name.name });
                                    try self.method_struct_map.put(func_decl.name.name, tname);
                                    try self.declareFunctionImpl(func_decl, mangled);
                                } else {
                                    try self.declareFunction(func_decl);
                                }
                            },
                            .constant => |const_decl| try self.generateConstant(const_decl),
                            else => {},
                        }
                    }
                    self.current_impl_type = null;
                },
                else => {},
            }
        }

        // Phase 3: Generate function bodies (including impl methods)
        for (source_file.declarations) |decl| {
            switch (decl.kind) {
                .function => |func_decl| {
                    if (func_decl.body != null and !func_decl.is_extern) {
                        try self.generateFunction(func_decl);
                    }
                },
                .impl_block => |impl_blk| {
                    const impl_type_name = self.getImplTargetName(impl_blk.target_type);
                    self.current_impl_type = impl_type_name;
                    for (impl_blk.items) |item| {
                        switch (item.kind) {
                            .function => |func_decl| {
                                if (func_decl.body != null and !func_decl.is_extern) {
                                    if (impl_type_name) |tname| {
                                        const mangled = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ tname, func_decl.name.name });
                                        try self.generateFunctionWithName(func_decl, mangled);
                                    } else {
                                        try self.generateFunction(func_decl);
                                    }
                                }
                            },
                            else => {},
                        }
                    }
                    self.current_impl_type = null;
                },
                else => {},
            }
        }
    }

    fn registerStruct(self: *IRGenerator, sd: *const ast.StructDecl) !void {
        var fields = std.ArrayList(FieldInfo).init(self.allocator);
        defer fields.deinit();

        var ir_fields = std.ArrayList(ir.Field).init(self.allocator);
        defer ir_fields.deinit();

        for (sd.fields) |field| {
            const ty = try self.mapTypeExpr(field.type_expr);

            // Detect List[T] fields and register them for field access list dispatch
            if (field.type_expr.kind == .named) {
                const named = field.type_expr.kind.named;
                if (named.path.segments.len == 1 and std.mem.eql(u8, named.path.segments[0].name, "List")) {
                    const elem_kind = self.resolveListElemKind(named);
                    const field_key = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ sd.name.name, field.name.name });
                    try self.field_list_elem_kinds.put(field_key, elem_kind);
                    // mapTypeExpr already creates and registers the list struct type

                    // Store element type info for .other kind
                    if (elem_kind == .other) {
                        try self.storeFieldListElemType(field_key, named);
                    }
                }
            }

            try fields.append(.{ .name = field.name.name, .ty = ty });
            try ir_fields.append(.{ .name = field.name.name, .ty = ty });
        }

        const fields_slice = try self.allocator.dupe(ir.Field, ir_fields.items);
        const ir_type = try self.builder.allocType(.{ .struct_type = .{
            .name = sd.name.name,
            .fields = fields_slice,
        } });

        const info = StructInfo{
            .name = sd.name.name,
            .fields = try self.allocator.dupe(FieldInfo, fields.items),
            .ir_type = ir_type,
        };
        try self.struct_defs.put(sd.name.name, info);
        try self.module.struct_defs.put(sd.name.name, ir_type);
    }

    fn registerEnum(self: *IRGenerator, ed: *const ast.EnumDecl) !void {
        var variants = std.ArrayList(EnumVariantInfo).init(self.allocator);
        defer variants.deinit();

        var ir_fields = std.ArrayList(ir.Field).init(self.allocator);
        defer ir_fields.deinit();

        // First field: tag (i32)
        const i32_ty = try self.builder.allocType(.i32_type);
        try ir_fields.append(.{ .name = "tag", .ty = i32_ty });

        for (ed.variants, 0..) |variant, tag_idx| {
            var payload_types_list = std.ArrayList(*const ir.IRType).init(self.allocator);
            defer payload_types_list.deinit();

            switch (variant.payload) {
                .tuple => |payload_exprs| {
                    for (payload_exprs) |pte| {
                        const pty = try self.mapTypeExpr(pte);
                        try payload_types_list.append(pty);
                    }
                },
                .none, .struct_fields => {},
            }

            const payload_types: ?[]const *const ir.IRType = if (payload_types_list.items.len > 0)
                try self.allocator.dupe(*const ir.IRType, payload_types_list.items)
            else
                null;

            try variants.append(.{
                .name = variant.name.name,
                .tag = @intCast(tag_idx),
                .payload_types = payload_types,
            });

            // Add payload fields to struct type (one field per variant with payload)
            if (payload_types) |pts| {
                for (pts, 0..) |pt, pi| {
                    const field_name = try std.fmt.allocPrint(self.allocator, "{s}_{d}", .{ variant.name.name, pi });
                    try ir_fields.append(.{ .name = field_name, .ty = pt });
                }
            }
        }

        // If no payload fields, add a dummy i64 payload field for uniform layout
        if (ir_fields.items.len == 1) {
            try ir_fields.append(.{ .name = "payload", .ty = try self.builder.allocType(.i64_type) });
        }

        const fields_slice = try self.allocator.dupe(ir.Field, ir_fields.items);
        const ir_type = try self.builder.allocType(.{ .struct_type = .{
            .name = ed.name.name,
            .fields = fields_slice,
        } });

        try self.enum_defs.put(ed.name.name, .{
            .name = ed.name.name,
            .variants = try self.allocator.dupe(EnumVariantInfo, variants.items),
            .ir_type = ir_type,
        });
        try self.module.struct_defs.put(ed.name.name, ir_type);
    }

    fn declareRuntimeFunctions(self: *IRGenerator) !void {
        const string_ty = try self.builder.allocType(.string_type);
        const void_ty = try self.builder.allocType(.void_type);
        const i64_ty = try self.builder.allocType(.i64_type);
        const f64_ty = try self.builder.allocType(.f64_type);
        const bool_ty = try self.builder.allocType(.bool_type);
        const ptr_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });

        // I/O
        try self.declareExtern("dm_println", &.{string_ty}, void_ty);
        try self.declareExtern("dm_print", &.{string_ty}, void_ty);
        try self.declareExtern("dm_eprintln", &.{string_ty}, void_ty);
        try self.declareExtern("dm_eprint", &.{string_ty}, void_ty);

        // Conversion
        try self.declareExtern("dm_int_to_string", &.{i64_ty}, string_ty);
        try self.declareExtern("dm_float_to_string", &.{f64_ty}, string_ty);
        try self.declareExtern("dm_bool_to_string", &.{bool_ty}, string_ty);
        try self.declareExtern("dm_parse_int", &.{string_ty}, i64_ty);
        try self.declareExtern("dm_parse_float", &.{string_ty}, f64_ty);

        // String operations
        try self.declareExtern("dm_string_new", &.{ptr_ty}, string_ty);
        try self.declareExtern("dm_string_concat", &.{ string_ty, string_ty }, string_ty);
        try self.declareExtern("dm_string_eq", &.{ string_ty, string_ty }, bool_ty);
        try self.declareExtern("dm_string_len", &.{string_ty}, i64_ty);
        try self.declareExtern("dm_string_contains", &.{ string_ty, string_ty }, bool_ty);
        try self.declareExtern("dm_string_cmp", &.{ string_ty, string_ty }, i64_ty);
        try self.declareExtern("dm_string_find", &.{ string_ty, string_ty }, i64_ty);
        try self.declareExtern("dm_char_at", &.{ string_ty, i64_ty }, string_ty);
        try self.declareExtern("dm_substr", &.{ string_ty, i64_ty, i64_ty }, string_ty);
        try self.declareExtern("dm_starts_with", &.{ string_ty, string_ty }, bool_ty);
        try self.declareExtern("dm_ends_with", &.{ string_ty, string_ty }, bool_ty);
        try self.declareExtern("dm_string_trim", &.{string_ty}, string_ty);
        try self.declareExtern("dm_string_replace", &.{ string_ty, string_ty, string_ty }, string_ty);
        try self.declareExtern("dm_string_to_upper", &.{string_ty}, string_ty);
        try self.declareExtern("dm_string_to_lower", &.{string_ty}, string_ty);

        // Character classification (takes string, checks first char)
        try self.declareExtern("dm_char_to_string", &.{string_ty}, string_ty);
        try self.declareExtern("dm_is_alpha", &.{string_ty}, bool_ty);
        try self.declareExtern("dm_is_digit", &.{string_ty}, bool_ty);
        try self.declareExtern("dm_is_whitespace", &.{string_ty}, bool_ty);
        try self.declareExtern("dm_is_alnum", &.{string_ty}, bool_ty);

        // List contains
        try self.declareExtern("dm_list_int64_contains", &.{ ptr_ty, i64_ty }, bool_ty);
        try self.declareExtern("dm_list_string_contains", &.{ ptr_ty, string_ty }, bool_ty);

        // Process
        try self.declareExtern("dm_panic", &.{string_ty}, void_ty);
        try self.declareExtern("exit", &.{i64_ty}, void_ty);

        // Stdin
        try self.declareExtern("dm_read_line", &.{}, string_ty);

        // System
        try self.declareExtern("dm_system", &.{string_ty}, i64_ty);
        try self.declareExtern("dm_args_len", &.{}, i64_ty);
        try self.declareExtern("dm_args_get", &.{i64_ty}, string_ty);

        // File I/O
        try self.declareExtern("dm_file_read", &.{string_ty}, string_ty);
        try self.declareExtern("dm_file_write", &.{ string_ty, string_ty }, void_ty);
        try self.declareExtern("dm_file_append", &.{ string_ty, string_ty }, void_ty);
        try self.declareExtern("dm_file_exists", &.{string_ty}, bool_ty);

        // Path utilities
        try self.declareExtern("dm_path_dirname", &.{string_ty}, string_ty);
        try self.declareExtern("dm_path_basename", &.{string_ty}, string_ty);
        try self.declareExtern("dm_path_extension", &.{string_ty}, string_ty);
        try self.declareExtern("dm_path_stem", &.{string_ty}, string_ty);
        try self.declareExtern("dm_path_join", &.{ string_ty, string_ty }, string_ty);

        // Memory allocation (for Box)
        try self.declareExtern("malloc", &.{i64_ty}, ptr_ty);
        try self.declareExtern("free", &.{ptr_ty}, void_ty);

        // List operations (int64 lists)
        try self.declareExtern("dm_list_int64_new", &.{ptr_ty}, void_ty);
        try self.declareExtern("dm_list_int64_push", &.{ ptr_ty, i64_ty }, void_ty);
        try self.declareExtern("dm_list_int64_get", &.{ ptr_ty, i64_ty }, i64_ty);
        try self.declareExtern("dm_list_int64_len", &.{ptr_ty}, i64_ty);
        try self.declareExtern("dm_list_int64_pop", &.{ptr_ty}, i64_ty);

        // List operations (string lists)
        try self.declareExtern("dm_list_string_new", &.{ptr_ty}, void_ty);
        try self.declareExtern("dm_list_string_push", &.{ ptr_ty, string_ty }, void_ty);
        try self.declareExtern("dm_list_string_get", &.{ ptr_ty, i64_ty }, string_ty);
        try self.declareExtern("dm_list_string_len", &.{ptr_ty}, i64_ty);

        // List operations (double lists)
        try self.declareExtern("dm_list_double_new", &.{ptr_ty}, void_ty);
        try self.declareExtern("dm_list_double_push", &.{ ptr_ty, f64_ty }, void_ty);
        try self.declareExtern("dm_list_double_get", &.{ ptr_ty, i64_ty }, f64_ty);
        try self.declareExtern("dm_list_double_len", &.{ptr_ty}, i64_ty);

        // List operations (generic — for struct types, uses void* + elem_size)
        try self.declareExtern("dm_list_generic_new", &.{ptr_ty}, void_ty);
        try self.declareExtern("dm_list_generic_push", &.{ ptr_ty, ptr_ty, i64_ty }, void_ty);
        try self.declareExtern("dm_list_generic_get", &.{ ptr_ty, ptr_ty, i64_ty, i64_ty }, void_ty);
        try self.declareExtern("dm_list_generic_len", &.{ptr_ty}, i64_ty);

        // Filesystem operations
        try self.declareExtern("dm_fs_mkdir", &.{string_ty}, i64_ty);
        try self.declareExtern("dm_fs_readdir", &.{string_ty}, string_ty);
        try self.declareExtern("dm_fs_remove", &.{string_ty}, i64_ty);
        try self.declareExtern("dm_fs_rename", &.{ string_ty, string_ty }, i64_ty);
        try self.declareExtern("dm_fs_getcwd", &.{}, string_ty);

        // OS operations
        try self.declareExtern("dm_os_getenv", &.{string_ty}, string_ty);

        // String split (returns list struct via output pointer)
        try self.declareExtern("dm_string_split", &.{ ptr_ty, string_ty, string_ty }, void_ty);

        // Map[string, int] operations
        try self.declareExtern("dm_map_string_int_new", &.{ptr_ty}, void_ty);
        try self.declareExtern("dm_map_string_int_insert", &.{ ptr_ty, string_ty, i64_ty }, void_ty);
        try self.declareExtern("dm_map_string_int_get", &.{ ptr_ty, string_ty }, i64_ty);
        try self.declareExtern("dm_map_string_int_contains", &.{ ptr_ty, string_ty }, bool_ty);
        try self.declareExtern("dm_map_string_int_remove", &.{ ptr_ty, string_ty }, void_ty);
        try self.declareExtern("dm_map_string_int_len", &.{ptr_ty}, i64_ty);
        try self.declareExtern("dm_map_string_int_keys", &.{ ptr_ty, ptr_ty }, void_ty);
        try self.declareExtern("dm_map_string_int_values", &.{ ptr_ty, ptr_ty }, void_ty);

        // Map[int, string] operations
        try self.declareExtern("dm_map_int_string_new", &.{ptr_ty}, void_ty);
        try self.declareExtern("dm_map_int_string_insert", &.{ ptr_ty, i64_ty, string_ty }, void_ty);
        try self.declareExtern("dm_map_int_string_get", &.{ ptr_ty, ptr_ty, i64_ty }, void_ty);
        try self.declareExtern("dm_map_int_string_contains", &.{ ptr_ty, i64_ty }, bool_ty);
        try self.declareExtern("dm_map_int_string_remove", &.{ ptr_ty, i64_ty }, void_ty);
        try self.declareExtern("dm_map_int_string_len", &.{ptr_ty}, i64_ty);
    }

    fn declareExtern(self: *IRGenerator, name: []const u8, param_types: []const *const ir.IRType, ret_type: *const ir.IRType) !void {
        const params = try self.allocator.alloc(ir.Param, param_types.len);
        for (param_types, 0..) |ty, i| {
            params[i] = .{ .name = try std.fmt.allocPrint(self.allocator, "p{}", .{i}), .ty = ty };
        }
        var func = try self.module.addFunction(name, params, ret_type);
        func.is_extern = true;
    }

    fn declareFunction(self: *IRGenerator, func_decl: *const ast.FunctionDecl) !void {
        // Skip generic function declarations — they will be monomorphized at call sites
        if (func_decl.generic_params) |gp| {
            if (gp.len > 0) {
                try self.generic_fn_decls.put(func_decl.name.name, func_decl);
                return;
            }
        }

        try self.declareFunctionImpl(func_decl, func_decl.name.name);
    }

    fn declareFunctionImpl(self: *IRGenerator, func_decl: *const ast.FunctionDecl, name: []const u8) !void {
        var params = std.ArrayList(ir.Param).init(self.allocator);
        defer params.deinit();

        // Track which parameters are mut (passed by reference)
        var has_mut = false;
        var mut_flags = std.ArrayList(bool).init(self.allocator);
        defer mut_flags.deinit();

        for (func_decl.params) |param| {
            var ty = try self.mapTypeExpr(param.type_expr);
            // For `mut` parameters (including `mut self`), use pointer type
            // so mutations affect the caller's variable.
            if (param.is_mut) {
                ty = try self.builder.allocType(.{ .ptr = ty });
                has_mut = true;
                try mut_flags.append(true);
            } else {
                try mut_flags.append(false);
            }
            try params.append(.{ .name = param.name.name, .ty = ty });
        }

        const ret_type = if (func_decl.return_type) |rt|
            try self.mapTypeExpr(rt)
        else
            try self.builder.allocType(.void_type);

        const params_slice = try self.allocator.dupe(ir.Param, params.items);

        const final_name = if (std.mem.eql(u8, name, "main"))
            "main"
        else
            name;

        var func = try self.module.addFunction(final_name, params_slice, ret_type);
        func.is_extern = func_decl.is_extern;
        func.is_async = func_decl.is_async;

        // Track user-declared extern functions (not runtime builtins)
        if (func_decl.is_extern and func_decl.body == null) {
            // Check if this is a known runtime function (starts with "dm_" or is a C stdlib func we declared)
            const is_runtime = std.mem.startsWith(u8, final_name, "dm_") or
                std.mem.eql(u8, final_name, "malloc") or
                std.mem.eql(u8, final_name, "free") or
                std.mem.eql(u8, final_name, "exit");
            if (!is_runtime) {
                try self.user_extern_fns.put(final_name, {});
                try self.module.user_extern_fns.put(final_name, {});
            }
        }

        // Register mut param info for call-site arg passing
        if (has_mut) {
            const flags_slice = try self.allocator.dupe(bool, mut_flags.items);
            try self.mut_param_fns.put(final_name, flags_slice);
        }
    }

    fn generateConstant(self: *IRGenerator, const_decl: *const ast.ConstDecl) !void {
        if (const_decl.value) |value_expr| {
            const init_val = self.evalConstExpr(value_expr);
            const ty = if (const_decl.type_expr) |te|
                try self.mapTypeExpr(te)
            else if (init_val) |v| switch (v) {
                .const_int => try self.builder.allocType(.i64_type),
                .const_float => try self.builder.allocType(.f64_type),
                .const_bool => try self.builder.allocType(.bool_type),
                .const_string => try self.builder.allocType(.string_type),
                else => try self.builder.allocType(.i64_type),
            } else try self.builder.allocType(.i64_type);
            try self.module.addGlobal(.{
                .name = const_decl.name.name,
                .ty = ty,
                .init_value = init_val,
                .is_const = true,
            });
        }
    }

    /// Comptime evaluation context — maintains local variables during evaluation
    const ComptimeEnv = struct {
        vars: std.StringHashMap(ir.Value),
        allocator: Allocator,
        /// Function declarations accessible during comptime (for comptime fn calls)
        fn_decls: ?[]const *ast.Declaration = null,
        /// Set when a return statement is encountered inside a comptime function call
        return_flag: bool = false,
        /// Set when a break statement is encountered inside a loop
        break_flag: bool = false,
        /// Recursion depth counter to prevent stack overflow
        call_depth: u32 = 0,

        fn init(allocator: Allocator) ComptimeEnv {
            return .{ .vars = std.StringHashMap(ir.Value).init(allocator), .allocator = allocator };
        }
        fn deinit(self: *ComptimeEnv) void {
            self.vars.deinit();
        }
        fn get(self: *ComptimeEnv, name: []const u8) ?ir.Value {
            return self.vars.get(name);
        }
        fn put(self: *ComptimeEnv, name: []const u8, val: ir.Value) !void {
            try self.vars.put(name, val);
        }
    };

    fn evalConstExpr(self: *IRGenerator, expr: *const ast.Expr) ?ir.Value {
        var env = ComptimeEnv.init(self.allocator);
        defer env.deinit();
        env.fn_decls = self.comptime_declarations;
        return self.evalExprComptime(expr, &env);
    }

    fn evalExprComptime(self: *IRGenerator, expr: *const ast.Expr, env: *ComptimeEnv) ?ir.Value {
        return switch (expr.kind) {
            .literal => |lit| switch (lit.kind) {
                .int => |int_lit| blk: {
                    const val = std.fmt.parseInt(i64, int_lit.value, 10) catch break :blk null;
                    break :blk ir.Value{ .const_int = val };
                },
                .float => |float_lit| blk: {
                    const val = std.fmt.parseFloat(f64, float_lit.value) catch break :blk null;
                    break :blk ir.Value{ .const_float = val };
                },
                .bool => |b| ir.Value{ .const_bool = b },
                .string => |s| ir.Value{ .const_string = s.value },
                else => null,
            },
            .comptime_expr => |ct| self.evalExprComptime(ct.expr, env),
            .grouped => |inner| self.evalExprComptime(inner, env),
            .identifier => |id| blk: {
                // Check comptime local variables first
                if (env.get(id.name)) |v| break :blk v;
                // Then check module globals
                for (self.module.globals.items) |g| {
                    if (std.mem.eql(u8, g.name, id.name)) {
                        if (g.init_value) |v| break :blk v;
                    }
                }
                break :blk null;
            },
            .binary => |bin| self.evalBinaryComptime(bin, env),
            .unary => |un| blk: {
                const operand = self.evalExprComptime(un.operand, env) orelse break :blk null;
                break :blk switch (un.op) {
                    .neg => switch (operand) {
                        .const_int => |v| ir.Value{ .const_int = -v },
                        .const_float => |v| ir.Value{ .const_float = -v },
                        else => null,
                    },
                    .not => switch (operand) {
                        .const_bool => |v| ir.Value{ .const_bool = !v },
                        else => null,
                    },
                    else => null,
                };
            },
            .block => |block| self.evalBlockComptime(block, env),
            .if_expr => |if_e| self.evalIfComptime(if_e, env),
            .match_expr => |me| self.evalMatchComptime(me, env),
            .function_call => |call| self.evalCallComptime(call, env),
            .index_access => |ia| blk: {
                // Array indexing: arr[i]
                const idx_val = self.evalExprComptime(ia.index, env) orelse break :blk null;
                const idx = if (idx_val == .const_int) idx_val.const_int else break :blk null;
                // Try array element lookup (stored as varname__IDX in env)
                if (ia.object.kind == .identifier) {
                    const arr_name = ia.object.kind.identifier.name;
                    const key = std.fmt.allocPrint(self.allocator, "{s}__{d}", .{ arr_name, idx }) catch break :blk null;
                    if (env.get(key)) |v| break :blk v;
                }
                // Fallback: array literal directly indexed
                if (ia.object.kind == .array_literal) {
                    const arr = ia.object.kind.array_literal;
                    if (arr.kind == .elements) {
                        const elems = arr.kind.elements;
                        if (idx >= 0 and idx < @as(i64, @intCast(elems.len))) {
                            break :blk self.evalExprComptime(elems[@intCast(idx)], env);
                        }
                    }
                }
                // String indexing
                const base = self.evalExprComptime(ia.object, env) orelse break :blk null;
                if (base == .const_string) {
                    const s = base.const_string;
                    if (idx >= 0 and idx < @as(i64, @intCast(s.len))) {
                        break :blk ir.Value{ .const_int = s[@intCast(idx)] };
                    }
                }
                break :blk null;
            },
            .method_call => |mc| blk: {
                // Handle string.len() at comptime
                if (std.mem.eql(u8, mc.method.name, "len")) {
                    const obj = self.evalExprComptime(mc.object, env) orelse break :blk null;
                    if (obj == .const_string) {
                        break :blk ir.Value{ .const_int = @intCast(obj.const_string.len) };
                    }
                }
                break :blk null;
            },
            else => null,
        };
    }

    fn evalBinaryComptime(self: *IRGenerator, bin: *const ast.BinaryExpr, env: *ComptimeEnv) ?ir.Value {
        const lhs = self.evalExprComptime(bin.left, env) orelse return null;
        const rhs = self.evalExprComptime(bin.right, env) orelse return null;
        // Integer arithmetic
        if (lhs == .const_int and rhs == .const_int) {
            const a = lhs.const_int;
            const b = rhs.const_int;
            return switch (bin.op) {
                .add => ir.Value{ .const_int = a + b },
                .sub => ir.Value{ .const_int = a - b },
                .mul => ir.Value{ .const_int = a * b },
                .div => if (b != 0) ir.Value{ .const_int = @divTrunc(a, b) } else null,
                .mod => if (b != 0) ir.Value{ .const_int = @mod(a, b) } else null,
                .eq => ir.Value{ .const_bool = a == b },
                .ne => ir.Value{ .const_bool = a != b },
                .lt => ir.Value{ .const_bool = a < b },
                .le => ir.Value{ .const_bool = a <= b },
                .gt => ir.Value{ .const_bool = a > b },
                .ge => ir.Value{ .const_bool = a >= b },
                else => null,
            };
        }
        // Float arithmetic
        if (lhs == .const_float and rhs == .const_float) {
            const a = lhs.const_float;
            const b = rhs.const_float;
            return switch (bin.op) {
                .add => ir.Value{ .const_float = a + b },
                .sub => ir.Value{ .const_float = a - b },
                .mul => ir.Value{ .const_float = a * b },
                .div => ir.Value{ .const_float = a / b },
                .eq => ir.Value{ .const_bool = a == b },
                .ne => ir.Value{ .const_bool = a != b },
                .lt => ir.Value{ .const_bool = a < b },
                .le => ir.Value{ .const_bool = a <= b },
                .gt => ir.Value{ .const_bool = a > b },
                .ge => ir.Value{ .const_bool = a >= b },
                else => null,
            };
        }
        // Boolean logic
        if (lhs == .const_bool and rhs == .const_bool) {
            const a = lhs.const_bool;
            const b = rhs.const_bool;
            return switch (bin.op) {
                .@"and" => ir.Value{ .const_bool = a and b },
                .@"or" => ir.Value{ .const_bool = a or b },
                .eq => ir.Value{ .const_bool = a == b },
                .ne => ir.Value{ .const_bool = a != b },
                else => null,
            };
        }
        // String concatenation
        if (lhs == .const_string and rhs == .const_string) {
            if (bin.op == .add) {
                const result = std.fmt.allocPrint(self.allocator, "{s}{s}", .{ lhs.const_string, rhs.const_string }) catch return null;
                return ir.Value{ .const_string = result };
            }
        }
        return null;
    }

    fn evalBlockComptime(self: *IRGenerator, block: *const ast.BlockExpr, env: *ComptimeEnv) ?ir.Value {
        for (block.statements) |stmt| {
            switch (stmt.kind) {
                .let_binding => |lb| {
                    const var_name = switch (lb.pattern.kind) {
                        .identifier => |id| id.name.name,
                        else => continue,
                    };
                    if (lb.value) |val_expr| {
                        // Handle array literal: store elements as varname__0, varname__1, etc.
                        if (val_expr.kind == .array_literal) {
                            const arr = val_expr.kind.array_literal;
                            if (arr.kind == .elements) {
                                const elems = arr.kind.elements;
                                for (elems, 0..) |elem, idx| {
                                    const elem_val = self.evalExprComptime(elem, env) orelse continue;
                                    const key = std.fmt.allocPrint(self.allocator, "{s}__{d}", .{ var_name, idx }) catch continue;
                                    env.put(key, elem_val) catch {};
                                }
                                env.put(var_name, .{ .const_int = @intCast(elems.len) }) catch {};
                                continue;
                            }
                        }
                        const val = self.evalExprComptime(val_expr, env) orelse continue;
                        env.put(var_name, val) catch {};
                    }
                },
                .expression => |expr| {
                    const expr_result = self.evalExprComptime(expr, env);
                    if (env.return_flag or env.break_flag) return expr_result;
                },
                .return_stmt => |ret| {
                    env.return_flag = true;
                    if (ret.value) |val_expr| {
                        return self.evalExprComptime(val_expr, env);
                    }
                    return ir.Value{ .const_int = 0 };
                },
                .assignment => |asgn| {
                    const val = self.evalExprComptime(asgn.value, env) orelse continue;
                    if (asgn.target.kind == .identifier) {
                        const name = asgn.target.kind.identifier.name;
                        // Handle compound assignment
                        switch (asgn.op) {
                            .assign => env.put(name, val) catch {},
                            .add_assign => {
                                if (env.get(name)) |cur| {
                                    if (cur == .const_int and val == .const_int) {
                                        env.put(name, .{ .const_int = cur.const_int + val.const_int }) catch {};
                                    }
                                }
                            },
                            .sub_assign => {
                                if (env.get(name)) |cur| {
                                    if (cur == .const_int and val == .const_int) {
                                        env.put(name, .{ .const_int = cur.const_int - val.const_int }) catch {};
                                    }
                                }
                            },
                            .mul_assign => {
                                if (env.get(name)) |cur| {
                                    if (cur == .const_int and val == .const_int) {
                                        env.put(name, .{ .const_int = cur.const_int * val.const_int }) catch {};
                                    }
                                }
                            },
                            .div_assign => {
                                if (env.get(name)) |cur| {
                                    if (cur == .const_int and val == .const_int and val.const_int != 0) {
                                        env.put(name, .{ .const_int = @divTrunc(cur.const_int, val.const_int) }) catch {};
                                    }
                                }
                            },
                            .mod_assign, .bit_and_assign, .bit_or_assign, .bit_xor_assign, .shl_assign, .shr_assign => {},
                        }
                    }
                },
                .while_loop => |wl| {
                    // Comptime while loop
                    var iterations: usize = 0;
                    while (iterations < 100000) : (iterations += 1) {
                        const cond = self.evalExprComptime(wl.condition, env) orelse break;
                        if (cond != .const_bool or !cond.const_bool) break;
                        const body_result = self.evalBlockComptime(wl.body, env);
                        if (env.return_flag) return body_result;
                        if (env.break_flag) {
                            env.break_flag = false; // consume the break
                            break;
                        }
                    }
                },
                .for_loop => |fl| {
                    const loop_var = switch (fl.pattern.kind) {
                        .identifier => |id| id.name.name,
                        else => continue,
                    };
                    if (fl.iterator.kind == .range) {
                        // Range-based for loop
                        const range = fl.iterator.kind.range;
                        const start_expr = range.start orelse continue;
                        const end_expr = range.end orelse continue;
                        const start_val = self.evalExprComptime(start_expr, env) orelse continue;
                        const end_val = self.evalExprComptime(end_expr, env) orelse continue;
                        if (start_val == .const_int and end_val == .const_int) {
                            var i = start_val.const_int;
                            const end_i = end_val.const_int;
                            while (i < end_i) : (i += 1) {
                                env.put(loop_var, .{ .const_int = i }) catch {};
                                const body_result = self.evalBlockComptime(fl.body, env);
                                if (env.return_flag) return body_result;
                            }
                        }
                    } else if (fl.iterator.kind == .array_literal) {
                        // Array literal iteration: for v in [1, 2, 3]
                        const arr = fl.iterator.kind.array_literal;
                        switch (arr.kind) {
                            .elements => |elems| {
                                for (elems) |elem| {
                                    const val = self.evalExprComptime(elem, env) orelse continue;
                                    env.put(loop_var, val) catch {};
                                    const body_result = self.evalBlockComptime(fl.body, env);
                                    if (env.return_flag) return body_result;
                                }
                            },
                            else => {},
                        }
                    } else if (fl.iterator.kind == .identifier) {
                        // Identifier referencing a comptime array (stored as name__0, name__1, etc.)
                        const arr_name = fl.iterator.kind.identifier.name;
                        // Get array length from the stored value
                        if (env.get(arr_name)) |arr_len_val| {
                            if (arr_len_val == .const_int) {
                                const arr_len = arr_len_val.const_int;
                                var idx: i64 = 0;
                                while (idx < arr_len) : (idx += 1) {
                                    const key = std.fmt.allocPrint(self.allocator, "{s}__{d}", .{ arr_name, idx }) catch break;
                                    if (env.get(key)) |elem_val| {
                                        env.put(loop_var, elem_val) catch {};
                                        const body_result = self.evalBlockComptime(fl.body, env);
                                        if (env.return_flag) return body_result;
                                        if (env.break_flag) {
                                            env.break_flag = false;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                .break_stmt => {
                    env.break_flag = true;
                    return null;
                },
                .if_stmt => |if_e| {
                    const if_result = self.evalIfComptime(if_e, env);
                    if (env.return_flag or env.break_flag) return if_result;
                },
                else => {},
            }
        }
        // Return the block result expression
        if (block.result) |result| {
            return self.evalExprComptime(result, env);
        }
        return null;
    }

    fn evalIfComptime(self: *IRGenerator, if_e: *const ast.IfExpr, env: *ComptimeEnv) ?ir.Value {
        const cond = self.evalExprComptime(if_e.condition, env) orelse return null;
        if (cond == .const_bool and cond.const_bool) {
            return self.evalBlockComptime(if_e.then_branch, env);
        } else if (if_e.else_branch) |else_br| {
            return switch (else_br) {
                .else_block => |blk| self.evalBlockComptime(blk, env),
                .else_if => |nested_if| self.evalIfComptime(nested_if, env),
            };
        }
        return null;
    }

    fn evalMatchComptime(self: *IRGenerator, me: *const ast.MatchExpr, env: *ComptimeEnv) ?ir.Value {
        const scrutinee = self.evalExprComptime(me.scrutinee, env) orelse return null;
        for (me.arms) |arm| {
            switch (arm.pattern.kind) {
                .literal => |lit| {
                    const pat_val: ?ir.Value = switch (lit.kind) {
                        .int => |int_lit| blk: {
                            const v = std.fmt.parseInt(i64, int_lit.value, 10) catch break :blk null;
                            break :blk ir.Value{ .const_int = v };
                        },
                        .bool => |b| ir.Value{ .const_bool = b },
                        .string => |s| ir.Value{ .const_string = s.value },
                        else => null,
                    };
                    if (pat_val) |pv| {
                        const matches = (pv == .const_int and scrutinee == .const_int and pv.const_int == scrutinee.const_int) or
                            (pv == .const_bool and scrutinee == .const_bool and pv.const_bool == scrutinee.const_bool);
                        if (matches) {
                            return self.evalMatchArmComptime(arm, env);
                        }
                    }
                },
                .wildcard => return self.evalMatchArmComptime(arm, env),
                .identifier => |ident| {
                    env.put(ident.name.name, scrutinee) catch {};
                    if (arm.guard) |guard| {
                        const guard_val = self.evalExprComptime(guard, env) orelse continue;
                        if (guard_val == .const_bool and guard_val.const_bool) {
                            return self.evalMatchArmComptime(arm, env);
                        }
                    } else {
                        return self.evalMatchArmComptime(arm, env);
                    }
                },
                else => {},
            }
        }
        return null;
    }

    fn evalMatchArmComptime(self: *IRGenerator, arm: *const ast.MatchArm, env: *ComptimeEnv) ?ir.Value {
        return switch (arm.body) {
            .expression => |e| self.evalExprComptime(e, env),
            .block => |b| self.evalBlockComptime(b, env),
        };
    }

    fn evalCallComptime(self: *IRGenerator, call: *const ast.FunctionCall, env: *ComptimeEnv) ?ir.Value {
        const callee_name = switch (call.function.kind) {
            .identifier => |id| id.name,
            else => return null,
        };

        // Built-in comptime functions
        if (std.mem.eql(u8, callee_name, "len")) {
            if (call.args.len > 0) {
                const arg = self.evalExprComptime(call.args[0].value, env) orelse return null;
                if (arg == .const_string) {
                    return ir.Value{ .const_int = @intCast(arg.const_string.len) };
                }
            }
            return null;
        }

        // Recursion depth check to prevent stack overflow
        if (env.call_depth > 200) return null;

        // Look up user-defined function in the source AST
        // Search through declarations that were passed to generate()
        if (env.fn_decls) |decls| {
            for (decls) |decl| {
                if (decl.kind == .function) {
                    const fn_decl = decl.kind.function;
                    if (std.mem.eql(u8, fn_decl.name.name, callee_name)) {
                        // Create a new env with function params bound to arg values
                        var fn_env = ComptimeEnv.init(self.allocator);
                        defer fn_env.deinit();
                        fn_env.fn_decls = env.fn_decls;
                        fn_env.call_depth = env.call_depth + 1;
                        for (fn_decl.params, 0..) |param, i| {
                            if (i < call.args.len) {
                                const arg_val = self.evalExprComptime(call.args[i].value, env) orelse continue;
                                fn_env.put(param.name.name, arg_val) catch {};
                            }
                        }
                        const body = fn_decl.body orelse return null;
                        return switch (body) {
                            .block => |blk| self.evalBlockComptime(blk, &fn_env),
                            .expression => |e| self.evalExprComptime(e, &fn_env),
                        };
                    }
                }
            }
        }
        return null;
    }

    // ========================================================================
    // Function Body Generation
    // ========================================================================

    fn generateFunction(self: *IRGenerator, func_decl: *const ast.FunctionDecl) !void {
        // Skip generic functions — they are monomorphized at call sites
        if (func_decl.generic_params) |gp| {
            if (gp.len > 0) return;
        }

        const name = if (std.mem.eql(u8, func_decl.name.name, "main"))
            "main"
        else
            func_decl.name.name;

        try self.generateFunctionWithName(func_decl, name);
    }

    fn generateFunctionWithName(self: *IRGenerator, func_decl: *const ast.FunctionDecl, name: []const u8) !void {
        var target_func: ?*ir.Function = null;
        for (self.module.functions.items) |*f| {
            if (std.mem.eql(u8, f.name, name)) {
                target_func = f;
                break;
            }
        }
        const func = target_func orelse return;

        self.current_function = func;
        self.current_return_type = func.return_type;
        self.builder.setFunction(func);
        self.variable_map.clearRetainingCapacity();
        self.variable_types.clearRetainingCapacity();

        const entry = try func.addBlock("entry");
        self.builder.setInsertBlock(entry);

        for (func_decl.params, 0..) |param, i| {
            const ty = try self.mapTypeExpr(param.type_expr);
            const is_self_ptr = std.mem.eql(u8, param.name.name, "self") and (param.is_mut or ty.* == .ptr);
            if (is_self_ptr or param.is_mut) {
                // For `mut` parameters (including `mut self`): parameter is already a pointer.
                // Use param_ref directly as the variable — loads/stores go through
                // the caller's memory, so mutations are visible to the caller.
                try self.variable_map.put(param.name.name, .{ .param_ref = @intCast(i) });
                // Store the pointee type for field access and load operations
                const pointee_ty = if (ty.* == .ptr) ty.ptr else ty;
                try self.variable_types.put(param.name.name, pointee_ty);
            } else {
                const alloca = try self.builder.buildAlloca(ty);
                try self.builder.buildStore(alloca, .{ .param_ref = @intCast(i) });
                try self.variable_map.put(param.name.name, alloca);
                try self.variable_types.put(param.name.name, ty);
            }

            // Detect struct, List[T] and Box[T] parameter type annotations
            if (param.type_expr.kind == .named) {
                const named = param.type_expr.kind.named;
                if (named.path.segments.len == 1) {
                    const ptype_name = named.path.segments[0].name;
                    // Track struct-typed parameters for field access dispatch
                    if (self.struct_defs.contains(ptype_name)) {
                        try self.var_struct_types.put(param.name.name, ptype_name);
                    }
                    // Track enum-typed parameters
                    if (self.enum_defs.contains(ptype_name)) {
                        try self.var_enum_types.put(param.name.name, ptype_name);
                    }
                    if (std.mem.eql(u8, ptype_name, "List")) {
                        const elem_kind = self.resolveListElemKind(named);
                        try self.list_elem_kinds.put(param.name.name, elem_kind);
                        if (elem_kind == .other) {
                            try self.storeListElemType(param.name.name, named);
                        }
                    }
                    if (std.mem.eql(u8, ptype_name, "Box")) {
                        // Box[T] parameter: track the inner type
                        if (named.generic_args) |gargs| {
                            if (gargs.len > 0 and gargs[0].kind == .named) {
                                const elem_named = gargs[0].kind.named;
                                if (elem_named.path.segments.len == 1) {
                                    const tname = elem_named.path.segments[0].name;
                                    if (self.struct_defs.get(tname)) |sd| {
                                        try self.box_inner_types.put(param.name.name, sd.ir_type);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        const body = func_decl.body orelse return;
        switch (body) {
            .block => |block| {
                try self.generateBlock(block);

                if (self.builder.current_block) |blk| {
                    if (blk.terminator == null) {
                        if (func.return_type.* == .void_type) {
                            self.builder.buildRetVoid();
                        } else if (std.mem.eql(u8, name, "main")) {
                            self.builder.buildRet(.{ .const_int = 0 });
                        } else {
                            // All control paths returned — this block is unreachable.
                            // Emit a dummy return with default value to satisfy LLVM.
                            self.builder.buildRet(self.defaultValueForType(func.return_type));
                        }
                    }
                }
            },
            .expression => |expr| {
                const val = try self.generateExpr(expr);
                if (func.return_type.* == .void_type) {
                    self.builder.buildRetVoid();
                } else {
                    self.builder.buildRet(val);
                }
            },
        }
    }

    // ========================================================================
    // Statement Generation
    // ========================================================================

    fn generateBlock(self: *IRGenerator, block: *const ast.BlockExpr) anyerror!void {
        for (block.statements) |stmt| {
            try self.generateStatement(stmt);
        }
        if (block.result) |result| {
            // Generate the trailing expression but don't add ret.
            // Only explicit `return` statements generate ret instructions.
            // This prevents spurious returns inside while/for/if bodies.
            _ = try self.generateExpr(result);
        }
    }

    fn generateStatement(self: *IRGenerator, stmt: *const ast.Statement) anyerror!void {
        switch (stmt.kind) {
            .let_binding => |lb| try self.generateLetBinding(lb),
            .return_stmt => |ret| try self.generateReturn(ret),
            .if_stmt => |if_expr| try self.generateIfStatement(if_expr),
            .while_loop => |wl| try self.generateWhileLoop(wl),
            .for_loop => |fl| try self.generateForLoop(fl),
            .loop_stmt => |ls| try self.generateLoopStmt(ls),
            .expression => |expr| {
                _ = try self.generateExpr(expr);
            },
            .assignment => |assign| try self.generateAssignment(assign),
            .break_stmt => try self.generateBreak(),
            .continue_stmt => try self.generateContinue(),
            .match_stmt => |match_expr| {
                _ = try self.generateMatchExpr(match_expr);
            },
            .region_block => |rb| try self.generateBlock(rb.body),
            .discard => |d| {
                _ = try self.generateExpr(d.value);
            },
        }
    }

    fn generateLetBinding(self: *IRGenerator, lb: *const ast.LetBinding) !void {
        const var_name = switch (lb.pattern.kind) {
            .identifier => |id| id.name.name,
            else => return,
        };

        // Detect List[T] type annotation and handle specially
        if (lb.type_annotation) |ta| {
            if (ta.kind == .named) {
                const named = ta.kind.named;
                if (named.path.segments.len == 1 and std.mem.eql(u8, named.path.segments[0].name, "List")) {
                    // This is a List[T] variable — use typed list struct
                    const elem_kind = self.resolveListElemKind(named);
                    try self.list_elem_kinds.put(var_name, elem_kind);
                    if (elem_kind == .other) {
                        try self.storeListElemType(var_name, named);
                    }

                    // List is a struct: { ptr, i64, i64 } — use the type from mapTypeExpr
                    const list_ty = try self.mapTypeExpr(ta);
                    const alloca = try self.builder.buildAlloca(list_ty);

                    // Initialize list: call dm_list_TYPE_new(&list)
                    const new_fn = switch (elem_kind) {
                        .int64 => "dm_list_int64_new",
                        .float64 => "dm_list_double_new",
                        .string => "dm_list_string_new",
                        .other => "dm_list_generic_new",
                    };
                    const new_args = try self.allocator.dupe(ir.Value, &.{alloca});
                    try self.builder.buildCallVoid(new_fn, new_args);

                    try self.variable_map.put(var_name, alloca);
                    try self.variable_types.put(var_name, list_ty);
                    return;
                }

                // Detect Map[K,V] type annotation
                if (named.path.segments.len == 1 and std.mem.eql(u8, named.path.segments[0].name, "Map")) {
                    const map_kind = self.resolveMapKind(named);
                    if (map_kind) |mk| {
                        try self.map_var_kinds.put(var_name, mk);

                        // Map struct: { entries_ptr, i64 len, i64 capacity }
                        const map_struct_name: []const u8 = switch (mk) {
                            .string_int => "dm_map_string_int",
                            .int_string => "dm_map_int_string",
                        };
                        const ptr_field_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                        const i64_field_ty = try self.builder.allocType(.i64_type);
                        const map_fields = try self.allocator.dupe(ir.Field, &.{
                            .{ .name = "entries", .ty = ptr_field_ty },
                            .{ .name = "len", .ty = i64_field_ty },
                            .{ .name = "capacity", .ty = i64_field_ty },
                        });
                        const map_ty = try self.builder.allocType(.{ .struct_type = .{
                            .name = map_struct_name,
                            .fields = map_fields,
                        } });

                        const map_alloca = try self.builder.buildAlloca(map_ty);

                        // Initialize map if value is Map_new()
                        if (lb.value) |val_expr| {
                            if (val_expr.kind == .function_call) {
                                const fc = val_expr.kind.function_call;
                                const cname = switch (fc.function.kind) {
                                    .identifier => |id| id.name,
                                    else => "",
                                };
                                if (std.mem.eql(u8, cname, "Map_new")) {
                                    const new_fn = switch (mk) {
                                        .string_int => "dm_map_string_int_new",
                                        .int_string => "dm_map_int_string_new",
                                    };
                                    const new_args = try self.allocator.dupe(ir.Value, &.{map_alloca});
                                    try self.builder.buildCallVoid(new_fn, new_args);
                                }
                            }
                        }

                        try self.variable_map.put(var_name, map_alloca);
                        try self.variable_types.put(var_name, map_ty);
                        return;
                    }
                }

                // Detect Box[T] type annotation
                if (named.path.segments.len == 1 and std.mem.eql(u8, named.path.segments[0].name, "Box")) {
                    // Box[T] is a pointer type — resolve inner type
                    const inner_ty = blk: {
                        if (named.generic_args) |gargs| {
                            if (gargs.len > 0) {
                                const elem = gargs[0];
                                if (elem.kind == .named) {
                                    const elem_named = elem.kind.named;
                                    if (elem_named.path.segments.len == 1) {
                                        const tname = elem_named.path.segments[0].name;
                                        if (self.struct_defs.get(tname)) |sd| {
                                            break :blk sd.ir_type;
                                        }
                                        // Primitive types
                                        if (std.mem.eql(u8, tname, "int") or std.mem.eql(u8, tname, "i64"))
                                            break :blk try self.builder.allocType(.i64_type);
                                        if (std.mem.eql(u8, tname, "float") or std.mem.eql(u8, tname, "f64"))
                                            break :blk try self.builder.allocType(.f64_type);
                                        if (std.mem.eql(u8, tname, "string") or std.mem.eql(u8, tname, "String"))
                                            break :blk try self.builder.allocType(.string_type);
                                        if (std.mem.eql(u8, tname, "bool"))
                                            break :blk try self.builder.allocType(.bool_type);
                                    }
                                }
                            }
                        }
                        break :blk try self.builder.allocType(.i64_type);
                    };
                    try self.box_inner_types.put(var_name, inner_ty);

                    // Box is stored as a pointer (ptr to i8)
                    const ptr_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                    const alloca = try self.builder.buildAlloca(ptr_ty);

                    // Generate and store the value
                    if (lb.value) |val_expr| {
                        const val = try self.generateExpr(val_expr);
                        try self.builder.buildStore(alloca, val);
                    }

                    try self.variable_map.put(var_name, alloca);
                    try self.variable_types.put(var_name, ptr_ty);
                    return;
                }
            }

            // Detect dyn Trait type annotation
            if (ta.kind == .trait_object) {
                const trait_obj = ta.kind.trait_object;
                const trait_name = self.extractTraitName(trait_obj.trait_type);
                if (trait_name != null) {
                    // Infer the concrete type from the value expression
                    var concrete_type_name: ?[]const u8 = null;
                    if (lb.value) |val_expr| {
                        if (val_expr.kind == .identifier) {
                            // e.g., `let d1: dyn Display = p` where `p` was declared earlier
                            concrete_type_name = self.var_struct_types.get(val_expr.kind.identifier.name);
                        } else if (val_expr.kind == .struct_literal) {
                            const sl = val_expr.kind.struct_literal;
                            if (sl.type_path) |tp| {
                                if (tp.segments.len > 0) {
                                    concrete_type_name = tp.segments[0].name;
                                }
                            }
                        }
                    }

                    if (concrete_type_name) |ctype| {
                        // Track this variable as a dyn trait object
                        try self.dyn_var_concrete.put(var_name, ctype);
                        try self.dyn_var_traits.put(var_name, trait_name.?);

                        // Get the concrete struct type
                        if (self.struct_defs.get(ctype)) |sd| {
                            // Allocate a fat pointer: { ptr data, ptr vtable }
                            // For simplicity, we store the concrete struct value in a heap-allocated block
                            // and the data pointer in a ptr alloca
                            const ptr_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                            const alloca = try self.builder.buildAlloca(ptr_ty);

                            // Heap-allocate the concrete struct: call malloc(sizeof(struct))
                            const struct_size = estimateTypeSize(sd.ir_type);
                            const malloc_args = try self.allocator.dupe(ir.Value, &.{.{ .const_int = struct_size }});
                            const data_ptr = try self.builder.buildCall("malloc", malloc_args, ptr_ty);

                            // Store the value into the heap-allocated block
                            if (lb.value) |val_expr| {
                                // Generate the concrete value
                                const val = try self.generateExpr(val_expr);
                                // Store the struct value via memcpy-like store:
                                // We need to store the struct value through the data pointer
                                // Cast data_ptr to a pointer to the struct type and store
                                const struct_ptr_ty = try self.builder.allocType(.{ .ptr = sd.ir_type });
                                _ = struct_ptr_ty;
                                try self.builder.buildStore(data_ptr, val);
                            }

                            // Store the data pointer in the alloca
                            try self.builder.buildStore(alloca, data_ptr);

                            try self.variable_map.put(var_name, alloca);
                            try self.variable_types.put(var_name, ptr_ty);
                            return;
                        }
                    }
                }
            }
        }

        // Detect string_split call — result is a List[string]
        if (lb.type_annotation == null) {
            if (lb.value) |val_expr| {
                if (val_expr.kind == .function_call) {
                    const fc = val_expr.kind.function_call;
                    const cname = switch (fc.function.kind) {
                        .identifier => |id| id.name,
                        else => "",
                    };
                    if (std.mem.eql(u8, cname, "string_split")) {
                        // string_split returns a List[string] — allocate list struct and call
                        try self.list_elem_kinds.put(var_name, .string);

                        const list_struct_name = "dm_list_dm_string";
                        const ptr_field_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                        const i64_field_ty = try self.builder.allocType(.i64_type);
                        const list_fields = try self.allocator.dupe(ir.Field, &.{
                            .{ .name = "data", .ty = ptr_field_ty },
                            .{ .name = "len", .ty = i64_field_ty },
                            .{ .name = "capacity", .ty = i64_field_ty },
                        });
                        const list_ty = try self.builder.allocType(.{ .struct_type = .{
                            .name = list_struct_name,
                            .fields = list_fields,
                        } });
                        try self.module.struct_defs.put(list_struct_name, list_ty);

                        const list_alloca = try self.builder.buildAlloca(list_ty);

                        // Generate args: dm_string_split(&out_list, str, delim)
                        var split_args = std.ArrayList(ir.Value).init(self.allocator);
                        defer split_args.deinit();
                        try split_args.append(list_alloca);
                        for (fc.args) |arg| {
                            try split_args.append(try self.generateExpr(arg.value));
                        }
                        const split_args_slice = try self.allocator.dupe(ir.Value, split_args.items);
                        try self.builder.buildCallVoid("dm_string_split", split_args_slice);

                        try self.variable_map.put(var_name, list_alloca);
                        try self.variable_types.put(var_name, list_ty);
                        return;
                    }
                }

                // Detect map method calls that return lists: m.keys(), m.values()
                if (val_expr.kind == .method_call) {
                    const mc_val = val_expr.kind.method_call;
                    const mc_obj_name: ?[]const u8 = if (mc_val.object.kind == .identifier)
                        mc_val.object.kind.identifier.name
                    else
                        null;
                    if (mc_obj_name) |obj_name| {
                        if (self.map_var_kinds.get(obj_name)) |mk| {
                            if (std.mem.eql(u8, mc_val.method.name, "keys")) {
                                // keys() returns a List[K]
                                const elem_kind: ListElemKind = switch (mk) {
                                    .string_int => .string,
                                    .int_string => .int64,
                                };
                                try self.list_elem_kinds.put(var_name, elem_kind);

                                // Generate the map method call — returns alloca to list
                                const map_ptr = self.variable_map.get(obj_name) orelse return;
                                const result_alloca = try self.generateMapMethodCall(mk, map_ptr, "keys", mc_val.args);

                                // Build list type for tracking
                                const list_struct_name: []const u8 = switch (elem_kind) {
                                    .string => "dm_list_dm_string",
                                    .int64 => "dm_list_int64",
                                    else => "dm_list_int64",
                                };
                                const ptr_field_ty2 = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                                const i64_field_ty2 = try self.builder.allocType(.i64_type);
                                const lf = try self.allocator.dupe(ir.Field, &.{
                                    .{ .name = "data", .ty = ptr_field_ty2 },
                                    .{ .name = "len", .ty = i64_field_ty2 },
                                    .{ .name = "capacity", .ty = i64_field_ty2 },
                                });
                                const list_ty2 = try self.builder.allocType(.{ .struct_type = .{
                                    .name = list_struct_name,
                                    .fields = lf,
                                } });
                                try self.module.struct_defs.put(list_struct_name, list_ty2);

                                try self.variable_map.put(var_name, result_alloca);
                                try self.variable_types.put(var_name, list_ty2);
                                return;
                            }
                            if (std.mem.eql(u8, mc_val.method.name, "values")) {
                                const elem_kind: ListElemKind = switch (mk) {
                                    .string_int => .int64,
                                    .int_string => .string,
                                };
                                try self.list_elem_kinds.put(var_name, elem_kind);

                                const map_ptr = self.variable_map.get(obj_name) orelse return;
                                const result_alloca = try self.generateMapMethodCall(mk, map_ptr, "values", mc_val.args);

                                const list_struct_name: []const u8 = switch (elem_kind) {
                                    .int64 => "dm_list_int64",
                                    .string => "dm_list_dm_string",
                                    else => "dm_list_int64",
                                };
                                const ptr_field_ty2 = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                                const i64_field_ty2 = try self.builder.allocType(.i64_type);
                                const lf = try self.allocator.dupe(ir.Field, &.{
                                    .{ .name = "data", .ty = ptr_field_ty2 },
                                    .{ .name = "len", .ty = i64_field_ty2 },
                                    .{ .name = "capacity", .ty = i64_field_ty2 },
                                });
                                const list_ty2 = try self.builder.allocType(.{ .struct_type = .{
                                    .name = list_struct_name,
                                    .fields = lf,
                                } });
                                try self.module.struct_defs.put(list_struct_name, list_ty2);

                                try self.variable_map.put(var_name, result_alloca);
                                try self.variable_types.put(var_name, list_ty2);
                                return;
                            }
                        }
                    }
                }
            }
        }

        const ty = if (lb.type_annotation) |ta|
            try self.mapTypeExpr(ta)
        else if (lb.value) |val_expr|
            try self.inferExprType(val_expr)
        else
            try self.builder.allocType(.i64_type);

        const alloca = try self.builder.buildAlloca(ty);

        if (lb.value) |val_expr| {
            // Handle array literal: store each element in its own alloca (arr__0, arr__1, etc.)
            if (val_expr.kind == .array_literal) {
                const arr = val_expr.kind.array_literal;
                if (arr.kind == .elements) {
                    const elems = arr.kind.elements;
                    for (elems, 0..) |elem, idx| {
                        const elem_val = try self.generateExpr(elem);
                        const elem_ty = try self.inferExprType(elem);
                        const key = try std.fmt.allocPrint(self.allocator, "{s}__{d}", .{ var_name, idx });
                        const elem_alloca = try self.builder.buildAlloca(elem_ty);
                        try self.builder.buildStore(elem_alloca, elem_val);
                        try self.variable_map.put(key, elem_alloca);
                        try self.variable_types.put(key, elem_ty);
                    }
                    // Store the length in the main variable
                    const len_val = ir.Value{ .const_int = @intCast(elems.len) };
                    try self.builder.buildStore(alloca, len_val);
                    try self.variable_map.put(var_name, alloca);
                    try self.variable_types.put(var_name, ty);
                    return;
                }
            }

            const val = try self.generateExpr(val_expr);
            try self.builder.buildStore(alloca, val);
            // Track if this variable holds a function pointer (from lambda or fn-type annotation)
            if (val_expr.kind == .lambda) {
                // The value stored is a global_ref to the lambda function name
                if (val == .global_ref) {
                    try self.fn_ptr_vars.put(var_name, val.global_ref);
                }
            }
            // Track struct type for method dispatch
            if (val_expr.kind == .struct_literal) {
                const sl = val_expr.kind.struct_literal;
                if (sl.type_path) |tp| {
                    if (tp.segments.len > 0) {
                        try self.var_struct_types.put(var_name, tp.segments[0].name);
                    }
                }
            }
            // Propagate struct type when assigning from another struct-typed variable
            // e.g., `let mut cc = c` where c is of type Compiler
            if (val_expr.kind == .identifier) {
                const src_name = val_expr.kind.identifier.name;
                if (self.var_struct_types.get(src_name)) |stype| {
                    try self.var_struct_types.put(var_name, stype);
                }
                // Also propagate list elem kinds
                if (self.list_elem_kinds.get(src_name)) |lek| {
                    try self.list_elem_kinds.put(var_name, lek);
                }
                // Also propagate enum type
                if (self.var_enum_types.get(src_name)) |etype| {
                    try self.var_enum_types.put(var_name, etype);
                }
            }
            // Propagate struct type from function call returns
            // e.g., `let cc = some_fn(...)` where the fn returns a struct type
            if (val_expr.kind == .function_call) {
                // If the return type is a struct, try to determine which struct
                const call_ret_ty = try self.inferExprType(val_expr);
                if (call_ret_ty.* == .struct_type) {
                    // Find the struct name by matching the IR type
                    var sit = self.struct_defs.iterator();
                    while (sit.next()) |entry| {
                        if (entry.value_ptr.ir_type == call_ret_ty) {
                            try self.var_struct_types.put(var_name, entry.key_ptr.*);
                            break;
                        }
                    }
                }
            }
            // Propagate struct/enum type from index_access on lists
            // e.g., `let tok = cc.tokens[i]` where tokens is List[Token]
            if (val_expr.kind == .index_access) {
                const idx_ret_ty = try self.inferExprType(val_expr);
                if (idx_ret_ty.* == .struct_type) {
                    var sit2 = self.struct_defs.iterator();
                    while (sit2.next()) |entry| {
                        if (entry.value_ptr.ir_type == idx_ret_ty) {
                            try self.var_struct_types.put(var_name, entry.key_ptr.*);
                            break;
                        }
                    }
                }
            }
            // Propagate struct/enum type from field_access on structs
            // e.g., `let tok = some_struct.field` where field is a struct type
            if (val_expr.kind == .field_access) {
                const fa_ret_ty = try self.inferExprType(val_expr);
                if (fa_ret_ty.* == .struct_type) {
                    var sit3 = self.struct_defs.iterator();
                    while (sit3.next()) |entry| {
                        if (entry.value_ptr.ir_type == fa_ret_ty) {
                            try self.var_struct_types.put(var_name, entry.key_ptr.*);
                            break;
                        }
                    }
                }
                // Also propagate enum type from field_access
                if (fa_ret_ty.* == .i64_type) {
                    // Check if the field is an enum type by looking at the parent struct's AST
                    const fa_val = val_expr.kind.field_access;
                    if (fa_val.object.kind == .identifier) {
                        const fa_obj_name = fa_val.object.kind.identifier.name;
                        if (self.var_struct_types.get(fa_obj_name)) |parent_struct| {
                            if (self.struct_defs.get(parent_struct)) |sd| {
                                for (sd.fields) |fld| {
                                    if (std.mem.eql(u8, fld.name, fa_val.field.name)) {
                                        // Check if this field type is an enum
                                        if (fld.ty.* == .struct_type) {
                                            if (self.enum_defs.contains(fld.ty.struct_type.name)) {
                                                try self.var_enum_types.put(var_name, fld.ty.struct_type.name);
                                            }
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Track struct/enum type from type annotation
        if (lb.type_annotation) |ta| {
            if (ta.kind == .named) {
                const named = ta.kind.named;
                if (named.path.segments.len == 1) {
                    const type_name = named.path.segments[0].name;
                    if (self.struct_defs.contains(type_name)) {
                        try self.var_struct_types.put(var_name, type_name);
                    }
                    if (self.enum_defs.contains(type_name)) {
                        try self.var_enum_types.put(var_name, type_name);
                    }
                }
            }
        }

        try self.variable_map.put(var_name, alloca);
        try self.variable_types.put(var_name, ty);
    }

    fn resolveListElemKind(self: *IRGenerator, named: *const ast.NamedType) ListElemKind {
        _ = self;
        if (named.generic_args) |args| {
            if (args.len > 0) {
                const elem = args[0];
                if (elem.kind == .named) {
                    const elem_named = elem.kind.named;
                    if (elem_named.path.segments.len == 1) {
                        const name = elem_named.path.segments[0].name;
                        if (std.mem.eql(u8, name, "int") or std.mem.eql(u8, name, "i64")) return .int64;
                        if (std.mem.eql(u8, name, "float") or std.mem.eql(u8, name, "f64")) return .float64;
                        if (std.mem.eql(u8, name, "string") or std.mem.eql(u8, name, "String")) return .string;
                        // Any other named type (struct, enum, List, etc.) is a generic list element
                        return .other;
                    }
                }
            }
        }
        return .int64; // default when no generic args
    }

    /// Store the struct element type for a List[StructName] variable
    fn storeListElemType(self: *IRGenerator, var_name: []const u8, named: *const ast.NamedType) !void {
        if (named.generic_args) |args| {
            if (args.len > 0) {
                const elem = args[0];
                if (elem.kind == .named) {
                    const elem_named = elem.kind.named;
                    if (elem_named.path.segments.len == 1) {
                        const name = elem_named.path.segments[0].name;
                        if (self.struct_defs.get(name)) |info| {
                            try self.list_elem_types.put(var_name, info.ir_type);
                        } else if (std.mem.eql(u8, name, "List")) {
                            // Nested List[T] — element is a dm_list_* struct
                            const inner_kind = self.resolveListElemKind(elem_named);
                            const inner_list_name: []const u8 = switch (inner_kind) {
                                .int64 => "dm_list_int64",
                                .float64 => "dm_list_double",
                                .string => "dm_list_dm_string",
                                .other => "dm_list_generic",
                            };
                            // Create or reuse the inner list struct type
                            const ptr_field_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                            const i64_field_ty = try self.builder.allocType(.i64_type);
                            const list_fields = try self.allocator.dupe(ir.Field, &.{
                                .{ .name = "data", .ty = ptr_field_ty },
                                .{ .name = "len", .ty = i64_field_ty },
                                .{ .name = "capacity", .ty = i64_field_ty },
                            });
                            const inner_list_ty = try self.builder.allocType(.{ .struct_type = .{
                                .name = inner_list_name,
                                .fields = list_fields,
                            } });
                            try self.module.struct_defs.put(inner_list_name, inner_list_ty);
                            try self.list_elem_types.put(var_name, inner_list_ty);
                        }
                    }
                }
            }
        }
    }

    /// Store the struct element type for a List[StructName] field (keyed by "StructName.field_name")
    fn storeFieldListElemType(self: *IRGenerator, field_key: []const u8, named: *const ast.NamedType) !void {
        if (named.generic_args) |args| {
            if (args.len > 0) {
                const elem = args[0];
                if (elem.kind == .named) {
                    const elem_named = elem.kind.named;
                    if (elem_named.path.segments.len == 1) {
                        const name = elem_named.path.segments[0].name;
                        if (self.struct_defs.get(name)) |info| {
                            try self.field_list_elem_types.put(field_key, info.ir_type);
                        } else if (std.mem.eql(u8, name, "List")) {
                            const inner_kind = self.resolveListElemKind(elem_named);
                            const inner_list_name: []const u8 = switch (inner_kind) {
                                .int64 => "dm_list_int64",
                                .float64 => "dm_list_double",
                                .string => "dm_list_dm_string",
                                .other => "dm_list_generic",
                            };
                            const ptr_field_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                            const i64_field_ty = try self.builder.allocType(.i64_type);
                            const list_fields = try self.allocator.dupe(ir.Field, &.{
                                .{ .name = "data", .ty = ptr_field_ty },
                                .{ .name = "len", .ty = i64_field_ty },
                                .{ .name = "capacity", .ty = i64_field_ty },
                            });
                            const inner_list_ty = try self.builder.allocType(.{ .struct_type = .{
                                .name = inner_list_name,
                                .fields = list_fields,
                            } });
                            try self.module.struct_defs.put(inner_list_name, inner_list_ty);
                            try self.field_list_elem_types.put(field_key, inner_list_ty);
                        }
                    }
                }
            }
        }
    }

    /// Determine if a field_access expression accesses a List-typed struct field.
    /// Returns the ListElemKind if so, null otherwise.
    fn inferFieldAccessListKind(self: *IRGenerator, expr: *const ast.Expr) ?ListElemKind {
        if (expr.kind != .field_access) return null;
        const fa = expr.kind.field_access;

        // Get the struct variable name from the object
        const obj_name = if (fa.object.kind == .identifier) fa.object.kind.identifier.name else return null;

        // Look up the struct type for this variable
        const struct_name = self.var_struct_types.get(obj_name) orelse return null;

        // Look up field list elem kind using "StructName.field_name" key
        const field_key = std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ struct_name, fa.field.name }) catch return null;
        return self.field_list_elem_kinds.get(field_key);
    }

    /// Get a temporary alloca containing the list struct extracted from a field access.
    /// Returns the temp alloca value (a pointer to the list struct), the parent struct alloca,
    /// the parent struct type, and the field index for writeback.
    const FieldListInfo = struct {
        temp_alloca: ir.Value,
        parent_alloca: ir.Value,
        parent_ty: *const ir.IRType,
        field_index: u32,
        list_ty: *const ir.IRType,
    };

    fn getFieldListTempAlloca(self: *IRGenerator, fa: *const ast.FieldAccess) !?FieldListInfo {
        const obj_name = if (fa.object.kind == .identifier) fa.object.kind.identifier.name else return null;
        const parent_alloca = self.variable_map.get(obj_name) orelse return null;
        const parent_ty = self.variable_types.get(obj_name) orelse return null;

        if (parent_ty.* != .struct_type) return null;

        // Determine the struct name and list elem kind
        const struct_name = self.var_struct_types.get(obj_name) orelse return null;
        const field_key = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ struct_name, fa.field.name });
        const ek = self.field_list_elem_kinds.get(field_key) orelse return null;

        // Get the actual list struct type (not the generic ptr(i8) used in the parent struct)
        const list_name: []const u8 = switch (ek) {
            .int64 => "dm_list_int64",
            .float64 => "dm_list_double",
            .string => "dm_list_dm_string",
            .other => "dm_list_generic",
        };
        const list_ty = self.module.struct_defs.get(list_name) orelse return null;

        // Find the field index in the parent struct
        for (parent_ty.struct_type.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, fa.field.name)) {
                // Load the parent struct, extract the list field as ptr(i8)
                const parent_val = try self.builder.buildLoad(parent_alloca, parent_ty);
                const field_val = try self.builder.buildExtractField(parent_val, @intCast(i), field.ty);
                // The field is ptr(i8) in the struct, but we need a dm_list_* struct.
                // Create a temp alloca with the proper list struct type and
                // use bitcast semantics: store the ptr value, then treat the alloca as a list struct.
                // Actually, since the list is embedded as ptr(i8) which is 8 bytes,
                // and the list struct is {ptr, i64, i64} = 24 bytes, we can't just reinterpret.
                // Instead, we need to treat the ptr(i8) as a pointer TO the list struct
                // and load through it.
                // Wait - actually, in the C codegen, lists within structs ARE embedded as
                // dm_list_* structs (24 bytes), not as pointers. The IR type is wrong (ptr(i8))
                // but the runtime layout is a 24-byte struct.
                // So we create a temp alloca of the list struct type, store as that type,
                // then the list operations work on it.
                _ = field_val;
                const temp_alloca = try self.builder.buildAlloca(list_ty);
                // Re-load the parent and extract the field as the list struct type
                const parent_val2 = try self.builder.buildLoad(parent_alloca, parent_ty);
                const list_val = try self.builder.buildExtractField(parent_val2, @intCast(i), list_ty);
                try self.builder.buildStore(temp_alloca, list_val);
                return FieldListInfo{
                    .temp_alloca = temp_alloca,
                    .parent_alloca = parent_alloca,
                    .parent_ty = parent_ty,
                    .field_index = @intCast(i),
                    .list_ty = list_ty,
                };
            }
        }
        return null;
    }

    /// Write back the modified list struct from a temp alloca to the parent struct field.
    fn writebackFieldList(self: *IRGenerator, info: FieldListInfo) !void {
        // Load the updated list struct from temp alloca
        const updated_list = try self.builder.buildLoad(info.temp_alloca, info.list_ty);
        // Load the parent struct
        const parent_val = try self.builder.buildLoad(info.parent_alloca, info.parent_ty);
        // Insert the updated list field
        const updated_parent = try self.builder.buildInsertField(parent_val, updated_list, info.field_index, info.parent_ty);
        // Store the updated parent struct back
        try self.builder.buildStore(info.parent_alloca, updated_parent);
    }

    /// Get the field list elem type for .other kind fields
    fn getFieldListElemType(self: *IRGenerator, struct_name: []const u8, field_name: []const u8) ?*const ir.IRType {
        const field_key = std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ struct_name, field_name }) catch return null;
        return self.field_list_elem_types.get(field_key);
    }

    /// Resolve the MapKind from a Map[K,V] type annotation
    fn resolveMapKind(_: *IRGenerator, named: *const ast.NamedType) ?MapKind {
        if (named.generic_args) |args| {
            if (args.len >= 2) {
                const key_arg = args[0];
                const val_arg = args[1];
                const key_name: ?[]const u8 = if (key_arg.kind == .named) blk: {
                    const kn = key_arg.kind.named;
                    break :blk if (kn.path.segments.len == 1) kn.path.segments[0].name else null;
                } else null;
                const val_name: ?[]const u8 = if (val_arg.kind == .named) blk: {
                    const vn = val_arg.kind.named;
                    break :blk if (vn.path.segments.len == 1) vn.path.segments[0].name else null;
                } else null;
                if (key_name) |kn| {
                    if (val_name) |vn| {
                        if ((std.mem.eql(u8, kn, "string")) and (std.mem.eql(u8, vn, "int") or std.mem.eql(u8, vn, "i64"))) {
                            return .string_int;
                        }
                        if ((std.mem.eql(u8, kn, "int") or std.mem.eql(u8, kn, "i64")) and (std.mem.eql(u8, vn, "string"))) {
                            return .int_string;
                        }
                    }
                }
            }
        }
        return null;
    }

    fn estimateTypeSize(ty: *const ir.IRType) i64 {
        return switch (ty.*) {
            .i64_type, .u64_type, .f64_type => 8,
            .i32_type, .u32_type, .f32_type => 4,
            .i16_type, .u16_type => 2,
            .i8_type, .u8_type, .bool_type => 1,
            .ptr => 8,
            .string_type => 24, // dm_string: ptr(8) + len(8) + cap(8)
            .struct_type => |st| blk: {
                var size: i64 = 0;
                for (st.fields) |f| {
                    const field_size = estimateTypeSize(f.ty);
                    // Round up to 8-byte alignment per field for x86-64
                    const aligned_size = if (field_size < 8) @as(i64, 8) else field_size;
                    size += aligned_size;
                }
                break :blk size;
            },
            else => 8,
        };
    }

    fn generateReturn(self: *IRGenerator, ret: *const ast.ReturnStmt) !void {
        if (ret.value) |val_expr| {
            const val = try self.generateExpr(val_expr);
            self.builder.buildRet(val);
        } else {
            self.builder.buildRetVoid();
        }
    }

    fn generateIfStatement(self: *IRGenerator, if_expr: *const ast.IfExpr) !void {
        const cond = try self.generateExpr(if_expr.condition);

        const then_label = try self.nextLabel("then");
        const else_label = try self.nextLabel("else");
        const join_label = try self.nextLabel("if_end");

        const func = self.current_function orelse return;

        if (if_expr.else_branch != null) {
            self.builder.buildCondBr(cond, then_label, else_label);
        } else {
            self.builder.buildCondBr(cond, then_label, join_label);
        }

        const then_block = try func.addBlock(then_label);
        self.builder.setInsertBlock(then_block);
        try self.generateBlock(if_expr.then_branch);
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) {
                self.builder.buildBr(join_label);
            }
        }

        if (if_expr.else_branch) |else_br| {
            const else_block = try func.addBlock(else_label);
            self.builder.setInsertBlock(else_block);
            switch (else_br) {
                .else_block => |block| {
                    try self.generateBlock(block);
                },
                .else_if => |nested_if| {
                    try self.generateIfStatement(nested_if);
                },
            }
            if (self.builder.current_block) |blk| {
                if (blk.terminator == null) {
                    self.builder.buildBr(join_label);
                }
            }
        }

        const join_block = try func.addBlock(join_label);
        self.builder.setInsertBlock(join_block);
    }

    fn generateWhileLoop(self: *IRGenerator, wl: *const ast.WhileLoop) !void {
        const header_label = try self.nextLabel("while_header");
        const body_label = try self.nextLabel("while_body");
        const exit_label = try self.nextLabel("while_exit");

        const func = self.current_function orelse return;

        const prev_break = self.break_target;
        const prev_continue = self.continue_target;
        self.break_target = exit_label;
        self.continue_target = header_label;
        defer {
            self.break_target = prev_break;
            self.continue_target = prev_continue;
        }

        self.builder.buildBr(header_label);

        const header_block = try func.addBlock(header_label);
        self.builder.setInsertBlock(header_block);
        const cond = try self.generateExpr(wl.condition);
        self.builder.buildCondBr(cond, body_label, exit_label);

        const body_block = try func.addBlock(body_label);
        self.builder.setInsertBlock(body_block);
        try self.generateBlock(wl.body);
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) {
                self.builder.buildBr(header_label);
            }
        }

        const exit_block = try func.addBlock(exit_label);
        self.builder.setInsertBlock(exit_block);
    }

    fn generateForLoop(self: *IRGenerator, fl: *const ast.ForLoop) !void {
        const func = self.current_function orelse return;
        const i64_ty = try self.builder.allocType(.i64_type);

        // Check if iterator is a range expression (0..5, 0..=5)
        if (fl.iterator.kind == .range) {
            return self.generateRangeForLoop(fl, fl.iterator.kind.range);
        }

        // Check if iterating over a string (for c in "abc" or for c in str_var)
        const iter_ty = try self.inferExprType(fl.iterator);
        if (iter_ty.* == .string_type) {
            return self.generateStringForLoop(fl);
        }

        // Get the iterator variable name to look up list/map element kind
        const iter_var_name: ?[]const u8 = if (fl.iterator.kind == .identifier)
            fl.iterator.kind.identifier.name
        else
            null;

        // Check if iterating over a map (for k in map iterates over keys)
        if (iter_var_name) |name| {
            if (self.map_var_kinds.get(name)) |mk| {
                return self.generateMapForIn(fl, mk, name);
            }
        }

        // Check if iterating over a struct field that is a list (for item in obj.field)
        const field_list_info: ?FieldListInfo = if (fl.iterator.kind == .field_access) blk: {
            const fek = self.inferFieldAccessListKind(fl.iterator);
            if (fek != null) {
                break :blk try self.getFieldListTempAlloca(fl.iterator.kind.field_access);
            }
            break :blk null;
        } else null;

        const list_kind: ListElemKind = if (field_list_info != null)
            self.inferFieldAccessListKind(fl.iterator) orelse .int64
        else if (iter_var_name) |name|
            self.list_elem_kinds.get(name) orelse .int64
        else
            .int64;

        // Get pointer to the list (alloca, not loaded value)
        const list_ptr = if (field_list_info) |fli|
            fli.temp_alloca
        else if (iter_var_name) |name|
            self.variable_map.get(name) orelse return
        else
            try self.generateExpr(fl.iterator);

        // Get list length: __len = dm_list_TYPE_len(&list)
        const len_fn = switch (list_kind) {
            .int64 => "dm_list_int64_len",
            .float64 => "dm_list_double_len",
            .string => "dm_list_string_len",
            .other => "dm_list_generic_len",
        };
        const len_args = try self.allocator.dupe(ir.Value, &.{list_ptr});
        const list_len = try self.builder.buildCall(len_fn, len_args, i64_ty);

        // Create index variable: __idx = 0
        const idx_alloca = try self.builder.buildAlloca(i64_ty);
        try self.builder.buildStore(idx_alloca, .{ .const_int = 0 });

        // Determine element type
        const elem_ty: *const ir.IRType = switch (list_kind) {
            .int64 => try self.builder.allocType(.i64_type),
            .float64 => try self.builder.allocType(.f64_type),
            .string => try self.builder.allocType(.string_type),
            .other => if (iter_var_name) |name|
                self.list_elem_types.get(name) orelse try self.builder.allocType(.i64_type)
            else if (fl.iterator.kind == .field_access) blk: {
                const fa = fl.iterator.kind.field_access;
                const fa_obj_name = if (fa.object.kind == .identifier) fa.object.kind.identifier.name else break :blk try self.builder.allocType(.i64_type);
                const fa_struct_name = self.var_struct_types.get(fa_obj_name) orelse break :blk try self.builder.allocType(.i64_type);
                break :blk self.getFieldListElemType(fa_struct_name, fa.field.name) orelse try self.builder.allocType(.i64_type);
            } else
                try self.builder.allocType(.i64_type),
        };

        // Create loop element variable alloca
        const elem_alloca = try self.builder.buildAlloca(elem_ty);

        // Get the loop variable name from the pattern
        const var_name = switch (fl.pattern.kind) {
            .identifier => |id| id.name.name,
            else => return,
        };
        try self.variable_map.put(var_name, elem_alloca);
        try self.variable_types.put(var_name, elem_ty);

        // Create blocks: header, body, exit
        const header_label = try self.nextLabel("for_header");
        const body_label = try self.nextLabel("for_body");
        const exit_label = try self.nextLabel("for_exit");

        // Save and set break/continue targets
        const prev_break = self.break_target;
        const prev_continue = self.continue_target;
        self.break_target = exit_label;
        self.continue_target = header_label;
        defer {
            self.break_target = prev_break;
            self.continue_target = prev_continue;
        }

        // Branch to header
        self.builder.buildBr(header_label);

        // Header block: check idx < len
        const header_block = try func.addBlock(header_label);
        self.builder.setInsertBlock(header_block);
        const idx_val = try self.builder.buildLoad(idx_alloca, i64_ty);
        const cond = try self.builder.buildLt(idx_val, list_len);
        self.builder.buildCondBr(cond, body_label, exit_label);

        // Body block: elem = list[idx]; ... body ...; idx++
        const body_block = try func.addBlock(body_label);
        self.builder.setInsertBlock(body_block);

        // Get element: elem = dm_list_TYPE_get(&list, idx)
        const idx_val2 = try self.builder.buildLoad(idx_alloca, i64_ty);
        if (list_kind == .other) {
            // Generic get: dm_list_generic_get(out_ptr, list_ptr, idx, elem_size)
            const elem_size = estimateTypeSize(elem_ty);
            const get_args = try self.allocator.dupe(ir.Value, &.{
                elem_alloca, list_ptr, idx_val2, .{ .const_int = elem_size },
            });
            try self.builder.buildCallVoid("dm_list_generic_get", get_args);
            // elem_alloca is already populated by the get function via output pointer
        } else {
            const get_fn = switch (list_kind) {
                .int64 => "dm_list_int64_get",
                .float64 => "dm_list_double_get",
                .string => "dm_list_string_get",
                .other => unreachable,
            };
            const get_args = try self.allocator.dupe(ir.Value, &.{ list_ptr, idx_val2 });
            const elem_val = try self.builder.buildCall(get_fn, get_args, elem_ty);
            try self.builder.buildStore(elem_alloca, elem_val);
        }

        // Generate loop body
        try self.generateBlock(fl.body);

        // Increment index: idx = idx + 1
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) {
                const idx_val3 = try self.builder.buildLoad(idx_alloca, i64_ty);
                const one = ir.Value{ .const_int = 1 };
                const next_idx = try self.builder.buildAdd(idx_val3, one, i64_ty);
                try self.builder.buildStore(idx_alloca, next_idx);
                self.builder.buildBr(header_label);
            }
        }

        // Exit block
        const exit_block = try func.addBlock(exit_label);
        self.builder.setInsertBlock(exit_block);
    }

    /// Generate for-in loop over a map (iterates over keys via keys() then list iteration)
    fn generateMapForIn(self: *IRGenerator, fl: *const ast.ForLoop, mk: MapKind, map_var_name: []const u8) !void {
        const func = self.current_function orelse return;
        const i64_ty = try self.builder.allocType(.i64_type);
        const map_ptr = self.variable_map.get(map_var_name) orelse return;

        // First, generate a temporary keys list by calling map_keys
        const key_elem_kind: ListElemKind = switch (mk) {
            .string_int => .string,
            .int_string => .int64,
        };
        const key_elem_ty: *const ir.IRType = switch (key_elem_kind) {
            .string => try self.builder.allocType(.string_type),
            .int64 => try self.builder.allocType(.i64_type),
            else => try self.builder.allocType(.i64_type),
        };

        // Allocate keys list struct
        const list_struct_name: []const u8 = switch (key_elem_kind) {
            .string => "dm_list_dm_string",
            .int64 => "dm_list_int64",
            else => "dm_list_int64",
        };
        const ptr_field_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
        const keys_fields = try self.allocator.dupe(ir.Field, &.{
            .{ .name = "data", .ty = ptr_field_ty },
            .{ .name = "len", .ty = i64_ty },
            .{ .name = "capacity", .ty = i64_ty },
        });
        const keys_list_ty = try self.builder.allocType(.{ .struct_type = .{
            .name = list_struct_name,
            .fields = keys_fields,
        } });
        const keys_alloca = try self.builder.buildAlloca(keys_list_ty);

        // Call keys function: dm_map_TYPE_keys(&keys_list, &map)
        if (mk == .string_int) {
            const call_args = try self.allocator.dupe(ir.Value, &.{ keys_alloca, map_ptr });
            try self.builder.buildCallVoid("dm_map_string_int_keys", call_args);
        }

        // Get keys list length
        const len_fn: []const u8 = switch (key_elem_kind) {
            .string => "dm_list_string_len",
            .int64 => "dm_list_int64_len",
            else => "dm_list_int64_len",
        };
        const len_args = try self.allocator.dupe(ir.Value, &.{keys_alloca});
        const keys_len = try self.builder.buildCall(len_fn, len_args, i64_ty);

        // Create index variable
        const idx_alloca = try self.builder.buildAlloca(i64_ty);
        try self.builder.buildStore(idx_alloca, .{ .const_int = 0 });

        // Create loop element variable
        const elem_alloca = try self.builder.buildAlloca(key_elem_ty);
        const var_name = switch (fl.pattern.kind) {
            .identifier => |id| id.name.name,
            else => return,
        };
        try self.variable_map.put(var_name, elem_alloca);
        try self.variable_types.put(var_name, key_elem_ty);

        // Create blocks
        const header_label = try self.nextLabel("mapfor_header");
        const body_label = try self.nextLabel("mapfor_body");
        const exit_label = try self.nextLabel("mapfor_exit");

        const prev_break = self.break_target;
        const prev_continue = self.continue_target;
        self.break_target = exit_label;
        self.continue_target = header_label;
        defer {
            self.break_target = prev_break;
            self.continue_target = prev_continue;
        }

        self.builder.buildBr(header_label);

        // Header: check idx < keys_len
        const header_block = try func.addBlock(header_label);
        self.builder.setInsertBlock(header_block);
        const idx_val = try self.builder.buildLoad(idx_alloca, i64_ty);
        const cond = try self.builder.buildLt(idx_val, keys_len);
        self.builder.buildCondBr(cond, body_label, exit_label);

        // Body: get key at idx, run body, increment
        const body_block = try func.addBlock(body_label);
        self.builder.setInsertBlock(body_block);

        const idx_val2 = try self.builder.buildLoad(idx_alloca, i64_ty);
        const get_fn: []const u8 = switch (key_elem_kind) {
            .string => "dm_list_string_get",
            .int64 => "dm_list_int64_get",
            else => "dm_list_int64_get",
        };
        const get_args = try self.allocator.dupe(ir.Value, &.{ keys_alloca, idx_val2 });
        const elem_val = try self.builder.buildCall(get_fn, get_args, key_elem_ty);
        try self.builder.buildStore(elem_alloca, elem_val);

        // Generate loop body
        try self.generateBlock(fl.body);

        // Increment index
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) {
                const idx_val3 = try self.builder.buildLoad(idx_alloca, i64_ty);
                const one = ir.Value{ .const_int = 1 };
                const next_idx = try self.builder.buildAdd(idx_val3, one, i64_ty);
                try self.builder.buildStore(idx_alloca, next_idx);
                self.builder.buildBr(header_label);
            }
        }

        const exit_block = try func.addBlock(exit_label);
        self.builder.setInsertBlock(exit_block);
    }

    fn generateRangeForLoop(self: *IRGenerator, fl: *const ast.ForLoop, range: *const ast.RangeExpr) !void {
        const func = self.current_function orelse return;
        const i64_ty = try self.builder.allocType(.i64_type);

        // Generate start and end values
        const start_val = if (range.start) |s| try self.generateExpr(s) else ir.Value{ .const_int = 0 };
        const end_val = if (range.end) |e| try self.generateExpr(e) else ir.Value{ .const_int = 0 };

        // Create loop variable alloca
        const idx_alloca = try self.builder.buildAlloca(i64_ty);
        try self.builder.buildStore(idx_alloca, start_val);

        // Bind loop variable name
        const var_name = switch (fl.pattern.kind) {
            .identifier => |id| id.name.name,
            else => return,
        };
        try self.variable_map.put(var_name, idx_alloca);
        try self.variable_types.put(var_name, i64_ty);

        // Create blocks
        const header_label = try self.nextLabel("range_header");
        const body_label = try self.nextLabel("range_body");
        const exit_label = try self.nextLabel("range_exit");

        // Save break/continue targets
        const prev_break = self.break_target;
        const prev_continue = self.continue_target;
        self.break_target = exit_label;
        self.continue_target = header_label;
        defer {
            self.break_target = prev_break;
            self.continue_target = prev_continue;
        }

        self.builder.buildBr(header_label);

        // Header: check i < end (or i <= end for inclusive)
        const header_block = try func.addBlock(header_label);
        self.builder.setInsertBlock(header_block);
        const idx_val = try self.builder.buildLoad(idx_alloca, i64_ty);
        const cond = if (range.inclusive)
            try self.builder.buildLe(idx_val, end_val)
        else
            try self.builder.buildLt(idx_val, end_val);
        self.builder.buildCondBr(cond, body_label, exit_label);

        // Body
        const body_block = try func.addBlock(body_label);
        self.builder.setInsertBlock(body_block);
        try self.generateBlock(fl.body);

        // Increment
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) {
                const idx_val2 = try self.builder.buildLoad(idx_alloca, i64_ty);
                const next = try self.builder.buildAdd(idx_val2, .{ .const_int = 1 }, i64_ty);
                try self.builder.buildStore(idx_alloca, next);
                self.builder.buildBr(header_label);
            }
        }

        // Exit
        const exit_block = try func.addBlock(exit_label);
        self.builder.setInsertBlock(exit_block);
    }

    /// Generate for-in loop over a string: `for c in str { ... }`
    /// Iterates character-by-character using dm_string_len and dm_char_at
    fn generateStringForLoop(self: *IRGenerator, fl: *const ast.ForLoop) !void {
        const func = self.current_function orelse return;
        const i64_ty = try self.builder.allocType(.i64_type);
        const string_ty = try self.builder.allocType(.string_type);

        // Generate the string value
        const str_val = try self.generateExpr(fl.iterator);

        // Get string length
        const len_args = try self.allocator.dupe(ir.Value, &.{str_val});
        const str_len = try self.builder.buildCall("dm_string_len", len_args, i64_ty);

        // Create index variable
        const idx_alloca = try self.builder.buildAlloca(i64_ty);
        try self.builder.buildStore(idx_alloca, .{ .const_int = 0 });

        // Create loop element variable (string type - single char string)
        const elem_alloca = try self.builder.buildAlloca(string_ty);

        // Bind loop variable
        const var_name = switch (fl.pattern.kind) {
            .identifier => |id| id.name.name,
            else => return,
        };
        try self.variable_map.put(var_name, elem_alloca);
        try self.variable_types.put(var_name, string_ty);

        // Create blocks
        const header_label = try self.nextLabel("strfor_header");
        const body_label = try self.nextLabel("strfor_body");
        const exit_label = try self.nextLabel("strfor_exit");

        const prev_break = self.break_target;
        const prev_continue = self.continue_target;
        self.break_target = exit_label;
        self.continue_target = header_label;
        defer {
            self.break_target = prev_break;
            self.continue_target = prev_continue;
        }

        self.builder.buildBr(header_label);

        // Header: check idx < len
        const header_block = try func.addBlock(header_label);
        self.builder.setInsertBlock(header_block);
        const idx_val = try self.builder.buildLoad(idx_alloca, i64_ty);
        const cond = try self.builder.buildLt(idx_val, str_len);
        self.builder.buildCondBr(cond, body_label, exit_label);

        // Body: get char at index
        const body_block = try func.addBlock(body_label);
        self.builder.setInsertBlock(body_block);
        const idx_val2 = try self.builder.buildLoad(idx_alloca, i64_ty);
        const char_args = try self.allocator.dupe(ir.Value, &.{ str_val, idx_val2 });
        const char_val = try self.builder.buildCall("dm_char_at", char_args, string_ty);
        try self.builder.buildStore(elem_alloca, char_val);

        // Generate body
        try self.generateBlock(fl.body);

        // Increment index
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) {
                const idx_val3 = try self.builder.buildLoad(idx_alloca, i64_ty);
                const next = try self.builder.buildAdd(idx_val3, .{ .const_int = 1 }, i64_ty);
                try self.builder.buildStore(idx_alloca, next);
                self.builder.buildBr(header_label);
            }
        }

        // Exit
        const exit_block = try func.addBlock(exit_label);
        self.builder.setInsertBlock(exit_block);
    }

    fn generateLoopStmt(self: *IRGenerator, ls: *const ast.LoopStmt) !void {
        const body_label = try self.nextLabel("loop_body");
        const exit_label = try self.nextLabel("loop_exit");

        const func = self.current_function orelse return;

        const prev_break = self.break_target;
        const prev_continue = self.continue_target;
        self.break_target = exit_label;
        self.continue_target = body_label;
        defer {
            self.break_target = prev_break;
            self.continue_target = prev_continue;
        }

        self.builder.buildBr(body_label);

        const body_block = try func.addBlock(body_label);
        self.builder.setInsertBlock(body_block);
        try self.generateBlock(ls.body);
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) {
                self.builder.buildBr(body_label);
            }
        }

        const exit_block = try func.addBlock(exit_label);
        self.builder.setInsertBlock(exit_block);
    }

    fn generateBreak(self: *IRGenerator) !void {
        if (self.break_target) |target| {
            self.builder.buildBr(target);
        }
    }

    fn generateContinue(self: *IRGenerator) !void {
        if (self.continue_target) |target| {
            self.builder.buildBr(target);
        }
    }

    fn generateAssignment(self: *IRGenerator, assign: *const ast.Assignment) !void {
        const val = try self.generateExpr(assign.value);

        switch (assign.op) {
            .assign => {
                // Handle field access assignment: obj.field = value
                if (assign.target.kind == .field_access) {
                    try self.generateFieldAssign(assign.target.kind.field_access, val);
                    return;
                }
                const ptr = try self.generateLValue(assign.target);
                try self.builder.buildStore(ptr, val);
            },
            .add_assign, .sub_assign, .mul_assign, .div_assign, .mod_assign => {
                const ptr = try self.generateLValue(assign.target);
                const ty = self.inferLValueType(assign.target);
                const old_val = try self.builder.buildLoad(ptr, ty);
                const result = switch (assign.op) {
                    .add_assign => try self.builder.buildAdd(old_val, val, ty),
                    .sub_assign => try self.builder.buildSub(old_val, val, ty),
                    .mul_assign => try self.builder.buildMul(old_val, val, ty),
                    .div_assign => try self.buildSafeDivOrMod(old_val, val, ty, isFloatIRType(ty), true),
                    .mod_assign => try self.buildSafeDivOrMod(old_val, val, ty, isFloatIRType(ty), false),
                    else => unreachable,
                };
                try self.builder.buildStore(ptr, result);
            },
            else => {
                const ptr = try self.generateLValue(assign.target);
                try self.builder.buildStore(ptr, val);
            },
        }
    }

    /// Handle assignment to a struct field: obj.field = value
    fn generateFieldAssign(self: *IRGenerator, fa: *const ast.FieldAccess, new_val: ir.Value) !void {
        // Get the base variable alloca
        if (fa.object.kind == .identifier) {
            const id_name = fa.object.kind.identifier.name;
            const alloca = self.variable_map.get(id_name) orelse return;
            const var_ty = self.variable_types.get(id_name) orelse return;
            if (var_ty.* == .struct_type) {
                // Load current struct, insert new field value, store back
                const current = try self.builder.buildLoad(alloca, var_ty);
                for (var_ty.struct_type.fields, 0..) |field, i| {
                    if (std.mem.eql(u8, field.name, fa.field.name)) {
                        const updated = try self.builder.buildInsertField(current, new_val, @intCast(i), var_ty);
                        try self.builder.buildStore(alloca, updated);
                        return;
                    }
                }
            }
        }
    }

    fn generateLValue(self: *IRGenerator, expr: *const ast.Expr) !ir.Value {
        return switch (expr.kind) {
            .identifier => |id| self.variable_map.get(id.name) orelse error.UndefinedVariable,
            else => error.InvalidLValue,
        };
    }

    fn inferLValueType(self: *IRGenerator, expr: *const ast.Expr) *const ir.IRType {
        return switch (expr.kind) {
            .identifier => |id| self.variable_types.get(id.name) orelse &ir.IRType{ .i64_type = {} },
            else => &ir.IRType{ .i64_type = {} },
        };
    }

    // ========================================================================
    // Expression Generation
    // ========================================================================

    fn generateExpr(self: *IRGenerator, expr: *const ast.Expr) anyerror!ir.Value {
        return switch (expr.kind) {
            .literal => |lit| self.generateLiteral(lit),
            .identifier => |id| self.generateIdentifier(id),
            .binary => |bin| self.generateBinary(bin),
            .unary => |un| self.generateUnary(un),
            .function_call => |call| self.generateCall(call),
            .method_call => |mc| self.generateMethodCall(mc),
            .if_expr => |if_e| self.generateIfExpr(if_e),
            .block => |block| self.generateBlockExpr(block),
            .grouped => |inner| self.generateExpr(inner),
            .field_access => |fa| self.generateFieldAccess(fa),
            .index_access => |ia| self.generateIndexAccess(ia),
            .cast => |ce| self.generateCast(ce),
            .string_interpolation => |si| self.generateStringInterpolation(si),
            .comptime_expr => |ct| self.generateExpr(ct.expr),
            .match_expr => |me| self.generateMatchExpr(me),
            .pipeline => |pe| self.generatePipeline(pe),
            .array_literal => |al| self.generateArrayLiteral(al),
            .struct_literal => |sl| self.generateStructLiteral(sl),
            .lambda => |lam| try self.generateLambda(lam),
            .error_propagate => |ep| self.generateErrorPropagate(ep),
            .await_expr => |ae| self.generateExpr(ae.operand),
            .range => ir.Value{ .undef = {} },
            .coalesce => |ce| self.generateExpr(ce.left),
            .enum_literal => |el| self.generateEnumLiteral(el),
            .tuple_literal => ir.Value{ .undef = {} },
            .type_check => ir.Value{ .const_bool = false },
            .path => |p| self.generatePathExpr(p),
        };
    }

    fn generateLiteral(self: *IRGenerator, lit: *const ast.Literal) !ir.Value {
        return switch (lit.kind) {
            .int => |int_lit| blk: {
                const val = std.fmt.parseInt(i64, int_lit.value, 10) catch 0;
                break :blk ir.Value{ .const_int = val };
            },
            .float => |float_lit| blk: {
                const val = std.fmt.parseFloat(f64, float_lit.value) catch 0.0;
                break :blk ir.Value{ .const_float = val };
            },
            .bool => |b| ir.Value{ .const_bool = b },
            .string => |s| blk: {
                const string_ty = try self.builder.allocType(.string_type);
                const args = try self.allocator.dupe(ir.Value, &.{ir.Value{ .const_string = s.value }});
                break :blk self.builder.buildCall("dm_string_new", args, string_ty);
            },
            .null_lit => ir.Value{ .undef = {} },
            .char => ir.Value{ .const_int = 0 },
        };
    }

    fn generateIdentifier(self: *IRGenerator, id: ast.Identifier) !ir.Value {
        // Handle None as Option constructor (bare identifier, not a call)
        if (std.mem.eql(u8, id.name, "None")) {
            return self.generateOptionNone();
        }
        if (self.variable_map.get(id.name)) |alloca| {
            const ty = self.variable_types.get(id.name) orelse return ir.Value{ .undef = {} };
            return self.builder.buildLoad(alloca, ty);
        }
        for (self.module.globals.items) |g| {
            if (std.mem.eql(u8, g.name, id.name)) {
                if (g.init_value) |v| return v;
            }
        }
        return ir.Value{ .global_ref = id.name };
    }

    fn generateBinary(self: *IRGenerator, bin: *const ast.BinaryExpr) !ir.Value {
        const lhs = try self.generateExpr(bin.left);
        const rhs = try self.generateExpr(bin.right);
        const ty = try self.inferExprType(bin.left);
        const is_float = isFloatIRType(ty);
        const is_string = ty.* == .string_type;

        // Enum comparison: compare tag fields (field 0)
        if (ty.* == .struct_type and ty.struct_type.fields.len > 0 and
            std.mem.eql(u8, ty.struct_type.fields[0].name, "tag"))
        {
            if (bin.op == .eq or bin.op == .ne) {
                const i32_ty = try self.builder.allocType(.i32_type);
                const lhs_tag = try self.builder.buildExtractField(lhs, 0, i32_ty);
                const rhs_tag = try self.builder.buildExtractField(rhs, 0, i32_ty);
                if (bin.op == .eq) return self.builder.buildEq(lhs_tag, rhs_tag);
                return self.builder.buildNe(lhs_tag, rhs_tag);
            }
        }

        // Operator overloading: for struct types, dispatch to the appropriate method
        if (ty.* == .struct_type) {
            const op_method: ?[]const u8 = switch (bin.op) {
                .add => "add",
                .sub => "sub",
                .mul => "mul",
                .div => "div",
                .eq => "eq",
                .ne => "ne",
                .lt => "lt",
                .gt => "gt",
                .le => "le",
                .ge => "ge",
                else => null,
            };
            if (op_method) |method_name| {
                // Find the struct name for this type
                const struct_name: ?[]const u8 = blk: {
                    if (ty.struct_type.name.len > 0) break :blk ty.struct_type.name;
                    // Also try looking up from the left expression's variable
                    if (bin.left.kind == .identifier) {
                        if (self.var_struct_types.get(bin.left.kind.identifier.name)) |sn| break :blk sn;
                    }
                    break :blk null;
                };
                if (struct_name) |sname| {
                    const mangled = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ sname, method_name });
                    // Check if this mangled method exists
                    var method_exists = false;
                    var takes_self_ptr = false;
                    var method_ret_ty: ?*const ir.IRType = null;
                    for (self.module.functions.items) |f| {
                        if (std.mem.eql(u8, f.name, mangled)) {
                            method_exists = true;
                            method_ret_ty = f.return_type;
                            if (f.params.len > 0 and f.params[0].ty.* == .ptr) {
                                takes_self_ptr = true;
                            }
                            break;
                        }
                    }
                    if (method_exists) {
                        const ret_ty = method_ret_ty orelse ty;
                        // Build args: (self_ptr_or_val, rhs)
                        if (takes_self_ptr) {
                            // Need to get the alloca for the left operand
                            const self_ptr: ir.Value = if (bin.left.kind == .identifier)
                                self.variable_map.get(bin.left.kind.identifier.name) orelse lhs
                            else
                                lhs;
                            const args_slice = try self.allocator.dupe(ir.Value, &.{ self_ptr, rhs });
                            return self.builder.buildCall(mangled, args_slice, ret_ty);
                        } else {
                            const args_slice = try self.allocator.dupe(ir.Value, &.{ lhs, rhs });
                            return self.builder.buildCall(mangled, args_slice, ret_ty);
                        }
                    }
                }
            }
        }

        return switch (bin.op) {
            .add => blk: {
                if (is_string) {
                    const concat_args = try self.allocator.dupe(ir.Value, &.{ lhs, rhs });
                    break :blk self.builder.buildCall("dm_string_concat", concat_args, ty);
                }
                break :blk self.builder.buildAdd(lhs, rhs, ty);
            },
            .sub => self.builder.buildSub(lhs, rhs, ty),
            .mul => self.builder.buildMul(lhs, rhs, ty),
            .div => try self.buildSafeDivOrMod(lhs, rhs, ty, is_float, true),
            .mod => try self.buildSafeDivOrMod(lhs, rhs, ty, is_float, false),
            .eq => blk: {
                if (is_string) {
                    const bool_ty = try self.builder.allocType(.bool_type);
                    const eq_args = try self.allocator.dupe(ir.Value, &.{ lhs, rhs });
                    break :blk self.builder.buildCall("dm_string_eq", eq_args, bool_ty);
                }
                if (is_float) {
                    break :blk self.builder.buildEq(lhs, rhs);
                }
                break :blk self.builder.buildEq(lhs, rhs);
            },
            .ne => blk: {
                if (is_string) {
                    // dm_string_eq(a, b), then NOT
                    const bool_ty = try self.builder.allocType(.bool_type);
                    const eq_args = try self.allocator.dupe(ir.Value, &.{ lhs, rhs });
                    const eq_result = try self.builder.buildCall("dm_string_eq", eq_args, bool_ty);
                    break :blk self.builder.addInstruction(.{ .logical_not = .{ .operand = eq_result } }, bool_ty);
                }
                break :blk self.builder.buildNe(lhs, rhs);
            },
            .lt => blk: {
                if (is_string) break :blk try self.buildStringCmp(lhs, rhs, .lt);
                break :blk self.builder.buildLt(lhs, rhs);
            },
            .le => blk: {
                if (is_string) break :blk try self.buildStringCmp(lhs, rhs, .le);
                break :blk self.builder.buildLe(lhs, rhs);
            },
            .gt => blk: {
                if (is_string) break :blk try self.buildStringCmp(lhs, rhs, .gt);
                break :blk self.builder.buildGt(lhs, rhs);
            },
            .ge => blk: {
                if (is_string) break :blk try self.buildStringCmp(lhs, rhs, .ge);
                break :blk self.builder.buildGe(lhs, rhs);
            },
            .@"and" => blk: {
                const bool_ty = try self.builder.allocType(.bool_type);
                break :blk self.builder.addInstruction(.{ .logical_and = .{ .lhs = lhs, .rhs = rhs } }, bool_ty);
            },
            .@"or" => blk: {
                const bool_ty = try self.builder.allocType(.bool_type);
                break :blk self.builder.addInstruction(.{ .logical_or = .{ .lhs = lhs, .rhs = rhs } }, bool_ty);
            },
            .bit_and, .bit_or, .bit_xor, .shl, .shr, .in => ir.Value{ .undef = {} },
        };
    }

    /// Build string comparison via dm_string_cmp
    fn buildStringCmp(self: *IRGenerator, lhs: ir.Value, rhs: ir.Value, op: enum { lt, le, gt, ge }) !ir.Value {
        const i64_ty = try self.builder.allocType(.i64_type);
        const cmp_args = try self.allocator.dupe(ir.Value, &.{ lhs, rhs });
        const cmp_result = try self.builder.buildCall("dm_string_cmp", cmp_args, i64_ty);
        const zero = ir.Value{ .const_int = 0 };
        return switch (op) {
            .lt => self.builder.buildLt(cmp_result, zero),
            .le => self.builder.buildLe(cmp_result, zero),
            .gt => self.builder.buildGt(cmp_result, zero),
            .ge => self.builder.buildGe(cmp_result, zero),
        };
    }

    /// Emit a division or modulo with a runtime zero-check.
    /// If the divisor is zero, calls dm_panic and terminates.
    /// Otherwise, performs the div/mod in a continuation block.
    fn buildSafeDivOrMod(self: *IRGenerator, lhs: ir.Value, rhs: ir.Value, ty: *const ir.IRType, is_float: bool, is_div: bool) !ir.Value {
        const func = self.current_function orelse return error.IRGenFailed;

        // Compare rhs == 0
        const zero: ir.Value = if (is_float) .{ .const_float = 0.0 } else .{ .const_int = 0 };
        const is_zero = try self.builder.buildEq(rhs, zero);

        const op_name: []const u8 = if (is_div) "div" else "mod";
        const panic_label = try self.nextLabel(op_name);
        const ok_label = try self.nextLabel(op_name);

        self.builder.buildCondBr(is_zero, panic_label, ok_label);

        // Panic block: call dm_panic with error message, then unreachable
        const panic_block = try func.addBlock(panic_label);
        self.builder.setInsertBlock(panic_block);
        const string_ty = try self.builder.allocType(.string_type);
        const msg = if (is_div)
            "Runtime error: division by zero"
        else
            "Runtime error: modulo by zero";
        const msg_args = try self.allocator.dupe(ir.Value, &.{ir.Value{ .const_string = msg }});
        const panic_str = try self.builder.buildCall("dm_string_new", msg_args, string_ty);
        const panic_call_args = try self.allocator.dupe(ir.Value, &.{panic_str});
        try self.builder.buildCallVoid("dm_panic", panic_call_args);
        self.builder.current_block.?.terminator = .unreachable_term;

        // OK block: perform the actual division/modulo
        const ok_block = try func.addBlock(ok_label);
        self.builder.setInsertBlock(ok_block);
        if (is_div) {
            return self.builder.buildDiv(lhs, rhs, ty);
        } else {
            return self.builder.buildMod(lhs, rhs, ty);
        }
    }

    fn generateUnary(self: *IRGenerator, un: *const ast.UnaryExpr) !ir.Value {
        const operand = try self.generateExpr(un.operand);
        return switch (un.op) {
            .neg => blk: {
                const ty = try self.inferExprType(un.operand);
                if (isFloatIRType(ty)) {
                    const fzero = ir.Value{ .const_float = 0.0 };
                    break :blk self.builder.buildSub(fzero, operand, ty);
                }
                const zero = ir.Value{ .const_int = 0 };
                break :blk self.builder.buildSub(zero, operand, ty);
            },
            .not => blk: {
                const bool_ty = try self.builder.allocType(.bool_type);
                break :blk self.builder.addInstruction(.{ .logical_not = .{ .operand = operand } }, bool_ty);
            },
            else => ir.Value{ .undef = {} },
        };
    }

    fn generateCall(self: *IRGenerator, call: *const ast.FunctionCall) !ir.Value {
        const callee_name = switch (call.function.kind) {
            .identifier => |id| id.name,
            .path => |p| blk: {
                // Check if this is an enum variant constructor: EnumName.VariantName(payload...)
                if (p.segments.len == 2) {
                    const enum_name = p.segments[0].name;
                    const variant_name = p.segments[1].name;
                    if (self.enum_defs.get(enum_name)) |enum_info| {
                        for (enum_info.variants) |variant| {
                            if (std.mem.eql(u8, variant.name, variant_name)) {
                                return self.generateEnumVariantConstruct(enum_info, variant, call);
                            }
                        }
                    }
                }
                break :blk if (p.segments.len > 0) p.segments[p.segments.len - 1].name else return ir.Value{ .undef = {} };
            },
            else => return ir.Value{ .undef = {} },
        };

        // Handle Box_new and Box_null builtins
        if (std.mem.eql(u8, callee_name, "Box_new")) {
            if (call.args.len > 0) {
                // Box_new(value) → heap allocate, store value, return pointer
                const val = try self.generateExpr(call.args[0].value);
                const inner_ty = try self.inferExprType(call.args[0].value);
                const elem_size = estimateTypeSize(inner_ty);
                const ptr_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                const size_args = try self.allocator.dupe(ir.Value, &.{ir.Value{ .const_int = elem_size }});
                const heap_ptr = try self.builder.buildCall("malloc", size_args, ptr_ty);
                // Store value at heap pointer
                try self.builder.buildStore(heap_ptr, val);
                return heap_ptr;
            }
            return ir.Value{ .undef = {} };
        }
        if (std.mem.eql(u8, callee_name, "Box_null")) {
            // Return null pointer (as const_int 0, will be treated as null ptr in comparisons)
            return ir.Value{ .const_int = 0 };
        }

        // Handle Ok(value) — Result constructor with tag=0
        if (std.mem.eql(u8, callee_name, "Ok")) {
            return self.generateResultOk(call);
        }
        // Handle Err(value) — Result constructor with tag=1
        if (std.mem.eql(u8, callee_name, "Err")) {
            return self.generateResultErr(call);
        }
        // Handle Some(value) — Option constructor with has_value=1
        if (std.mem.eql(u8, callee_name, "Some")) {
            return self.generateOptionSome(call);
        }
        // Handle None — Option constructor with has_value=0
        if (std.mem.eql(u8, callee_name, "None") and call.args.len == 0) {
            return self.generateOptionNone();
        }

        // Handle assert/assert_eq builtins (simplified: just evaluate and continue)
        if (std.mem.eql(u8, callee_name, "assert")) {
            if (call.args.len > 0) {
                _ = try self.generateExpr(call.args[0].value);
            }
            return ir.Value{ .undef = {} };
        }
        if (std.mem.eql(u8, callee_name, "assert_eq")) {
            if (call.args.len >= 2) {
                _ = try self.generateExpr(call.args[0].value);
                _ = try self.generateExpr(call.args[1].value);
            }
            return ir.Value{ .undef = {} };
        }

        // Handle SIMD builtins
        if (std.mem.startsWith(u8, callee_name, "simd_")) {
            return self.generateSimdBuiltin(callee_name, call);
        }

        // Check if this is a generic function call — needs monomorphization
        if (self.generic_fn_decls.get(callee_name)) |generic_decl| {
            return self.generateGenericCall(call, generic_decl);
        }

        // Check if callee is a function pointer variable (lambda/fn param)
        if (self.fn_ptr_vars.contains(callee_name)) {
            return self.generateLambdaCall(callee_name, call);
        }
        if (self.variable_types.get(callee_name)) |var_ty| {
            if (var_ty.* == .fn_type) {
                return self.generateFnPtrCall(callee_name, var_ty, call);
            }
        }

        // Auto-wrap print/println/eprint/eprintln args: non-string args get int_to_string/etc.
        const is_print = std.mem.eql(u8, callee_name, "println") or
            std.mem.eql(u8, callee_name, "print") or
            std.mem.eql(u8, callee_name, "eprintln") or
            std.mem.eql(u8, callee_name, "eprint");

        // Check if callee has mut params (pass by reference)
        const mut_flags = self.mut_param_fns.get(callee_name);

        var args = std.ArrayList(ir.Value).init(self.allocator);
        defer args.deinit();
        for (call.args, 0..) |arg, arg_idx| {
            // For `mut` params, pass the variable's alloca pointer directly
            // instead of loading the value (pass-by-reference semantics).
            const is_mut_param = if (mut_flags) |flags| (arg_idx < flags.len and flags[arg_idx]) else false;
            if (is_mut_param) {
                if (arg.value.kind == .identifier) {
                    const id_name = arg.value.kind.identifier.name;
                    if (self.variable_map.get(id_name)) |alloca_ptr| {
                        try args.append(alloca_ptr);
                        continue;
                    }
                }
            }

            const val = try self.generateExpr(arg.value);
            if (is_print) {
                // Auto-convert non-string args to string
                const arg_type = self.inferExprIRType(arg.value);
                if (arg_type == .i64_type or arg_type == .i32_type or arg_type == .i16_type or arg_type == .i8_type or
                    arg_type == .u64_type or arg_type == .u32_type or arg_type == .u16_type or arg_type == .u8_type)
                {
                    const string_ty = try self.builder.allocType(.string_type);
                    const wrapped_args = try self.allocator.dupe(ir.Value, &.{val});
                    const converted = try self.builder.buildCall("dm_int_to_string", wrapped_args, string_ty);
                    try args.append(converted);
                } else if (arg_type == .f64_type or arg_type == .f32_type) {
                    const string_ty = try self.builder.allocType(.string_type);
                    const wrapped_args = try self.allocator.dupe(ir.Value, &.{val});
                    const converted = try self.builder.buildCall("dm_float_to_string", wrapped_args, string_ty);
                    try args.append(converted);
                } else if (arg_type == .bool_type) {
                    const string_ty = try self.builder.allocType(.string_type);
                    const wrapped_args = try self.allocator.dupe(ir.Value, &.{val});
                    const converted = try self.builder.buildCall("dm_bool_to_string", wrapped_args, string_ty);
                    try args.append(converted);
                } else {
                    try args.append(val);
                }
            } else {
                try args.append(val);
            }
        }

        // Special handling for len() on lists: call list-specific length function
        if (std.mem.eql(u8, callee_name, "len") and call.args.len > 0) {
            if (call.args[0].value.kind == .identifier) {
                const arg_name = call.args[0].value.kind.identifier.name;
                if (self.list_elem_kinds.get(arg_name)) |ek| {
                    const i64_ty = try self.builder.allocType(.i64_type);
                    const list_len_fn: []const u8 = switch (ek) {
                        .int64 => "dm_list_int64_len",
                        .float64 => "dm_list_double_len",
                        .string => "dm_list_string_len",
                        .other => "dm_list_generic_len",
                    };
                    // len() on list takes a pointer to the list struct
                    const list_alloca = self.variable_map.get(arg_name) orelse return ir.Value{ .undef = {} };
                    const len_args = try self.allocator.dupe(ir.Value, &.{list_alloca});
                    return self.builder.buildCall(list_len_fn, len_args, i64_ty);
                }
            }
            // Handle len() on struct field access (e.g., len(p.tokens))
            if (call.args[0].value.kind == .field_access) {
                const field_ek = self.inferFieldAccessListKind(call.args[0].value);
                if (field_ek) |ek| {
                    const field_info = try self.getFieldListTempAlloca(call.args[0].value.kind.field_access) orelse {
                        // Fall through to normal handling
                        const runtime_name2 = self.mapBuiltinName(callee_name);
                        const ret_type2 = try self.inferCallReturnType(callee_name);
                        if (ret_type2.* == .void_type) {
                            const args_slice2 = try self.allocator.dupe(ir.Value, args.items);
                            try self.builder.buildCallVoid(runtime_name2, args_slice2);
                            return ir.Value{ .undef = {} };
                        } else {
                            const args_slice2 = try self.allocator.dupe(ir.Value, args.items);
                            return self.builder.buildCall(runtime_name2, args_slice2, ret_type2);
                        }
                    };
                    const list_len_fn: []const u8 = switch (ek) {
                        .int64 => "dm_list_int64_len",
                        .float64 => "dm_list_double_len",
                        .string => "dm_list_string_len",
                        .other => "dm_list_generic_len",
                    };
                    const fl_i64_ty = try self.builder.allocType(.i64_type);
                    const len_args = try self.allocator.dupe(ir.Value, &.{field_info.temp_alloca});
                    return self.builder.buildCall(list_len_fn, len_args, fl_i64_ty);
                }
            }
        }

        const runtime_name = self.mapBuiltinName(callee_name);
        const ret_type = try self.inferCallReturnType(callee_name);

        if (ret_type.* == .void_type) {
            const args_slice = try self.allocator.dupe(ir.Value, args.items);
            try self.builder.buildCallVoid(runtime_name, args_slice);
            return ir.Value{ .undef = {} };
        } else {
            const args_slice = try self.allocator.dupe(ir.Value, args.items);
            return self.builder.buildCall(runtime_name, args_slice, ret_type);
        }
    }

    fn generateFnPtrCall(self: *IRGenerator, callee_name: []const u8, var_ty: *const ir.IRType, call: *const ast.FunctionCall) !ir.Value {
        // Load the function pointer from the variable
        const alloca = self.variable_map.get(callee_name) orelse return ir.Value{ .undef = {} };
        const fn_ptr_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
        const fn_ptr = try self.builder.buildLoad(alloca, fn_ptr_ty);

        // Generate arguments
        var args = std.ArrayList(ir.Value).init(self.allocator);
        defer args.deinit();
        for (call.args) |arg| {
            try args.append(try self.generateExpr(arg.value));
        }

        const args_slice = try self.allocator.dupe(ir.Value, args.items);
        const ret_type = var_ty.fn_type.ret;

        if (ret_type.* == .void_type) {
            try self.builder.buildCallPtrVoid(fn_ptr, args_slice);
            return ir.Value{ .undef = {} };
        } else {
            return self.builder.buildCallPtr(fn_ptr, args_slice, ret_type);
        }
    }

    /// Generate a direct call to a lambda function, passing captured variables as extra args
    fn generateLambdaCall(self: *IRGenerator, var_name: []const u8, call: *const ast.FunctionCall) !ir.Value {
        const fn_name = self.fn_ptr_vars.get(var_name) orelse return ir.Value{ .undef = {} };

        // Generate explicit arguments
        var args = std.ArrayList(ir.Value).init(self.allocator);
        defer args.deinit();
        for (call.args) |arg| {
            try args.append(try self.generateExpr(arg.value));
        }

        // Add captured variable values as extra arguments
        if (self.lambda_captures.get(fn_name)) |cap_names| {
            for (cap_names) |cap_name| {
                // Load the captured variable's current value from outer scope
                if (self.variable_map.get(cap_name)) |cap_alloca| {
                    const cap_ty = self.variable_types.get(cap_name) orelse try self.builder.allocType(.i64_type);
                    const cap_val = try self.builder.buildLoad(cap_alloca, cap_ty);
                    try args.append(cap_val);
                } else {
                    try args.append(ir.Value{ .const_int = 0 });
                }
            }
        }

        const args_slice = try self.allocator.dupe(ir.Value, args.items);

        // Look up return type
        for (self.module.functions.items) |f| {
            if (std.mem.eql(u8, f.name, fn_name)) {
                if (f.return_type.* == .void_type) {
                    try self.builder.buildCallVoid(fn_name, args_slice);
                    return ir.Value{ .undef = {} };
                }
                return self.builder.buildCall(fn_name, args_slice, f.return_type);
            }
        }

        return ir.Value{ .undef = {} };
    }

    /// Generate a call to a generic function by monomorphizing it
    fn generateGenericCall(self: *IRGenerator, call: *const ast.FunctionCall, generic_decl: *const ast.FunctionDecl) !ir.Value {
        // Determine concrete type arguments
        var type_args = std.ArrayList(*const ir.IRType).init(self.allocator);
        defer type_args.deinit();

        if (call.generic_args) |gargs| {
            // Explicit type arguments: identity[int](42)
            for (gargs) |ga| {
                try type_args.append(try self.mapTypeExpr(ga));
            }
        } else {
            // Implicit: infer from argument types
            for (call.args) |arg| {
                try type_args.append(try self.inferExprType(arg.value));
            }
        }

        // Build mangled name: fn_name + "_" + type suffixes
        var mangled = std.ArrayList(u8).init(self.allocator);
        defer mangled.deinit();
        try mangled.appendSlice(generic_decl.name.name);
        for (type_args.items) |ty| {
            try mangled.append('_');
            try mangled.appendSlice(irTypeSuffix(ty));
        }
        const mangled_name = try self.allocator.dupe(u8, mangled.items);

        // Generate monomorphized copy if not already done
        if (!self.mono_generated.contains(mangled_name)) {
            try self.mono_generated.put(mangled_name, {});

            // Save current generator state
            const saved_function = self.current_function;
            const saved_break = self.break_target;
            const saved_continue = self.continue_target;
            const saved_label_counter = self.label_counter;
            const saved_vars = self.variable_map;
            const saved_types = self.variable_types;
            const saved_list_kinds = self.list_elem_kinds;
            const saved_list_elem_types = self.list_elem_types;
            const saved_builder_func = self.builder.current_function;
            const saved_builder_block = self.builder.current_block;
            const saved_builder_next_id = self.builder.next_id;
            const saved_return_type = self.current_return_type;

            // Create fresh maps for the monomorphized function
            self.variable_map = std.StringHashMap(ir.Value).init(self.allocator);
            self.variable_types = std.StringHashMap(*const ir.IRType).init(self.allocator);
            self.list_elem_kinds = std.StringHashMap(ListElemKind).init(self.allocator);
            self.list_elem_types = std.StringHashMap(*const ir.IRType).init(self.allocator);

            // Save and set type substitutions
            var saved_subs = std.StringHashMap(*const ir.IRType).init(self.allocator);
            defer saved_subs.deinit();
            var sub_iter = self.type_substitutions.iterator();
            while (sub_iter.next()) |entry| {
                try saved_subs.put(entry.key_ptr.*, entry.value_ptr.*);
            }
            self.type_substitutions.clearRetainingCapacity();

            // Map type params to concrete types
            if (generic_decl.generic_params) |gparams| {
                for (gparams, 0..) |gp, i| {
                    if (i < type_args.items.len) {
                        try self.type_substitutions.put(gp.name.name, type_args.items[i]);
                    }
                }
            }

            // Declare and generate the monomorphized function
            try self.declareFunctionImpl(generic_decl, mangled_name);
            if (generic_decl.body != null) {
                try self.generateFunctionWithName(generic_decl, mangled_name);
            }

            // Restore previous type substitutions
            self.type_substitutions.clearRetainingCapacity();
            var restore_iter = saved_subs.iterator();
            while (restore_iter.next()) |entry| {
                try self.type_substitutions.put(entry.key_ptr.*, entry.value_ptr.*);
            }

            // Restore generator state
            self.variable_map.deinit();
            self.variable_types.deinit();
            self.list_elem_kinds.deinit();
            self.list_elem_types.deinit();
            self.variable_map = saved_vars;
            self.variable_types = saved_types;
            self.list_elem_kinds = saved_list_kinds;
            self.list_elem_types = saved_list_elem_types;
            self.current_function = saved_function;
            self.break_target = saved_break;
            self.continue_target = saved_continue;
            self.label_counter = saved_label_counter;
            self.builder.current_function = saved_builder_func;
            self.builder.current_block = saved_builder_block;
            self.builder.next_id = saved_builder_next_id;
            self.current_return_type = saved_return_type;
        }

        // Generate the call to the monomorphized function
        var args = std.ArrayList(ir.Value).init(self.allocator);
        defer args.deinit();
        for (call.args) |arg| {
            try args.append(try self.generateExpr(arg.value));
        }

        const ret_type = try self.inferCallReturnType(mangled_name);
        const args_slice = try self.allocator.dupe(ir.Value, args.items);

        if (ret_type.* == .void_type) {
            try self.builder.buildCallVoid(mangled_name, args_slice);
            return ir.Value{ .undef = {} };
        }
        return self.builder.buildCall(mangled_name, args_slice, ret_type);
    }

    fn mapBuiltinName(self: *IRGenerator, name: []const u8) []const u8 {
        _ = self;
        if (std.mem.eql(u8, name, "println")) return "dm_println";
        if (std.mem.eql(u8, name, "print")) return "dm_print";
        if (std.mem.eql(u8, name, "eprintln")) return "dm_eprintln";
        if (std.mem.eql(u8, name, "eprint")) return "dm_eprint";
        if (std.mem.eql(u8, name, "int_to_string")) return "dm_int_to_string";
        if (std.mem.eql(u8, name, "float_to_string")) return "dm_float_to_string";
        if (std.mem.eql(u8, name, "bool_to_string")) return "dm_bool_to_string";
        if (std.mem.eql(u8, name, "parse_int")) return "dm_parse_int";
        if (std.mem.eql(u8, name, "parse_float")) return "dm_parse_float";
        if (std.mem.eql(u8, name, "panic")) return "dm_panic";
        if (std.mem.eql(u8, name, "len")) return "dm_string_len";
        if (std.mem.eql(u8, name, "string_contains")) return "dm_string_contains";
        if (std.mem.eql(u8, name, "string_find")) return "dm_string_find";
        if (std.mem.eql(u8, name, "char_at")) return "dm_char_at";
        if (std.mem.eql(u8, name, "substr")) return "dm_substr";
        if (std.mem.eql(u8, name, "starts_with")) return "dm_starts_with";
        if (std.mem.eql(u8, name, "ends_with")) return "dm_ends_with";
        if (std.mem.eql(u8, name, "string_trim")) return "dm_string_trim";
        if (std.mem.eql(u8, name, "string_replace")) return "dm_string_replace";
        if (std.mem.eql(u8, name, "string_to_upper")) return "dm_string_to_upper";
        if (std.mem.eql(u8, name, "string_to_lower")) return "dm_string_to_lower";
        if (std.mem.eql(u8, name, "read_line")) return "dm_read_line";
        if (std.mem.eql(u8, name, "system")) return "dm_system";
        if (std.mem.eql(u8, name, "args_len")) return "dm_args_len";
        if (std.mem.eql(u8, name, "args_get")) return "dm_args_get";
        if (std.mem.eql(u8, name, "file_read")) return "dm_file_read";
        if (std.mem.eql(u8, name, "file_write")) return "dm_file_write";
        if (std.mem.eql(u8, name, "file_append")) return "dm_file_append";
        if (std.mem.eql(u8, name, "file_exists")) return "dm_file_exists";
        if (std.mem.eql(u8, name, "exit")) return "exit";
        if (std.mem.eql(u8, name, "string_split")) return "dm_string_split";
        if (std.mem.eql(u8, name, "string_to_int")) return "dm_parse_int";
        // Filesystem
        if (std.mem.eql(u8, name, "fs_mkdir")) return "dm_fs_mkdir";
        if (std.mem.eql(u8, name, "fs_readdir")) return "dm_fs_readdir";
        if (std.mem.eql(u8, name, "fs_remove")) return "dm_fs_remove";
        if (std.mem.eql(u8, name, "fs_rename")) return "dm_fs_rename";
        if (std.mem.eql(u8, name, "fs_getcwd")) return "dm_fs_getcwd";
        // OS
        if (std.mem.eql(u8, name, "env_get")) return "dm_os_getenv";
        // Path utilities
        if (std.mem.eql(u8, name, "path_dirname")) return "dm_path_dirname";
        if (std.mem.eql(u8, name, "path_basename")) return "dm_path_basename";
        if (std.mem.eql(u8, name, "path_extension")) return "dm_path_extension";
        if (std.mem.eql(u8, name, "path_stem")) return "dm_path_stem";
        if (std.mem.eql(u8, name, "path_join")) return "dm_path_join";
        // Character functions
        if (std.mem.eql(u8, name, "char_to_string")) return "dm_char_to_string";
        if (std.mem.eql(u8, name, "is_alpha")) return "dm_is_alpha";
        if (std.mem.eql(u8, name, "is_digit")) return "dm_is_digit";
        if (std.mem.eql(u8, name, "is_whitespace")) return "dm_is_whitespace";
        if (std.mem.eql(u8, name, "is_alnum")) return "dm_is_alnum";
        return name;
    }

    fn inferCallReturnType(self: *IRGenerator, name: []const u8) !*const ir.IRType {
        if (std.mem.eql(u8, name, "println") or
            std.mem.eql(u8, name, "print") or
            std.mem.eql(u8, name, "eprintln") or
            std.mem.eql(u8, name, "eprint") or
            std.mem.eql(u8, name, "panic") or
            std.mem.eql(u8, name, "exit") or
            std.mem.eql(u8, name, "file_write") or
            std.mem.eql(u8, name, "file_append"))
            return self.builder.allocType(.void_type);

        if (std.mem.eql(u8, name, "int_to_string") or
            std.mem.eql(u8, name, "float_to_string") or
            std.mem.eql(u8, name, "bool_to_string") or
            std.mem.eql(u8, name, "read_line") or
            std.mem.eql(u8, name, "char_at") or
            std.mem.eql(u8, name, "substr") or
            std.mem.eql(u8, name, "string_trim") or
            std.mem.eql(u8, name, "string_replace") or
            std.mem.eql(u8, name, "string_to_upper") or
            std.mem.eql(u8, name, "string_to_lower") or
            std.mem.eql(u8, name, "file_read") or
            std.mem.eql(u8, name, "args_get") or
            std.mem.eql(u8, name, "fs_readdir") or
            std.mem.eql(u8, name, "fs_getcwd") or
            std.mem.eql(u8, name, "env_get") or
            std.mem.eql(u8, name, "char_to_string") or
            std.mem.eql(u8, name, "path_dirname") or
            std.mem.eql(u8, name, "path_basename") or
            std.mem.eql(u8, name, "path_extension") or
            std.mem.eql(u8, name, "path_stem") or
            std.mem.eql(u8, name, "path_join"))
            return self.builder.allocType(.string_type);

        if (std.mem.eql(u8, name, "parse_int") or
            std.mem.eql(u8, name, "string_to_int") or
            std.mem.eql(u8, name, "len") or
            std.mem.eql(u8, name, "string_find") or
            std.mem.eql(u8, name, "system") or
            std.mem.eql(u8, name, "args_len") or
            std.mem.eql(u8, name, "fs_mkdir") or
            std.mem.eql(u8, name, "fs_remove") or
            std.mem.eql(u8, name, "fs_rename"))
            return self.builder.allocType(.i64_type);

        if (std.mem.eql(u8, name, "parse_float"))
            return self.builder.allocType(.f64_type);

        if (std.mem.eql(u8, name, "string_contains") or
            std.mem.eql(u8, name, "starts_with") or
            std.mem.eql(u8, name, "ends_with") or
            std.mem.eql(u8, name, "is_alpha") or
            std.mem.eql(u8, name, "is_digit") or
            std.mem.eql(u8, name, "is_whitespace") or
            std.mem.eql(u8, name, "is_alnum") or
            std.mem.eql(u8, name, "file_exists"))
            return self.builder.allocType(.bool_type);

        // Look up user-defined function
        for (self.module.functions.items) |f| {
            if (std.mem.eql(u8, f.name, name)) {
                return f.return_type;
            }
        }

        return self.builder.allocType(.i64_type);
    }

    fn generateSimdBuiltin(self: *IRGenerator, callee_name: []const u8, call: *const ast.FunctionCall) !ir.Value {
        if (std.mem.startsWith(u8, callee_name, "simd_splat_")) {
            const suffix = callee_name["simd_splat_".len..];
            const vec_type = try self.parseSimdTypeName(suffix);
            if (call.args.len < 1) return ir.Value{ .undef = {} };
            const scalar = try self.generateExpr(call.args[0].value);
            return self.builder.addInstruction(.{ .simd_splat = .{ .scalar = scalar, .vec_type = vec_type } }, vec_type);
        }
        if (std.mem.startsWith(u8, callee_name, "simd_set_")) {
            const suffix = callee_name["simd_set_".len..];
            const vec_type = try self.parseSimdTypeName(suffix);
            var elements = std.ArrayList(ir.Value).init(self.allocator);
            defer elements.deinit();
            for (call.args) |arg| {
                try elements.append(try self.generateExpr(arg.value));
            }
            const elems_slice = try self.allocator.dupe(ir.Value, elements.items);
            return self.builder.addInstruction(.{ .simd_set = .{ .elements = elems_slice, .vec_type = vec_type } }, vec_type);
        }
        if (std.mem.eql(u8, callee_name, "simd_add")) {
            if (call.args.len < 2) return ir.Value{ .undef = {} };
            const lhs = try self.generateExpr(call.args[0].value);
            const rhs = try self.generateExpr(call.args[1].value);
            const vec_type = try self.inferSimdVecType(call.args[0].value);
            return self.builder.addInstruction(.{ .simd_add = .{ .lhs = lhs, .rhs = rhs } }, vec_type);
        }
        if (std.mem.eql(u8, callee_name, "simd_sub")) {
            if (call.args.len < 2) return ir.Value{ .undef = {} };
            const lhs = try self.generateExpr(call.args[0].value);
            const rhs = try self.generateExpr(call.args[1].value);
            const vec_type = try self.inferSimdVecType(call.args[0].value);
            return self.builder.addInstruction(.{ .simd_sub = .{ .lhs = lhs, .rhs = rhs } }, vec_type);
        }
        if (std.mem.eql(u8, callee_name, "simd_mul")) {
            if (call.args.len < 2) return ir.Value{ .undef = {} };
            const lhs = try self.generateExpr(call.args[0].value);
            const rhs = try self.generateExpr(call.args[1].value);
            const vec_type = try self.inferSimdVecType(call.args[0].value);
            return self.builder.addInstruction(.{ .simd_mul = .{ .lhs = lhs, .rhs = rhs } }, vec_type);
        }
        if (std.mem.eql(u8, callee_name, "simd_div")) {
            if (call.args.len < 2) return ir.Value{ .undef = {} };
            const lhs = try self.generateExpr(call.args[0].value);
            const rhs = try self.generateExpr(call.args[1].value);
            const vec_type = try self.inferSimdVecType(call.args[0].value);
            return self.builder.addInstruction(.{ .simd_div = .{ .lhs = lhs, .rhs = rhs } }, vec_type);
        }
        if (std.mem.eql(u8, callee_name, "simd_extract")) {
            if (call.args.len < 2) return ir.Value{ .undef = {} };
            const vec = try self.generateExpr(call.args[0].value);
            const idx = try self.generateExpr(call.args[1].value);
            const scalar_type = try self.inferSimdExtractType(call.args[0].value);
            return self.builder.addInstruction(.{ .simd_extract = .{ .vector = vec, .index = idx } }, scalar_type);
        }
        return ir.Value{ .undef = {} };
    }

    fn parseSimdTypeName(self: *IRGenerator, suffix: []const u8) !*const ir.IRType {
        if (std.mem.eql(u8, suffix, "f32x4")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .f32_elem, .lanes = 4 } });
        if (std.mem.eql(u8, suffix, "f32x8")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .f32_elem, .lanes = 8 } });
        if (std.mem.eql(u8, suffix, "f64x2")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .f64_elem, .lanes = 2 } });
        if (std.mem.eql(u8, suffix, "f64x4")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .f64_elem, .lanes = 4 } });
        if (std.mem.eql(u8, suffix, "i32x4")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .i32_elem, .lanes = 4 } });
        if (std.mem.eql(u8, suffix, "i32x8")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .i32_elem, .lanes = 8 } });
        if (std.mem.eql(u8, suffix, "i64x2")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .i64_elem, .lanes = 2 } });
        if (std.mem.eql(u8, suffix, "i64x4")) return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .i64_elem, .lanes = 4 } });
        return self.builder.allocType(.i64_type);
    }

    fn inferSimdVecType(self: *IRGenerator, expr: *const ast.Expr) !*const ir.IRType {
        if (expr.kind == .identifier) {
            const id_name = expr.kind.identifier.name;
            if (self.variable_types.get(id_name)) |ty| {
                if (ty.* == .vector_type) return ty;
            }
        }
        if (expr.kind == .function_call) {
            const fn_call = expr.kind.function_call;
            const fn_name = switch (fn_call.function.kind) {
                .identifier => |id| id.name,
                else => return self.builder.allocType(.i64_type),
            };
            if (std.mem.startsWith(u8, fn_name, "simd_splat_")) return self.parseSimdTypeName(fn_name["simd_splat_".len..]);
            if (std.mem.startsWith(u8, fn_name, "simd_set_")) return self.parseSimdTypeName(fn_name["simd_set_".len..]);
        }
        return self.builder.allocType(.{ .vector_type = .{ .elem_kind = .f32_elem, .lanes = 4 } });
    }

    fn inferSimdExtractType(self: *IRGenerator, vec_expr: *const ast.Expr) !*const ir.IRType {
        if (vec_expr.kind == .identifier) {
            const id_name = vec_expr.kind.identifier.name;
            if (self.variable_types.get(id_name)) |ty| {
                if (ty.* == .vector_type) {
                    return switch (ty.vector_type.elem_kind) {
                        .f32_elem => self.builder.allocType(.f32_type),
                        .f64_elem => self.builder.allocType(.f64_type),
                        .i32_elem => self.builder.allocType(.i32_type),
                        .i64_elem => self.builder.allocType(.i64_type),
                    };
                }
            }
        }
        return self.builder.allocType(.f64_type);
    }

    /// Infer the IR type tag of an AST expression (for auto-wrapping print args)
    fn inferExprIRType(self: *IRGenerator, expr: *const ast.Expr) ir.IRType {
        return switch (expr.kind) {
            .literal => |lit| switch (lit.kind) {
                .string => .string_type,
                .int => .i64_type,
                .float => .f64_type,
                .bool => .bool_type,
                .char => .string_type,
                else => .i64_type,
            },
            .string_interpolation => .string_type,
            .identifier => |id| {
                if (self.variable_types.get(id.name)) |ty| return ty.*;
                return .string_type; // default assumption
            },
            .binary => |bin| {
                // String concatenation
                if (bin.op == .add) {
                    const left_ty = self.inferExprIRType(bin.left);
                    if (left_ty == .string_type) return .string_type;
                }
                // Comparison and boolean ops return bool
                if (bin.op == .eq or bin.op == .ne or bin.op == .lt or bin.op == .gt or
                    bin.op == .le or bin.op == .ge or bin.op == .@"and" or bin.op == .@"or")
                    return .bool_type;
                // Arithmetic ops return the left operand type
                return self.inferExprIRType(bin.left);
            },
            .unary => |un| {
                if (un.op == .not) return .bool_type;
                return self.inferExprIRType(un.operand);
            },
            .function_call => |call| {
                const name = switch (call.function.kind) {
                    .identifier => |id| id.name,
                    else => return .string_type,
                };
                // Check known return types
                if (std.mem.eql(u8, name, "int_to_string") or
                    std.mem.eql(u8, name, "float_to_string") or
                    std.mem.eql(u8, name, "bool_to_string") or
                    std.mem.eql(u8, name, "char_at") or
                    std.mem.eql(u8, name, "substr") or
                    std.mem.eql(u8, name, "string_trim") or
                    std.mem.eql(u8, name, "string_replace") or
                    std.mem.eql(u8, name, "string_to_upper") or
                    std.mem.eql(u8, name, "string_to_lower") or
                    std.mem.eql(u8, name, "read_line") or
                    std.mem.eql(u8, name, "file_read") or
                    std.mem.eql(u8, name, "args_get") or
                    std.mem.eql(u8, name, "fs_getcwd") or
                    std.mem.eql(u8, name, "env_get"))
                    return .string_type;
                if (std.mem.eql(u8, name, "parse_int") or
                    std.mem.eql(u8, name, "string_to_int") or
                    std.mem.eql(u8, name, "len") or
                    std.mem.eql(u8, name, "string_find") or
                    std.mem.eql(u8, name, "system") or
                    std.mem.eql(u8, name, "args_len"))
                    return .i64_type;
                if (std.mem.eql(u8, name, "parse_float"))
                    return .f64_type;
                if (std.mem.eql(u8, name, "string_contains") or
                    std.mem.eql(u8, name, "starts_with") or
                    std.mem.eql(u8, name, "ends_with") or
                    std.mem.eql(u8, name, "file_exists") or
                    std.mem.eql(u8, name, "is_alpha") or
                    std.mem.eql(u8, name, "is_digit") or
                    std.mem.eql(u8, name, "is_whitespace") or
                    std.mem.eql(u8, name, "is_alnum"))
                    return .bool_type;
                if (std.mem.eql(u8, name, "char_to_string"))
                    return .string_type;
                // Look up user-defined function return types (including monomorphized names)
                for (self.module.functions.items) |f| {
                    if (std.mem.eql(u8, f.name, name)) return f.return_type.*;
                }
                // Check if this is a generic call — look up monomorphized name
                if (call.generic_args) |gargs| {
                    if (gargs.len > 0) {
                        // Build monomorphized name: name_type1_type2
                        var mono_name = std.ArrayList(u8).init(self.allocator);
                        defer mono_name.deinit();
                        mono_name.appendSlice(name) catch {};
                        for (gargs) |ga| {
                            mono_name.append('_') catch {};
                            const ty_name = self.typeExprToString(ga);
                            mono_name.appendSlice(ty_name) catch {};
                        }
                        for (self.module.functions.items) |f| {
                            if (std.mem.eql(u8, f.name, mono_name.items)) return f.return_type.*;
                        }
                    }
                }
                // Also try to look up from source AST declarations
                if (self.comptime_declarations) |decls| {
                    for (decls) |decl| {
                        if (decl.kind == .function) {
                            const fd = decl.kind.function;
                            if (std.mem.eql(u8, fd.name.name, name)) {
                                if (fd.return_type) |rt| {
                                    return self.mapTypeExprInfer(rt);
                                }
                            }
                        }
                    }
                }
                return .string_type; // default
            },
            .if_expr => .string_type, // conservative default
            .method_call => |mc| {
                if (std.mem.eql(u8, mc.method.name, "len")) return .i64_type;
                if (std.mem.eql(u8, mc.method.name, "contains")) return .bool_type;
                if (std.mem.eql(u8, mc.method.name, "pop")) return .i64_type;
                // Map method return types
                if (mc.object.kind == .identifier) {
                    if (self.map_var_kinds.get(mc.object.kind.identifier.name)) |mk| {
                        if (std.mem.eql(u8, mc.method.name, "get")) {
                            return switch (mk) {
                                .string_int => .i64_type,
                                .int_string => .string_type,
                            };
                        }
                    }
                }
                // Try to look up the actual method's return type (including mangled names)
                const obj_sname: ?[]const u8 = if (mc.object.kind == .identifier)
                    self.var_struct_types.get(mc.object.kind.identifier.name)
                else
                    null;
                if (obj_sname) |sname| {
                    const mangled = std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ sname, mc.method.name }) catch return .string_type;
                    for (self.module.functions.items) |f| {
                        if (std.mem.eql(u8, f.name, mangled)) {
                            return f.return_type.*;
                        }
                    }
                }
                // Also try bare method name
                for (self.module.functions.items) |f| {
                    if (std.mem.eql(u8, f.name, mc.method.name)) {
                        return f.return_type.*;
                    }
                }
                return .string_type;
            },
            .field_access => |fa| blk: {
                // Check Box field access first
                if (fa.object.kind == .identifier) {
                    const id_name = fa.object.kind.identifier.name;
                    if (self.box_inner_types.get(id_name)) |inner_ty| {
                        if (inner_ty.* == .struct_type) {
                            for (inner_ty.struct_type.fields) |field| {
                                if (std.mem.eql(u8, field.name, fa.field.name)) {
                                    break :blk field.ty.*;
                                }
                            }
                        }
                    }
                }
                // Look up the struct type of the object to find field type
                const obj_type = self.inferExprIRType(fa.object);
                if (obj_type == .struct_type) {
                    // Find the field by name
                    for (obj_type.struct_type.fields) |field| {
                        if (std.mem.eql(u8, field.name, fa.field.name)) {
                            break :blk field.ty.*;
                        }
                    }
                }
                // If object is an identifier, look up its type then check struct fields
                if (fa.object.kind == .identifier) {
                    const id_name = fa.object.kind.identifier.name;
                    if (self.variable_types.get(id_name)) |ty| {
                        if (ty.* == .struct_type) {
                            for (ty.struct_type.fields) |field| {
                                if (std.mem.eql(u8, field.name, fa.field.name)) {
                                    break :blk field.ty.*;
                                }
                            }
                        }
                    }
                    // Box variable: look up inner struct fields
                    if (self.box_inner_types.get(id_name)) |inner_ty| {
                        if (inner_ty.* == .struct_type) {
                            for (inner_ty.struct_type.fields) |field| {
                                if (std.mem.eql(u8, field.name, fa.field.name)) {
                                    break :blk field.ty.*;
                                }
                            }
                        }
                    }
                }
                // If base is ptr (Box pointer), search all struct defs for the field
                if (obj_type == .ptr or obj_type == .i64_type) {
                    var struct_it = self.struct_defs.iterator();
                    while (struct_it.next()) |entry| {
                        const sty = entry.value_ptr.ir_type;
                        if (sty.* == .struct_type) {
                            for (sty.struct_type.fields) |field| {
                                if (std.mem.eql(u8, field.name, fa.field.name)) {
                                    break :blk field.ty.*;
                                }
                            }
                        }
                    }
                }
                break :blk ir.IRType.string_type;
            },
            .cast => |ce| {
                // as operator: check target type name
                if (ce.target_type.kind == .named) {
                    const named = ce.target_type.kind.named;
                    if (named.path.segments.len == 1) {
                        const tname = named.path.segments[0].name;
                        if (std.mem.eql(u8, tname, "int") or std.mem.eql(u8, tname, "i64") or
                            std.mem.eql(u8, tname, "i32") or std.mem.eql(u8, tname, "i16") or
                            std.mem.eql(u8, tname, "i8") or std.mem.eql(u8, tname, "u64") or
                            std.mem.eql(u8, tname, "u32") or std.mem.eql(u8, tname, "u16") or
                            std.mem.eql(u8, tname, "u8"))
                            return .i64_type;
                        if (std.mem.eql(u8, tname, "float") or std.mem.eql(u8, tname, "f64") or
                            std.mem.eql(u8, tname, "f32"))
                            return .f64_type;
                    }
                }
                return .string_type;
            },
            .index_access => |ia| blk: {
                // List[int][i] → i64, List[string][i] → string, string[i] → string
                if (ia.object.kind == .identifier) {
                    const obj_name = ia.object.kind.identifier.name;
                    if (self.list_elem_kinds.get(obj_name)) |ek| {
                        break :blk switch (ek) {
                            .int64 => ir.IRType.i64_type,
                            .float64 => ir.IRType.f64_type,
                            .string => ir.IRType.string_type,
                            .other => ir.IRType.i64_type,
                        };
                    }
                    // Check if it's a string variable
                    if (self.variable_types.get(obj_name)) |vty| {
                        if (vty.* == .string_type) break :blk ir.IRType.string_type;
                    }
                }
                break :blk ir.IRType.string_type;
            },
            else => .string_type, // conservative default: assume string
        };
    }

    /// Convert a type expression AST node to a simple string (for monomorphization name building)
    fn typeExprToString(self: *IRGenerator, type_expr: *const ast.TypeExpr) []const u8 {
        _ = self;
        return switch (type_expr.kind) {
            .named => |named| blk: {
                if (named.path.segments.len > 0)
                    break :blk named.path.segments[0].name;
                break :blk "unknown";
            },
            else => "unknown",
        };
    }

    /// Non-error-returning version of mapTypeExpr for use in type inference
    fn mapTypeExprInfer(_: *IRGenerator, type_expr: *const ast.TypeExpr) ir.IRType {
        return switch (type_expr.kind) {
            .named => |named| blk: {
                if (named.path.segments.len > 0) {
                    const tname = named.path.segments[0].name;
                    if (std.mem.eql(u8, tname, "int") or std.mem.eql(u8, tname, "i64")) break :blk ir.IRType.i64_type;
                    if (std.mem.eql(u8, tname, "i32")) break :blk ir.IRType.i32_type;
                    if (std.mem.eql(u8, tname, "float") or std.mem.eql(u8, tname, "f64")) break :blk ir.IRType.f64_type;
                    if (std.mem.eql(u8, tname, "bool")) break :blk ir.IRType.bool_type;
                    if (std.mem.eql(u8, tname, "string")) break :blk ir.IRType.string_type;
                    if (std.mem.eql(u8, tname, "void")) break :blk ir.IRType.void_type;
                }
                break :blk ir.IRType.string_type;
            },
            else => ir.IRType.string_type,
        };
    }

    fn generateMethodCall(self: *IRGenerator, mc: *const ast.MethodCall) !ir.Value {
        const method_name = mc.method.name;

        // Check if this is an enum dot constructor: EnumName.Variant(args)
        if (mc.object.kind == .identifier) {
            const obj_name = mc.object.kind.identifier.name;
            if (self.enum_defs.get(obj_name)) |enum_info| {
                // This is EnumName.VariantName(args) — construct enum value
                for (enum_info.variants) |variant| {
                    if (std.mem.eql(u8, variant.name, method_name)) {
                        var result: ir.Value = ir.Value{ .undef = {} };
                        result = try self.builder.buildInsertField(result, ir.Value{ .const_int = variant.tag }, 0, enum_info.ir_type);
                        // Handle payload
                        if (mc.args.len > 0) {
                            var field_idx: u32 = 1; // skip tag
                            for (enum_info.variants) |v| {
                                if (std.mem.eql(u8, v.name, method_name)) break;
                                if (v.payload_types) |pts| field_idx += @intCast(pts.len);
                            }
                            for (mc.args, 0..) |arg, pi| {
                                const val = try self.generateExpr(arg);
                                result = try self.builder.buildInsertField(result, val, field_idx + @as(u32, @intCast(pi)), enum_info.ir_type);
                            }
                        }
                        return result;
                    }
                }
            }
        }

        // Check if this is a dyn trait method call — dispatch through concrete type
        if (mc.object.kind == .identifier) {
            const dyn_obj_name = mc.object.kind.identifier.name;
            if (self.dyn_var_concrete.get(dyn_obj_name)) |concrete_type| {
                // Dynamic dispatch: call ConcreteType_method(data_ptr)
                const mangled = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ concrete_type, method_name });

                // Load the data pointer from the dyn variable's alloca
                const alloca_val = self.variable_map.get(dyn_obj_name) orelse return ir.Value{ .undef = {} };
                const ptr_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                const data_ptr = try self.builder.buildLoad(alloca_val, ptr_ty);

                // Build args: data_ptr (as self), then the rest
                var args = std.ArrayList(ir.Value).init(self.allocator);
                defer args.deinit();
                try args.append(data_ptr);
                for (mc.args) |arg| {
                    try args.append(try self.generateExpr(arg));
                }
                const args_slice = try self.allocator.dupe(ir.Value, args.items);

                // Look up the method's return type
                const ret_type = try self.inferCallReturnType(mangled);
                if (ret_type.* == .void_type) {
                    try self.builder.buildCallVoid(mangled, args_slice);
                    return ir.Value{ .undef = {} };
                }
                return self.builder.buildCall(mangled, args_slice, ret_type);
            }
        }

        // Get the object variable name for list elem kind lookup
        const obj_var_name: ?[]const u8 = if (mc.object.kind == .identifier)
            mc.object.kind.identifier.name
        else
            null;

        // Check if this is a map method call
        const map_kind: ?MapKind = if (obj_var_name) |name| self.map_var_kinds.get(name) else null;
        if (map_kind) |mk| {
            const map_ptr = if (obj_var_name) |name| self.variable_map.get(name) orelse return ir.Value{ .undef = {} } else return ir.Value{ .undef = {} };
            return self.generateMapMethodCall(mk, map_ptr, method_name, mc.args);
        }

        // Check if this is a list method call
        const list_kind: ?ListElemKind = if (obj_var_name) |name| self.list_elem_kinds.get(name) else null;

        if (list_kind != null) {
            // This is a list method call — get pointer to list (alloca, not loaded value)
            const list_ptr = if (obj_var_name) |name| self.variable_map.get(name) orelse return ir.Value{ .undef = {} } else return ir.Value{ .undef = {} };
            const ek = list_kind.?;

            if (std.mem.eql(u8, method_name, "push")) {
                if (ek == .other) {
                    // Generic push: alloca temp, store struct value, pass pointer + elem_size
                    const elem_ty = if (obj_var_name) |name| self.list_elem_types.get(name) else null;
                    const val = try self.generateExpr(mc.args[0]);
                    const actual_elem_ty = elem_ty orelse try self.builder.allocType(.i64_type);
                    const temp_alloca = try self.builder.buildAlloca(actual_elem_ty);
                    try self.builder.buildStore(temp_alloca, val);
                    const elem_size = estimateTypeSize(actual_elem_ty);
                    const push_args_slice = try self.allocator.dupe(ir.Value, &.{
                        list_ptr, temp_alloca, .{ .const_int = elem_size },
                    });
                    try self.builder.buildCallVoid("dm_list_generic_push", push_args_slice);
                    return ir.Value{ .undef = {} };
                }
                var push_args = std.ArrayList(ir.Value).init(self.allocator);
                defer push_args.deinit();
                try push_args.append(list_ptr); // pass pointer to list struct
                for (mc.args) |arg| {
                    try push_args.append(try self.generateExpr(arg));
                }
                const push_fn = switch (ek) {
                    .int64 => "dm_list_int64_push",
                    .float64 => "dm_list_double_push",
                    .string => "dm_list_string_push",
                    .other => unreachable,
                };
                const push_args_slice = try self.allocator.dupe(ir.Value, push_args.items);
                try self.builder.buildCallVoid(push_fn, push_args_slice);
                return ir.Value{ .undef = {} };
            }
            if (std.mem.eql(u8, method_name, "pop")) {
                const pop_fn = switch (ek) {
                    .int64 => "dm_list_int64_pop",
                    .float64 => "dm_list_double_get", // no pop for double yet
                    .string => "dm_list_string_get", // no pop for string yet
                    .other => "dm_list_int64_pop",
                };
                const ret_ty = try self.builder.allocType(.i64_type);
                const pop_args = try self.allocator.dupe(ir.Value, &.{list_ptr});
                return self.builder.buildCall(pop_fn, pop_args, ret_ty);
            }
            if (std.mem.eql(u8, method_name, "len")) {
                const len_fn = switch (ek) {
                    .int64 => "dm_list_int64_len",
                    .float64 => "dm_list_double_len",
                    .string => "dm_list_string_len",
                    .other => "dm_list_generic_len",
                };
                const i64_ty = try self.builder.allocType(.i64_type);
                const len_args = try self.allocator.dupe(ir.Value, &.{list_ptr});
                return self.builder.buildCall(len_fn, len_args, i64_ty);
            }
            if (std.mem.eql(u8, method_name, "contains")) {
                if (mc.args.len > 0) {
                    const search_val = try self.generateExpr(mc.args[0]);
                    const contains_fn: []const u8 = switch (ek) {
                        .int64 => "dm_list_int64_contains",
                        .string => "dm_list_string_contains",
                        else => "dm_list_int64_contains",
                    };
                    const bool_ty = try self.builder.allocType(.bool_type);
                    const contains_args = try self.allocator.dupe(ir.Value, &.{ list_ptr, search_val });
                    return self.builder.buildCall(contains_fn, contains_args, bool_ty);
                }
                return ir.Value{ .const_bool = false };
            }
        }

        // Check if this is a list method call on a struct field access (e.g., p.tokens.push(x))
        if (mc.object.kind == .field_access) {
            const field_ek = self.inferFieldAccessListKind(mc.object);
            if (field_ek) |ek| {
                const field_info = try self.getFieldListTempAlloca(mc.object.kind.field_access) orelse return ir.Value{ .undef = {} };
                const list_ptr = field_info.temp_alloca;

                if (std.mem.eql(u8, method_name, "push")) {
                    if (ek == .other) {
                        const fa = mc.object.kind.field_access;
                        const fa_obj_name = if (fa.object.kind == .identifier) fa.object.kind.identifier.name else "";
                        const fa_struct_name = self.var_struct_types.get(fa_obj_name) orelse "";
                        const elem_ty = self.getFieldListElemType(fa_struct_name, fa.field.name);
                        const val = try self.generateExpr(mc.args[0]);
                        const actual_elem_ty = elem_ty orelse try self.builder.allocType(.i64_type);
                        const temp_val_alloca = try self.builder.buildAlloca(actual_elem_ty);
                        try self.builder.buildStore(temp_val_alloca, val);
                        const elem_size = estimateTypeSize(actual_elem_ty);
                        const push_args_slice = try self.allocator.dupe(ir.Value, &.{
                            list_ptr, temp_val_alloca, .{ .const_int = elem_size },
                        });
                        try self.builder.buildCallVoid("dm_list_generic_push", push_args_slice);
                    } else {
                        var push_args = std.ArrayList(ir.Value).init(self.allocator);
                        defer push_args.deinit();
                        try push_args.append(list_ptr);
                        for (mc.args) |arg| {
                            try push_args.append(try self.generateExpr(arg));
                        }
                        const push_fn: []const u8 = switch (ek) {
                            .int64 => "dm_list_int64_push",
                            .float64 => "dm_list_double_push",
                            .string => "dm_list_string_push",
                            .other => unreachable,
                        };
                        const push_args_slice = try self.allocator.dupe(ir.Value, push_args.items);
                        try self.builder.buildCallVoid(push_fn, push_args_slice);
                    }
                    // Writeback the modified list to the struct field
                    try self.writebackFieldList(field_info);
                    return ir.Value{ .undef = {} };
                }
                if (std.mem.eql(u8, method_name, "pop")) {
                    const pop_fn: []const u8 = switch (ek) {
                        .int64 => "dm_list_int64_pop",
                        .float64 => "dm_list_double_get",
                        .string => "dm_list_string_get",
                        .other => "dm_list_int64_pop",
                    };
                    const ret_ty = try self.builder.allocType(.i64_type);
                    const pop_args = try self.allocator.dupe(ir.Value, &.{list_ptr});
                    const result = try self.builder.buildCall(pop_fn, pop_args, ret_ty);
                    try self.writebackFieldList(field_info);
                    return result;
                }
                if (std.mem.eql(u8, method_name, "len")) {
                    const len_fn: []const u8 = switch (ek) {
                        .int64 => "dm_list_int64_len",
                        .float64 => "dm_list_double_len",
                        .string => "dm_list_string_len",
                        .other => "dm_list_generic_len",
                    };
                    const i64_ty = try self.builder.allocType(.i64_type);
                    const len_args = try self.allocator.dupe(ir.Value, &.{list_ptr});
                    return self.builder.buildCall(len_fn, len_args, i64_ty);
                }
                if (std.mem.eql(u8, method_name, "contains")) {
                    if (mc.args.len > 0) {
                        const search_val = try self.generateExpr(mc.args[0]);
                        const contains_fn: []const u8 = switch (ek) {
                            .int64 => "dm_list_int64_contains",
                            .string => "dm_list_string_contains",
                            else => "dm_list_int64_contains",
                        };
                        const bool_ty = try self.builder.allocType(.bool_type);
                        const contains_args = try self.allocator.dupe(ir.Value, &.{ list_ptr, search_val });
                        return self.builder.buildCall(contains_fn, contains_args, bool_ty);
                    }
                    return ir.Value{ .const_bool = false };
                }
            }
        }

        // Non-list method call — standard handling
        // Determine the actual function name (possibly mangled with struct type)
        var actual_method_name: []const u8 = method_name;

        // Try to find mangled name: look up the receiver's struct type
        const obj_struct_name: ?[]const u8 = if (mc.object.kind == .identifier) blk: {
            const obj_name = mc.object.kind.identifier.name;
            // Check var_struct_types first
            if (self.var_struct_types.get(obj_name)) |sname| break :blk sname;
            // Check if variable type is a struct type
            if (self.variable_types.get(obj_name)) |ty| {
                var check_ty = ty;
                // Unwrap pointer types
                if (check_ty.* == .ptr) check_ty = check_ty.ptr;
                if (check_ty.* == .struct_type) {
                    var it = self.struct_defs.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.ir_type == check_ty) break :blk entry.key_ptr.*;
                    }
                }
            }
            break :blk null;
        } else null;

        if (obj_struct_name) |sname| {
            const mangled = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ sname, method_name });
            // Check if this mangled name exists
            for (self.module.functions.items) |f| {
                if (std.mem.eql(u8, f.name, mangled)) {
                    actual_method_name = mangled;
                    break;
                }
            }
        }

        // Check if the method takes a self pointer (first param is a pointer type)
        var takes_self_ptr = false;
        for (self.module.functions.items) |f| {
            if (std.mem.eql(u8, f.name, actual_method_name)) {
                if (f.params.len > 0 and f.params[0].ty.* == .ptr) {
                    takes_self_ptr = true;
                }
                break;
            }
        }

        var args = std.ArrayList(ir.Value).init(self.allocator);
        defer args.deinit();

        if (takes_self_ptr) {
            // Pass the alloca pointer (address of the struct) for &self / mut self
            if (mc.object.kind == .identifier) {
                const obj_name = mc.object.kind.identifier.name;
                if (self.variable_map.get(obj_name)) |alloca_ptr| {
                    try args.append(alloca_ptr);
                } else {
                    try args.append(try self.generateExpr(mc.object));
                }
            } else {
                try args.append(try self.generateExpr(mc.object));
            }
        } else {
            try args.append(try self.generateExpr(mc.object));
        }

        for (mc.args) |arg| {
            try args.append(try self.generateExpr(arg));
        }
        const args_slice = try self.allocator.dupe(ir.Value, args.items);

        // Look up user-defined function/method return type
        const ret_type = try self.inferCallReturnType(actual_method_name);
        if (ret_type.* == .void_type) {
            try self.builder.buildCallVoid(actual_method_name, args_slice);
            return ir.Value{ .undef = {} };
        }
        return self.builder.buildCall(actual_method_name, args_slice, ret_type);
    }

    /// Generate map method calls (insert, get, contains, remove, len, keys, values, set)
    fn generateMapMethodCall(self: *IRGenerator, mk: MapKind, map_ptr: ir.Value, method_name: []const u8, args: []const *ast.Expr) !ir.Value {
        const i64_ty = try self.builder.allocType(.i64_type);
        const bool_ty = try self.builder.allocType(.bool_type);

        if (std.mem.eql(u8, method_name, "insert") or std.mem.eql(u8, method_name, "set")) {
            if (args.len >= 2) {
                const key_val = try self.generateExpr(args[0]);
                const val_val = try self.generateExpr(args[1]);
                const fn_name: []const u8 = switch (mk) {
                    .string_int => "dm_map_string_int_insert",
                    .int_string => "dm_map_int_string_insert",
                };
                const call_args = try self.allocator.dupe(ir.Value, &.{ map_ptr, key_val, val_val });
                try self.builder.buildCallVoid(fn_name, call_args);
            }
            return ir.Value{ .undef = {} };
        }

        if (std.mem.eql(u8, method_name, "get")) {
            if (args.len >= 1) {
                const key_val = try self.generateExpr(args[0]);
                switch (mk) {
                    .string_int => {
                        const call_args = try self.allocator.dupe(ir.Value, &.{ map_ptr, key_val });
                        return self.builder.buildCall("dm_map_string_int_get", call_args, i64_ty);
                    },
                    .int_string => {
                        const string_ty = try self.builder.allocType(.string_type);
                        const out_alloca = try self.builder.buildAlloca(string_ty);
                        const call_args = try self.allocator.dupe(ir.Value, &.{ out_alloca, map_ptr, key_val });
                        try self.builder.buildCallVoid("dm_map_int_string_get", call_args);
                        return self.builder.buildLoad(out_alloca, string_ty);
                    },
                }
            }
            return ir.Value{ .undef = {} };
        }

        if (std.mem.eql(u8, method_name, "contains")) {
            if (args.len >= 1) {
                const key_val = try self.generateExpr(args[0]);
                const fn_name: []const u8 = switch (mk) {
                    .string_int => "dm_map_string_int_contains",
                    .int_string => "dm_map_int_string_contains",
                };
                const call_args = try self.allocator.dupe(ir.Value, &.{ map_ptr, key_val });
                return self.builder.buildCall(fn_name, call_args, bool_ty);
            }
            return ir.Value{ .undef = {} };
        }

        if (std.mem.eql(u8, method_name, "remove")) {
            if (args.len >= 1) {
                const key_val = try self.generateExpr(args[0]);
                const fn_name: []const u8 = switch (mk) {
                    .string_int => "dm_map_string_int_remove",
                    .int_string => "dm_map_int_string_remove",
                };
                const call_args = try self.allocator.dupe(ir.Value, &.{ map_ptr, key_val });
                try self.builder.buildCallVoid(fn_name, call_args);
            }
            return ir.Value{ .undef = {} };
        }

        if (std.mem.eql(u8, method_name, "len")) {
            const fn_name: []const u8 = switch (mk) {
                .string_int => "dm_map_string_int_len",
                .int_string => "dm_map_int_string_len",
            };
            const call_args = try self.allocator.dupe(ir.Value, &.{map_ptr});
            return self.builder.buildCall(fn_name, call_args, i64_ty);
        }

        if (std.mem.eql(u8, method_name, "keys")) {
            // keys() returns a List — allocate list struct and call keys function
            if (mk == .string_int) {
                const list_struct_name = "dm_list_dm_string";
                const ptr_field_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                const i64_field_ty = try self.builder.allocType(.i64_type);
                const list_fields = try self.allocator.dupe(ir.Field, &.{
                    .{ .name = "data", .ty = ptr_field_ty },
                    .{ .name = "len", .ty = i64_field_ty },
                    .{ .name = "capacity", .ty = i64_field_ty },
                });
                const list_ty = try self.builder.allocType(.{ .struct_type = .{
                    .name = list_struct_name,
                    .fields = list_fields,
                } });
                try self.module.struct_defs.put(list_struct_name, list_ty);

                const list_alloca = try self.builder.buildAlloca(list_ty);
                const call_args = try self.allocator.dupe(ir.Value, &.{ list_alloca, map_ptr });
                try self.builder.buildCallVoid("dm_map_string_int_keys", call_args);
                // Return the alloca — the caller (let binding) will store this
                return list_alloca;
            }
            return ir.Value{ .undef = {} };
        }

        if (std.mem.eql(u8, method_name, "values")) {
            if (mk == .string_int) {
                const list_struct_name = "dm_list_int64";
                const ptr_field_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                const i64_field_ty = try self.builder.allocType(.i64_type);
                const list_fields = try self.allocator.dupe(ir.Field, &.{
                    .{ .name = "data", .ty = ptr_field_ty },
                    .{ .name = "len", .ty = i64_field_ty },
                    .{ .name = "capacity", .ty = i64_field_ty },
                });
                const list_ty = try self.builder.allocType(.{ .struct_type = .{
                    .name = list_struct_name,
                    .fields = list_fields,
                } });
                try self.module.struct_defs.put(list_struct_name, list_ty);

                const list_alloca = try self.builder.buildAlloca(list_ty);
                const call_args = try self.allocator.dupe(ir.Value, &.{ list_alloca, map_ptr });
                try self.builder.buildCallVoid("dm_map_string_int_values", call_args);
                return list_alloca;
            }
            return ir.Value{ .undef = {} };
        }

        // Unknown method — fall through
        return ir.Value{ .undef = {} };
    }

    fn generateIfExpr(self: *IRGenerator, if_expr: *const ast.IfExpr) !ir.Value {
        try self.generateIfStatement(if_expr);
        return ir.Value{ .undef = {} };
    }

    fn generateBlockExpr(self: *IRGenerator, block: *const ast.BlockExpr) !ir.Value {
        for (block.statements) |stmt| {
            try self.generateStatement(stmt);
        }
        if (block.result) |result| {
            return self.generateExpr(result);
        }
        return ir.Value{ .undef = {} };
    }

    fn generateStructLiteral(self: *IRGenerator, sl: *const ast.StructLiteral) !ir.Value {
        // Get struct name from type_path
        const struct_name = if (sl.type_path) |tp| blk: {
            if (tp.segments.len > 0) break :blk tp.segments[0].name;
            break :blk @as(?[]const u8, null);
        } else null;
        const sname = struct_name orelse return ir.Value{ .undef = {} };
        const struct_info = self.struct_defs.get(sname) orelse return ir.Value{ .undef = {} };

        // Build struct value using insertfield chain
        var result: ir.Value = ir.Value{ .undef = {} };

        for (sl.fields) |field_init| {
            const val = try self.generateExpr(field_init.value);
            for (struct_info.fields, 0..) |f, i| {
                if (std.mem.eql(u8, f.name, field_init.name.name)) {
                    result = try self.builder.buildInsertField(result, val, @intCast(i), struct_info.ir_type);
                    break;
                }
            }
        }

        return result;
    }

    fn generateFieldAccess(self: *IRGenerator, fa: *const ast.FieldAccess) !ir.Value {
        // Check for Box[T] field access: dereference pointer then access struct field
        if (fa.object.kind == .identifier) {
            const obj_name = fa.object.kind.identifier.name;
            if (self.box_inner_types.get(obj_name)) |inner_ty| {
                // Box variable: load the pointer, then load struct through it, then extract field
                const alloca = self.variable_map.get(obj_name) orelse return ir.Value{ .undef = {} };
                const ptr_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                const box_ptr = try self.builder.buildLoad(alloca, ptr_ty);
                // Load the inner struct through the pointer
                const inner_val = try self.builder.buildLoad(box_ptr, inner_ty);
                // Extract the field
                if (inner_ty.* == .struct_type) {
                    for (inner_ty.struct_type.fields, 0..) |field, i| {
                        if (std.mem.eql(u8, field.name, fa.field.name)) {
                            return self.builder.buildExtractField(inner_val, @intCast(i), field.ty);
                        }
                    }
                }
                return ir.Value{ .undef = {} };
            }
        }

        // Generate base expression (loads struct value from alloca)
        const base_val = try self.generateExpr(fa.object);

        // Determine type
        const base_ty = try self.inferExprType(fa.object);

        // Box pointer dereference: if base_ty is ptr, load the pointed-to struct and extract field
        if (base_ty.* == .ptr) {
            // Try to determine the inner struct type
            // Look at the source: if base is a field_access on a struct, find the Box[T] field's T
            const inner_struct_ty: ?*const ir.IRType = blk: {
                if (fa.object.kind == .field_access) {
                    const inner_fa = fa.object.kind.field_access;
                    const parent_ty = try self.inferExprType(inner_fa.object);
                    if (parent_ty.* == .struct_type) {
                        // Find the field and check if original AST type was Box[T]
                        for (parent_ty.struct_type.fields) |_| {
                            // The field is ptr type; look up what struct it points to
                            // by searching struct_defs for the field name pattern
                        }
                    }
                }
                // Try all known struct types to find one with a matching field name
                var sit = self.struct_defs.iterator();
                while (sit.next()) |entry| {
                    const sty = entry.value_ptr.ir_type;
                    if (sty.* == .struct_type) {
                        for (sty.struct_type.fields) |field| {
                            if (std.mem.eql(u8, field.name, fa.field.name)) {
                                break :blk sty;
                            }
                        }
                    }
                }
                break :blk null;
            };
            if (inner_struct_ty) |ist| {
                const loaded_struct = try self.builder.buildLoad(base_val, ist);
                if (ist.* == .struct_type) {
                    for (ist.struct_type.fields, 0..) |field, i| {
                        if (std.mem.eql(u8, field.name, fa.field.name)) {
                            return self.builder.buildExtractField(loaded_struct, @intCast(i), field.ty);
                        }
                    }
                }
            }
        }

        // Option[T] field access: .has_value, .value
        if (base_ty.* == .option_type) {
            const i32_ty = try self.builder.allocType(.i32_type);
            if (std.mem.eql(u8, fa.field.name, "has_value")) {
                // Extract tag field (0) and convert to bool
                const tag = try self.builder.buildExtractField(base_val, 0, i32_ty);
                const zero = ir.Value{ .const_int = 0 };
                return self.builder.buildNe(tag, zero);
            }
            if (std.mem.eql(u8, fa.field.name, "value")) {
                return self.builder.buildExtractField(base_val, 1, base_ty.option_type);
            }
        }

        // Result[T, E] field access: .is_ok, .is_err, .ok_value, .err_value
        if (base_ty.* == .result_type) {
            const i32_ty = try self.builder.allocType(.i32_type);
            if (std.mem.eql(u8, fa.field.name, "is_ok")) {
                const tag = try self.builder.buildExtractField(base_val, 0, i32_ty);
                return self.builder.buildEq(tag, ir.Value{ .const_int = 0 });
            }
            if (std.mem.eql(u8, fa.field.name, "is_err")) {
                const tag = try self.builder.buildExtractField(base_val, 0, i32_ty);
                return self.builder.buildNe(tag, ir.Value{ .const_int = 0 });
            }
            if (std.mem.eql(u8, fa.field.name, "ok_value") or std.mem.eql(u8, fa.field.name, "ok") or std.mem.eql(u8, fa.field.name, "value")) {
                return self.builder.buildExtractField(base_val, 1, base_ty.result_type.ok);
            }
            if (std.mem.eql(u8, fa.field.name, "err_value") or std.mem.eql(u8, fa.field.name, "err") or std.mem.eql(u8, fa.field.name, "error")) {
                return self.builder.buildExtractField(base_val, 2, base_ty.result_type.err);
            }
        }

        // Struct field access
        if (base_ty.* == .struct_type) {
            for (base_ty.struct_type.fields, 0..) |field, i| {
                if (std.mem.eql(u8, field.name, fa.field.name)) {
                    return self.builder.buildExtractField(base_val, @intCast(i), field.ty);
                }
            }
        }

        return ir.Value{ .undef = {} };
    }

    fn generateIndexAccess(self: *IRGenerator, ia: *const ast.IndexAccess) !ir.Value {
        // Get the object variable name for list elem kind lookup
        const obj_var_name: ?[]const u8 = if (ia.object.kind == .identifier)
            ia.object.kind.identifier.name
        else
            null;

        const list_kind: ?ListElemKind = if (obj_var_name) |name| self.list_elem_kinds.get(name) else null;

        if (list_kind) |ek| {
            // List index access: dm_list_TYPE_get(&list, idx)
            const list_ptr = if (obj_var_name) |name| self.variable_map.get(name) orelse return ir.Value{ .undef = {} } else return ir.Value{ .undef = {} };
            const idx = try self.generateExpr(ia.index);

            if (ek == .other) {
                const elem_ty = if (obj_var_name) |name| self.list_elem_types.get(name) else null;
                const actual_elem_ty = elem_ty orelse try self.builder.allocType(.i64_type);
                const out_alloca = try self.builder.buildAlloca(actual_elem_ty);
                const elem_size = estimateTypeSize(actual_elem_ty);
                const get_args = try self.allocator.dupe(ir.Value, &.{
                    out_alloca, list_ptr, idx, .{ .const_int = elem_size },
                });
                try self.builder.buildCallVoid("dm_list_generic_get", get_args);
                return self.builder.buildLoad(out_alloca, actual_elem_ty);
            }

            const get_fn = switch (ek) {
                .int64 => "dm_list_int64_get",
                .float64 => "dm_list_double_get",
                .string => "dm_list_string_get",
                .other => unreachable,
            };
            const ret_ty: *const ir.IRType = switch (ek) {
                .int64 => try self.builder.allocType(.i64_type),
                .float64 => try self.builder.allocType(.f64_type),
                .string => try self.builder.allocType(.string_type),
                .other => unreachable,
            };
            const get_args = try self.allocator.dupe(ir.Value, &.{ list_ptr, idx });
            return self.builder.buildCall(get_fn, get_args, ret_ty);
        }

        // Field access list index: obj.field[i] where field is a List[T]
        if (ia.object.kind == .field_access) {
            const field_ek = self.inferFieldAccessListKind(ia.object);
            if (field_ek) |ek| {
                const field_info = try self.getFieldListTempAlloca(ia.object.kind.field_access) orelse return ir.Value{ .undef = {} };
                const fl_list_ptr = field_info.temp_alloca;
                const idx = try self.generateExpr(ia.index);

                if (ek == .other) {
                    const fa = ia.object.kind.field_access;
                    const fa_obj_name = if (fa.object.kind == .identifier) fa.object.kind.identifier.name else "";
                    const fa_struct_name = self.var_struct_types.get(fa_obj_name) orelse "";
                    const elem_ty = self.getFieldListElemType(fa_struct_name, fa.field.name) orelse try self.builder.allocType(.i64_type);
                    const out_alloca = try self.builder.buildAlloca(elem_ty);
                    const elem_size = estimateTypeSize(elem_ty);
                    const get_args = try self.allocator.dupe(ir.Value, &.{
                        out_alloca, fl_list_ptr, idx, .{ .const_int = elem_size },
                    });
                    try self.builder.buildCallVoid("dm_list_generic_get", get_args);
                    return self.builder.buildLoad(out_alloca, elem_ty);
                }

                const get_fn: []const u8 = switch (ek) {
                    .int64 => "dm_list_int64_get",
                    .float64 => "dm_list_double_get",
                    .string => "dm_list_string_get",
                    .other => unreachable,
                };
                const ret_ty: *const ir.IRType = switch (ek) {
                    .int64 => try self.builder.allocType(.i64_type),
                    .float64 => try self.builder.allocType(.f64_type),
                    .string => try self.builder.allocType(.string_type),
                    .other => unreachable,
                };
                const get_args = try self.allocator.dupe(ir.Value, &.{ fl_list_ptr, idx });
                return self.builder.buildCall(get_fn, get_args, ret_ty);
            }
        }

        // Chained index access: outer[i][j] where outer is a List[List[T]]
        // The object is itself an IndexAccess on a list-of-lists variable
        if (ia.object.kind == .index_access) {
            const inner_ia = ia.object.kind.index_access;
            if (inner_ia.object.kind == .identifier) {
                const inner_var = inner_ia.object.kind.identifier.name;
                const inner_ek = self.list_elem_kinds.get(inner_var);
                if (inner_ek != null and inner_ek.? == .other) {
                    // outer[i] returns an inner list struct; we need to index into that
                    const inner_elem_ty = self.list_elem_types.get(inner_var);
                    if (inner_elem_ty) |iety| {
                        if (iety.* == .struct_type) {
                            const sname = iety.struct_type.name;
                            // Determine what kind of inner list this is
                            const inner_list_kind: ListElemKind = if (std.mem.eql(u8, sname, "dm_list_int64"))
                                .int64
                            else if (std.mem.eql(u8, sname, "dm_list_double"))
                                .float64
                            else if (std.mem.eql(u8, sname, "dm_list_dm_string"))
                                .string
                            else
                                .other;

                            // Generate the inner index access to get the inner list struct
                            const inner_val = try self.generateExpr(ia.object);
                            // Store the loaded inner list struct to a temp alloca for use as a pointer
                            const temp_alloca = try self.builder.buildAlloca(iety);
                            try self.builder.buildStore(temp_alloca, inner_val);
                            // Now index into the inner list
                            const outer_idx = try self.generateExpr(ia.index);

                            if (inner_list_kind == .other) {
                                // Nested generic list — use dm_list_generic_get
                                const nested_elem_ty = try self.builder.allocType(.i64_type);
                                const nested_out = try self.builder.buildAlloca(nested_elem_ty);
                                const nested_size = estimateTypeSize(nested_elem_ty);
                                const nested_args = try self.allocator.dupe(ir.Value, &.{
                                    nested_out, temp_alloca, outer_idx, .{ .const_int = nested_size },
                                });
                                try self.builder.buildCallVoid("dm_list_generic_get", nested_args);
                                return self.builder.buildLoad(nested_out, nested_elem_ty);
                            }

                            const inner_get_fn: []const u8 = switch (inner_list_kind) {
                                .int64 => "dm_list_int64_get",
                                .float64 => "dm_list_double_get",
                                .string => "dm_list_string_get",
                                .other => unreachable,
                            };
                            const inner_ret_ty: *const ir.IRType = switch (inner_list_kind) {
                                .int64 => try self.builder.allocType(.i64_type),
                                .float64 => try self.builder.allocType(.f64_type),
                                .string => try self.builder.allocType(.string_type),
                                .other => unreachable,
                            };
                            const inner_get_args = try self.allocator.dupe(ir.Value, &.{ temp_alloca, outer_idx });
                            return self.builder.buildCall(inner_get_fn, inner_get_args, inner_ret_ty);
                        }
                    }
                }
            }
        }

        // String indexing: char_at(str, idx)
        const obj_ty = try self.inferExprType(ia.object);
        if (obj_ty.* == .string_type) {
            const str_val = try self.generateExpr(ia.object);
            const idx = try self.generateExpr(ia.index);
            const string_ty = try self.builder.allocType(.string_type);
            const args = try self.allocator.dupe(ir.Value, &.{ str_val, idx });
            return self.builder.buildCall("dm_char_at", args, string_ty);
        }

        // Array element access: look up arr__<idx> variable
        if (obj_var_name) |name| {
            // Check if this is an array variable (has arr__0 element)
            const check_key = try std.fmt.allocPrint(self.allocator, "{s}__0", .{name});
            if (self.variable_map.contains(check_key)) {
                // Try to resolve constant index
                if (ia.index.kind == .literal) {
                    const lit = ia.index.kind.literal;
                    if (lit.kind == .int) {
                        const idx_val = std.fmt.parseInt(i64, lit.kind.int.value, 10) catch 0;
                        const key = try std.fmt.allocPrint(self.allocator, "{s}__{d}", .{ name, idx_val });
                        if (self.variable_map.get(key)) |elem_alloca| {
                            const elem_ty = self.variable_types.get(key) orelse try self.builder.allocType(.i64_type);
                            return self.builder.buildLoad(elem_alloca, elem_ty);
                        }
                    }
                }
                // Dynamic index: generate the index and try to resolve at runtime
                // For now, fall through to undef for dynamic indices
            }
        }

        _ = try self.generateExpr(ia.object);
        _ = try self.generateExpr(ia.index);
        return ir.Value{ .undef = {} };
    }

    fn generateCast(self: *IRGenerator, ce: *const ast.CastExpr) !ir.Value {
        const val = try self.generateExpr(ce.expr);
        const from_ty = try self.inferExprType(ce.expr);
        const to_ty = try self.mapTypeExpr(ce.target_type);
        // If types are the same, just return the value
        if (std.meta.eql(from_ty.*, to_ty.*)) return val;
        // Build a cast instruction
        return self.builder.buildCast(val, from_ty, to_ty);
    }

    fn generateStringInterpolation(self: *IRGenerator, si: *const ast.StringInterpolation) !ir.Value {
        const string_ty = try self.builder.allocType(.string_type);
        var result: ?ir.Value = null;

        for (si.parts) |part| {
            const part_val = switch (part) {
                .literal => |s| blk: {
                    const args = try self.allocator.dupe(ir.Value, &.{ir.Value{ .const_string = s }});
                    break :blk try self.builder.buildCall("dm_string_new", args, string_ty);
                },
                .expr => |e| blk: {
                    const val = try self.generateExpr(e);
                    // Auto-convert non-string expressions to string for interpolation
                    const ety = self.inferExprIRType(e);
                    if (ety == .i64_type or ety == .i32_type or ety == .i16_type or ety == .i8_type or
                        ety == .u64_type or ety == .u32_type or ety == .u16_type or ety == .u8_type)
                    {
                        const conv_args = try self.allocator.dupe(ir.Value, &.{val});
                        break :blk try self.builder.buildCall("dm_int_to_string", conv_args, string_ty);
                    } else if (ety == .f64_type or ety == .f32_type) {
                        const conv_args = try self.allocator.dupe(ir.Value, &.{val});
                        break :blk try self.builder.buildCall("dm_float_to_string", conv_args, string_ty);
                    } else if (ety == .bool_type) {
                        const conv_args = try self.allocator.dupe(ir.Value, &.{val});
                        break :blk try self.builder.buildCall("dm_bool_to_string", conv_args, string_ty);
                    }
                    break :blk val;
                },
            };

            if (result) |prev| {
                const args = try self.allocator.dupe(ir.Value, &.{ prev, part_val });
                result = try self.builder.buildCall("dm_string_concat", args, string_ty);
            } else {
                result = part_val;
            }
        }

        return result orelse ir.Value{ .const_string = "" };
    }

    fn generateMatchExpr(self: *IRGenerator, me: *const ast.MatchExpr) !ir.Value {
        const scrutinee_val = try self.generateExpr(me.scrutinee);
        // Check if scrutinee is a Result type
        const scrutinee_ty = try self.inferExprType(me.scrutinee);
        if (scrutinee_ty.* == .result_type) {
            return self.generateResultMatch(me, scrutinee_val, scrutinee_ty);
        }
        if (scrutinee_ty.* == .option_type) {
            return self.generateOptionMatch(me, scrutinee_val, scrutinee_ty);
        }

        // General match: integer/string/wildcard patterns with conditional branching
        const func = self.current_function orelse return ir.Value{ .undef = {} };
        const join_label = try self.nextLabel("match_end");
        // Allocate result storage if match is used as expression
        const result_alloca = try self.builder.buildAlloca(try self.builder.allocType(.i64_type));

        var i: usize = 0;
        while (i < me.arms.len) : (i += 1) {
            const arm = me.arms[i];
            const is_last = (i == me.arms.len - 1);

            switch (arm.pattern.kind) {
                .literal => |lit| {
                    // Compare scrutinee with literal value
                    const lit_val: ir.Value = switch (lit.kind) {
                        .int => |int_lit| blk: {
                            const v = std.fmt.parseInt(i64, int_lit.value, 10) catch 0;
                            break :blk .{ .const_int = v };
                        },
                        .string => |s| .{ .const_string = s.value },
                        .bool => |b| .{ .const_bool = b },
                        else => .{ .const_int = 0 },
                    };

                    const cmp = try self.builder.buildEq(scrutinee_val, lit_val);
                    const arm_label = try self.nextLabel("match_arm");
                    const next_label = if (is_last) join_label else try self.nextLabel("match_next");

                    // Check guard
                    if (arm.guard) |guard| {
                        const guard_label = try self.nextLabel("match_guard");
                        self.builder.buildCondBr(cmp, guard_label, next_label);
                        const guard_block = try func.addBlock(guard_label);
                        self.builder.setInsertBlock(guard_block);
                        const guard_val = try self.generateExpr(guard);
                        self.builder.buildCondBr(guard_val, arm_label, next_label);
                    } else {
                        self.builder.buildCondBr(cmp, arm_label, next_label);
                    }

                    const arm_block = try func.addBlock(arm_label);
                    self.builder.setInsertBlock(arm_block);
                    const arm_result = try self.generateMatchArmBody(arm);
                    if (arm_result != null) {
                        try self.builder.buildStore(result_alloca, arm_result.?);
                    }
                    // Only branch if block isn't already terminated (e.g. by return)
                    if (self.builder.current_block.?.terminator == null) {
                        self.builder.buildBr(join_label);
                    }

                    if (!is_last) {
                        const next_block = try func.addBlock(next_label);
                        self.builder.setInsertBlock(next_block);
                    }
                },
                .wildcard => {
                    // Wildcard: always matches — generate body directly
                    const arm_result = try self.generateMatchArmBody(arm);
                    if (arm_result != null) {
                        try self.builder.buildStore(result_alloca, arm_result.?);
                    }
                    if (self.builder.current_block.?.terminator == null) {
                        self.builder.buildBr(join_label);
                    }
                },
                .identifier => |ident| {
                    // Binding pattern: bind scrutinee to a variable
                    const ty = try self.inferExprType(me.scrutinee);
                    const alloca = try self.builder.buildAlloca(ty);
                    try self.builder.buildStore(alloca, scrutinee_val);
                    try self.variable_map.put(ident.name.name, alloca);
                    try self.variable_types.put(ident.name.name, ty);

                    if (arm.guard) |guard| {
                        // Identifier with guard: bind variable, evaluate guard, branch
                        const arm_label = try self.nextLabel("match_arm");
                        const next_label = if (is_last) join_label else try self.nextLabel("match_next");
                        const guard_val = try self.generateExpr(guard);
                        self.builder.buildCondBr(guard_val, arm_label, next_label);

                        const arm_block = try func.addBlock(arm_label);
                        self.builder.setInsertBlock(arm_block);
                        const arm_result = try self.generateMatchArmBody(arm);
                        if (arm_result != null) {
                            try self.builder.buildStore(result_alloca, arm_result.?);
                        }
                        if (self.builder.current_block.?.terminator == null) {
                            self.builder.buildBr(join_label);
                        }

                        if (!is_last) {
                            const next_block = try func.addBlock(next_label);
                            self.builder.setInsertBlock(next_block);
                        }
                    } else {
                        // Identifier without guard: always matches (like wildcard with binding)
                        const arm_result = try self.generateMatchArmBody(arm);
                        if (arm_result != null) {
                            try self.builder.buildStore(result_alloca, arm_result.?);
                        }
                        if (self.builder.current_block.?.terminator == null) {
                            self.builder.buildBr(join_label);
                        }
                    }
                },
                .enum_variant => |ev| {
                    // Check if this is a variant of a registered enum
                    const variant_name = ev.variant.name;
                    var found_tag: ?i64 = null;
                    var found_enum: ?EnumInfo = null;

                    // Search enum defs for this variant
                    var eit = self.enum_defs.iterator();
                    while (eit.next()) |entry| {
                        for (entry.value_ptr.variants) |variant| {
                            if (std.mem.eql(u8, variant.name, variant_name)) {
                                found_tag = variant.tag;
                                found_enum = entry.value_ptr.*;
                                break;
                            }
                        }
                        if (found_tag != null) break;
                    }

                    if (found_tag) |tag_val| {
                        // Extract tag from scrutinee and compare
                        const i32_ty = try self.builder.allocType(.i32_type);
                        const tag = try self.builder.buildExtractField(scrutinee_val, 0, i32_ty);
                        const tag_cmp = ir.Value{ .const_int = tag_val };
                        const cmp = try self.builder.buildEq(tag, tag_cmp);

                        const arm_label = try self.nextLabel("match_arm");
                        const next_label = if (is_last) join_label else try self.nextLabel("match_next");
                        self.builder.buildCondBr(cmp, arm_label, next_label);

                        const arm_block = try func.addBlock(arm_label);
                        self.builder.setInsertBlock(arm_block);

                        // Bind payload variables if present
                        if (ev.payload == .tuple) {
                            const tuple_pats = ev.payload.tuple;
                            if (tuple_pats.len > 0) {
                                const enum_info = found_enum.?;
                                // Find the payload field index for this variant
                                var field_idx: u32 = 1; // field 0 is tag
                                for (enum_info.variants) |v| {
                                    if (std.mem.eql(u8, v.name, variant_name)) break;
                                    if (v.payload_types) |pts| field_idx += @intCast(pts.len);
                                }
                                for (tuple_pats, 0..) |pat, pi| {
                                    if (pat.kind == .identifier) {
                                        const bind_name = pat.kind.identifier.name.name;
                                        // Determine payload type
                                        var payload_ty: *const ir.IRType = try self.builder.allocType(.i64_type);
                                        for (enum_info.variants) |v| {
                                            if (std.mem.eql(u8, v.name, variant_name)) {
                                                if (v.payload_types) |pts| {
                                                    if (pi < pts.len) payload_ty = pts[pi];
                                                }
                                                break;
                                            }
                                        }
                                        const payload_val = try self.builder.buildExtractField(scrutinee_val, field_idx + @as(u32, @intCast(pi)), payload_ty);
                                        const var_alloca = try self.builder.buildAlloca(payload_ty);
                                        try self.builder.buildStore(var_alloca, payload_val);
                                        try self.variable_map.put(bind_name, var_alloca);
                                        try self.variable_types.put(bind_name, payload_ty);
                                    }
                                }
                            }
                        }

                        const arm_result = try self.generateMatchArmBody(arm);
                        if (arm_result != null) {
                            try self.builder.buildStore(result_alloca, arm_result.?);
                        }
                        if (self.builder.current_block.?.terminator == null) {
                            self.builder.buildBr(join_label);
                        }

                        if (!is_last) {
                            const next_block = try func.addBlock(next_label);
                            self.builder.setInsertBlock(next_block);
                        }
                    } else {
                        // Unknown variant — treat as wildcard
                        const arm_result = try self.generateMatchArmBody(arm);
                        if (arm_result != null) {
                            try self.builder.buildStore(result_alloca, arm_result.?);
                        }
                        if (self.builder.current_block.?.terminator == null) {
                            self.builder.buildBr(join_label);
                        }
                    }
                },
                else => {
                    // Unknown pattern type — treat as wildcard for now
                    const arm_result = try self.generateMatchArmBody(arm);
                    if (arm_result != null) {
                        try self.builder.buildStore(result_alloca, arm_result.?);
                    }
                    if (self.builder.current_block.?.terminator == null) {
                        self.builder.buildBr(join_label);
                    }
                },
            }
        }

        const join_block = try func.addBlock(join_label);
        self.builder.setInsertBlock(join_block);
        return self.builder.buildLoad(result_alloca, try self.builder.allocType(.i64_type));
    }

    fn generateMatchArmBody(self: *IRGenerator, arm: *const ast.MatchArm) !?ir.Value {
        return switch (arm.body) {
            .expression => |e| try self.generateExpr(e),
            .block => |b| blk: {
                try self.generateBlock(b);
                break :blk null;
            },
        };
    }

    fn generatePipeline(self: *IRGenerator, pe: *const ast.PipelineExpr) !ir.Value {
        const val = try self.generateExpr(pe.left);
        switch (pe.right.kind) {
            .function_call => |call| {
                var args = std.ArrayList(ir.Value).init(self.allocator);
                defer args.deinit();
                try args.append(val);
                for (call.args) |arg| {
                    try args.append(try self.generateExpr(arg.value));
                }
                const callee_name = switch (call.function.kind) {
                    .identifier => |id| id.name,
                    else => return ir.Value{ .undef = {} },
                };
                const runtime_name = self.mapBuiltinName(callee_name);
                const ret_type = try self.inferCallReturnType(callee_name);
                const args_slice = try self.allocator.dupe(ir.Value, args.items);
                if (ret_type.* == .void_type) {
                    try self.builder.buildCallVoid(runtime_name, args_slice);
                    return ir.Value{ .undef = {} };
                }
                return self.builder.buildCall(runtime_name, args_slice, ret_type);
            },
            .identifier => |id| {
                const runtime_name = self.mapBuiltinName(id.name);
                const ret_type = try self.inferCallReturnType(id.name);
                const args_slice = try self.allocator.dupe(ir.Value, &.{val});
                if (ret_type.* == .void_type) {
                    try self.builder.buildCallVoid(runtime_name, args_slice);
                    return ir.Value{ .undef = {} };
                }
                return self.builder.buildCall(runtime_name, args_slice, ret_type);
            },
            else => return ir.Value{ .undef = {} },
        }
    }

    fn generateArrayLiteral(self: *IRGenerator, al: *const ast.ArrayLiteral) !ir.Value {
        _ = al;
        _ = self;
        return ir.Value{ .undef = {} };
    }

    // ========================================================================
    // Enum Support
    // ========================================================================

    /// Construct an enum variant with payload from a call: EnumName.Variant(args...)
    fn generateEnumVariantConstruct(self: *IRGenerator, enum_info: EnumInfo, variant: EnumVariantInfo, call: *const ast.FunctionCall) !ir.Value {
        var result: ir.Value = ir.Value{ .undef = {} };
        // Set tag
        result = try self.builder.buildInsertField(result, ir.Value{ .const_int = variant.tag }, 0, enum_info.ir_type);
        // Set payload fields
        var field_idx: u32 = 1; // skip tag
        // Skip to this variant's fields by counting prior variant payload fields
        for (enum_info.variants) |v| {
            if (std.mem.eql(u8, v.name, variant.name)) break;
            if (v.payload_types) |pts| field_idx += @intCast(pts.len);
        }
        for (call.args, 0..) |arg, ai| {
            const val = try self.generateExpr(arg.value);
            result = try self.builder.buildInsertField(result, val, field_idx + @as(u32, @intCast(ai)), enum_info.ir_type);
        }
        return result;
    }

    /// Handle enum literal expressions (e.g., `Color.Red`, `Shape.Circle(5.0)`)
    fn generateEnumLiteral(self: *IRGenerator, el: *const ast.EnumLiteral) !ir.Value {
        const variant_name = el.variant.name;

        // Find which enum this variant belongs to
        var found_enum: ?EnumInfo = null;
        var found_variant: ?EnumVariantInfo = null;

        // Check type_path first (e.g., Color.Red → type_path = "Color")
        if (el.type_path) |tp| {
            if (tp.segments.len > 0) {
                if (self.enum_defs.get(tp.segments[0].name)) |ei| {
                    for (ei.variants) |v| {
                        if (std.mem.eql(u8, v.name, variant_name)) {
                            found_enum = ei;
                            found_variant = v;
                            break;
                        }
                    }
                }
            }
        }

        // If not found via type_path, search all enums
        if (found_enum == null) {
            var it = self.enum_defs.iterator();
            while (it.next()) |entry| {
                for (entry.value_ptr.variants) |v| {
                    if (std.mem.eql(u8, v.name, variant_name)) {
                        found_enum = entry.value_ptr.*;
                        found_variant = v;
                        break;
                    }
                }
                if (found_enum != null) break;
            }
        }

        if (found_enum == null or found_variant == null) return ir.Value{ .undef = {} };
        const enum_info = found_enum.?;
        const variant = found_variant.?;

        // Build enum value with tag
        var result: ir.Value = ir.Value{ .undef = {} };
        result = try self.builder.buildInsertField(result, ir.Value{ .const_int = variant.tag }, 0, enum_info.ir_type);

        // Handle payload
        switch (el.payload) {
            .tuple => |payload_exprs| {
                var field_idx: u32 = 1; // skip tag
                for (enum_info.variants) |v| {
                    if (std.mem.eql(u8, v.name, variant_name)) break;
                    if (v.payload_types) |pts| field_idx += @intCast(pts.len);
                }
                for (payload_exprs, 0..) |pe, pi| {
                    const val = try self.generateExpr(pe);
                    result = try self.builder.buildInsertField(result, val, field_idx + @as(u32, @intCast(pi)), enum_info.ir_type);
                }
            },
            .none, .struct_fields => {},
        }

        return result;
    }

    /// Handle path expressions like Color::Red or Color.Red
    fn generatePathExpr(self: *IRGenerator, p: ast.Path) !ir.Value {
        if (p.segments.len == 2) {
            const enum_name = p.segments[0].name;
            const variant_name = p.segments[1].name;
            if (self.enum_defs.get(enum_name)) |enum_info| {
                for (enum_info.variants) |variant| {
                    if (std.mem.eql(u8, variant.name, variant_name)) {
                        var result: ir.Value = ir.Value{ .undef = {} };
                        result = try self.builder.buildInsertField(result, ir.Value{ .const_int = variant.tag }, 0, enum_info.ir_type);
                        return result;
                    }
                }
            }
        }
        // Single-segment path — might be a variant name like None
        if (p.segments.len == 1) {
            const name = p.segments[0].name;
            if (std.mem.eql(u8, name, "None")) {
                return self.generateOptionNone();
            }
        }
        return ir.Value{ .undef = {} };
    }

    // ========================================================================
    // Result[T, E] / Option[T] Support
    // ========================================================================

    /// Get the Option type from the current function's return type
    fn getOptionType(self: *IRGenerator) ?*const ir.IRType {
        if (self.current_return_type) |rt| {
            if (rt.* == .option_type) return rt;
        }
        return null;
    }

    /// Some(value) → Option struct { has_value=1, value=val }
    fn generateOptionSome(self: *IRGenerator, call: *const ast.FunctionCall) !ir.Value {
        if (call.args.len == 0) return ir.Value{ .undef = {} };
        const val = try self.generateExpr(call.args[0].value);
        const inner_ty = try self.inferExprType(call.args[0].value);

        // Build or get Option type
        const option_ty = self.getOptionType() orelse try self.builder.allocType(.{ .option_type = inner_ty });

        var result: ir.Value = ir.Value{ .undef = {} };
        result = try self.builder.buildInsertField(result, ir.Value{ .const_int = 1 }, 0, option_ty); // has_value=1
        result = try self.builder.buildInsertField(result, val, 1, option_ty); // value
        return result;
    }

    /// None → Option struct { has_value=0 }
    fn generateOptionNone(self: *IRGenerator) !ir.Value {
        const i64_ty = try self.builder.allocType(.i64_type);
        const option_ty = self.getOptionType() orelse try self.builder.allocType(.{ .option_type = i64_ty });

        var result: ir.Value = ir.Value{ .undef = {} };
        result = try self.builder.buildInsertField(result, ir.Value{ .const_int = 0 }, 0, option_ty); // has_value=0
        return result;
    }

    /// Get the Result type from the current function's return type, or from variable context
    fn getResultType(self: *IRGenerator) ?*const ir.IRType {
        if (self.current_return_type) |rt| {
            if (rt.* == .result_type) return rt;
        }
        return null;
    }

    /// Ok(value) → Result struct { tag=0, ok_value=value, err_value=undef }
    fn generateResultOk(self: *IRGenerator, call: *const ast.FunctionCall) !ir.Value {
        const result_ty = self.getResultType() orelse return ir.Value{ .undef = {} };
        if (call.args.len == 0) return ir.Value{ .undef = {} };

        const val = try self.generateExpr(call.args[0].value);
        const i32_ty = try self.builder.allocType(.i32_type);

        // Build: { tag=0, ok=val, err=undef }
        var result: ir.Value = ir.Value{ .undef = {} };
        result = try self.builder.buildInsertField(result, ir.Value{ .const_int = 0 }, 0, result_ty); // tag
        result = try self.builder.buildInsertField(result, val, 1, result_ty); // ok value
        _ = i32_ty;
        return result;
    }

    /// Err(value) → Result struct { tag=1, ok_value=undef, err_value=value }
    fn generateResultErr(self: *IRGenerator, call: *const ast.FunctionCall) !ir.Value {
        const result_ty = self.getResultType() orelse return ir.Value{ .undef = {} };
        if (call.args.len == 0) return ir.Value{ .undef = {} };

        const val = try self.generateExpr(call.args[0].value);

        // Build: { tag=1, ok=undef, err=val }
        var result: ir.Value = ir.Value{ .undef = {} };
        result = try self.builder.buildInsertField(result, ir.Value{ .const_int = 1 }, 0, result_ty); // tag
        result = try self.builder.buildInsertField(result, val, 2, result_ty); // err value
        return result;
    }

    /// `expr?` — unwrap Ok or early return Err
    fn generateErrorPropagate(self: *IRGenerator, ep: *const ast.ErrorPropagateExpr) !ir.Value {
        const operand = try self.generateExpr(ep.operand);
        const func = self.current_function orelse return ir.Value{ .undef = {} };

        // Get Result type from the operand (should be stored in variable_types)
        const operand_ty = try self.inferExprType(ep.operand);
        if (operand_ty.* != .result_type) {
            // Not a result type, just forward the value
            return operand;
        }

        // Extract tag
        const i32_ty = try self.builder.allocType(.i32_type);
        const tag = try self.builder.buildExtractField(operand, 0, i32_ty);

        // Compare tag == 0 (Ok)
        const zero = ir.Value{ .const_int = 0 };
        const is_ok = try self.builder.buildEq(tag, zero);

        // Create branch labels
        const ok_label = try self.nextLabel("try_ok");
        const err_label = try self.nextLabel("try_err");
        const cont_label = try self.nextLabel("try_cont");

        self.builder.buildCondBr(is_ok, ok_label, err_label);

        // Err path: extract err value and return it wrapped in Result
        const err_block = try func.addBlock(err_label);
        self.builder.setInsertBlock(err_block);
        const err_val = try self.builder.buildExtractField(operand, 2, operand_ty.result_type.err);

        // Build Result { tag=1, err=err_val } for the enclosing function's return type
        const fn_ret_ty = self.current_return_type orelse operand_ty;
        var ret_result: ir.Value = ir.Value{ .undef = {} };
        ret_result = try self.builder.buildInsertField(ret_result, ir.Value{ .const_int = 1 }, 0, fn_ret_ty);
        ret_result = try self.builder.buildInsertField(ret_result, err_val, 2, fn_ret_ty);
        self.builder.buildRet(ret_result);

        // Ok path: extract ok value
        const ok_block = try func.addBlock(ok_label);
        self.builder.setInsertBlock(ok_block);
        const ok_val = try self.builder.buildExtractField(operand, 1, operand_ty.result_type.ok);
        self.builder.buildBr(cont_label);

        // Continue — ok_val is the result of the expression
        const cont_block = try func.addBlock(cont_label);
        self.builder.setInsertBlock(cont_block);

        return ok_val;
    }

    /// Generate match expression for Result[T, E] patterns (Ok(v) / Err(e))
    fn generateResultMatch(self: *IRGenerator, me: *const ast.MatchExpr, scrutinee_val: ir.Value, result_ty: *const ir.IRType) !ir.Value {
        const func = self.current_function orelse return ir.Value{ .undef = {} };
        const i32_ty = try self.builder.allocType(.i32_type);

        // Extract tag from scrutinee
        const tag = try self.builder.buildExtractField(scrutinee_val, 0, i32_ty);

        // Create labels
        const ok_label = try self.nextLabel("match_ok");
        const err_label = try self.nextLabel("match_err");
        const join_label = try self.nextLabel("match_end");

        // Compare tag == 0 (Ok)
        const zero = ir.Value{ .const_int = 0 };
        const is_ok = try self.builder.buildEq(tag, zero);
        self.builder.buildCondBr(is_ok, ok_label, err_label);

        // Process arms — find Ok and Err arms
        var ok_arm: ?*const ast.MatchArm = null;
        var err_arm: ?*const ast.MatchArm = null;
        for (me.arms) |arm| {
            if (arm.pattern.kind == .enum_variant) {
                const ev = arm.pattern.kind.enum_variant;
                if (std.mem.eql(u8, ev.variant.name, "Ok")) ok_arm = arm;
                if (std.mem.eql(u8, ev.variant.name, "Err")) err_arm = arm;
            }
        }

        // Ok arm
        const ok_block = try func.addBlock(ok_label);
        self.builder.setInsertBlock(ok_block);
        if (ok_arm) |arm| {
            // Bind payload variable
            const ok_val = try self.builder.buildExtractField(scrutinee_val, 1, result_ty.result_type.ok);
            if (arm.pattern.kind.enum_variant.payload == .tuple) {
                const tuple_pats = arm.pattern.kind.enum_variant.payload.tuple;
                if (tuple_pats.len > 0) {
                    if (tuple_pats[0].kind == .identifier) {
                        const bind_name = tuple_pats[0].kind.identifier.name.name;
                        const var_alloca = try self.builder.buildAlloca(result_ty.result_type.ok);
                        try self.builder.buildStore(var_alloca, ok_val);
                        try self.variable_map.put(bind_name, var_alloca);
                        try self.variable_types.put(bind_name, result_ty.result_type.ok);
                    }
                }
            }
            // Generate arm body
            switch (arm.body) {
                .expression => |e| _ = try self.generateExpr(e),
                .block => |b| try self.generateBlock(b),
            }
        }
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) self.builder.buildBr(join_label);
        }

        // Err arm
        const err_block = try func.addBlock(err_label);
        self.builder.setInsertBlock(err_block);
        if (err_arm) |arm| {
            // Bind payload variable
            const err_val = try self.builder.buildExtractField(scrutinee_val, 2, result_ty.result_type.err);
            if (arm.pattern.kind.enum_variant.payload == .tuple) {
                const tuple_pats = arm.pattern.kind.enum_variant.payload.tuple;
                if (tuple_pats.len > 0) {
                    if (tuple_pats[0].kind == .identifier) {
                        const bind_name = tuple_pats[0].kind.identifier.name.name;
                        const var_alloca = try self.builder.buildAlloca(result_ty.result_type.err);
                        try self.builder.buildStore(var_alloca, err_val);
                        try self.variable_map.put(bind_name, var_alloca);
                        try self.variable_types.put(bind_name, result_ty.result_type.err);
                    }
                }
            }
            // Generate arm body
            switch (arm.body) {
                .expression => |e| _ = try self.generateExpr(e),
                .block => |b| try self.generateBlock(b),
            }
        }
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) self.builder.buildBr(join_label);
        }

        // Join block
        const join_block = try func.addBlock(join_label);
        self.builder.setInsertBlock(join_block);

        return ir.Value{ .undef = {} };
    }

    /// Generate match expression for Option[T] patterns (Some(v) / None)
    fn generateOptionMatch(self: *IRGenerator, me: *const ast.MatchExpr, scrutinee_val: ir.Value, option_ty: *const ir.IRType) !ir.Value {
        const func = self.current_function orelse return ir.Value{ .undef = {} };
        const i32_ty = try self.builder.allocType(.i32_type);

        // Option layout: { has_value: i32, value: T }
        const has_value = try self.builder.buildExtractField(scrutinee_val, 0, i32_ty);

        const some_label = try self.nextLabel("match_some");
        const none_label = try self.nextLabel("match_none");
        const join_label = try self.nextLabel("match_end");

        const zero = ir.Value{ .const_int = 0 };
        const is_some = try self.builder.buildNe(has_value, zero);
        self.builder.buildCondBr(is_some, some_label, none_label);

        var some_arm: ?*const ast.MatchArm = null;
        var none_arm: ?*const ast.MatchArm = null;
        for (me.arms) |arm| {
            if (arm.pattern.kind == .enum_variant) {
                const ev = arm.pattern.kind.enum_variant;
                if (std.mem.eql(u8, ev.variant.name, "Some")) some_arm = arm;
                if (std.mem.eql(u8, ev.variant.name, "None")) none_arm = arm;
            } else if (arm.pattern.kind == .identifier) {
                const name = arm.pattern.kind.identifier.name.name;
                if (std.mem.eql(u8, name, "None")) none_arm = arm;
            } else if (arm.pattern.kind == .wildcard) {
                if (none_arm == null) none_arm = arm;
            }
        }

        // Some arm
        const some_block = try func.addBlock(some_label);
        self.builder.setInsertBlock(some_block);
        if (some_arm) |arm| {
            const inner_val = try self.builder.buildExtractField(scrutinee_val, 1, option_ty.option_type);
            if (arm.pattern.kind == .enum_variant) {
                if (arm.pattern.kind.enum_variant.payload == .tuple) {
                    const tuple_pats = arm.pattern.kind.enum_variant.payload.tuple;
                    if (tuple_pats.len > 0 and tuple_pats[0].kind == .identifier) {
                        const bind_name = tuple_pats[0].kind.identifier.name.name;
                        const var_alloca = try self.builder.buildAlloca(option_ty.option_type);
                        try self.builder.buildStore(var_alloca, inner_val);
                        try self.variable_map.put(bind_name, var_alloca);
                        try self.variable_types.put(bind_name, option_ty.option_type);
                    }
                }
            }
            switch (arm.body) {
                .expression => |e| _ = try self.generateExpr(e),
                .block => |b| try self.generateBlock(b),
            }
        }
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) self.builder.buildBr(join_label);
        }

        // None arm
        const none_block = try func.addBlock(none_label);
        self.builder.setInsertBlock(none_block);
        if (none_arm) |arm| {
            switch (arm.body) {
                .expression => |e| _ = try self.generateExpr(e),
                .block => |b| try self.generateBlock(b),
            }
        }
        if (self.builder.current_block) |blk| {
            if (blk.terminator == null) self.builder.buildBr(join_label);
        }

        const join_block = try func.addBlock(join_label);
        self.builder.setInsertBlock(join_block);
        return ir.Value{ .undef = {} };
    }

    // ========================================================================
    // Lambda / Function Pointer Support
    // ========================================================================

    fn generateLambda(self: *IRGenerator, lam: *const ast.LambdaExpr) !ir.Value {
        // Generate a unique name for the lambda function
        const lambda_name = try std.fmt.allocPrint(self.allocator, "__dm_lambda_{d}", .{self.lambda_counter});
        self.lambda_counter += 1;

        // Build parameter list for the lambda function
        var params = std.ArrayList(ir.Param).init(self.allocator);
        defer params.deinit();
        for (lam.params) |p| {
            const param_ty = if (p.type_expr) |te|
                try self.mapTypeExpr(te)
            else
                try self.builder.allocType(.i64_type);
            try params.append(.{ .name = p.name.name, .ty = param_ty });
        }

        // Determine return type
        const ret_ty = if (lam.return_type) |rt|
            try self.mapTypeExpr(rt)
        else
            try self.builder.allocType(.i64_type);

        // Save current generation context
        const saved_function = self.current_function;
        const saved_block = self.builder.current_block;
        const saved_return_type = self.current_return_type;
        const saved_var_map = self.variable_map;
        const saved_var_types = self.variable_types;
        const saved_break = self.break_target;
        const saved_continue = self.continue_target;

        // Collect captured variable names from the lambda body
        // (identifiers in the body that are not parameters and exist in outer scope)
        var param_set = std.StringHashMap(void).init(self.allocator);
        defer param_set.deinit();
        for (lam.params) |p| {
            try param_set.put(p.name.name, {});
        }

        // Create the lambda function in the IR module — add captured variables as extra params
        var captured_vars = std.ArrayList(struct { name: []const u8, ty: *const ir.IRType }).init(self.allocator);
        defer captured_vars.deinit();
        {
            // Scan outer scope for variables that might be captured
            var it = saved_var_map.iterator();
            while (it.next()) |entry| {
                if (param_set.contains(entry.key_ptr.*)) continue;
                // We'll add all outer variables as extra parameters for simplicity
                // A more precise approach would walk the AST to find only used variables
                try captured_vars.append(.{
                    .name = entry.key_ptr.*,
                    .ty = saved_var_types.get(entry.key_ptr.*) orelse try self.builder.allocType(.i64_type),
                });
            }
        }

        // Build params: original params + captured variables
        for (captured_vars.items) |cap| {
            try params.append(.{ .name = cap.name, .ty = cap.ty });
        }

        // Create fresh variable maps for the lambda scope
        self.variable_map = std.StringHashMap(ir.Value).init(self.allocator);
        self.variable_types = std.StringHashMap(*const ir.IRType).init(self.allocator);

        const params_slice = try self.allocator.dupe(ir.Param, params.items);
        var lambda_func = try self.module.addFunction(lambda_name, params_slice, ret_ty);
        lambda_func.is_extern = false;
        self.current_function = lambda_func;
        self.current_return_type = ret_ty;
        self.break_target = null;
        self.continue_target = null;

        // Create entry block
        const entry_block = try lambda_func.addBlock("entry");
        self.builder.setInsertBlock(entry_block);

        // Create allocas for all parameters (original + captured)
        for (params_slice, 0..) |p, i| {
            const alloca = try self.builder.buildAlloca(p.ty);
            try self.builder.buildStore(alloca, ir.Value{ .param_ref = @intCast(i) });
            try self.variable_map.put(p.name, alloca);
            try self.variable_types.put(p.name, p.ty);
        }

        // Generate lambda body
        switch (lam.body) {
            .expression => |expr| {
                const val = try self.generateExpr(expr);
                self.builder.buildRet(val);
            },
            .block => |block| {
                try self.generateBlock(block);
                // If block didn't end with a return, add a void return
                if (self.builder.current_block) |blk| {
                    if (blk.terminator == null) {
                        if (block.result) |result| {
                            const val = try self.generateExpr(result);
                            self.builder.buildRet(val);
                        } else {
                            self.builder.buildRetVoid();
                        }
                    }
                }
            },
        }

        // Restore context
        self.current_function = saved_function;
        self.builder.current_block = saved_block;
        self.current_return_type = saved_return_type;
        self.variable_map = saved_var_map;
        self.variable_types = saved_var_types;
        self.break_target = saved_break;
        self.continue_target = saved_continue;

        // Store capture info for use at call sites
        if (captured_vars.items.len > 0) {
            var cap_names = try self.allocator.alloc([]const u8, captured_vars.items.len);
            for (captured_vars.items, 0..) |cap, i| {
                cap_names[i] = cap.name;
            }
            try self.lambda_captures.put(lambda_name, cap_names);
        }

        // Return a reference to the lambda function
        return ir.Value{ .global_ref = lambda_name };
    }

    // ========================================================================
    // Type Inference (from AST expressions)
    // ========================================================================

    fn inferExprType(self: *IRGenerator, expr: *const ast.Expr) !*const ir.IRType {
        return switch (expr.kind) {
            .literal => |lit| switch (lit.kind) {
                .int => self.builder.allocType(.i64_type),
                .float => self.builder.allocType(.f64_type),
                .bool => self.builder.allocType(.bool_type),
                .string => self.builder.allocType(.string_type),
                .null_lit => self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) }),
                .char => self.builder.allocType(.i8_type),
            },
            .identifier => |id| blk: {
                // None as bare identifier is Option[i64]
                if (std.mem.eql(u8, id.name, "None")) {
                    const i64_ty = try self.builder.allocType(.i64_type);
                    break :blk self.builder.allocType(.{ .option_type = i64_ty });
                }
                if (self.variable_types.get(id.name)) |ty| {
                    // If the stored type is i64 but we know this var holds a struct, use the struct type
                    if (ty.* == .i64_type) {
                        if (self.var_struct_types.get(id.name)) |sname| {
                            if (self.struct_defs.get(sname)) |sd| {
                                break :blk sd.ir_type;
                            }
                        }
                    }
                    break :blk ty;
                }
                // Fallback: check var_struct_types for struct-typed variables
                if (self.var_struct_types.get(id.name)) |sname| {
                    if (self.struct_defs.get(sname)) |sd| {
                        break :blk sd.ir_type;
                    }
                }
                break :blk self.builder.allocType(.i64_type);
            },
            .binary => |bin| switch (bin.op) {
                .eq, .ne, .lt, .le, .gt, .ge, .@"and", .@"or" => self.builder.allocType(.bool_type),
                else => self.inferExprType(bin.left),
            },
            .unary => |un| switch (un.op) {
                .not => self.builder.allocType(.bool_type),
                else => self.inferExprType(un.operand),
            },
            .function_call => |call| blk: {
                const callee_name = switch (call.function.kind) {
                    .identifier => |id| id.name,
                    .path => |p| {
                        // Enum variant constructor: EnumName.Variant(...)
                        if (p.segments.len == 2) {
                            if (self.enum_defs.get(p.segments[0].name)) |ei| {
                                break :blk ei.ir_type;
                            }
                        }
                        break :blk try self.builder.allocType(.i64_type);
                    },
                    else => break :blk self.builder.allocType(.i64_type),
                };
                // Some(value) returns Option[typeof(value)]
                if (std.mem.eql(u8, callee_name, "Some")) {
                    if (call.args.len > 0) {
                        const inner = try self.inferExprType(call.args[0].value);
                        break :blk self.builder.allocType(.{ .option_type = inner });
                    }
                }
                if (std.mem.eql(u8, callee_name, "None")) {
                    const i64_ty = try self.builder.allocType(.i64_type);
                    break :blk self.builder.allocType(.{ .option_type = i64_ty });
                }
                if (std.mem.eql(u8, callee_name, "Ok") or std.mem.eql(u8, callee_name, "Err")) {
                    if (self.current_return_type) |rt| {
                        if (rt.* == .result_type) break :blk rt;
                    }
                }
                // Check if this is a generic function call — resolve return type from declaration
                if (self.generic_fn_decls.get(callee_name)) |generic_decl| {
                    break :blk self.inferGenericReturnType(call, generic_decl);
                }
                // SIMD builtins: infer return types
                if (std.mem.startsWith(u8, callee_name, "simd_splat_"))
                    break :blk self.parseSimdTypeName(callee_name["simd_splat_".len..]);
                if (std.mem.startsWith(u8, callee_name, "simd_set_"))
                    break :blk self.parseSimdTypeName(callee_name["simd_set_".len..]);
                if (std.mem.eql(u8, callee_name, "simd_add") or
                    std.mem.eql(u8, callee_name, "simd_sub") or
                    std.mem.eql(u8, callee_name, "simd_mul") or
                    std.mem.eql(u8, callee_name, "simd_div"))
                {
                    if (call.args.len > 0)
                        break :blk self.inferSimdVecType(call.args[0].value);
                }
                if (std.mem.eql(u8, callee_name, "simd_extract")) {
                    if (call.args.len > 0)
                        break :blk self.inferSimdExtractType(call.args[0].value);
                }
                break :blk self.inferCallReturnType(callee_name);
            },
            .pipeline => |pe| blk: {
                // Pipeline x |> f has the type of f(x)
                const func_name: ?[]const u8 = switch (pe.right.kind) {
                    .function_call => |fc| if (fc.function.kind == .identifier)
                        fc.function.kind.identifier.name
                    else
                        null,
                    .identifier => |id| id.name,
                    else => null,
                };
                if (func_name) |fname| {
                    break :blk self.inferCallReturnType(fname);
                }
                break :blk self.builder.allocType(.i64_type);
            },
            .string_interpolation => self.builder.allocType(.string_type),
            .grouped => |inner| self.inferExprType(inner),
            .comptime_expr => |ct| self.inferExprType(ct.expr),
            .struct_literal => |sl| blk: {
                if (sl.type_path) |tp| {
                    if (tp.segments.len > 0) {
                        if (self.struct_defs.get(tp.segments[0].name)) |sd| {
                            break :blk sd.ir_type;
                        }
                    }
                }
                break :blk self.builder.allocType(.i64_type);
            },
            .field_access => |fa| blk: {
                const base_ty = try self.inferExprType(fa.object);
                // Option[T] field access type inference
                if (base_ty.* == .option_type) {
                    if (std.mem.eql(u8, fa.field.name, "has_value")) break :blk self.builder.allocType(.bool_type);
                    if (std.mem.eql(u8, fa.field.name, "value")) break :blk base_ty.option_type;
                }
                // Result[T, E] field access type inference
                if (base_ty.* == .result_type) {
                    if (std.mem.eql(u8, fa.field.name, "is_ok") or std.mem.eql(u8, fa.field.name, "is_err"))
                        break :blk self.builder.allocType(.bool_type);
                    if (std.mem.eql(u8, fa.field.name, "ok_value") or std.mem.eql(u8, fa.field.name, "ok") or std.mem.eql(u8, fa.field.name, "value"))
                        break :blk base_ty.result_type.ok;
                    if (std.mem.eql(u8, fa.field.name, "err_value") or std.mem.eql(u8, fa.field.name, "err") or std.mem.eql(u8, fa.field.name, "error"))
                        break :blk base_ty.result_type.err;
                }
                if (base_ty.* == .struct_type) {
                    for (base_ty.struct_type.fields) |field| {
                        if (std.mem.eql(u8, field.name, fa.field.name)) {
                            break :blk field.ty;
                        }
                    }
                }
                break :blk self.builder.allocType(.i64_type);
            },
            .error_propagate => |ep| blk: {
                // expr? on Result[T, E] yields T
                const inner = try self.inferExprType(ep.operand);
                if (inner.* == .result_type) break :blk inner.result_type.ok;
                break :blk self.builder.allocType(.i64_type);
            },
            .lambda => |lam| blk: {
                // Build fn_type from lambda signature
                var param_types = std.ArrayList(*const ir.IRType).init(self.allocator);
                defer param_types.deinit();
                for (lam.params) |p| {
                    const pt = if (p.type_expr) |te|
                        try self.mapTypeExpr(te)
                    else
                        try self.builder.allocType(.i64_type);
                    try param_types.append(pt);
                }
                const ret = if (lam.return_type) |rt|
                    try self.mapTypeExpr(rt)
                else
                    try self.builder.allocType(.i64_type);
                const params_slice = try self.allocator.dupe(*const ir.IRType, param_types.items);
                break :blk self.builder.allocType(.{ .fn_type = .{ .params = params_slice, .ret = ret } });
            },
            .cast => |ce| self.mapTypeExpr(ce.target_type),
            .path => |p| blk: {
                // Path expression for enum variant: EnumName.Variant
                if (p.segments.len == 2) {
                    if (self.enum_defs.get(p.segments[0].name)) |ei| {
                        break :blk ei.ir_type;
                    }
                }
                break :blk self.builder.allocType(.i64_type);
            },
            .await_expr => |ae| self.inferExprType(ae.operand),
            .enum_literal => blk: {
                // .variant_name — search enum defs
                break :blk self.builder.allocType(.i64_type);
            },
            .index_access => |ia| blk: {
                // Indexing a list returns the element type; indexing a string returns string
                if (ia.object.kind == .identifier) {
                    const obj_name = ia.object.kind.identifier.name;
                    if (self.list_elem_kinds.get(obj_name)) |ek| {
                        break :blk switch (ek) {
                            .int64 => try self.builder.allocType(.i64_type),
                            .float64 => try self.builder.allocType(.f64_type),
                            .string => try self.builder.allocType(.string_type),
                            .other => if (self.list_elem_types.get(obj_name)) |et| et else try self.builder.allocType(.i64_type),
                        };
                    }
                    if (self.variable_types.get(obj_name)) |vty| {
                        if (vty.* == .string_type) break :blk try self.builder.allocType(.string_type);
                    }
                }
                // Field access list index: obj.field[i] where field is a List[T]
                if (ia.object.kind == .field_access) {
                    const field_ek = self.inferFieldAccessListKind(ia.object);
                    if (field_ek) |ek| {
                        const fa_ia = ia.object.kind.field_access;
                        const fa_ia_obj_name = if (fa_ia.object.kind == .identifier) fa_ia.object.kind.identifier.name else "";
                        const fa_ia_struct_name = self.var_struct_types.get(fa_ia_obj_name) orelse "";
                        break :blk switch (ek) {
                            .int64 => try self.builder.allocType(.i64_type),
                            .float64 => try self.builder.allocType(.f64_type),
                            .string => try self.builder.allocType(.string_type),
                            .other => self.getFieldListElemType(fa_ia_struct_name, fa_ia.field.name) orelse try self.builder.allocType(.i64_type),
                        };
                    }
                }
                // Default: infer from the object type
                const obj_ty = try self.inferExprType(ia.object);
                if (obj_ty.* == .string_type) break :blk try self.builder.allocType(.string_type);
                break :blk try self.builder.allocType(.i64_type);
            },
            .method_call => |mc| blk: {
                // Check if this is an enum dot constructor: EnumName.Variant(args)
                if (mc.object.kind == .identifier) {
                    const obj_name = mc.object.kind.identifier.name;
                    if (self.enum_defs.get(obj_name)) |enum_info| {
                        break :blk enum_info.ir_type;
                    }
                }
                // Method calls: infer return type based on method name
                const method_name = mc.method.name;
                if (std.mem.eql(u8, method_name, "len")) break :blk try self.builder.allocType(.i64_type);
                if (std.mem.eql(u8, method_name, "contains")) break :blk try self.builder.allocType(.bool_type);
                if (std.mem.eql(u8, method_name, "pop")) {
                    // pop on a list returns the element type
                    if (mc.object.kind == .identifier) {
                        const obj_name_pop = mc.object.kind.identifier.name;
                        if (self.list_elem_kinds.get(obj_name_pop)) |ek| {
                            break :blk switch (ek) {
                                .int64 => try self.builder.allocType(.i64_type),
                                .float64 => try self.builder.allocType(.f64_type),
                                .string => try self.builder.allocType(.string_type),
                                .other => try self.builder.allocType(.i64_type),
                            };
                        }
                    }
                }
                break :blk try self.builder.allocType(.i64_type);
            },
            else => self.builder.allocType(.i64_type),
        };
    }

    /// Infer the return type of a generic function call by matching type parameters
    /// to concrete argument types.
    fn inferGenericReturnType(self: *IRGenerator, call: *const ast.FunctionCall, generic_decl: *const ast.FunctionDecl) anyerror!*const ir.IRType {
        const gparams = generic_decl.generic_params orelse return self.builder.allocType(.i64_type);
        const ret_type_expr = generic_decl.return_type orelse return self.builder.allocType(.void_type);

        // Build type parameter name → concrete type mapping
        var type_map = std.StringHashMap(*const ir.IRType).init(self.allocator);
        defer type_map.deinit();

        if (call.generic_args) |gargs| {
            // Explicit type arguments: identity[string]("hello")
            for (gparams, 0..) |gp, i| {
                if (i < gargs.len) {
                    try type_map.put(gp.name.name, try self.mapTypeExpr(gargs[i]));
                }
            }
        } else {
            // Implicit: match parameter type names to argument expression types
            for (generic_decl.params, 0..) |param, pi| {
                if (pi < call.args.len) {
                    if (param.type_expr.kind == .named) {
                        const pnamed = param.type_expr.kind.named;
                        if (pnamed.path.segments.len == 1) {
                            const pname = pnamed.path.segments[0].name;
                            for (gparams) |gp| {
                                if (std.mem.eql(u8, gp.name.name, pname)) {
                                    const concrete_ty = try self.inferExprType(call.args[pi].value);
                                    try type_map.put(pname, concrete_ty);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Resolve return type using the type map
        if (ret_type_expr.kind == .named) {
            const rnamed = ret_type_expr.kind.named;
            if (rnamed.path.segments.len == 1) {
                if (type_map.get(rnamed.path.segments[0].name)) |concrete| {
                    return concrete;
                }
            }
        }

        // Fall back to regular type expression mapping
        return self.mapTypeExpr(ret_type_expr);
    }

    // Note: generateSimdBuiltin is defined earlier in the file with full SIMD instruction support
};

/// Get a short suffix string for a type (used for monomorphization name mangling)
fn irTypeSuffix(ty: *const ir.IRType) []const u8 {
    return switch (ty.*) {
        .i8_type => "i8",
        .i16_type => "i16",
        .i32_type => "i32",
        .i64_type => "int",
        .u8_type => "u8",
        .u16_type => "u16",
        .u32_type => "u32",
        .u64_type => "u64",
        .f32_type => "f32",
        .f64_type => "float",
        .bool_type => "bool",
        .string_type => "string",
        .void_type => "void",
        else => "unknown",
    };
}

// ========================================================================
// Tests
// ========================================================================

test "IR generator creates module" {
    const allocator = std.testing.allocator;
    var gen = IRGenerator.init(allocator);
    defer gen.deinit();

    try std.testing.expectEqual(@as(usize, 0), gen.module.functions.items.len);
}
