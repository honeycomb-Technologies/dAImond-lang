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

/// Element type category for typed list operations
const ListElemKind = enum { int64, float64, string, other };

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
    /// Current function being generated
    current_function: ?*ir.Function = null,
    /// Counter for generating unique block labels
    label_counter: u32 = 0,
    /// Break/continue targets for loop control flow
    break_target: ?[]const u8 = null,
    continue_target: ?[]const u8 = null,

    pub fn init(allocator: Allocator) IRGenerator {
        return .{
            .allocator = allocator,
            .module = ir.Module.init(allocator),
            .builder = ir.IRBuilder.init(allocator),
            .variable_map = std.StringHashMap(ir.Value).init(allocator),
            .variable_types = std.StringHashMap(*const ir.IRType).init(allocator),
            .struct_defs = std.StringHashMap(StructInfo).init(allocator),
            .list_elem_kinds = std.StringHashMap(ListElemKind).init(allocator),
        };
    }

    pub fn deinit(self: *IRGenerator) void {
        self.module.deinit();
        self.builder.deinit();
        self.variable_map.deinit();
        self.variable_types.deinit();
        self.struct_defs.deinit();
        self.list_elem_kinds.deinit();
    }

    pub fn getModule(self: *IRGenerator) *ir.Module {
        return &self.module;
    }

    fn nextLabel(self: *IRGenerator, prefix: []const u8) ![]const u8 {
        const label = try std.fmt.allocPrint(self.allocator, "{s}_{}", .{ prefix, self.label_counter });
        self.label_counter += 1;
        return label;
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
            .reference => self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) }),
            .slice => |sl| blk: {
                const inner = try self.mapTypeExpr(sl.element_type);
                _ = inner;
                break :blk self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
            },
            .tuple => self.builder.allocType(.void_type),
            .trait_object => self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) }),
            .self_type => self.builder.allocType(.i64_type),
        };
    }

    fn mapNamedType(self: *IRGenerator, named: *const ast.NamedType) anyerror!*const ir.IRType {
        if (named.path.segments.len == 1) {
            const name = named.path.segments[0].name;
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

            // User-defined struct types
            if (self.struct_defs.get(name)) |sd| {
                return sd.ir_type;
            }

            // Generic types
            if (std.mem.eql(u8, name, "Option") or std.mem.eql(u8, name, "Box")) {
                if (named.generic_args) |args| {
                    if (args.len > 0) {
                        const inner = try self.mapTypeExpr(args[0]);
                        return self.builder.allocType(.{ .option_type = inner });
                    }
                }
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
                return self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
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
        // Phase 0: Register struct definitions
        for (source_file.declarations) |decl| {
            switch (decl.kind) {
                .struct_def => |sd| try self.registerStruct(sd),
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
                    for (impl_blk.items) |item| {
                        switch (item.kind) {
                            .function => |func_decl| try self.declareFunction(func_decl),
                            .constant => |const_decl| try self.generateConstant(const_decl),
                            else => {},
                        }
                    }
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
                    for (impl_blk.items) |item| {
                        switch (item.kind) {
                            .function => |func_decl| {
                                if (func_decl.body != null and !func_decl.is_extern) {
                                    try self.generateFunction(func_decl);
                                }
                            },
                            else => {},
                        }
                    }
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
        var params = std.ArrayList(ir.Param).init(self.allocator);
        defer params.deinit();

        for (func_decl.params) |param| {
            const ty = try self.mapTypeExpr(param.type_expr);
            try params.append(.{ .name = param.name.name, .ty = ty });
        }

        const ret_type = if (func_decl.return_type) |rt|
            try self.mapTypeExpr(rt)
        else
            try self.builder.allocType(.void_type);

        const params_slice = try self.allocator.dupe(ir.Param, params.items);

        const name = if (std.mem.eql(u8, func_decl.name.name, "main"))
            "main"
        else
            func_decl.name.name;

        var func = try self.module.addFunction(name, params_slice, ret_type);
        func.is_extern = func_decl.is_extern;
        func.is_async = func_decl.is_async;
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

    fn evalConstExpr(self: *IRGenerator, expr: *const ast.Expr) ?ir.Value {
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
            .comptime_expr => |ct| self.evalConstExpr(ct.expr),
            .grouped => |inner| self.evalConstExpr(inner),
            .identifier => |id| blk: {
                // Look up previously defined constants
                for (self.module.globals.items) |g| {
                    if (std.mem.eql(u8, g.name, id.name)) {
                        if (g.init_value) |v| break :blk v;
                    }
                }
                break :blk null;
            },
            .binary => |bin| blk: {
                const lhs = self.evalConstExpr(bin.left) orelse break :blk null;
                const rhs = self.evalConstExpr(bin.right) orelse break :blk null;
                // Integer arithmetic
                if (lhs == .const_int and rhs == .const_int) {
                    const a = lhs.const_int;
                    const b = rhs.const_int;
                    break :blk switch (bin.op) {
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
                    break :blk switch (bin.op) {
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
                    break :blk switch (bin.op) {
                        .@"and" => ir.Value{ .const_bool = a and b },
                        .@"or" => ir.Value{ .const_bool = a or b },
                        .eq => ir.Value{ .const_bool = a == b },
                        .ne => ir.Value{ .const_bool = a != b },
                        else => null,
                    };
                }
                break :blk null;
            },
            .unary => |un| blk: {
                const operand = self.evalConstExpr(un.operand) orelse break :blk null;
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
            else => null,
        };
    }

    // ========================================================================
    // Function Body Generation
    // ========================================================================

    fn generateFunction(self: *IRGenerator, func_decl: *const ast.FunctionDecl) !void {
        const name = if (std.mem.eql(u8, func_decl.name.name, "main"))
            "main"
        else
            func_decl.name.name;

        var target_func: ?*ir.Function = null;
        for (self.module.functions.items) |*f| {
            if (std.mem.eql(u8, f.name, name)) {
                target_func = f;
                break;
            }
        }
        const func = target_func orelse return;

        self.current_function = func;
        self.builder.setFunction(func);
        self.variable_map.clearRetainingCapacity();
        self.variable_types.clearRetainingCapacity();

        const entry = try func.addBlock("entry");
        self.builder.setInsertBlock(entry);

        for (func_decl.params, 0..) |param, i| {
            const ty = try self.mapTypeExpr(param.type_expr);
            const alloca = try self.builder.buildAlloca(ty);
            try self.builder.buildStore(alloca, .{ .param_ref = @intCast(i) });
            try self.variable_map.put(param.name.name, alloca);
            try self.variable_types.put(param.name.name, ty);
        }

        const body = func_decl.body orelse return;
        switch (body) {
            .block => |block| {
                try self.generateBlock(block);

                if (self.builder.current_block) |blk| {
                    if (blk.terminator == null) {
                        if (func.return_type.* == .void_type) {
                            self.builder.buildRetVoid();
                        } else {
                            if (std.mem.eql(u8, name, "main")) {
                                self.builder.buildRet(.{ .const_int = 0 });
                            } else {
                                self.builder.buildRetVoid();
                            }
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
            const val = try self.generateExpr(result);
            if (self.builder.current_block) |blk| {
                if (blk.terminator == null) {
                    const func = self.current_function orelse return;
                    if (func.return_type.* != .void_type) {
                        self.builder.buildRet(val);
                    }
                }
            }
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

                    // List is a struct: { ptr, i64, i64 } — same layout as dm_list_int64/dm_list_dm_string
                    const list_name: []const u8 = switch (elem_kind) {
                        .int64 => "dm_list_int64",
                        .float64 => "dm_list_double",
                        .string => "dm_list_dm_string",
                        .other => "dm_list_int64",
                    };
                    const ptr_field_ty = try self.builder.allocType(.{ .ptr = try self.builder.allocType(.i8_type) });
                    const i64_field_ty = try self.builder.allocType(.i64_type);
                    const list_fields = try self.allocator.dupe(ir.Field, &.{
                        .{ .name = "data", .ty = ptr_field_ty },
                        .{ .name = "len", .ty = i64_field_ty },
                        .{ .name = "capacity", .ty = i64_field_ty },
                    });
                    const list_ty = try self.builder.allocType(.{ .struct_type = .{
                        .name = list_name,
                        .fields = list_fields,
                    } });
                    try self.module.struct_defs.put(list_name, list_ty);

                    const alloca = try self.builder.buildAlloca(list_ty);

                    // Initialize list: call dm_list_TYPE_new(&list)
                    const new_fn = switch (elem_kind) {
                        .int64 => "dm_list_int64_new",
                        .float64 => "dm_list_double_new",
                        .string => "dm_list_string_new",
                        .other => "dm_list_int64_new",
                    };
                    const new_args = try self.allocator.dupe(ir.Value, &.{alloca});
                    try self.builder.buildCallVoid(new_fn, new_args);

                    try self.variable_map.put(var_name, alloca);
                    try self.variable_types.put(var_name, list_ty);
                    return;
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
            const val = try self.generateExpr(val_expr);
            try self.builder.buildStore(alloca, val);
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
                        if (std.mem.eql(u8, name, "string")) return .string;
                    }
                }
            }
        }
        return .int64; // default
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

        // Get the iterator variable name to look up list element kind
        const iter_var_name: ?[]const u8 = if (fl.iterator.kind == .identifier)
            fl.iterator.kind.identifier.name
        else
            null;

        const list_kind: ListElemKind = if (iter_var_name) |name|
            self.list_elem_kinds.get(name) orelse .int64
        else
            .int64;

        // Get pointer to the list (alloca, not loaded value)
        const list_ptr = if (iter_var_name) |name|
            self.variable_map.get(name) orelse return
        else
            try self.generateExpr(fl.iterator);

        // Get list length: __len = dm_list_TYPE_len(&list)
        const len_fn = switch (list_kind) {
            .int64 => "dm_list_int64_len",
            .float64 => "dm_list_double_len",
            .string => "dm_list_string_len",
            .other => "dm_list_int64_len",
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
            .other => try self.builder.allocType(.i64_type),
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
        const get_fn = switch (list_kind) {
            .int64 => "dm_list_int64_get",
            .float64 => "dm_list_double_get",
            .string => "dm_list_string_get",
            .other => "dm_list_int64_get",
        };
        const idx_val2 = try self.builder.buildLoad(idx_alloca, i64_ty);
        const get_args = try self.allocator.dupe(ir.Value, &.{ list_ptr, idx_val2 });
        const elem_val = try self.builder.buildCall(get_fn, get_args, elem_ty);
        try self.builder.buildStore(elem_alloca, elem_val);

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
                    .div_assign => try self.builder.buildDiv(old_val, val, ty),
                    .mod_assign => try self.builder.buildMod(old_val, val, ty),
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
            .lambda => ir.Value{ .undef = {} },
            .error_propagate => |ep| self.generateExpr(ep.operand),
            .await_expr => |ae| self.generateExpr(ae.operand),
            .range => ir.Value{ .undef = {} },
            .coalesce => |ce| self.generateExpr(ce.left),
            .enum_literal => ir.Value{ .undef = {} },
            .tuple_literal => ir.Value{ .undef = {} },
            .type_check => ir.Value{ .const_bool = false },
            .path => ir.Value{ .undef = {} },
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
            .div => self.builder.buildDiv(lhs, rhs, ty),
            .mod => self.builder.buildMod(lhs, rhs, ty),
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
            .path => |p| if (p.segments.len > 0) p.segments[p.segments.len - 1].name else return ir.Value{ .undef = {} },
            else => return ir.Value{ .undef = {} },
        };

        var args = std.ArrayList(ir.Value).init(self.allocator);
        defer args.deinit();
        for (call.args) |arg| {
            try args.append(try self.generateExpr(arg.value));
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
            std.mem.eql(u8, name, "args_get"))
            return self.builder.allocType(.string_type);

        if (std.mem.eql(u8, name, "parse_int") or
            std.mem.eql(u8, name, "len") or
            std.mem.eql(u8, name, "string_find") or
            std.mem.eql(u8, name, "system") or
            std.mem.eql(u8, name, "args_len"))
            return self.builder.allocType(.i64_type);

        if (std.mem.eql(u8, name, "parse_float"))
            return self.builder.allocType(.f64_type);

        if (std.mem.eql(u8, name, "string_contains") or
            std.mem.eql(u8, name, "starts_with") or
            std.mem.eql(u8, name, "ends_with") or
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

    fn generateMethodCall(self: *IRGenerator, mc: *const ast.MethodCall) !ir.Value {
        const method_name = mc.method.name;

        // Get the object variable name for list elem kind lookup
        const obj_var_name: ?[]const u8 = if (mc.object.kind == .identifier)
            mc.object.kind.identifier.name
        else
            null;

        // Check if this is a list method call
        const list_kind: ?ListElemKind = if (obj_var_name) |name| self.list_elem_kinds.get(name) else null;

        if (list_kind != null) {
            // This is a list method call — get pointer to list (alloca, not loaded value)
            const list_ptr = if (obj_var_name) |name| self.variable_map.get(name) orelse return ir.Value{ .undef = {} } else return ir.Value{ .undef = {} };
            const ek = list_kind.?;

            if (std.mem.eql(u8, method_name, "push")) {
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
                    .other => "dm_list_int64_push",
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
                    .other => "dm_list_int64_len",
                };
                const i64_ty = try self.builder.allocType(.i64_type);
                const len_args = try self.allocator.dupe(ir.Value, &.{list_ptr});
                return self.builder.buildCall(len_fn, len_args, i64_ty);
            }
        }

        // Non-list method call — standard handling
        const obj = try self.generateExpr(mc.object);
        var args = std.ArrayList(ir.Value).init(self.allocator);
        defer args.deinit();
        try args.append(obj);
        for (mc.args) |arg| {
            try args.append(try self.generateExpr(arg));
        }
        const args_slice = try self.allocator.dupe(ir.Value, args.items);

        // Look up user-defined function/method return type
        const ret_type = try self.inferCallReturnType(method_name);
        if (ret_type.* == .void_type) {
            try self.builder.buildCallVoid(method_name, args_slice);
            return ir.Value{ .undef = {} };
        }
        return self.builder.buildCall(method_name, args_slice, ret_type);
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
        // Generate base expression (loads struct value from alloca)
        const base_val = try self.generateExpr(fa.object);

        // Determine struct type
        const struct_ty = try self.inferExprType(fa.object);
        if (struct_ty.* != .struct_type) return ir.Value{ .undef = {} };

        // Find field and extract it
        for (struct_ty.struct_type.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, fa.field.name)) {
                return self.builder.buildExtractField(base_val, @intCast(i), field.ty);
            }
        }

        return ir.Value{ .undef = {} };
    }

    fn generateIndexAccess(self: *IRGenerator, ia: *const ast.IndexAccess) !ir.Value {
        _ = try self.generateExpr(ia.object);
        _ = try self.generateExpr(ia.index);
        return ir.Value{ .undef = {} };
    }

    fn generateCast(self: *IRGenerator, ce: *const ast.CastExpr) !ir.Value {
        const val = try self.generateExpr(ce.expr);
        _ = val;
        return ir.Value{ .undef = {} };
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
                .expr => |e| try self.generateExpr(e),
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
        _ = try self.generateExpr(me.scrutinee);
        for (me.arms) |arm| {
            switch (arm.body) {
                .expression => |e| {
                    _ = try self.generateExpr(e);
                },
                .block => |b| {
                    try self.generateBlock(b);
                },
            }
            break;
        }
        return ir.Value{ .undef = {} };
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
                if (self.variable_types.get(id.name)) |ty| {
                    break :blk ty;
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
                    else => break :blk self.builder.allocType(.i64_type),
                };
                break :blk self.inferCallReturnType(callee_name);
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
                if (base_ty.* == .struct_type) {
                    for (base_ty.struct_type.fields) |field| {
                        if (std.mem.eql(u8, field.name, fa.field.name)) {
                            break :blk field.ty;
                        }
                    }
                }
                break :blk self.builder.allocType(.i64_type);
            },
            else => self.builder.allocType(.i64_type),
        };
    }
};

// ========================================================================
// Tests
// ========================================================================

test "IR generator creates module" {
    const allocator = std.testing.allocator;
    var gen = IRGenerator.init(allocator);
    defer gen.deinit();

    try std.testing.expectEqual(@as(usize, 0), gen.module.functions.items.len);
}
