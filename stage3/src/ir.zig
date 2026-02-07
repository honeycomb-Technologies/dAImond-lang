//! dAImond Intermediate Representation
//!
//! SSA-based typed IR that sits between the AST and LLVM IR.
//! Designed per docs/ir-spec.md.

const std = @import("std");
const Allocator = std.mem.Allocator;

// ========================================================================
// IR Types
// ========================================================================

/// Represents all types in the dAImond IR
pub const IRType = union(enum) {
    // Signed integers
    i8_type: void,
    i16_type: void,
    i32_type: void,
    i64_type: void,
    // Unsigned integers
    u8_type: void,
    u16_type: void,
    u32_type: void,
    u64_type: void,
    // Floating point
    f32_type: void,
    f64_type: void,
    // Special
    bool_type: void,
    void_type: void,
    string_type: void,
    never_type: void,
    // Pointer
    ptr: *const IRType,
    // Array
    array: struct {
        elem: *const IRType,
        size: u64,
    },
    // Struct
    struct_type: struct {
        name: []const u8,
        fields: []const Field,
    },
    // Tagged union (enum with payloads)
    tagged_union: struct {
        name: []const u8,
        variants: []const Variant,
    },
    // Function
    fn_type: struct {
        params: []const *const IRType,
        ret: *const IRType,
    },
    // Function pointer (for closures, dynamic dispatch)
    fn_ptr: struct {
        params: []const *const IRType,
        ret: *const IRType,
    },
    // Generics: Option[T], Result[T,E], Future[T]
    option_type: *const IRType,
    result_type: struct {
        ok: *const IRType,
        err: *const IRType,
    },
    future_type: *const IRType,
    // Slice (fat pointer: ptr + len)
    slice_type: *const IRType,
    // SIMD vector types
    vector_type: VectorType,

    pub fn format(self: IRType, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .i8_type => try writer.writeAll("i8"),
            .i16_type => try writer.writeAll("i16"),
            .i32_type => try writer.writeAll("i32"),
            .i64_type => try writer.writeAll("i64"),
            .u8_type => try writer.writeAll("u8"),
            .u16_type => try writer.writeAll("u16"),
            .u32_type => try writer.writeAll("u32"),
            .u64_type => try writer.writeAll("u64"),
            .f32_type => try writer.writeAll("f32"),
            .f64_type => try writer.writeAll("f64"),
            .bool_type => try writer.writeAll("bool"),
            .void_type => try writer.writeAll("void"),
            .string_type => try writer.writeAll("string"),
            .never_type => try writer.writeAll("!"),
            .ptr => |inner| try writer.print("ptr({s})", .{inner.*}),
            .array => |a| try writer.print("[{s}; {}]", .{ a.elem.*, a.size }),
            .struct_type => |s| try writer.print("struct {s}", .{s.name}),
            .tagged_union => |t| try writer.print("enum {s}", .{t.name}),
            .fn_type => |f| {
                try writer.writeAll("fn(");
                for (f.params, 0..) |p, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{s}", .{p.*});
                }
                try writer.print(") -> {s}", .{f.ret.*});
            },
            .fn_ptr => |f| {
                try writer.writeAll("fn_ptr(");
                for (f.params, 0..) |p, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{s}", .{p.*});
                }
                try writer.print(") -> {s}", .{f.ret.*});
            },
            .option_type => |inner| try writer.print("Option[{s}]", .{inner.*}),
            .result_type => |r| try writer.print("Result[{s}, {s}]", .{ r.ok.*, r.err.* }),
            .future_type => |inner| try writer.print("Future[{s}]", .{inner.*}),
            .slice_type => |inner| try writer.print("Slice[{s}]", .{inner.*}),
            .vector_type => |v| try writer.writeAll(v.name()),
        }
    }
};

pub const Field = struct {
    name: []const u8,
    ty: *const IRType,
};

pub const Variant = struct {
    name: []const u8,
    tag: u32,
    payload: ?*const IRType,
};

/// SIMD vector element kind
pub const VectorElementKind = enum {
    f32_elem,
    f64_elem,
    i32_elem,
    i64_elem,

    pub fn isFloat(self: VectorElementKind) bool {
        return self == .f32_elem or self == .f64_elem;
    }
};

/// SIMD vector type descriptor
pub const VectorType = struct {
    elem_kind: VectorElementKind,
    lanes: u8,

    pub fn name(self: VectorType) []const u8 {
        if (self.elem_kind == .f32_elem and self.lanes == 4) return "f32x4";
        if (self.elem_kind == .f32_elem and self.lanes == 8) return "f32x8";
        if (self.elem_kind == .f64_elem and self.lanes == 2) return "f64x2";
        if (self.elem_kind == .f64_elem and self.lanes == 4) return "f64x4";
        if (self.elem_kind == .i32_elem and self.lanes == 4) return "i32x4";
        if (self.elem_kind == .i32_elem and self.lanes == 8) return "i32x8";
        if (self.elem_kind == .i64_elem and self.lanes == 2) return "i64x2";
        if (self.elem_kind == .i64_elem and self.lanes == 4) return "i64x4";
        return "vector_unknown";
    }
};

// ========================================================================
// IR Values
// ========================================================================

/// Represents a value in the IR (operand to instructions)
pub const Value = union(enum) {
    const_int: i64,
    const_float: f64,
    const_bool: bool,
    const_string: []const u8,
    /// Reference to result of instruction N in current function
    instruction_ref: u32,
    /// Reference to function parameter N
    param_ref: u32,
    /// Reference to a global (function or variable)
    global_ref: []const u8,
    /// Undefined value (for phi nodes, uninitialized)
    undef: void,

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .const_int => |v| try writer.print("{}", .{v}),
            .const_float => |v| try writer.print("{d}", .{v}),
            .const_bool => |v| try writer.print("{}", .{v}),
            .const_string => |s| try writer.print("\"{s}\"", .{s}),
            .instruction_ref => |id| try writer.print("%{}", .{id}),
            .param_ref => |id| try writer.print("$arg{}", .{id}),
            .global_ref => |name| try writer.print("@{s}", .{name}),
            .undef => try writer.writeAll("undef"),
        }
    }
};

// ========================================================================
// IR Instructions
// ========================================================================

/// An instruction in the IR
pub const Instruction = struct {
    /// Unique ID within the function
    id: u32,
    /// The operation
    op: Op,
    /// Result type (null for void instructions like store)
    result_type: ?*const IRType,
    /// Source location for debug info
    source_line: u32,
    source_col: u32,

    pub const Op = union(enum) {
        // Arithmetic
        add: BinaryOp,
        sub: BinaryOp,
        mul: BinaryOp,
        div: BinaryOp,
        mod: BinaryOp,
        neg: UnaryOp,
        // Comparison
        eq: BinaryOp,
        ne: BinaryOp,
        lt: BinaryOp,
        le: BinaryOp,
        gt: BinaryOp,
        ge: BinaryOp,
        // Logical
        logical_and: BinaryOp,
        logical_or: BinaryOp,
        logical_not: UnaryOp,
        // Bitwise
        bit_and: BinaryOp,
        bit_or: BinaryOp,
        bit_xor: BinaryOp,
        shl: BinaryOp,
        shr: BinaryOp,
        // Memory
        alloca: AllocaOp,
        load: LoadOp,
        store: StoreOp,
        gep: GepOp,
        extractfield: ExtractFieldOp,
        insertfield: InsertFieldOp,
        // Function calls
        call: CallOp,
        callptr: CallPtrOp,
        // Type conversion
        cast: CastOp,
        // Phi
        phi: PhiOp,
        // String operations
        string_concat: BinaryOp,
        string_eq: BinaryOp,
        string_len: UnaryOp,
        // List operations
        list_new: ListNewOp,
        list_push: BinaryOp,
        list_get: BinaryOp,
        list_len: UnaryOp,
        list_pop: UnaryOp,
        // Map operations
        map_new: MapNewOp,
        map_insert: MapInsertOp,
        map_get: BinaryOp,
        map_contains: BinaryOp,
        // Arena operations
        arena_create: Value, // size
        arena_destroy: Value, // arena ptr
        arena_alloc: struct { arena: Value, size: Value },
        // SIMD operations
        simd_splat: SimdSplatOp,
        simd_set: SimdSetOp,
        simd_add: BinaryOp,
        simd_sub: BinaryOp,
        simd_mul: BinaryOp,
        simd_div: BinaryOp,
        simd_extract: SimdExtractOp,
        // Runtime
        panic_inst: Value, // message string
    };
};

pub const BinaryOp = struct {
    lhs: Value,
    rhs: Value,
};

pub const UnaryOp = struct {
    operand: Value,
};

pub const AllocaOp = struct {
    alloc_type: *const IRType,
};

pub const LoadOp = struct {
    ptr: Value,
    load_type: *const IRType,
};

pub const StoreOp = struct {
    ptr: Value,
    value: Value,
};

pub const GepOp = struct {
    base: Value,
    indices: []const Value,
};

pub const ExtractFieldOp = struct {
    base: Value,
    field_index: u32,
    field_type: ?*const IRType = null,
};

pub const InsertFieldOp = struct {
    base: Value,
    value: Value,
    field_index: u32,
};

pub const CallOp = struct {
    callee: []const u8,
    args: []const Value,
};

pub const CallPtrOp = struct {
    callee: Value,
    args: []const Value,
};

pub const CastOp = struct {
    value: Value,
    from_type: *const IRType,
    to_type: *const IRType,
};

pub const PhiOp = struct {
    entries: []const PhiEntry,
};

pub const PhiEntry = struct {
    value: Value,
    block_label: []const u8,
};

pub const ListNewOp = struct {
    elem_type: *const IRType,
};

pub const MapNewOp = struct {
    key_type: *const IRType,
    val_type: *const IRType,
};

pub const MapInsertOp = struct {
    map: Value,
    key: Value,
    val: Value,
};

pub const SimdSplatOp = struct {
    scalar: Value,
    vec_type: *const IRType, // must be vector_type
};

pub const SimdSetOp = struct {
    elements: []const Value,
    vec_type: *const IRType, // must be vector_type
};

pub const SimdExtractOp = struct {
    vector: Value,
    index: Value,
};

// ========================================================================
// Terminators
// ========================================================================

/// Block terminators - control flow
pub const Terminator = union(enum) {
    /// Unconditional branch
    br: []const u8,
    /// Conditional branch
    br_cond: struct {
        cond: Value,
        true_label: []const u8,
        false_label: []const u8,
    },
    /// Return with value
    ret: Value,
    /// Return void
    ret_void: void,
    /// Unreachable (after panic, etc.)
    unreachable_term: void,

    pub fn format(self: Terminator, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .br => |label| try writer.print("br %{s}", .{label}),
            .br_cond => |bc| try writer.print("br.cond {s}, %{s}, %{s}", .{ bc.cond, bc.true_label, bc.false_label }),
            .ret => |v| try writer.print("ret {s}", .{v}),
            .ret_void => try writer.writeAll("ret void"),
            .unreachable_term => try writer.writeAll("unreachable"),
        }
    }
};

// ========================================================================
// Basic Blocks
// ========================================================================

pub const BasicBlock = struct {
    label: []const u8,
    instructions: std.ArrayList(Instruction),
    terminator: ?Terminator,

    pub fn init(allocator: Allocator, label: []const u8) BasicBlock {
        return .{
            .label = label,
            .instructions = std.ArrayList(Instruction).init(allocator),
            .terminator = null,
        };
    }

    pub fn deinit(self: *BasicBlock) void {
        self.instructions.deinit();
    }
};

// ========================================================================
// Functions
// ========================================================================

pub const Param = struct {
    name: []const u8,
    ty: *const IRType,
};

pub const Function = struct {
    name: []const u8,
    params: []const Param,
    return_type: *const IRType,
    blocks: std.ArrayList(BasicBlock),
    is_async: bool,
    is_extern: bool,
    is_comptime: bool,

    pub fn init(allocator: Allocator, name: []const u8, params: []const Param, return_type: *const IRType) Function {
        return .{
            .name = name,
            .params = params,
            .return_type = return_type,
            .blocks = std.ArrayList(BasicBlock).init(allocator),
            .is_async = false,
            .is_extern = false,
            .is_comptime = false,
        };
    }

    pub fn deinit(self: *Function) void {
        for (self.blocks.items) |*blk| {
            blk.deinit();
        }
        self.blocks.deinit();
    }

    pub fn addBlock(self: *Function, label: []const u8) !*BasicBlock {
        try self.blocks.append(BasicBlock.init(self.blocks.allocator, label));
        return &self.blocks.items[self.blocks.items.len - 1];
    }
};

// ========================================================================
// Module
// ========================================================================

pub const GlobalVar = struct {
    name: []const u8,
    ty: *const IRType,
    init_value: ?Value,
    is_const: bool,
};

pub const Module = struct {
    functions: std.ArrayList(Function),
    globals: std.ArrayList(GlobalVar),
    struct_defs: std.StringHashMap(*const IRType),
    extern_decls: std.ArrayList(ExternDecl),
    /// User-declared extern functions (not runtime builtins) that need special wrapper generation
    user_extern_fns: std.StringHashMap(void),
    allocator: Allocator,

    pub const ExternDecl = struct {
        name: []const u8,
        ty: *const IRType, // fn_type
    };

    pub fn init(allocator: Allocator) Module {
        return .{
            .functions = std.ArrayList(Function).init(allocator),
            .globals = std.ArrayList(GlobalVar).init(allocator),
            .struct_defs = std.StringHashMap(*const IRType).init(allocator),
            .extern_decls = std.ArrayList(ExternDecl).init(allocator),
            .user_extern_fns = std.StringHashMap(void).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Module) void {
        for (self.functions.items) |*f| {
            f.deinit();
        }
        self.functions.deinit();
        self.globals.deinit();
        self.struct_defs.deinit();
        self.extern_decls.deinit();
        self.user_extern_fns.deinit();
    }

    pub fn addFunction(self: *Module, name: []const u8, params: []const Param, return_type: *const IRType) !*Function {
        try self.functions.append(Function.init(self.allocator, name, params, return_type));
        return &self.functions.items[self.functions.items.len - 1];
    }

    pub fn addGlobal(self: *Module, global: GlobalVar) !void {
        try self.globals.append(global);
    }

    pub fn addExternDecl(self: *Module, name: []const u8, ty: *const IRType) !void {
        try self.extern_decls.append(.{ .name = name, .ty = ty });
    }
};

// ========================================================================
// IR Builder
// ========================================================================

pub const IRBuilder = struct {
    allocator: Allocator,
    current_function: ?*Function,
    current_block: ?*BasicBlock,
    next_id: u32,
    /// Arena for allocating types that need to be heap-stable
    type_arena: std.heap.ArenaAllocator,

    pub fn init(allocator: Allocator) IRBuilder {
        return .{
            .allocator = allocator,
            .current_function = null,
            .current_block = null,
            .next_id = 0,
            .type_arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *IRBuilder) void {
        self.type_arena.deinit();
    }

    pub fn setInsertBlock(self: *IRBuilder, block: *BasicBlock) void {
        self.current_block = block;
    }

    pub fn setFunction(self: *IRBuilder, func: *Function) void {
        self.current_function = func;
        self.next_id = 0;
    }

    /// Allocate a type on the arena so it has stable pointer
    pub fn allocType(self: *IRBuilder, ty: IRType) !*const IRType {
        const ptr = try self.type_arena.allocator().create(IRType);
        ptr.* = ty;
        return ptr;
    }

    pub fn addInstruction(self: *IRBuilder, op: Instruction.Op, result_type: ?*const IRType) !Value {
        const id = self.next_id;
        self.next_id += 1;
        const inst = Instruction{
            .id = id,
            .op = op,
            .result_type = result_type,
            .source_line = 0,
            .source_col = 0,
        };
        try self.current_block.?.instructions.append(inst);
        return .{ .instruction_ref = id };
    }

    fn addVoidInstruction(self: *IRBuilder, op: Instruction.Op) !void {
        const id = self.next_id;
        self.next_id += 1;
        const inst = Instruction{
            .id = id,
            .op = op,
            .result_type = null,
            .source_line = 0,
            .source_col = 0,
        };
        try self.current_block.?.instructions.append(inst);
    }

    // Arithmetic builders
    pub fn buildAdd(self: *IRBuilder, lhs: Value, rhs: Value, ty: *const IRType) !Value {
        return self.addInstruction(.{ .add = .{ .lhs = lhs, .rhs = rhs } }, ty);
    }

    pub fn buildSub(self: *IRBuilder, lhs: Value, rhs: Value, ty: *const IRType) !Value {
        return self.addInstruction(.{ .sub = .{ .lhs = lhs, .rhs = rhs } }, ty);
    }

    pub fn buildMul(self: *IRBuilder, lhs: Value, rhs: Value, ty: *const IRType) !Value {
        return self.addInstruction(.{ .mul = .{ .lhs = lhs, .rhs = rhs } }, ty);
    }

    pub fn buildDiv(self: *IRBuilder, lhs: Value, rhs: Value, ty: *const IRType) !Value {
        return self.addInstruction(.{ .div = .{ .lhs = lhs, .rhs = rhs } }, ty);
    }

    pub fn buildMod(self: *IRBuilder, lhs: Value, rhs: Value, ty: *const IRType) !Value {
        return self.addInstruction(.{ .mod = .{ .lhs = lhs, .rhs = rhs } }, ty);
    }

    // Comparison builders
    pub fn buildEq(self: *IRBuilder, lhs: Value, rhs: Value) !Value {
        return self.addInstruction(.{ .eq = .{ .lhs = lhs, .rhs = rhs } }, try self.allocType(.bool_type));
    }

    pub fn buildNe(self: *IRBuilder, lhs: Value, rhs: Value) !Value {
        return self.addInstruction(.{ .ne = .{ .lhs = lhs, .rhs = rhs } }, try self.allocType(.bool_type));
    }

    pub fn buildLt(self: *IRBuilder, lhs: Value, rhs: Value) !Value {
        return self.addInstruction(.{ .lt = .{ .lhs = lhs, .rhs = rhs } }, try self.allocType(.bool_type));
    }

    pub fn buildLe(self: *IRBuilder, lhs: Value, rhs: Value) !Value {
        return self.addInstruction(.{ .le = .{ .lhs = lhs, .rhs = rhs } }, try self.allocType(.bool_type));
    }

    pub fn buildGt(self: *IRBuilder, lhs: Value, rhs: Value) !Value {
        return self.addInstruction(.{ .gt = .{ .lhs = lhs, .rhs = rhs } }, try self.allocType(.bool_type));
    }

    pub fn buildGe(self: *IRBuilder, lhs: Value, rhs: Value) !Value {
        return self.addInstruction(.{ .ge = .{ .lhs = lhs, .rhs = rhs } }, try self.allocType(.bool_type));
    }

    // Memory builders
    pub fn buildAlloca(self: *IRBuilder, alloc_type: *const IRType) !Value {
        const ptr_type = try self.allocType(.{ .ptr = alloc_type });
        return self.addInstruction(.{ .alloca = .{ .alloc_type = alloc_type } }, ptr_type);
    }

    pub fn buildLoad(self: *IRBuilder, ptr: Value, load_type: *const IRType) !Value {
        return self.addInstruction(.{ .load = .{ .ptr = ptr, .load_type = load_type } }, load_type);
    }

    pub fn buildStore(self: *IRBuilder, ptr: Value, value: Value) !void {
        return self.addVoidInstruction(.{ .store = .{ .ptr = ptr, .value = value } });
    }

    // Struct builders
    pub fn buildExtractField(self: *IRBuilder, base: Value, field_index: u32, result_ty: *const IRType) !Value {
        return self.addInstruction(.{ .extractfield = .{ .base = base, .field_index = field_index, .field_type = result_ty } }, result_ty);
    }

    pub fn buildInsertField(self: *IRBuilder, base: Value, value: Value, field_index: u32, struct_ty: *const IRType) !Value {
        return self.addInstruction(.{ .insertfield = .{ .base = base, .value = value, .field_index = field_index } }, struct_ty);
    }

    pub fn buildCast(self: *IRBuilder, value: Value, from_type: *const IRType, to_type: *const IRType) !Value {
        return self.addInstruction(.{ .cast = .{ .value = value, .from_type = from_type, .to_type = to_type } }, to_type);
    }

    // Call builders
    pub fn buildCall(self: *IRBuilder, callee: []const u8, args: []const Value, ret_type: *const IRType) !Value {
        return self.addInstruction(.{ .call = .{ .callee = callee, .args = args } }, ret_type);
    }

    pub fn buildCallVoid(self: *IRBuilder, callee: []const u8, args: []const Value) !void {
        return self.addVoidInstruction(.{ .call = .{ .callee = callee, .args = args } });
    }

    pub fn buildCallPtr(self: *IRBuilder, callee: Value, args: []const Value, ret_type: *const IRType) !Value {
        return self.addInstruction(.{ .callptr = .{ .callee = callee, .args = args } }, ret_type);
    }

    pub fn buildCallPtrVoid(self: *IRBuilder, callee: Value, args: []const Value) !void {
        return self.addVoidInstruction(.{ .callptr = .{ .callee = callee, .args = args } });
    }

    // Terminator builders
    pub fn buildBr(self: *IRBuilder, label: []const u8) void {
        self.current_block.?.terminator = .{ .br = label };
    }

    pub fn buildCondBr(self: *IRBuilder, cond: Value, true_label: []const u8, false_label: []const u8) void {
        self.current_block.?.terminator = .{ .br_cond = .{
            .cond = cond,
            .true_label = true_label,
            .false_label = false_label,
        } };
    }

    pub fn buildRet(self: *IRBuilder, value: Value) void {
        self.current_block.?.terminator = .{ .ret = value };
    }

    pub fn buildRetVoid(self: *IRBuilder) void {
        self.current_block.?.terminator = .ret_void;
    }
};

// ========================================================================
// IR Printer
// ========================================================================

pub fn printModule(module: *const Module, writer: anytype) !void {
    // Print struct definitions
    var struct_iter = module.struct_defs.iterator();
    while (struct_iter.next()) |entry| {
        try writer.print("type {s} = {s}\n", .{ entry.key_ptr.*, entry.value_ptr.*.* });
    }

    // Print extern declarations
    for (module.extern_decls.items) |ext| {
        try writer.print("declare {s} : {s}\n", .{ ext.name, ext.ty.* });
    }

    // Print globals
    for (module.globals.items) |g| {
        try writer.print("global @{s} : {s}", .{ g.name, g.ty.* });
        if (g.init_value) |v| {
            try writer.print(" = {s}", .{v});
        }
        try writer.writeAll("\n");
    }

    // Print functions
    for (module.functions.items) |func| {
        try printFunction(&func, writer);
        try writer.writeAll("\n");
    }
}

pub fn printFunction(func: *const Function, writer: anytype) !void {
    const prefix = if (func.is_extern) "declare" else "define";
    try writer.print("{s} @{s}(", .{ prefix, func.name });

    for (func.params, 0..) |p, i| {
        if (i > 0) try writer.writeAll(", ");
        try writer.print("{s} %{s}", .{ p.ty.*, p.name });
    }

    try writer.print(") -> {s}", .{func.return_type.*});

    if (func.is_extern) {
        try writer.writeAll("\n");
        return;
    }

    try writer.writeAll(" {\n");

    for (func.blocks.items) |block| {
        try writer.print("  {s}:\n", .{block.label});
        for (block.instructions.items) |inst| {
            try writer.writeAll("    ");
            if (inst.result_type != null) {
                try writer.print("%{} = ", .{inst.id});
            }
            try printOp(inst.op, writer);
            try writer.writeAll("\n");
        }
        if (block.terminator) |term| {
            try writer.print("    {s}\n", .{term});
        }
    }

    try writer.writeAll("}\n");
}

fn printOp(op: Instruction.Op, writer: anytype) !void {
    switch (op) {
        .add => |b| try writer.print("add {s}, {s}", .{ b.lhs, b.rhs }),
        .sub => |b| try writer.print("sub {s}, {s}", .{ b.lhs, b.rhs }),
        .mul => |b| try writer.print("mul {s}, {s}", .{ b.lhs, b.rhs }),
        .div => |b| try writer.print("div {s}, {s}", .{ b.lhs, b.rhs }),
        .mod => |b| try writer.print("mod {s}, {s}", .{ b.lhs, b.rhs }),
        .neg => |u| try writer.print("neg {s}", .{u.operand}),
        .eq => |b| try writer.print("eq {s}, {s}", .{ b.lhs, b.rhs }),
        .ne => |b| try writer.print("ne {s}, {s}", .{ b.lhs, b.rhs }),
        .lt => |b| try writer.print("lt {s}, {s}", .{ b.lhs, b.rhs }),
        .le => |b| try writer.print("le {s}, {s}", .{ b.lhs, b.rhs }),
        .gt => |b| try writer.print("gt {s}, {s}", .{ b.lhs, b.rhs }),
        .ge => |b| try writer.print("ge {s}, {s}", .{ b.lhs, b.rhs }),
        .logical_and => |b| try writer.print("and {s}, {s}", .{ b.lhs, b.rhs }),
        .logical_or => |b| try writer.print("or {s}, {s}", .{ b.lhs, b.rhs }),
        .logical_not => |u| try writer.print("not {s}", .{u.operand}),
        .bit_and => |b| try writer.print("bitand {s}, {s}", .{ b.lhs, b.rhs }),
        .bit_or => |b| try writer.print("bitor {s}, {s}", .{ b.lhs, b.rhs }),
        .bit_xor => |b| try writer.print("bitxor {s}, {s}", .{ b.lhs, b.rhs }),
        .shl => |b| try writer.print("shl {s}, {s}", .{ b.lhs, b.rhs }),
        .shr => |b| try writer.print("shr {s}, {s}", .{ b.lhs, b.rhs }),
        .alloca => |a| try writer.print("alloca {s}", .{a.alloc_type.*}),
        .load => |l| try writer.print("load {s}, {s}", .{ l.load_type.*, l.ptr }),
        .store => |s| try writer.print("store {s}, {s}", .{ s.ptr, s.value }),
        .gep => |g| {
            try writer.print("gep {s}", .{g.base});
            for (g.indices) |idx| {
                try writer.print(", {s}", .{idx});
            }
        },
        .extractfield => |e| try writer.print("extractfield {s}, {}", .{ e.base, e.field_index }),
        .insertfield => |e| try writer.print("insertfield {s}, {s}, {}", .{ e.base, e.value, e.field_index }),
        .call => |c| {
            try writer.print("call @{s}(", .{c.callee});
            for (c.args, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("{s}", .{arg});
            }
            try writer.writeAll(")");
        },
        .callptr => |c| {
            try writer.print("callptr {s}(", .{c.callee});
            for (c.args, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("{s}", .{arg});
            }
            try writer.writeAll(")");
        },
        .cast => |c| try writer.print("cast {s} : {s} -> {s}", .{ c.value, c.from_type.*, c.to_type.* }),
        .phi => |p| {
            try writer.writeAll("phi ");
            for (p.entries, 0..) |e, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("[{s}, %{s}]", .{ e.value, e.block_label });
            }
        },
        .string_concat => |b| try writer.print("string.concat {s}, {s}", .{ b.lhs, b.rhs }),
        .string_eq => |b| try writer.print("string.eq {s}, {s}", .{ b.lhs, b.rhs }),
        .string_len => |u| try writer.print("string.len {s}", .{u.operand}),
        .list_new => |l| try writer.print("list.new {s}", .{l.elem_type.*}),
        .list_push => |b| try writer.print("list.push {s}, {s}", .{ b.lhs, b.rhs }),
        .list_get => |b| try writer.print("list.get {s}, {s}", .{ b.lhs, b.rhs }),
        .list_len => |u| try writer.print("list.len {s}", .{u.operand}),
        .list_pop => |u| try writer.print("list.pop {s}", .{u.operand}),
        .map_new => |m| try writer.print("map.new {s}, {s}", .{ m.key_type.*, m.val_type.* }),
        .map_insert => |m| try writer.print("map.insert {s}, {s}, {s}", .{ m.map, m.key, m.val }),
        .map_get => |b| try writer.print("map.get {s}, {s}", .{ b.lhs, b.rhs }),
        .map_contains => |b| try writer.print("map.contains {s}, {s}", .{ b.lhs, b.rhs }),
        .arena_create => |v| try writer.print("arena.create {s}", .{v}),
        .arena_destroy => |v| try writer.print("arena.destroy {s}", .{v}),
        .arena_alloc => |a| try writer.print("arena.alloc {s}, {s}", .{ a.arena, a.size }),
        .simd_splat => |s| try writer.print("simd.splat {s}", .{s.scalar}),
        .simd_set => |s| {
            try writer.writeAll("simd.set ");
            for (s.elements, 0..) |e, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("{s}", .{e});
            }
        },
        .simd_add => |b| try writer.print("simd.add {s}, {s}", .{ b.lhs, b.rhs }),
        .simd_sub => |b| try writer.print("simd.sub {s}, {s}", .{ b.lhs, b.rhs }),
        .simd_mul => |b| try writer.print("simd.mul {s}, {s}", .{ b.lhs, b.rhs }),
        .simd_div => |b| try writer.print("simd.div {s}, {s}", .{ b.lhs, b.rhs }),
        .simd_extract => |e| try writer.print("simd.extract {s}, {s}", .{ e.vector, e.index }),
        .panic_inst => |v| try writer.print("panic {s}", .{v}),
    }
}

// ========================================================================
// Tests
// ========================================================================

test "IR module creation" {
    const allocator = std.testing.allocator;
    var module = Module.init(allocator);
    defer module.deinit();

    var builder = IRBuilder.init(allocator);
    defer builder.deinit();

    // Create i64 type
    const i64_type = try builder.allocType(.i64_type);
    const void_type = try builder.allocType(.void_type);
    const string_type = try builder.allocType(.string_type);

    // Create main function
    var func = try module.addFunction("dm_main", &.{}, void_type);
    const entry = try func.addBlock("entry");

    builder.setFunction(func);
    builder.setInsertBlock(entry);

    // Build: %0 = add 40, 2
    const forty = Value{ .const_int = 40 };
    const two = Value{ .const_int = 2 };
    const sum = try builder.buildAdd(forty, two, i64_type);

    // Build: call @dm_println_int(%0)
    _ = string_type;
    try builder.buildCallVoid("dm_println_int", &.{sum});

    // Build: ret void
    builder.buildRetVoid();

    // Verify structure
    try std.testing.expectEqual(@as(usize, 1), module.functions.items.len);
    try std.testing.expectEqual(@as(usize, 1), func.blocks.items.len);
    try std.testing.expectEqual(@as(usize, 2), entry.instructions.items.len);
    try std.testing.expect(entry.terminator != null);
}

test "IR printer" {
    const allocator = std.testing.allocator;
    var module = Module.init(allocator);
    defer module.deinit();

    var builder = IRBuilder.init(allocator);
    defer builder.deinit();

    const i64_type = try builder.allocType(.i64_type);

    // Create a simple function: fn add(a: i64, b: i64) -> i64 { return a + b; }
    const params = try allocator.alloc(Param, 2);
    defer allocator.free(params);
    params[0] = .{ .name = "a", .ty = i64_type };
    params[1] = .{ .name = "b", .ty = i64_type };

    var func = try module.addFunction("dm_add", params, i64_type);
    const entry = try func.addBlock("entry");

    builder.setFunction(func);
    builder.setInsertBlock(entry);

    const a = Value{ .param_ref = 0 };
    const b = Value{ .param_ref = 1 };
    const sum = try builder.buildAdd(a, b, i64_type);
    builder.buildRet(sum);

    // Print and verify
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();
    try printModule(&module, buf.writer());

    const output = buf.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "define @dm_add") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "add") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "ret") != null);
}
