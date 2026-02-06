//! LLVM IR Generation
//!
//! Translates dAImond IR into LLVM IR using the LLVM-C API bindings.
//!
//! Key design: The dm_string struct is 24 bytes (ptr + len + cap), which exceeds
//! the x86-64 SysV ABI's 16-byte threshold for register passing. Functions that
//! take/return dm_string by value require sret/byval attributes in LLVM IR.
//! Instead of implementing these complex attributes, we use thin C wrapper
//! functions (llvm_dm_*) with a pointer-based API:
//! - Functions returning dm_string → wrapper takes dm_string* as first param (sret)
//! - Functions taking dm_string → wrapper takes const dm_string* (pointer)
//! String values in the LLVM IR are represented as pointers to stack-allocated
//! dm_string structs (alloca).
//!
//! Structs are handled as LLVM aggregate value types, using insertvalue/extractvalue.

const std = @import("std");
const ir = @import("ir.zig");
const llvm = @import("llvm_bindings.zig");

pub const LLVMGenerator = struct {
    allocator: std.mem.Allocator,
    context: llvm.Context,
    module: llvm.Module,
    builder: llvm.Builder,
    /// Map IR instruction refs to LLVM values
    value_map: std.AutoHashMap(u32, llvm.Value),
    /// Map function names to LLVM functions
    function_map: std.StringHashMap(llvm.Value),
    /// Map function names to their LLVM function types
    fn_type_map: std.StringHashMap(llvm.Type),
    /// Map block labels to LLVM basic blocks
    block_map: std.StringHashMap(llvm.BasicBlock),
    /// Counter for unique global string names
    string_counter: u32 = 0,
    /// Counter for unique sret alloca names
    sret_counter: u32 = 0,
    /// Named dm_string struct type: { ptr, i64, i64 }
    dm_string_type: llvm.Type = .{ .ref = null },
    /// Wrapper info for extern functions that involve dm_string
    wrapper_map: std.StringHashMap(WrapperInfo),
    /// Track instruction IDs whose values are pointers to dm_string structs
    string_ptrs: std.AutoHashMap(u32, void),
    /// Map from struct name to LLVM named struct type
    struct_type_map: std.StringHashMap(llvm.Type),

    const WrapperInfo = struct {
        /// Whether the original function returns dm_string (wrapper uses sret)
        returns_string: bool,
    };

    pub fn init(allocator: std.mem.Allocator, module_name: [*:0]const u8) LLVMGenerator {
        const ctx = llvm.Context.create();
        return .{
            .allocator = allocator,
            .context = ctx,
            .module = llvm.Module.createWithName(module_name, ctx),
            .builder = llvm.Builder.create(ctx),
            .value_map = std.AutoHashMap(u32, llvm.Value).init(allocator),
            .function_map = std.StringHashMap(llvm.Value).init(allocator),
            .fn_type_map = std.StringHashMap(llvm.Type).init(allocator),
            .block_map = std.StringHashMap(llvm.BasicBlock).init(allocator),
            .wrapper_map = std.StringHashMap(WrapperInfo).init(allocator),
            .string_ptrs = std.AutoHashMap(u32, void).init(allocator),
            .struct_type_map = std.StringHashMap(llvm.Type).init(allocator),
        };
    }

    pub fn deinit(self: *LLVMGenerator) void {
        self.builder.dispose();
        self.module.dispose();
        self.context.dispose();
        self.value_map.deinit();
        self.function_map.deinit();
        self.fn_type_map.deinit();
        self.block_map.deinit();
        self.wrapper_map.deinit();
        self.string_ptrs.deinit();
        self.struct_type_map.deinit();
    }

    // ====================================================================
    // Type Mapping
    // ====================================================================

    /// Get or create the named dm_string struct type: { ptr, i64, i64 }
    fn getDmStringType(self: *LLVMGenerator) llvm.Type {
        if (self.dm_string_type.ref != null) return self.dm_string_type;
        const t = llvm.Type.namedStruct(self.context, "dm_string");
        t.structSetBody(&.{
            llvm.Type.ptr(self.context),
            llvm.Type.int64(self.context),
            llvm.Type.int64(self.context),
        }, false);
        self.dm_string_type = t;
        return t;
    }

    /// Get or create a list struct type: { ptr, i64, i64 }
    fn getListType(self: *LLVMGenerator, name: [*:0]const u8) llvm.Type {
        if (self.struct_type_map.get(std.mem.span(name))) |t| return t;
        const t = llvm.Type.namedStruct(self.context, name);
        t.structSetBody(&.{
            llvm.Type.ptr(self.context),
            llvm.Type.int64(self.context),
            llvm.Type.int64(self.context),
        }, false);
        self.struct_type_map.put(std.mem.span(name), t) catch {};
        return t;
    }

    /// Map a dAImond IR type to an LLVM type
    pub fn mapType(self: *LLVMGenerator, ty: *const ir.IRType) llvm.Type {
        return switch (ty.*) {
            .i8_type => llvm.Type.int8(self.context),
            .i16_type => llvm.Type.int16(self.context),
            .i32_type => llvm.Type.int32(self.context),
            .i64_type => llvm.Type.int64(self.context),
            .u8_type => llvm.Type.int8(self.context),
            .u16_type => llvm.Type.int16(self.context),
            .u32_type => llvm.Type.int32(self.context),
            .u64_type => llvm.Type.int64(self.context),
            .f32_type => llvm.Type.float(self.context),
            .f64_type => llvm.Type.double(self.context),
            .bool_type => llvm.Type.int1(self.context),
            .void_type, .never_type => llvm.Type.void_type(self.context),
            .string_type => self.getDmStringType(),
            .ptr => llvm.Type.ptr(self.context),
            .array => |a| llvm.Type.array(self.mapType(a.elem), a.size),
            .struct_type => |s| blk: {
                if (self.struct_type_map.get(s.name)) |t| break :blk t;
                // Check if it's a list type
                if (std.mem.startsWith(u8, s.name, "dm_list_")) {
                    const name_z = std.fmt.allocPrintZ(self.allocator, "{s}", .{s.name}) catch break :blk llvm.Type.int64(self.context);
                    defer self.allocator.free(name_z);
                    break :blk self.getListType(name_z);
                }
                break :blk llvm.Type.int64(self.context);
            },
            .fn_type => |f| {
                var param_types = std.ArrayList(llvm.Type).init(self.allocator);
                defer param_types.deinit();
                for (f.params) |p| {
                    param_types.append(self.mapType(p)) catch unreachable;
                }
                return llvm.Type.function(self.mapType(f.ret), param_types.items, false);
            },
            .fn_ptr => llvm.Type.ptr(self.context),
            else => llvm.Type.int64(self.context), // fallback
        };
    }

    /// Check if an IR type is a float type (f32 or f64)
    fn isIRFloat(ty: ?*const ir.IRType) bool {
        if (ty) |t| return t.* == .f32_type or t.* == .f64_type;
        return false;
    }

    /// Check if an LLVM value has floating-point type
    fn isLLVMFloat(val: llvm.Value) bool {
        if (val.ref == null) return false;
        const kind = val.typeOf().getTypeKind();
        return kind == llvm.c.LLVMFloatTypeKind or kind == llvm.c.LLVMDoubleTypeKind;
    }

    // ====================================================================
    // Wrapper Function Declaration
    // ====================================================================

    /// Check if an extern function needs ABI wrapper (has string params or return)
    fn needsWrapper(func: *const ir.Function) bool {
        if (!func.is_extern) return false;
        if (func.return_type.* == .string_type) return true;
        for (func.params) |p| {
            if (p.ty.* == .string_type) return true;
        }
        return false;
    }

    /// Declare a wrapper function for a runtime function that involves dm_string.
    /// The wrapper uses pointer-based API to avoid struct-by-value ABI issues.
    /// Wrapper name = "llvm_" + original name. Stored under ORIGINAL name
    /// in function_map so call instructions resolve correctly.
    fn declareWrapper(self: *LLVMGenerator, func: *const ir.Function) !void {
        const returns_string = func.return_type.* == .string_type;
        const ptr_ty = llvm.Type.ptr(self.context);
        const void_ty = llvm.Type.void_type(self.context);

        var params = std.ArrayList(llvm.Type).init(self.allocator);
        defer params.deinit();

        // If returns string: prepend sret output pointer
        if (returns_string) {
            try params.append(ptr_ty);
        }

        // Transform params: string_type → ptr, others → mapType as normal
        for (func.params) |p| {
            if (p.ty.* == .string_type) {
                try params.append(ptr_ty);
            } else {
                try params.append(self.mapType(p.ty));
            }
        }

        // Return type: void if returns_string, else mapType(original)
        const ret_ty = if (returns_string) void_ty else self.mapType(func.return_type);

        const fn_ty = llvm.Type.function(ret_ty, params.items, false);

        const wrapper_name = try std.fmt.allocPrintZ(self.allocator, "llvm_{s}", .{func.name});
        defer self.allocator.free(wrapper_name);

        const llvm_fn = self.module.addFunction(wrapper_name, fn_ty);

        // Store under ORIGINAL name
        try self.function_map.put(func.name, llvm_fn);
        try self.fn_type_map.put(func.name, fn_ty);
        try self.wrapper_map.put(func.name, .{ .returns_string = returns_string });
    }

    // ====================================================================
    // Top-level Generation
    // ====================================================================

    /// Generate LLVM IR from a dAImond IR module
    pub fn generate(self: *LLVMGenerator, ir_module: *const ir.Module) !void {
        _ = self.getDmStringType();

        // Phase 0: Create LLVM struct types from IR struct definitions
        var struct_iter = ir_module.struct_defs.iterator();
        while (struct_iter.next()) |entry| {
            const ir_type = entry.value_ptr.*;
            if (ir_type.* == .struct_type) {
                const name_z = try self.allocator.dupeZ(u8, ir_type.struct_type.name);
                defer self.allocator.free(name_z);
                const llvm_struct = llvm.Type.namedStruct(self.context, name_z);

                var field_types = std.ArrayList(llvm.Type).init(self.allocator);
                defer field_types.deinit();
                for (ir_type.struct_type.fields) |field| {
                    try field_types.append(self.mapType(field.ty));
                }
                llvm_struct.structSetBody(field_types.items, false);

                try self.struct_type_map.put(ir_type.struct_type.name, llvm_struct);
            }
        }

        // Phase 1: Declare all functions
        for (ir_module.functions.items) |func| {
            if (needsWrapper(&func)) {
                try self.declareWrapper(&func);
                continue; // Don't declare the original
            }

            var param_types = std.ArrayList(llvm.Type).init(self.allocator);
            defer param_types.deinit();
            for (func.params) |p| {
                try param_types.append(self.mapType(p.ty));
            }

            // C ABI: main returns i32
            const ret_type = if (std.mem.eql(u8, func.name, "main"))
                llvm.Type.int32(self.context)
            else
                self.mapType(func.return_type);
            const fn_ty = llvm.Type.function(ret_type, param_types.items, false);

            const name_z = try self.allocator.dupeZ(u8, func.name);
            defer self.allocator.free(name_z);
            const llvm_fn = self.module.addFunction(name_z, fn_ty);
            try self.function_map.put(func.name, llvm_fn);
            try self.fn_type_map.put(func.name, fn_ty);

            // Name parameters
            for (func.params, 0..) |p, i| {
                const param = llvm_fn.getParam(@intCast(i));
                const pname = try self.allocator.dupeZ(u8, p.name);
                defer self.allocator.free(pname);
                param.setName(pname);
            }
        }

        // Phase 2: Generate function bodies
        for (ir_module.functions.items) |func| {
            if (func.is_extern) continue;
            try self.generateFunction(&func);
        }
    }

    // ====================================================================
    // Function Body Generation
    // ====================================================================

    fn generateFunction(self: *LLVMGenerator, func: *const ir.Function) !void {
        const llvm_fn = self.function_map.get(func.name) orelse return;

        // Create basic blocks
        self.block_map.clearRetainingCapacity();
        for (func.blocks.items) |block| {
            const name_z = try self.allocator.dupeZ(u8, block.label);
            defer self.allocator.free(name_z);
            const bb = llvm.BasicBlock.append(llvm_fn, self.context, name_z);
            try self.block_map.put(block.label, bb);
        }

        // Generate instructions in each block
        self.value_map.clearRetainingCapacity();
        self.string_ptrs.clearRetainingCapacity();
        for (func.blocks.items) |block| {
            const bb = self.block_map.get(block.label) orelse continue;
            self.builder.positionAtEnd(bb);

            for (block.instructions.items) |inst| {
                try self.generateInstruction(&inst, func);
            }

            if (block.terminator) |term| {
                try self.generateTerminator(term, func);
            }
        }
    }

    // ====================================================================
    // Instruction Generation
    // ====================================================================

    fn generateInstruction(self: *LLVMGenerator, inst: *const ir.Instruction, func: *const ir.Function) !void {
        const result = switch (inst.op) {
            // Arithmetic (int/float dispatch)
            .add => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFAdd(lhs, rhs, "fadd");
                }
                break :blk self.builder.buildAdd(lhs, rhs, "add");
            },
            .sub => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFSub(lhs, rhs, "fsub");
                }
                break :blk self.builder.buildSub(lhs, rhs, "sub");
            },
            .mul => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFMul(lhs, rhs, "fmul");
                }
                break :blk self.builder.buildMul(lhs, rhs, "mul");
            },
            .div => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFDiv(lhs, rhs, "fdiv");
                }
                break :blk self.builder.buildSDiv(lhs, rhs, "div");
            },
            .mod => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFRem(lhs, rhs, "frem");
                }
                break :blk self.builder.buildSRem(lhs, rhs, "mod");
            },
            // Negation
            .neg => |u| blk: {
                const operand = try self.resolveValue(u.operand, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFNeg(operand, "fneg");
                }
                break :blk self.builder.buildNeg(operand, "neg");
            },
            // Comparison (int/float dispatch)
            .eq => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealOEQ, lhs, rhs, "feq");
                }
                break :blk self.builder.buildICmp(llvm.c.LLVMIntEQ, lhs, rhs, "eq");
            },
            .ne => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealONE, lhs, rhs, "fne");
                }
                break :blk self.builder.buildICmp(llvm.c.LLVMIntNE, lhs, rhs, "ne");
            },
            .lt => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealOLT, lhs, rhs, "flt");
                }
                break :blk self.builder.buildICmp(llvm.c.LLVMIntSLT, lhs, rhs, "lt");
            },
            .le => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealOLE, lhs, rhs, "fle");
                }
                break :blk self.builder.buildICmp(llvm.c.LLVMIntSLE, lhs, rhs, "le");
            },
            .gt => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealOGT, lhs, rhs, "fgt");
                }
                break :blk self.builder.buildICmp(llvm.c.LLVMIntSGT, lhs, rhs, "gt");
            },
            .ge => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealOGE, lhs, rhs, "fge");
                }
                break :blk self.builder.buildICmp(llvm.c.LLVMIntSGE, lhs, rhs, "ge");
            },
            // Memory: alloca
            .alloca => |a| blk: {
                const alloca_val = self.builder.buildAlloca(self.mapType(a.alloc_type), "alloca");
                if (a.alloc_type.* == .string_type) {
                    try self.string_ptrs.put(inst.id, {});
                }
                break :blk alloca_val;
            },
            // Memory: load (string loads return the pointer itself)
            .load => |l| blk: {
                if (l.load_type.* == .string_type) {
                    const ptr_val = try self.resolveValue(l.ptr, func);
                    try self.string_ptrs.put(inst.id, {});
                    break :blk ptr_val;
                }
                break :blk self.builder.buildLoad2(self.mapType(l.load_type), try self.resolveValue(l.ptr, func), "load");
            },
            // Memory: store (string stores copy the struct via load+store)
            .store => |s| blk: {
                const src = try self.resolveValue(s.value, func);
                const dst = try self.resolveValue(s.ptr, func);
                if (self.isStringValue(s.value)) {
                    // String copy: load struct from source alloca, store to dest alloca
                    const str_ty = self.getDmStringType();
                    const loaded = self.builder.buildLoad2(str_ty, src, "strcpy");
                    _ = self.builder.buildStore(loaded, dst);
                } else {
                    _ = self.builder.buildStore(src, dst);
                }
                break :blk llvm.Value{ .ref = null };
            },
            // Struct: extractfield → LLVM extractvalue
            .extractfield => |ef| blk: {
                const base = try self.resolveValue(ef.base, func);
                break :blk self.builder.buildExtractValue(base, ef.field_index, "field");
            },
            // Struct: insertfield → LLVM insertvalue
            .insertfield => |ins| blk: {
                const base_val = switch (ins.base) {
                    .undef => llvm.Value.getUndef(self.mapType(inst.result_type orelse break :blk llvm.Value{ .ref = null })),
                    else => try self.resolveValue(ins.base, func),
                };
                const val = try self.resolveValue(ins.value, func);
                break :blk self.builder.buildInsertValue(base_val, val, ins.field_index, "insert");
            },
            // Function calls (with wrapper handling)
            .call => |call| try self.generateCallInst(call, inst, func),
            // Logical operators
            .logical_and => |b| self.builder.buildAnd(try self.resolveValue(b.lhs, func), try self.resolveValue(b.rhs, func), "and"),
            .logical_or => |b| self.builder.buildOr(try self.resolveValue(b.lhs, func), try self.resolveValue(b.rhs, func), "or"),
            .logical_not => |u| self.builder.buildNot(try self.resolveValue(u.operand, func), "not"),
            // Unimplemented ops fallback
            else => llvm.Value{ .ref = null },
        };

        if (result.ref != null and inst.result_type != null) {
            try self.value_map.put(inst.id, result);
        }
    }

    /// Check if an IR Value represents a pointer to a dm_string struct
    fn isStringValue(self: *LLVMGenerator, val: ir.Value) bool {
        return switch (val) {
            .instruction_ref => |id| self.string_ptrs.contains(id),
            else => false,
        };
    }

    // ====================================================================
    // Call Instruction Generation
    // ====================================================================

    /// Generate a call instruction, routing to wrapper if needed
    fn generateCallInst(self: *LLVMGenerator, call: ir.CallOp, inst: *const ir.Instruction, func: *const ir.Function) !llvm.Value {
        const callee = self.function_map.get(call.callee) orelse return llvm.Value{ .ref = null };
        const fn_ty = self.fn_type_map.get(call.callee) orelse return llvm.Value{ .ref = null };

        if (self.wrapper_map.get(call.callee)) |wrapper| {
            return self.generateWrapperCall(call, callee, fn_ty, wrapper, inst, func);
        }

        // Regular (non-wrapper) call
        var args = std.ArrayList(llvm.Value).init(self.allocator);
        defer args.deinit();
        for (call.args) |arg| {
            try args.append(try self.resolveValue(arg, func));
        }
        const name: [*:0]const u8 = if (inst.result_type != null and inst.result_type.?.* != .void_type) "call" else "";
        return self.builder.buildCall2(fn_ty, callee, args.items, name);
    }

    /// Generate a call to a wrapper function with ABI-safe pointer passing
    fn generateWrapperCall(
        self: *LLVMGenerator,
        call: ir.CallOp,
        callee: llvm.Value,
        fn_ty: llvm.Type,
        wrapper: WrapperInfo,
        inst: *const ir.Instruction,
        func: *const ir.Function,
    ) !llvm.Value {
        var args = std.ArrayList(llvm.Value).init(self.allocator);
        defer args.deinit();

        if (wrapper.returns_string) {
            // Allocate stack space for the string result (sret pattern)
            const sret_name = try std.fmt.allocPrintZ(self.allocator, "sret.{}", .{self.sret_counter});
            defer self.allocator.free(sret_name);
            self.sret_counter += 1;
            const sret = self.builder.buildAlloca(self.getDmStringType(), sret_name);
            try args.append(sret);

            // Resolve remaining args (string args are already pointers from value_map)
            for (call.args) |arg| {
                try args.append(try self.resolveValue(arg, func));
            }

            // Call wrapper (returns void, writes result to sret)
            _ = self.builder.buildCall2(fn_ty, callee, args.items, "");

            // The sret alloca IS the result — a pointer to the dm_string
            try self.string_ptrs.put(inst.id, {});
            return sret;
        }

        // Non-string return, but has string params passed as pointers
        for (call.args) |arg| {
            try args.append(try self.resolveValue(arg, func));
        }
        const name: [*:0]const u8 = if (inst.result_type != null and inst.result_type.?.* != .void_type) "call" else "";
        return self.builder.buildCall2(fn_ty, callee, args.items, name);
    }

    // ====================================================================
    // Terminator Generation
    // ====================================================================

    fn generateTerminator(self: *LLVMGenerator, term: ir.Terminator, func: *const ir.Function) !void {
        switch (term) {
            .br => |label| {
                const bb = self.block_map.get(label) orelse return;
                _ = self.builder.buildBr(bb);
            },
            .br_cond => |bc| {
                const cond = switch (bc.cond) {
                    .instruction_ref => |id| self.value_map.get(id) orelse return,
                    .const_bool => |b| llvm.Value.constInt(llvm.Type.int1(self.context), if (b) 1 else 0, false),
                    else => return,
                };
                const true_bb = self.block_map.get(bc.true_label) orelse return;
                const false_bb = self.block_map.get(bc.false_label) orelse return;
                _ = self.builder.buildCondBr(cond, true_bb, false_bb);
            },
            .ret => |val| {
                const ret_type = if (std.mem.eql(u8, func.name, "main"))
                    llvm.Type.int32(self.context)
                else
                    self.mapType(func.return_type);
                _ = self.builder.buildRet(self.resolveValueWithType(val, ret_type, func));
            },
            .ret_void => {
                if (std.mem.eql(u8, func.name, "main")) {
                    _ = self.builder.buildRet(llvm.Value.constInt(llvm.Type.int32(self.context), 0, false));
                } else {
                    _ = self.builder.buildRetVoid();
                }
            },
            .unreachable_term => {
                _ = self.builder.buildRetVoid();
            },
        }
    }

    // ====================================================================
    // Value Resolution
    // ====================================================================

    fn resolveValue(self: *LLVMGenerator, val: ir.Value, func: *const ir.Function) !llvm.Value {
        return switch (val) {
            .const_int => |v| llvm.Value.constSignedInt(llvm.Type.int64(self.context), v),
            .const_float => |v| llvm.Value.constReal(llvm.Type.double(self.context), v),
            .const_bool => |v| llvm.Value.constInt(llvm.Type.int1(self.context), if (v) 1 else 0, false),
            .instruction_ref => |id| self.value_map.get(id) orelse error.UnresolvedInstructionRef,
            .param_ref => |id| blk: {
                const llvm_fn = self.function_map.get(func.name) orelse return error.UnresolvedFunction;
                break :blk llvm_fn.getParam(id);
            },
            .global_ref => |name| self.function_map.get(name) orelse error.UnresolvedGlobalRef,
            .undef => llvm.Value.constNull(llvm.Type.int64(self.context)),
            .const_string => |s| blk: {
                const str_z = try self.allocator.dupeZ(u8, s);
                defer self.allocator.free(str_z);
                const str_name = try std.fmt.allocPrintZ(self.allocator, ".str.{}", .{self.string_counter});
                defer self.allocator.free(str_name);
                self.string_counter += 1;
                break :blk self.builder.buildGlobalStringPtr(str_z, str_name);
            },
        };
    }

    /// Resolve a value using a specific LLVM type for constants (used by ret)
    fn resolveValueWithType(self: *LLVMGenerator, val: ir.Value, ty: llvm.Type, func: *const ir.Function) llvm.Value {
        return switch (val) {
            .const_int => |v| llvm.Value.constSignedInt(ty, v),
            .const_float => |v| llvm.Value.constReal(ty, v),
            .const_bool => |v| llvm.Value.constInt(llvm.Type.int1(self.context), if (v) 1 else 0, false),
            .instruction_ref => |id| self.value_map.get(id) orelse llvm.Value.constNull(ty),
            .param_ref => |id| blk: {
                const llvm_fn = self.function_map.get(func.name) orelse break :blk llvm.Value.constNull(ty);
                break :blk llvm_fn.getParam(id);
            },
            else => llvm.Value.constNull(ty),
        };
    }
};

// ====================================================================
// Tests
// ====================================================================

test "LLVM generator basic" {
    const allocator = std.testing.allocator;
    llvm.initializeAllTargets();

    // Create a simple IR module with one function
    var ir_builder = ir.IRBuilder.init(allocator);
    defer ir_builder.deinit();

    var ir_module = ir.Module.init(allocator);
    defer ir_module.deinit();

    const void_type = try ir_builder.allocType(.void_type);

    // Create: fn dm_main() -> void { ret void }
    var func = try ir_module.addFunction("dm_main", &.{}, void_type);
    const entry = try func.addBlock("entry");
    ir_builder.setFunction(func);
    ir_builder.setInsertBlock(entry);
    ir_builder.buildRetVoid();

    // Generate LLVM IR
    var gen = LLVMGenerator.init(allocator, "test");
    defer gen.deinit();

    try gen.generate(&ir_module);
    try gen.module.verify();
}
