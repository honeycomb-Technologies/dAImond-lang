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
    /// Reference to the IR module for type lookups
    ir_module_ref: ?*const ir.Module = null,
    /// Map from global variable name to LLVM value (for global_ref resolution)
    global_var_map: std.StringHashMap(llvm.Value),
    /// Deferred string global initializations (name -> string value)
    deferred_string_globals: std.ArrayList(DeferredStringGlobal),

    const DeferredStringGlobal = struct { name: []const u8, value: []const u8 };

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
            .global_var_map = std.StringHashMap(llvm.Value).init(allocator),
            .deferred_string_globals = std.ArrayList(DeferredStringGlobal).init(allocator),
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
        self.deferred_string_globals.deinit();
        self.struct_type_map.deinit();
        self.global_var_map.deinit();
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
                // Check if it's a map type (struct with ptr + i64 + i64 layout, same as list)
                if (std.mem.startsWith(u8, s.name, "dm_map_")) {
                    const name_z = std.fmt.allocPrintZ(self.allocator, "{s}", .{s.name}) catch break :blk llvm.Type.int64(self.context);
                    defer self.allocator.free(name_z);
                    break :blk self.getListType(name_z);
                }
                break :blk llvm.Type.int64(self.context);
            },
            .result_type => |r| blk: {
                // Result[T, E] → struct { i32 tag, T ok_value, E err_value }
                var fields: [3]llvm.Type = .{
                    llvm.Type.int32(self.context), // tag
                    self.mapType(r.ok),             // ok value
                    self.mapType(r.err),            // err value
                };
                break :blk llvm.Type.struct_type(self.context, &fields, false);
            },
            .option_type => |inner| blk: {
                // Option[T] → struct { i32 has_value, T value }
                var fields: [2]llvm.Type = .{
                    llvm.Type.int32(self.context), // has_value (0=None, 1=Some)
                    self.mapType(inner),            // value
                };
                break :blk llvm.Type.struct_type(self.context, &fields, false);
            },
            .fn_type => llvm.Type.ptr(self.context), // fn types are always pointers in LLVM
            .fn_ptr => llvm.Type.ptr(self.context),
            .vector_type => |v| blk: {
                const elem_ty: llvm.Type = switch (v.elem_kind) {
                    .f32_elem => llvm.Type.float(self.context),
                    .f64_elem => llvm.Type.double(self.context),
                    .i32_elem => llvm.Type.int32(self.context),
                    .i64_elem => llvm.Type.int64(self.context),
                };
                break :blk llvm.Type.vector(elem_ty, v.lanes);
            },
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

    /// Declare and generate a wrapper for a user-declared extern function that uses dm_string.
    /// For `extern fn getenv(name: string) -> string`:
    /// 1. Declare the real C function `getenv` as `ptr(i8) getenv(ptr(i8))`
    /// 2. Generate a wrapper function that converts dm_string <-> char*
    /// 3. Store the wrapper under the original name for call resolution
    fn declareUserExternWrapper(self: *LLVMGenerator, func: *const ir.Function) !void {
        const returns_string = func.return_type.* == .string_type;
        const ptr_ty = llvm.Type.ptr(self.context);
        const void_ty = llvm.Type.void_type(self.context);

        // Step 1: Declare the real C function with C types
        // For string params: use ptr (char*)
        // For string return: use ptr (char*)
        var c_param_types = std.ArrayList(llvm.Type).init(self.allocator);
        defer c_param_types.deinit();
        for (func.params) |p| {
            if (p.ty.* == .string_type) {
                try c_param_types.append(ptr_ty); // char*
            } else {
                try c_param_types.append(self.mapType(p.ty));
            }
        }
        const c_ret_ty = if (returns_string) ptr_ty else self.mapType(func.return_type);
        const c_fn_ty = llvm.Type.function(c_ret_ty, c_param_types.items, false);

        const real_fn_name = try self.allocator.dupeZ(u8, func.name);
        defer self.allocator.free(real_fn_name);
        const real_fn = self.module.addFunction(real_fn_name, c_fn_ty);

        // Step 2: Declare the wrapper function with dm_string pointer-based API
        // Same convention as runtime wrappers: string params -> ptr, string return -> sret ptr param
        var wrapper_params = std.ArrayList(llvm.Type).init(self.allocator);
        defer wrapper_params.deinit();

        if (returns_string) {
            try wrapper_params.append(ptr_ty); // sret output pointer
        }
        for (func.params) |p| {
            if (p.ty.* == .string_type) {
                try wrapper_params.append(ptr_ty); // dm_string* pointer
            } else {
                try wrapper_params.append(self.mapType(p.ty));
            }
        }
        const wrapper_ret_ty = if (returns_string) void_ty else self.mapType(func.return_type);
        const wrapper_fn_ty = llvm.Type.function(wrapper_ret_ty, wrapper_params.items, false);

        const wrapper_name = try std.fmt.allocPrintZ(self.allocator, "llvm_{s}", .{func.name});
        defer self.allocator.free(wrapper_name);
        const wrapper_fn = self.module.addFunction(wrapper_name, wrapper_fn_ty);

        // Step 3: Generate the wrapper function body
        const entry_bb = llvm.BasicBlock.append(wrapper_fn, self.context, "entry");
        // Save and restore builder position
        const saved_bb = self.builder.getInsertBlock();
        self.builder.positionAtEnd(entry_bb);

        // Build the call to the real C function, converting args
        var c_args = std.ArrayList(llvm.Value).init(self.allocator);
        defer c_args.deinit();

        var wrapper_param_idx: u32 = 0;
        if (returns_string) wrapper_param_idx = 1; // skip sret param

        for (func.params) |p| {
            const wrapper_param = wrapper_fn.getParam(wrapper_param_idx);
            if (p.ty.* == .string_type) {
                // Extract the .data field (field 0, which is a char*) from dm_string*
                const data_ptr = self.builder.buildStructGEP2(self.getDmStringType(), wrapper_param, 0, "data_ptr");
                const data = self.builder.buildLoad2(ptr_ty, data_ptr, "data");
                try c_args.append(data);
            } else {
                try c_args.append(wrapper_param);
            }
            wrapper_param_idx += 1;
        }

        const c_result = self.builder.buildCall2(c_fn_ty, real_fn, c_args.items, if (returns_string) "c_result" else "");

        if (returns_string) {
            // Convert char* result back to dm_string via dm_string_new
            // We need to call llvm_dm_string_new(sret_ptr, char_ptr)
            const sret_ptr = wrapper_fn.getParam(0);

            // Look up llvm_dm_string_new wrapper (stored under "dm_string_new" in function_map)
            const dm_str_new_fn = self.function_map.get("dm_string_new") orelse blk: {
                const new_fn_ty = llvm.Type.function(void_ty, &.{ ptr_ty, ptr_ty }, false);
                break :blk self.module.addFunction("llvm_dm_string_new", new_fn_ty);
            };
            const dm_str_new_fn_ty = self.fn_type_map.get("dm_string_new") orelse llvm.Type.function(void_ty, &.{ ptr_ty, ptr_ty }, false);

            // Handle NULL return from C function (e.g., getenv returns NULL for missing vars)
            // If result is NULL, create an empty string
            const null_val = llvm.Value.constNull(ptr_ty);
            const is_null = self.builder.buildICmp(llvm.c.LLVMIntEQ, c_result, null_val, "is_null");

            const then_bb = llvm.BasicBlock.append(wrapper_fn, self.context, "then_null");
            const else_bb = llvm.BasicBlock.append(wrapper_fn, self.context, "else_nonnull");
            const merge_bb = llvm.BasicBlock.append(wrapper_fn, self.context, "merge");

            _ = self.builder.buildCondBr(is_null, then_bb, else_bb);

            // Null case: create empty string
            self.builder.positionAtEnd(then_bb);
            const empty_str = self.builder.buildGlobalStringPtr("", "empty_str");
            _ = self.builder.buildCall2(dm_str_new_fn_ty, dm_str_new_fn, &.{ sret_ptr, empty_str }, "");
            _ = self.builder.buildBr(merge_bb);

            // Non-null case: create string from C result
            self.builder.positionAtEnd(else_bb);
            _ = self.builder.buildCall2(dm_str_new_fn_ty, dm_str_new_fn, &.{ sret_ptr, c_result }, "");
            _ = self.builder.buildBr(merge_bb);

            self.builder.positionAtEnd(merge_bb);
            _ = self.builder.buildRetVoid();
        } else {
            if (func.return_type.* == .void_type) {
                _ = self.builder.buildRetVoid();
            } else {
                _ = self.builder.buildRet(c_result);
            }
        }

        // Restore builder position
        if (saved_bb.ref != null) {
            self.builder.positionAtEnd(saved_bb);
        }

        // Store wrapper under ORIGINAL name for call resolution
        try self.function_map.put(func.name, wrapper_fn);
        try self.fn_type_map.put(func.name, wrapper_fn_ty);
        try self.wrapper_map.put(func.name, .{ .returns_string = returns_string });
    }

    // ====================================================================
    // Top-level Generation
    // ====================================================================

    /// Generate LLVM IR from a dAImond IR module
    pub fn generate(self: *LLVMGenerator, ir_module: *const ir.Module) !void {
        self.ir_module_ref = ir_module;
        _ = self.getDmStringType();

        // Phase 0a: Create LLVM named struct types (opaque, no body yet)
        // This ensures all struct types are registered before resolving field types,
        // handling forward references between structs (e.g. ParseResult containing ParseState).
        {
            var struct_iter = ir_module.struct_defs.iterator();
            while (struct_iter.next()) |entry| {
                const ir_type = entry.value_ptr.*;
                if (ir_type.* == .struct_type) {
                    const name_z = try self.allocator.dupeZ(u8, ir_type.struct_type.name);
                    defer self.allocator.free(name_z);
                    const llvm_struct = llvm.Type.namedStruct(self.context, name_z);
                    try self.struct_type_map.put(ir_type.struct_type.name, llvm_struct);
                }
            }
        }

        // Phase 0b: Set struct bodies (now all struct types are known)
        {
            var struct_iter = ir_module.struct_defs.iterator();
            while (struct_iter.next()) |entry| {
                const ir_type = entry.value_ptr.*;
                if (ir_type.* == .struct_type) {
                    const llvm_struct = self.struct_type_map.get(ir_type.struct_type.name) orelse continue;
                    var field_types = std.ArrayList(llvm.Type).init(self.allocator);
                    defer field_types.deinit();
                    for (ir_type.struct_type.fields) |field| {
                        try field_types.append(self.mapType(field.ty));
                    }
                    llvm_struct.structSetBody(field_types.items, false);
                }
            }
        }

        // Phase 0c: Generate global variables
        for (ir_module.globals.items) |global| {
            const llvm_ty = self.mapType(global.ty);
            const name_z = try self.allocator.dupeZ(u8, global.name);
            defer self.allocator.free(name_z);
            const gvar = self.module.addGlobal(llvm_ty, name_z);
            // Set initializer
            if (global.init_value) |init_val| {
                switch (init_val) {
                    .const_int => |v| gvar.setInitializer(llvm.Value.constSignedInt(llvm_ty, v)),
                    .const_float => |v| gvar.setInitializer(llvm.Value.constReal(llvm_ty, v)),
                    .const_bool => |v| gvar.setInitializer(llvm.Value.constInt(llvm_ty, @intFromBool(v), false)),
                    .const_string => |s| {
                        // String globals need runtime init via dm_string_new
                        // Defer to start of main()
                        gvar.setInitializer(llvm.Value.constNull(llvm_ty));
                        try self.deferred_string_globals.append(.{ .name = global.name, .value = s });
                    },
                    else => gvar.setInitializer(llvm.Value.constNull(llvm_ty)),
                }
            } else {
                gvar.setInitializer(llvm.Value.constNull(llvm_ty));
            }
            // String globals can't be LLVM constants since they're initialized at runtime
            const is_deferred_string = if (global.init_value) |iv| iv == .const_string else false;
            if (global.is_const and !is_deferred_string) gvar.setGlobalConstant(true);
            try self.global_var_map.put(global.name, gvar);
        }

        // Phase 1: Declare all functions
        for (ir_module.functions.items) |func| {
            if (needsWrapper(&func)) {
                // Check if this is a user-declared extern (not a runtime builtin)
                if (ir_module.user_extern_fns.contains(func.name)) {
                    try self.declareUserExternWrapper(&func);
                    continue;
                }
                try self.declareWrapper(&func);
                continue; // Don't declare the original
            }

            var param_types = std.ArrayList(llvm.Type).init(self.allocator);
            defer param_types.deinit();
            for (func.params) |p| {
                try param_types.append(self.mapType(p.ty));
            }

            // C ABI: main returns i32 and takes (i32 argc, i8** argv)
            const is_main = std.mem.eql(u8, func.name, "main");
            const ret_type = if (is_main)
                llvm.Type.int32(self.context)
            else
                self.mapType(func.return_type);
            if (is_main) {
                // Add argc (i32) and argv (i8**) params
                try param_types.append(llvm.Type.int32(self.context));
                try param_types.append(llvm.Type.ptr(self.context));
            }
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
        var is_first_block = true;
        for (func.blocks.items) |block| {
            const bb = self.block_map.get(block.label) orelse continue;
            self.builder.positionAtEnd(bb);

            // At the start of main(): store argc/argv to globals for dm_args_len/get
            if (is_first_block and std.mem.eql(u8, func.name, "main")) {
                // Declare dm_argc (i32) and dm_argv (i8**) as external globals
                const i32_ty = llvm.Type.int32(self.context);
                const ptr_ty = llvm.Type.ptr(self.context);

                const argc_global = self.module.addGlobal(i32_ty, "dm_argc");
                argc_global.setExternallyInitialized(true);
                argc_global.setLinkage(llvm.c.LLVMExternalLinkage);

                const argv_global = self.module.addGlobal(ptr_ty, "dm_argv");
                argv_global.setExternallyInitialized(true);
                argv_global.setLinkage(llvm.c.LLVMExternalLinkage);

                // Store argc and argv from main's params
                const argc_param = llvm_fn.getParam(@intCast(func.params.len));
                const argv_param = llvm_fn.getParam(@intCast(func.params.len + 1));
                _ = self.builder.buildStore(argc_param, argc_global);
                _ = self.builder.buildStore(argv_param, argv_global);
            }

            // Emit deferred string global initializations at the start of main()
            if (is_first_block and std.mem.eql(u8, func.name, "main")) {
                for (self.deferred_string_globals.items) |dsg| {
                    if (self.global_var_map.get(dsg.name)) |gvar| {
                        // Use the same sret pattern as regular string literals
                        const str_z = try self.allocator.dupeZ(u8, dsg.value);
                        defer self.allocator.free(str_z);
                        const raw_str = self.builder.buildGlobalStringPtr(str_z, "str_init");
                        // Call dm_string_new wrapper (stored under original name in function_map)
                        if (self.function_map.get("dm_string_new")) |wrapper_fn| {
                            if (self.fn_type_map.get("dm_string_new")) |wrapper_type| {
                                const sret_name = try std.fmt.allocPrintZ(self.allocator, "sret.{}", .{self.sret_counter});
                                defer self.allocator.free(sret_name);
                                self.sret_counter += 1;
                                const sret = self.builder.buildAlloca(self.getDmStringType(), sret_name);
                                var args = [_]llvm.Value{ sret, raw_str };
                                _ = self.builder.buildCall2(wrapper_type, wrapper_fn, &args, "");
                                const str_val = self.builder.buildLoad2(self.getDmStringType(), sret, "str_global_val");
                                _ = self.builder.buildStore(str_val, gvar);
                            }
                        }
                    }
                }
            }
            is_first_block = false;

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

    /// Coerce two integer LLVM values to the same bit width.
    /// If widths differ, the narrower operand is sign-extended to match the wider one.
    /// Returns the (possibly cast) pair, plus the result type.
    fn coerceIntOperands(self: *LLVMGenerator, lhs_in: llvm.Value, rhs_in: llvm.Value) struct { lhs: llvm.Value, rhs: llvm.Value } {
        var lhs = lhs_in;
        var rhs = rhs_in;
        if (lhs.ref == null or rhs.ref == null) return .{ .lhs = lhs, .rhs = rhs };
        const lk = lhs.typeOf().getTypeKind();
        const rk = rhs.typeOf().getTypeKind();
        if (lk == llvm.c.LLVMIntegerTypeKind and rk == llvm.c.LLVMIntegerTypeKind) {
            const lw = lhs.typeOf().getIntTypeWidth();
            const rw = rhs.typeOf().getIntTypeWidth();
            if (lw > rw) {
                rhs = self.builder.buildIntCast2(rhs, lhs.typeOf(), true, "coerce");
            } else if (rw > lw) {
                lhs = self.builder.buildIntCast2(lhs, rhs.typeOf(), true, "coerce");
            }
        }
        return .{ .lhs = lhs, .rhs = rhs };
    }

    fn generateInstruction(self: *LLVMGenerator, inst: *const ir.Instruction, func: *const ir.Function) !void {
        const result = switch (inst.op) {
            // Arithmetic (int/float dispatch)
            .add => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFAdd(lhs, rhs, "fadd");
                }
                const coerced = self.coerceIntOperands(lhs, rhs);
                break :blk self.builder.buildAdd(coerced.lhs, coerced.rhs, "add");
            },
            .sub => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFSub(lhs, rhs, "fsub");
                }
                const coerced = self.coerceIntOperands(lhs, rhs);
                break :blk self.builder.buildSub(coerced.lhs, coerced.rhs, "sub");
            },
            .mul => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFMul(lhs, rhs, "fmul");
                }
                const coerced = self.coerceIntOperands(lhs, rhs);
                break :blk self.builder.buildMul(coerced.lhs, coerced.rhs, "mul");
            },
            .div => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFDiv(lhs, rhs, "fdiv");
                }
                const coerced = self.coerceIntOperands(lhs, rhs);
                break :blk self.builder.buildSDiv(coerced.lhs, coerced.rhs, "div");
            },
            .mod => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                const rhs = try self.resolveValue(b.rhs, func);
                if (isIRFloat(inst.result_type)) {
                    break :blk self.builder.buildFRem(lhs, rhs, "frem");
                }
                const coerced = self.coerceIntOperands(lhs, rhs);
                break :blk self.builder.buildSRem(coerced.lhs, coerced.rhs, "mod");
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
                var rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealOEQ, lhs, rhs, "feq");
                }
                // Pointer comparison: coerce integer to null pointer
                if (lhs.typeOf().getTypeKind() == llvm.c.LLVMPointerTypeKind and
                    rhs.typeOf().getTypeKind() == llvm.c.LLVMIntegerTypeKind)
                {
                    rhs = llvm.Value.constNull(lhs.typeOf());
                } else if (rhs.typeOf().getTypeKind() == llvm.c.LLVMPointerTypeKind and
                    lhs.typeOf().getTypeKind() == llvm.c.LLVMIntegerTypeKind)
                {
                    // This case shouldn't happen often, but handle it
                    break :blk self.builder.buildICmp(llvm.c.LLVMIntEQ, rhs, llvm.Value.constNull(rhs.typeOf()), "eq");
                }
                rhs = coerceIntWidth(self.builder, lhs, rhs, "trunc.eq");
                break :blk self.builder.buildICmp(llvm.c.LLVMIntEQ, lhs, rhs, "eq");
            },
            .ne => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                var rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealONE, lhs, rhs, "fne");
                }
                // Pointer comparison: coerce integer to null pointer
                if (lhs.typeOf().getTypeKind() == llvm.c.LLVMPointerTypeKind and
                    rhs.typeOf().getTypeKind() == llvm.c.LLVMIntegerTypeKind)
                {
                    rhs = llvm.Value.constNull(lhs.typeOf());
                }
                rhs = coerceIntWidth(self.builder, lhs, rhs, "trunc.ne");
                break :blk self.builder.buildICmp(llvm.c.LLVMIntNE, lhs, rhs, "ne");
            },
            .lt => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                var rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealOLT, lhs, rhs, "flt");
                }
                rhs = coerceIntWidth(self.builder, lhs, rhs, "trunc.lt");
                break :blk self.builder.buildICmp(llvm.c.LLVMIntSLT, lhs, rhs, "lt");
            },
            .le => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                var rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealOLE, lhs, rhs, "fle");
                }
                rhs = coerceIntWidth(self.builder, lhs, rhs, "trunc.le");
                break :blk self.builder.buildICmp(llvm.c.LLVMIntSLE, lhs, rhs, "le");
            },
            .gt => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                var rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealOGT, lhs, rhs, "fgt");
                }
                rhs = coerceIntWidth(self.builder, lhs, rhs, "trunc.gt");
                break :blk self.builder.buildICmp(llvm.c.LLVMIntSGT, lhs, rhs, "gt");
            },
            .ge => |b| blk: {
                const lhs = try self.resolveValue(b.lhs, func);
                var rhs = try self.resolveValue(b.rhs, func);
                if (isLLVMFloat(lhs)) {
                    break :blk self.builder.buildFCmp(llvm.c.LLVMRealOGE, lhs, rhs, "fge");
                }
                rhs = coerceIntWidth(self.builder, lhs, rhs, "trunc.ge");
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
                var src = try self.resolveValue(s.value, func);
                const dst = try self.resolveValue(s.ptr, func);
                if (self.isStringValue(s.value)) {
                    // String copy: load struct from source alloca, store to dest alloca
                    const str_ty = self.getDmStringType();
                    const loaded = self.builder.buildLoad2(str_ty, src, "strcpy");
                    _ = self.builder.buildStore(loaded, dst);
                } else {
                    // Coerce value types to match alloca element type
                    if (src.ref != null and dst.ref != null) {
                        const alloca_ty = self.getAllocaElemType(s.ptr, func);
                        if (alloca_ty) |elem_ty| {
                            const src_kind = src.typeOf().getTypeKind();
                            const elem_kind = elem_ty.getTypeKind();
                            // Int-to-int width coercion (e.g., i64 const → i32 alloca)
                            if (src_kind == llvm.c.LLVMIntegerTypeKind and elem_kind == llvm.c.LLVMIntegerTypeKind) {
                                const src_width = src.typeOf().getIntTypeWidth();
                                const dst_width = elem_ty.getIntTypeWidth();
                                if (src_width != dst_width) {
                                    src = self.builder.buildIntCast2(src, elem_ty, true, "store.cast");
                                }
                            }
                            // Float-to-float width coercion (e.g., double const → float alloca)
                            const src_is_float = src_kind == llvm.c.LLVMFloatTypeKind or src_kind == llvm.c.LLVMDoubleTypeKind;
                            const dst_is_float = elem_kind == llvm.c.LLVMFloatTypeKind or elem_kind == llvm.c.LLVMDoubleTypeKind;
                            if (src_is_float and dst_is_float and src_kind != elem_kind) {
                                src = self.builder.buildFPCast(src, elem_ty, "store.fpcast");
                            }
                        }
                    }
                    _ = self.builder.buildStore(src, dst);
                }
                break :blk llvm.Value{ .ref = null };
            },
            // Struct: extractfield → LLVM extractvalue
            .extractfield => |ef| blk: {
                const base = try self.resolveValue(ef.base, func);
                const result_val = self.builder.buildExtractValue(base, ef.field_index, "field");
                // If the extracted field is a dm_string, store to alloca and track as string ptr
                if (ef.field_type) |ft| {
                    if (ft.* == .string_type) {
                        const str_alloca = self.builder.buildAlloca(self.getDmStringType(), "str.extract");
                        _ = self.builder.buildStore(result_val, str_alloca);
                        try self.string_ptrs.put(inst.id, {});
                        break :blk str_alloca;
                    }
                }
                break :blk result_val;
            },
            // Struct: insertfield → LLVM insertvalue
            .insertfield => |ins| blk: {
                const base_val = switch (ins.base) {
                    .undef => llvm.Value.getUndef(self.mapType(inst.result_type orelse break :blk llvm.Value{ .ref = null })),
                    else => try self.resolveValue(ins.base, func),
                };
                var val = try self.resolveValue(ins.value, func);
                // If inserting a string value (which is a pointer to dm_string),
                // load the actual struct value first
                if (self.isStringValue(ins.value)) {
                    val = self.builder.buildLoad2(self.getDmStringType(), val, "str.load");
                }
                // Coerce integer width to match the struct field type
                const base_type = base_val.typeOf();
                if (base_type.getTypeKind() == llvm.c.LLVMStructTypeKind) {
                    const field_type = base_type.getStructElementType(ins.field_index);
                    const val_type = val.typeOf();
                    if (val_type.getTypeKind() == llvm.c.LLVMIntegerTypeKind and
                        field_type.getTypeKind() == llvm.c.LLVMIntegerTypeKind)
                    {
                        const val_width = val_type.getIntTypeWidth();
                        const field_width = field_type.getIntTypeWidth();
                        if (val_width > field_width) {
                            val = self.builder.buildTrunc(val, field_type, "trunc.field");
                        } else if (val_width < field_width) {
                            val = self.builder.buildSExt(val, field_type, "sext.field");
                        }
                    }
                }
                break :blk self.builder.buildInsertValue(base_val, val, ins.field_index, "insert");
            },
            // Function calls (with wrapper handling)
            .call => |call| try self.generateCallInst(call, inst, func),
            // Indirect function call (through function pointer)
            .callptr => |cp| try self.generateCallPtrInst(cp, inst, func),
            // Logical operators
            .logical_and => |b| self.builder.buildAnd(try self.resolveValue(b.lhs, func), try self.resolveValue(b.rhs, func), "and"),
            .logical_or => |b| self.builder.buildOr(try self.resolveValue(b.lhs, func), try self.resolveValue(b.rhs, func), "or"),
            .logical_not => |u| self.builder.buildNot(try self.resolveValue(u.operand, func), "not"),
            // Type casts (as)
            .cast => |cast| try self.generateCastInst(cast, func),
            // SIMD operations
            .simd_splat => |s| try self.generateSimdSplat(s, inst, func),
            .simd_set => |s| try self.generateSimdSet(s, inst, func),
            .simd_add => |b| try self.generateSimdBinOp(b, inst, func, .add),
            .simd_sub => |b| try self.generateSimdBinOp(b, inst, func, .sub),
            .simd_mul => |b| try self.generateSimdBinOp(b, inst, func, .mul),
            .simd_div => |b| try self.generateSimdBinOp(b, inst, func, .div),
            .simd_extract => |e| try self.generateSimdExtract(e, inst, func),
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

    /// Get the LLVM element type of an alloca instruction referenced by an IR Value.
    /// Looks up the alloca op in the IR function to find the original allocated type,
    /// then maps it to the corresponding LLVM type.
    fn getAllocaElemType(self: *LLVMGenerator, val: ir.Value, func: *const ir.Function) ?llvm.Type {
        const inst_id = switch (val) {
            .instruction_ref => |id| id,
            else => return null,
        };
        // Search IR instructions for the alloca with this ID
        for (func.blocks.items) |block| {
            for (block.instructions.items) |instr| {
                if (instr.id == inst_id) {
                    if (instr.op == .alloca) {
                        return self.mapType(instr.op.alloca.alloc_type);
                    }
                    return null;
                }
            }
        }
        return null;
    }

    /// Find an IR function by name (for type info lookups)
    fn findIRFunction(self: *LLVMGenerator, name: []const u8) ?*const ir.Function {
        if (self.ir_module_ref) |mod| {
            for (mod.functions.items) |*f| {
                if (std.mem.eql(u8, f.name, name)) return f;
            }
        }
        return null;
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

        // Regular (non-wrapper) call — need to handle string args properly
        // For non-wrapper calls, string params expect dm_string by value, so load from pointer
        var args = std.ArrayList(llvm.Value).init(self.allocator);
        defer args.deinit();

        // Look up the callee function to get expected param types
        const callee_func = self.findIRFunction(call.callee);

        for (call.args, 0..) |arg, i| {
            var val = try self.resolveValue(arg, func);
            // If the arg is a string pointer but the function expects dm_string by value,
            // load the actual struct value
            if (self.isStringValue(arg)) {
                const expects_string = if (callee_func) |cf| blk: {
                    if (i < cf.params.len) break :blk cf.params[i].ty.* == .string_type;
                    break :blk false;
                } else false;
                if (expects_string) {
                    val = self.builder.buildLoad2(self.getDmStringType(), val, "str.arg");
                }
            }
            // Coerce float<->double and int widths to match expected parameter type
            val = self.coerceCallArg(val, callee_func, i);
            try args.append(val);
        }

        // Check if result is dm_string — if so, need to alloca + track as string ptr
        const returns_string = if (callee_func) |cf| cf.return_type.* == .string_type else false;
        const name: [*:0]const u8 = if (inst.result_type != null and inst.result_type.?.* != .void_type) "call" else "";
        const result = self.builder.buildCall2(fn_ty, callee, args.items, name);

        if (returns_string and result.ref != null) {
            // Store the returned dm_string value to an alloca for consistent pointer representation
            const str_alloca = self.builder.buildAlloca(self.getDmStringType(), "str.ret");
            _ = self.builder.buildStore(result, str_alloca);
            try self.string_ptrs.put(inst.id, {});
            return str_alloca;
        }

        return result;
    }

    /// Generate an indirect call through a function pointer
    fn generateCallPtrInst(self: *LLVMGenerator, cp: ir.CallPtrOp, inst: *const ir.Instruction, func: *const ir.Function) !llvm.Value {
        // Resolve the function pointer value
        const fn_ptr = try self.resolveValue(cp.callee, func);

        // Build the LLVM function type for the indirect call
        const ret_llvm_ty = if (inst.result_type) |rt| self.mapType(rt) else llvm.Type.void_type(self.context);
        var param_types = std.ArrayList(llvm.Type).init(self.allocator);
        defer param_types.deinit();
        for (cp.args) |arg| {
            const val = try self.resolveValue(arg, func);
            try param_types.append(val.typeOf());
        }
        const fn_ty = llvm.Type.function(ret_llvm_ty, param_types.items, false);

        // Resolve args
        var args = std.ArrayList(llvm.Value).init(self.allocator);
        defer args.deinit();
        for (cp.args) |arg| {
            try args.append(try self.resolveValue(arg, func));
        }

        const name: [*:0]const u8 = if (inst.result_type != null and inst.result_type.?.* != .void_type) "callptr" else "";
        return self.builder.buildCall2(fn_ty, fn_ptr, args.items, name);
    }

    /// Generate a type cast instruction (as)
    fn generateCastInst(self: *LLVMGenerator, cast: ir.CastOp, func: *const ir.Function) !llvm.Value {
        const val = try self.resolveValue(cast.value, func);
        const from = cast.from_type.*;
        const to = cast.to_type.*;

        const from_is_int = isIntegerIRType(from);
        const to_is_int = isIntegerIRType(to);
        const from_is_float = isFloatIRType(from);
        const to_is_float = isFloatIRType(to);

        if (from_is_int and to_is_int) {
            // Int-to-int: truncate or sign/zero-extend depending on signedness
            const from_unsigned = isUnsignedIRType(from);
            return self.builder.buildIntCast2(val, self.mapType(cast.to_type), !from_unsigned, "cast");
        } else if (from_is_int and to_is_float) {
            // Int-to-float: use unsigned-to-FP for unsigned types, signed for signed
            if (isUnsignedIRType(from)) {
                return self.builder.buildUIToFP(val, self.mapType(cast.to_type), "cast");
            }
            return self.builder.buildSIToFP(val, self.mapType(cast.to_type), "cast");
        } else if (from_is_float and to_is_int) {
            // Float-to-int: FP to signed int
            return self.builder.buildFPToSI(val, self.mapType(cast.to_type), "cast");
        } else if (from_is_float and to_is_float) {
            // Float-to-float: FP cast
            return self.builder.buildFPCast(val, self.mapType(cast.to_type), "cast");
        }
        // Fallback: bitcast or return value unchanged
        return val;
    }

    fn isIntegerIRType(ty: ir.IRType) bool {
        return switch (ty) {
            .i8_type, .i16_type, .i32_type, .i64_type,
            .u8_type, .u16_type, .u32_type, .u64_type,
            .bool_type,
            => true,
            else => false,
        };
    }

    fn isFloatIRType(ty: ir.IRType) bool {
        return switch (ty) {
            .f32_type, .f64_type => true,
            else => false,
        };
    }

    fn isUnsignedIRType(ty: ir.IRType) bool {
        return switch (ty) {
            .u8_type, .u16_type, .u32_type, .u64_type => true,
            else => false,
        };
    }

    // ====================================================================
    // SIMD Operations
    // ====================================================================

    const SimdBinOpKind = enum { add, sub, mul, div };

    /// Generate simd_splat: create a vector with all lanes set to a scalar value
    fn generateSimdSplat(self: *LLVMGenerator, s: ir.SimdSplatOp, inst: *const ir.Instruction, func: *const ir.Function) !llvm.Value {
        const vec_ir_type = inst.result_type orelse return llvm.Value{ .ref = null };
        const vec_llvm_type = self.mapType(vec_ir_type);
        var scalar = try self.resolveValue(s.scalar, func);

        // Coerce scalar to match vector element type
        const vt = vec_ir_type.vector_type;
        scalar = self.coerceSimdScalar(scalar, vt.elem_kind);

        // Build vector by inserting scalar into each lane
        var vec = llvm.Value.getUndef(vec_llvm_type);
        const i32_ty = llvm.Type.int32(self.context);
        var i: u32 = 0;
        while (i < vt.lanes) : (i += 1) {
            const idx = llvm.Value.constInt(i32_ty, i, false);
            vec = self.builder.buildInsertElement(vec, scalar, idx, "splat");
        }
        return vec;
    }

    /// Generate simd_set: create a vector from individual element values
    fn generateSimdSet(self: *LLVMGenerator, s: ir.SimdSetOp, inst: *const ir.Instruction, func: *const ir.Function) !llvm.Value {
        const vec_ir_type = inst.result_type orelse return llvm.Value{ .ref = null };
        const vec_llvm_type = self.mapType(vec_ir_type);
        const vt = vec_ir_type.vector_type;

        var vec = llvm.Value.getUndef(vec_llvm_type);
        const i32_ty = llvm.Type.int32(self.context);
        for (s.elements, 0..) |elem, i| {
            var val = try self.resolveValue(elem, func);
            val = self.coerceSimdScalar(val, vt.elem_kind);
            const idx = llvm.Value.constInt(i32_ty, @intCast(i), false);
            vec = self.builder.buildInsertElement(vec, val, idx, "set");
        }
        return vec;
    }

    /// Generate simd binary operation (add/sub/mul/div) on vectors
    fn generateSimdBinOp(self: *LLVMGenerator, b: ir.BinaryOp, inst: *const ir.Instruction, func: *const ir.Function, op: SimdBinOpKind) !llvm.Value {
        const lhs = try self.resolveValue(b.lhs, func);
        const rhs = try self.resolveValue(b.rhs, func);
        const vec_ir_type = inst.result_type orelse return llvm.Value{ .ref = null };
        const is_float = vec_ir_type.vector_type.elem_kind.isFloat();

        return switch (op) {
            .add => if (is_float) self.builder.buildFAdd(lhs, rhs, "vadd") else self.builder.buildAdd(lhs, rhs, "vadd"),
            .sub => if (is_float) self.builder.buildFSub(lhs, rhs, "vsub") else self.builder.buildSub(lhs, rhs, "vsub"),
            .mul => if (is_float) self.builder.buildFMul(lhs, rhs, "vmul") else self.builder.buildMul(lhs, rhs, "vmul"),
            .div => if (is_float) self.builder.buildFDiv(lhs, rhs, "vdiv") else self.builder.buildSDiv(lhs, rhs, "vdiv"),
        };
    }

    /// Generate simd_extract: extract a scalar element from a vector
    fn generateSimdExtract(self: *LLVMGenerator, e: ir.SimdExtractOp, _: *const ir.Instruction, func: *const ir.Function) !llvm.Value {
        const vec = try self.resolveValue(e.vector, func);
        var idx = try self.resolveValue(e.index, func);
        // LLVM extractelement needs an i32 index
        const idx_kind = idx.typeOf().getTypeKind();
        if (idx_kind == llvm.c.LLVMIntegerTypeKind) {
            const idx_width = idx.typeOf().getIntTypeWidth();
            if (idx_width != 32) {
                idx = self.builder.buildIntCast2(idx, llvm.Type.int32(self.context), true, "idx.cast");
            }
        }
        return self.builder.buildExtractElement(vec, idx, "extract");
    }

    /// Coerce a scalar value to match a SIMD element type
    fn coerceSimdScalar(self: *LLVMGenerator, val: llvm.Value, elem_kind: ir.VectorElementKind) llvm.Value {
        if (val.ref == null) return val;
        const val_kind = val.typeOf().getTypeKind();
        switch (elem_kind) {
            .f32_elem => {
                if (val_kind == llvm.c.LLVMDoubleTypeKind) {
                    return self.builder.buildFPCast(val, llvm.Type.float(self.context), "f64tof32");
                }
                if (val_kind == llvm.c.LLVMIntegerTypeKind) {
                    return self.builder.buildSIToFP(val, llvm.Type.float(self.context), "itof32");
                }
                return val;
            },
            .f64_elem => {
                if (val_kind == llvm.c.LLVMFloatTypeKind) {
                    return self.builder.buildFPCast(val, llvm.Type.double(self.context), "f32tof64");
                }
                if (val_kind == llvm.c.LLVMIntegerTypeKind) {
                    return self.builder.buildSIToFP(val, llvm.Type.double(self.context), "itof64");
                }
                return val;
            },
            .i32_elem => {
                if (val_kind == llvm.c.LLVMIntegerTypeKind) {
                    const width = val.typeOf().getIntTypeWidth();
                    if (width != 32) {
                        return self.builder.buildIntCast2(val, llvm.Type.int32(self.context), true, "toi32");
                    }
                }
                if (val_kind == llvm.c.LLVMFloatTypeKind or val_kind == llvm.c.LLVMDoubleTypeKind) {
                    return self.builder.buildFPToSI(val, llvm.Type.int32(self.context), "ftoi32");
                }
                return val;
            },
            .i64_elem => {
                if (val_kind == llvm.c.LLVMIntegerTypeKind) {
                    const width = val.typeOf().getIntTypeWidth();
                    if (width != 64) {
                        return self.builder.buildIntCast2(val, llvm.Type.int64(self.context), true, "toi64");
                    }
                }
                if (val_kind == llvm.c.LLVMFloatTypeKind or val_kind == llvm.c.LLVMDoubleTypeKind) {
                    return self.builder.buildFPToSI(val, llvm.Type.int64(self.context), "ftoi64");
                }
                return val;
            },
        }
    }

    /// Coerce a call argument value to match the expected parameter type.
    /// Handles float<->double (fpext/fptrunc) and int width mismatches (sext/trunc).
    fn coerceCallArg(self: *LLVMGenerator, val: llvm.Value, ir_func: ?*const ir.Function, param_idx: usize) llvm.Value {
        if (val.ref == null) return val;
        const func_info = ir_func orelse return val;
        if (param_idx >= func_info.params.len) return val;

        const expected_ir_type = func_info.params[param_idx].ty.*;
        const val_kind = val.typeOf().getTypeKind();

        switch (expected_ir_type) {
            .f64_type => {
                // Expected double, actual might be float
                if (val_kind == llvm.c.LLVMFloatTypeKind) {
                    return self.builder.buildFPCast(val, llvm.Type.double(self.context), "fpext");
                }
                // Might be integer (e.g., i32 from simd_extract passed where float expected)
                if (val_kind == llvm.c.LLVMIntegerTypeKind) {
                    return self.builder.buildSIToFP(val, llvm.Type.double(self.context), "itof64");
                }
            },
            .f32_type => {
                // Expected float, actual might be double
                if (val_kind == llvm.c.LLVMDoubleTypeKind) {
                    return self.builder.buildFPCast(val, llvm.Type.float(self.context), "fptrunc");
                }
                if (val_kind == llvm.c.LLVMIntegerTypeKind) {
                    return self.builder.buildSIToFP(val, llvm.Type.float(self.context), "itof32");
                }
            },
            .i64_type => {
                // Expected i64, actual might be i32 (e.g., from i32x4 simd_extract)
                if (val_kind == llvm.c.LLVMIntegerTypeKind) {
                    const width = val.typeOf().getIntTypeWidth();
                    if (width < 64) {
                        return self.builder.buildSExt(val, llvm.Type.int64(self.context), "sext");
                    }
                }
                // Might be float
                if (val_kind == llvm.c.LLVMFloatTypeKind or val_kind == llvm.c.LLVMDoubleTypeKind) {
                    return self.builder.buildFPToSI(val, llvm.Type.int64(self.context), "ftoi64");
                }
            },
            .i32_type => {
                if (val_kind == llvm.c.LLVMIntegerTypeKind) {
                    const width = val.typeOf().getIntTypeWidth();
                    if (width != 32) {
                        return self.builder.buildIntCast2(val, llvm.Type.int32(self.context), true, "icast");
                    }
                }
            },
            else => {},
        }
        return val;
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

            // Resolve remaining args
            const sret_orig_func = self.findIRFunction(call.callee);
            for (call.args, 0..) |arg, i| {
                var val = try self.resolveValue(arg, func);
                if (arg == .const_string) {
                    const param_is_string = if (sret_orig_func) |of| (i < of.params.len and of.params[i].ty.* == .string_type) else false;
                    if (param_is_string) {
                        try args.append(try self.createDmString(val));
                    } else {
                        try args.append(val);
                    }
                } else {
                    // Coerce float<->double and int widths to match expected parameter type
                    val = self.coerceCallArg(val, sret_orig_func, i);
                    try args.append(val);
                }
            }

            // Call wrapper (returns void, writes result to sret)
            _ = self.builder.buildCall2(fn_ty, callee, args.items, "");

            // The sret alloca IS the result — a pointer to the dm_string
            try self.string_ptrs.put(inst.id, {});
            return sret;
        }

        // Non-string return, string params passed as dm_string pointers
        const orig_func = self.findIRFunction(call.callee);
        for (call.args, 0..) |arg, i| {
            var val = try self.resolveValue(arg, func);
            // const_string resolves to raw C string ptr, but wrapper expects dm_string ptr
            // Only wrap if original function parameter is string_type (not ptr/i8)
            if (arg == .const_string) {
                const param_is_string = if (orig_func) |of| (i < of.params.len and of.params[i].ty.* == .string_type) else false;
                if (param_is_string) {
                    try args.append(try self.createDmString(val));
                } else {
                    try args.append(val);
                }
            } else {
                // Coerce float<->double and int widths to match expected parameter type
                val = self.coerceCallArg(val, orig_func, i);
                try args.append(val);
            }
        }
        const name: [*:0]const u8 = if (inst.result_type != null and inst.result_type.?.* != .void_type) "call" else "";
        return self.builder.buildCall2(fn_ty, callee, args.items, name);
    }

    /// Create a dm_string from a raw C string pointer via llvm_dm_string_new wrapper
    fn createDmString(self: *LLVMGenerator, raw_str: llvm.Value) !llvm.Value {
        const sret_name = try std.fmt.allocPrintZ(self.allocator, "sret.{}", .{self.sret_counter});
        defer self.allocator.free(sret_name);
        self.sret_counter += 1;
        const sret = self.builder.buildAlloca(self.getDmStringType(), sret_name);
        // dm_string_new is stored under original name in function_map (wrapper is llvm_dm_string_new)
        if (self.function_map.get("dm_string_new")) |string_new_fn| {
            if (self.fn_type_map.get("dm_string_new")) |string_new_type| {
                var new_args = [_]llvm.Value{ sret, raw_str };
                _ = self.builder.buildCall2(string_new_type, string_new_fn, &new_args, "");
            }
        }
        return sret;
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
                // If returning a string pointer from a non-main function,
                // load the dm_string struct value before returning
                if (func.return_type.* == .string_type and self.isStringValue(val)) {
                    const ptr_val = self.resolveValueWithType(val, ret_type, func);
                    const loaded = self.builder.buildLoad2(self.getDmStringType(), ptr_val, "ret.str");
                    _ = self.builder.buildRet(loaded);
                } else {
                    _ = self.builder.buildRet(self.resolveValueWithType(val, ret_type, func));
                }
            },
            .ret_void => {
                if (std.mem.eql(u8, func.name, "main")) {
                    _ = self.builder.buildRet(llvm.Value.constInt(llvm.Type.int32(self.context), 0, false));
                } else {
                    _ = self.builder.buildRetVoid();
                }
            },
            .unreachable_term => {
                _ = self.builder.buildUnreachable();
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
            .global_ref => |name| blk: {
                // Check functions first, then global variables
                if (self.function_map.get(name)) |fn_val| break :blk fn_val;
                if (self.global_var_map.get(name)) |gvar| {
                    // Check if this is a string global (dm_string type)
                    // String globals need to be passed as pointer (for wrapper ABI)
                    if (self.ir_module_ref) |ir_mod| {
                        for (ir_mod.globals.items) |g| {
                            if (std.mem.eql(u8, g.name, name) and g.ty.* == .string_type) {
                                // Return pointer to the global dm_string (don't load)
                                break :blk gvar;
                            }
                        }
                    }
                    // Non-string globals: load the value
                    break :blk self.builder.buildLoad2(llvm.Type.int64(self.context), gvar, "gload");
                }
                break :blk error.UnresolvedGlobalRef;
            },
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

    /// Coerce rhs integer value to match lhs integer width via trunc or sext
    fn coerceIntWidth(builder: anytype, lhs: llvm.Value, rhs: llvm.Value, name: [*:0]const u8) llvm.Value {
        const lhs_kind = lhs.typeOf().getTypeKind();
        const rhs_kind = rhs.typeOf().getTypeKind();
        if (lhs_kind == llvm.c.LLVMIntegerTypeKind and rhs_kind == llvm.c.LLVMIntegerTypeKind) {
            const lhs_width = lhs.typeOf().getIntTypeWidth();
            const rhs_width = rhs.typeOf().getIntTypeWidth();
            if (lhs_width != rhs_width) {
                if (rhs_width > lhs_width) {
                    return builder.buildTrunc(rhs, lhs.typeOf(), name);
                } else {
                    return builder.buildSExt(rhs, lhs.typeOf(), name);
                }
            }
        }
        return rhs;
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
