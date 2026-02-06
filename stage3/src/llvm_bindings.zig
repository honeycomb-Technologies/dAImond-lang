//! LLVM-C API Bindings for dAImond Stage 3
//!
//! Safe Zig wrappers around the LLVM-C API for IR generation,
//! module verification, and native code emission.

const std = @import("std");

pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/BitWriter.h");
});

// ========================================================================
// Context
// ========================================================================

pub const Context = struct {
    ref: c.LLVMContextRef,

    pub fn create() Context {
        return .{ .ref = c.LLVMContextCreate() };
    }

    pub fn dispose(self: Context) void {
        c.LLVMContextDispose(self.ref);
    }
};

// ========================================================================
// Module
// ========================================================================

pub const Module = struct {
    ref: c.LLVMModuleRef,

    pub fn createWithName(name: [*:0]const u8, ctx: Context) Module {
        return .{ .ref = c.LLVMModuleCreateWithNameInContext(name, ctx.ref) };
    }

    pub fn dispose(self: Module) void {
        c.LLVMDisposeModule(self.ref);
    }

    pub fn setTarget(self: Module, triple: [*:0]const u8) void {
        c.LLVMSetTarget(self.ref, triple);
    }

    pub fn setDataLayout(self: Module, layout: [*:0]const u8) void {
        c.LLVMSetDataLayout(self.ref, layout);
    }

    pub fn addFunction(self: Module, name: [*:0]const u8, fn_type: Type) Value {
        return .{ .ref = c.LLVMAddFunction(self.ref, name, fn_type.ref) };
    }

    pub fn getNamedFunction(self: Module, name: [*:0]const u8) ?Value {
        const f = c.LLVMGetNamedFunction(self.ref, name);
        if (f == null) return null;
        return .{ .ref = f };
    }

    pub fn addGlobal(self: Module, ty: Type, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMAddGlobal(self.ref, ty.ref, name) };
    }

    pub fn verify(self: Module) !void {
        var err_msg: [*c]u8 = null;
        if (c.LLVMVerifyModule(self.ref, c.LLVMReturnStatusAction, &err_msg) != 0) {
            defer c.LLVMDisposeMessage(err_msg);
            const msg = std.mem.span(err_msg);
            std.debug.print("LLVM Module verification failed:\n{s}\n", .{msg});
            return error.ModuleVerificationFailed;
        }
    }

    pub fn printToString(self: Module) [*:0]u8 {
        return c.LLVMPrintModuleToString(self.ref);
    }

    pub fn printToFile(self: Module, filename: [*:0]const u8) !void {
        var err_msg: [*c]u8 = null;
        if (c.LLVMPrintModuleToFile(self.ref, filename, &err_msg) != 0) {
            defer c.LLVMDisposeMessage(err_msg);
            return error.PrintToFileFailed;
        }
    }

    pub fn writeBitcodeToFile(self: Module, filename: [*:0]const u8) !void {
        if (c.LLVMWriteBitcodeToFile(self.ref, filename) != 0) {
            return error.WriteBitcodeFailed;
        }
    }
};

// ========================================================================
// Type
// ========================================================================

pub const Type = struct {
    ref: c.LLVMTypeRef,

    // Integer types
    pub fn int1(ctx: Context) Type {
        return .{ .ref = c.LLVMInt1TypeInContext(ctx.ref) };
    }

    pub fn int8(ctx: Context) Type {
        return .{ .ref = c.LLVMInt8TypeInContext(ctx.ref) };
    }

    pub fn int16(ctx: Context) Type {
        return .{ .ref = c.LLVMInt16TypeInContext(ctx.ref) };
    }

    pub fn int32(ctx: Context) Type {
        return .{ .ref = c.LLVMInt32TypeInContext(ctx.ref) };
    }

    pub fn int64(ctx: Context) Type {
        return .{ .ref = c.LLVMInt64TypeInContext(ctx.ref) };
    }

    // Float types
    pub fn float(ctx: Context) Type {
        return .{ .ref = c.LLVMFloatTypeInContext(ctx.ref) };
    }

    pub fn double(ctx: Context) Type {
        return .{ .ref = c.LLVMDoubleTypeInContext(ctx.ref) };
    }

    // Void type
    pub fn void_type(ctx: Context) Type {
        return .{ .ref = c.LLVMVoidTypeInContext(ctx.ref) };
    }

    // Pointer type (opaque in LLVM 17+)
    pub fn ptr(ctx: Context) Type {
        return .{ .ref = c.LLVMPointerTypeInContext(ctx.ref, 0) };
    }

    // Array type
    pub fn array(elem: Type, count: u64) Type {
        return .{ .ref = c.LLVMArrayType2(elem.ref, count) };
    }

    // Struct type
    pub fn struct_type(ctx: Context, fields: []const Type, is_packed: bool) Type {
        // Need to convert to C array
        const c_fields = @as([*c]c.LLVMTypeRef, @ptrCast(@constCast(fields.ptr)));
        return .{ .ref = c.LLVMStructTypeInContext(
            ctx.ref,
            c_fields,
            @intCast(fields.len),
            if (is_packed) 1 else 0,
        ) };
    }

    pub fn namedStruct(ctx: Context, name: [*:0]const u8) Type {
        return .{ .ref = c.LLVMStructCreateNamed(ctx.ref, name) };
    }

    pub fn structSetBody(self: Type, fields: []const Type, is_packed: bool) void {
        const c_fields = @as([*c]c.LLVMTypeRef, @ptrCast(@constCast(fields.ptr)));
        c.LLVMStructSetBody(self.ref, c_fields, @intCast(fields.len), if (is_packed) 1 else 0);
    }

    pub fn getTypeKind(self: Type) c.LLVMTypeKind {
        return c.LLVMGetTypeKind(self.ref);
    }

    // Function type
    pub fn function(ret: Type, params: []const Type, is_var_arg: bool) Type {
        const c_params = @as([*c]c.LLVMTypeRef, @ptrCast(@constCast(params.ptr)));
        return .{ .ref = c.LLVMFunctionType(
            ret.ref,
            c_params,
            @intCast(params.len),
            if (is_var_arg) 1 else 0,
        ) };
    }
};

// ========================================================================
// Value
// ========================================================================

pub const Value = struct {
    ref: c.LLVMValueRef,

    // Constants
    pub fn constInt(ty: Type, val: u64, sign_extend: bool) Value {
        return .{ .ref = c.LLVMConstInt(ty.ref, val, if (sign_extend) 1 else 0) };
    }

    pub fn constSignedInt(ty: Type, val: i64) Value {
        return .{ .ref = c.LLVMConstInt(ty.ref, @bitCast(val), 1) };
    }

    pub fn constReal(ty: Type, val: f64) Value {
        return .{ .ref = c.LLVMConstReal(ty.ref, val) };
    }

    pub fn constNull(ty: Type) Value {
        return .{ .ref = c.LLVMConstNull(ty.ref) };
    }

    pub fn constString(ctx: Context, str: []const u8, null_terminate: bool) Value {
        return .{ .ref = c.LLVMConstStringInContext(
            ctx.ref,
            str.ptr,
            @intCast(str.len),
            if (null_terminate) 0 else 1,
        ) };
    }

    pub fn constStruct(ctx: Context, values: []const Value, is_packed: bool) Value {
        const c_vals = @as([*c]c.LLVMValueRef, @ptrCast(@constCast(values.ptr)));
        return .{ .ref = c.LLVMConstStructInContext(
            ctx.ref,
            c_vals,
            @intCast(values.len),
            if (is_packed) 1 else 0,
        ) };
    }

    pub fn constArray(elem_ty: Type, values: []const Value) Value {
        const c_vals = @as([*c]c.LLVMValueRef, @ptrCast(@constCast(values.ptr)));
        return .{ .ref = c.LLVMConstArray2(elem_ty.ref, c_vals, @intCast(values.len)) };
    }

    pub fn setInitializer(self: Value, init: Value) void {
        c.LLVMSetInitializer(self.ref, init.ref);
    }

    pub fn setGlobalConstant(self: Value, is_const: bool) void {
        c.LLVMSetGlobalConstant(self.ref, if (is_const) 1 else 0);
    }

    pub fn setLinkage(self: Value, linkage: c.LLVMLinkage) void {
        c.LLVMSetLinkage(self.ref, linkage);
    }

    pub fn getParam(self: Value, index: u32) Value {
        return .{ .ref = c.LLVMGetParam(self.ref, index) };
    }

    pub fn setName(self: Value, name: [*:0]const u8) void {
        c.LLVMSetValueName2(self.ref, name, std.mem.len(name));
    }

    pub fn typeOf(self: Value) Type {
        return .{ .ref = c.LLVMTypeOf(self.ref) };
    }

    pub fn getUndef(ty: Type) Value {
        return .{ .ref = c.LLVMGetUndef(ty.ref) };
    }
};

// ========================================================================
// Basic Block
// ========================================================================

pub const BasicBlock = struct {
    ref: c.LLVMBasicBlockRef,

    pub fn append(func: Value, ctx: Context, name: [*:0]const u8) BasicBlock {
        return .{ .ref = c.LLVMAppendBasicBlockInContext(ctx.ref, func.ref, name) };
    }
};

// ========================================================================
// Builder
// ========================================================================

pub const Builder = struct {
    ref: c.LLVMBuilderRef,

    pub fn create(ctx: Context) Builder {
        return .{ .ref = c.LLVMCreateBuilderInContext(ctx.ref) };
    }

    pub fn dispose(self: Builder) void {
        c.LLVMDisposeBuilder(self.ref);
    }

    pub fn positionAtEnd(self: Builder, block: BasicBlock) void {
        c.LLVMPositionBuilderAtEnd(self.ref, block.ref);
    }

    // Arithmetic
    pub fn buildAdd(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildAdd(self.ref, lhs.ref, rhs.ref, name) };
    }

    pub fn buildSub(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildSub(self.ref, lhs.ref, rhs.ref, name) };
    }

    pub fn buildMul(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildMul(self.ref, lhs.ref, rhs.ref, name) };
    }

    pub fn buildSDiv(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildSDiv(self.ref, lhs.ref, rhs.ref, name) };
    }

    pub fn buildSRem(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildSRem(self.ref, lhs.ref, rhs.ref, name) };
    }

    pub fn buildNeg(self: Builder, val: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildNeg(self.ref, val.ref, name) };
    }

    pub fn buildFAdd(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildFAdd(self.ref, lhs.ref, rhs.ref, name) };
    }

    pub fn buildFSub(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildFSub(self.ref, lhs.ref, rhs.ref, name) };
    }

    pub fn buildFMul(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildFMul(self.ref, lhs.ref, rhs.ref, name) };
    }

    pub fn buildFDiv(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildFDiv(self.ref, lhs.ref, rhs.ref, name) };
    }

    // Comparison
    pub fn buildICmp(self: Builder, pred: c.LLVMIntPredicate, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildICmp(self.ref, pred, lhs.ref, rhs.ref, name) };
    }

    pub fn buildFCmp(self: Builder, pred: c.LLVMRealPredicate, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildFCmp(self.ref, pred, lhs.ref, rhs.ref, name) };
    }

    // Memory
    pub fn buildAlloca(self: Builder, ty: Type, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildAlloca(self.ref, ty.ref, name) };
    }

    pub fn buildLoad2(self: Builder, ty: Type, ptr: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildLoad2(self.ref, ty.ref, ptr.ref, name) };
    }

    pub fn buildStore(self: Builder, val: Value, ptr: Value) Value {
        return .{ .ref = c.LLVMBuildStore(self.ref, val.ref, ptr.ref) };
    }

    pub fn buildGEP2(self: Builder, ty: Type, ptr: Value, indices: []const Value, name: [*:0]const u8) Value {
        const c_indices = @as([*c]c.LLVMValueRef, @ptrCast(@constCast(indices.ptr)));
        return .{ .ref = c.LLVMBuildGEP2(self.ref, ty.ref, ptr.ref, c_indices, @intCast(indices.len), name) };
    }

    pub fn buildStructGEP2(self: Builder, ty: Type, ptr: Value, idx: u32, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildStructGEP2(self.ref, ty.ref, ptr.ref, idx, name) };
    }

    pub fn buildExtractValue(self: Builder, agg: Value, idx: u32, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildExtractValue(self.ref, agg.ref, idx, name) };
    }

    // Control flow
    pub fn buildBr(self: Builder, dest: BasicBlock) Value {
        return .{ .ref = c.LLVMBuildBr(self.ref, dest.ref) };
    }

    pub fn buildCondBr(self: Builder, cond: Value, then_bb: BasicBlock, else_bb: BasicBlock) Value {
        return .{ .ref = c.LLVMBuildCondBr(self.ref, cond.ref, then_bb.ref, else_bb.ref) };
    }

    pub fn buildRet(self: Builder, val: Value) Value {
        return .{ .ref = c.LLVMBuildRet(self.ref, val.ref) };
    }

    pub fn buildRetVoid(self: Builder) Value {
        return .{ .ref = c.LLVMBuildRetVoid(self.ref) };
    }

    // Function calls
    pub fn buildCall2(self: Builder, fn_ty: Type, func: Value, args: []const Value, name: [*:0]const u8) Value {
        const c_args = @as([*c]c.LLVMValueRef, @ptrCast(@constCast(args.ptr)));
        return .{ .ref = c.LLVMBuildCall2(self.ref, fn_ty.ref, func.ref, c_args, @intCast(args.len), name) };
    }

    // Phi
    pub fn buildPhi(self: Builder, ty: Type, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildPhi(self.ref, ty.ref, name) };
    }

    // Casts
    pub fn buildIntCast2(self: Builder, val: Value, dest_ty: Type, is_signed: bool, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildIntCast2(self.ref, val.ref, dest_ty.ref, if (is_signed) 1 else 0, name) };
    }

    pub fn buildFPCast(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildFPCast(self.ref, val.ref, dest_ty.ref, name) };
    }

    pub fn buildSIToFP(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildSIToFP(self.ref, val.ref, dest_ty.ref, name) };
    }

    pub fn buildFPToSI(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildFPToSI(self.ref, val.ref, dest_ty.ref, name) };
    }

    pub fn buildBitCast(self: Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildBitCast(self.ref, val.ref, dest_ty.ref, name) };
    }

    // Logical
    pub fn buildAnd(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildAnd(self.ref, lhs.ref, rhs.ref, name) };
    }

    pub fn buildOr(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildOr(self.ref, lhs.ref, rhs.ref, name) };
    }

    pub fn buildNot(self: Builder, val: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildNot(self.ref, val.ref, name) };
    }

    // Struct operations
    pub fn buildInsertValue(self: Builder, agg: Value, val: Value, idx: u32, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildInsertValue(self.ref, agg.ref, val.ref, idx, name) };
    }

    pub fn buildFNeg(self: Builder, val: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildFNeg(self.ref, val.ref, name) };
    }

    pub fn buildFRem(self: Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildFRem(self.ref, lhs.ref, rhs.ref, name) };
    }

    // Global strings
    pub fn buildGlobalStringPtr(self: Builder, str: [*:0]const u8, name: [*:0]const u8) Value {
        return .{ .ref = c.LLVMBuildGlobalStringPtr(self.ref, str, name) };
    }
};

// ========================================================================
// Phi helpers
// ========================================================================

pub fn addIncoming(phi: Value, values: []const Value, blocks: []const BasicBlock) void {
    const c_vals = @as([*c]c.LLVMValueRef, @ptrCast(@constCast(values.ptr)));
    const c_blocks = @as([*c]c.LLVMBasicBlockRef, @ptrCast(@constCast(blocks.ptr)));
    c.LLVMAddIncoming(phi.ref, c_vals, c_blocks, @intCast(values.len));
}

// ========================================================================
// Target
// ========================================================================

pub fn initializeAllTargets() void {
    c.LLVMInitializeX86TargetInfo();
    c.LLVMInitializeX86Target();
    c.LLVMInitializeX86TargetMC();
    c.LLVMInitializeX86AsmPrinter();
    c.LLVMInitializeX86AsmParser();

    // Also initialize AArch64 if available
    c.LLVMInitializeAArch64TargetInfo();
    c.LLVMInitializeAArch64Target();
    c.LLVMInitializeAArch64TargetMC();
    c.LLVMInitializeAArch64AsmPrinter();
    c.LLVMInitializeAArch64AsmParser();
}

pub fn getDefaultTriple() [*:0]u8 {
    return c.LLVMGetDefaultTargetTriple();
}

pub const TargetMachine = struct {
    ref: c.LLVMTargetMachineRef,

    pub fn create(triple: [*:0]const u8, opt_level: OptLevel) !TargetMachine {
        var target: c.LLVMTargetRef = null;
        var err_msg: [*c]u8 = null;
        if (c.LLVMGetTargetFromTriple(triple, &target, &err_msg) != 0) {
            defer c.LLVMDisposeMessage(err_msg);
            return error.TargetNotFound;
        }

        const llvm_opt = switch (opt_level) {
            .O0 => c.LLVMCodeGenLevelNone,
            .O1 => c.LLVMCodeGenLevelLess,
            .O2 => c.LLVMCodeGenLevelDefault,
            .O3 => c.LLVMCodeGenLevelAggressive,
        };

        const tm = c.LLVMCreateTargetMachine(
            target,
            triple,
            "generic",
            "",
            @as(c.LLVMCodeGenOptLevel, @intCast(llvm_opt)),
            @as(c.LLVMRelocMode, @intCast(c.LLVMRelocPIC)),
            @as(c.LLVMCodeModel, @intCast(c.LLVMCodeModelDefault)),
        );

        if (tm == null) return error.TargetMachineCreationFailed;
        return .{ .ref = tm };
    }

    pub fn dispose(self: TargetMachine) void {
        c.LLVMDisposeTargetMachine(self.ref);
    }

    pub fn emitToFile(self: TargetMachine, module: Module, filename: [*:0]const u8, file_type: FileType) !void {
        var err_msg: [*c]u8 = null;
        const ft = switch (file_type) {
            .object => c.LLVMObjectFile,
            .assembly => c.LLVMAssemblyFile,
        };
        if (c.LLVMTargetMachineEmitToFile(self.ref, module.ref, @constCast(filename), @as(c.LLVMCodeGenFileType, @intCast(ft)), &err_msg) != 0) {
            defer c.LLVMDisposeMessage(err_msg);
            return error.EmitFailed;
        }
    }

    pub fn getDataLayout(self: TargetMachine) [*:0]u8 {
        const dl = c.LLVMCreateTargetDataLayout(self.ref);
        return c.LLVMCopyStringRepOfTargetData(dl);
    }
};

pub const OptLevel = enum { O0, O1, O2, O3 };
pub const FileType = enum { object, assembly };

// ========================================================================
// Tests
// ========================================================================

test "LLVM create and verify module" {
    initializeAllTargets();

    const ctx = Context.create();
    defer ctx.dispose();

    const module = Module.createWithName("test_module", ctx);
    defer module.dispose();

    const builder = Builder.create(ctx);
    defer builder.dispose();

    // Create: define i32 @main() { ret i32 0 }
    const i32_ty = Type.int32(ctx);
    const fn_ty = Type.function(i32_ty, &.{}, false);
    const main_fn = module.addFunction("main", fn_ty);

    const entry = BasicBlock.append(main_fn, ctx, "entry");
    builder.positionAtEnd(entry);
    _ = builder.buildRet(Value.constInt(i32_ty, 0, false));

    // Verify module
    try module.verify();

    // Check printable IR
    const ir_str = module.printToString();
    const ir_text = std.mem.span(ir_str);
    try std.testing.expect(std.mem.indexOf(u8, ir_text, "define i32 @main()") != null);
    try std.testing.expect(std.mem.indexOf(u8, ir_text, "ret i32 0") != null);
}

test "LLVM emit object file" {
    initializeAllTargets();

    const ctx = Context.create();
    defer ctx.dispose();

    const module = Module.createWithName("emit_test", ctx);
    defer module.dispose();

    const builder = Builder.create(ctx);
    defer builder.dispose();

    // Create minimal main
    const i32_ty = Type.int32(ctx);
    const fn_ty = Type.function(i32_ty, &.{}, false);
    const main_fn = module.addFunction("main", fn_ty);

    const entry = BasicBlock.append(main_fn, ctx, "entry");
    builder.positionAtEnd(entry);
    _ = builder.buildRet(Value.constInt(i32_ty, 0, false));

    try module.verify();

    // Set target
    const triple = getDefaultTriple();
    module.setTarget(triple);

    // Create target machine
    const tm = try TargetMachine.create(triple, .O0);
    defer tm.dispose();

    // Emit to object file
    try tm.emitToFile(module, "/tmp/daimond_test.o", .object);

    // Verify file exists
    const stat = std.fs.cwd().statFile("/tmp/daimond_test.o") catch {
        try std.testing.expect(false);
        return;
    };
    try std.testing.expect(stat.size > 0);
}
