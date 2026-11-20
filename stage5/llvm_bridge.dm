module llvm_bridge

-- ============================================================
-- The LLVM bridge
-- The functions the compiler calls LLVM through, which llvm.dm
-- declares, written over LLVM's C API. An LLVM reference (a context,
-- module, type, value, block or builder) is an int that holds the
-- pointer.
--
-- The compiler adds this file to the program it compiles when it is
-- given --link-llvm, which is how it builds itself: each function
-- here takes the place of the extern fn of the same name.
--
-- C's `unsigned` is u32 here, LLVMBool and enums are i32, and size_t,
-- uint64_t and every pointer are int. The numbers of LLVM's enums are
-- written out where they are used. They are those of LLVM 19's
-- llvm-c headers, and the same in LLVM's main branch (November 2026).
-- ============================================================

-- ============================================================
-- LLVM-C
-- ============================================================

-- Contexts and modules
extern fn LLVMContextCreate() -> int
extern fn LLVMContextDispose(ctx: int)
extern fn LLVMModuleCreateWithNameInContext(name: string, ctx: int) -> int
extern fn LLVMDisposeModule(m: int)
extern fn LLVMSetTarget(m: int, triple: string)
extern fn LLVMSetDataLayout(m: int, layout: string)
extern fn LLVMAddFunction(m: int, name: string, fn_type: int) -> int
extern fn LLVMAddGlobal(m: int, ty: int, name: string) -> int
extern fn LLVMVerifyModule(m: int, action: i32, message: int) -> i32
extern fn LLVMPrintModuleToFile(m: int, filename: string, message: int) -> i32
extern fn LLVMWriteBitcodeToFile(m: int, filename: string) -> i32
extern fn LLVMDisposeMessage(message: int)
extern fn LLVMGetModuleContext(m: int) -> int
extern fn LLVMGetGlobalParent(global: int) -> int

-- Debug info. LLVMBool, the enums and `unsigned` are 32 bits wide; a
-- length is a size_t.
extern fn LLVMAddModuleFlag(m: int, behavior: i32, key: string, key_len: int, val: int)
extern fn LLVMValueAsMetadata(val: int) -> int
extern fn LLVMDebugMetadataVersion() -> u32
extern fn LLVMCreateDIBuilder(m: int) -> int
extern fn LLVMDIBuilderFinalize(dib: int)
extern fn LLVMDisposeDIBuilder(dib: int)
extern fn LLVMDIBuilderCreateFile(dib: int, name: string, name_len: int, directory: string, directory_len: int) -> int
extern fn LLVMDIBuilderCreateCompileUnit(dib: int, lang: i32, file: int, producer: string, producer_len: int, optimized: i32, flags: string, flags_len: int, runtime_version: u32, split_name: string, split_name_len: int, kind: i32, dwo_id: u32, split_debug_inlining: i32, debug_info_for_profiling: i32, sysroot: string, sysroot_len: int, sdk: string, sdk_len: int) -> int
extern fn LLVMDIBuilderCreateSubroutineType(dib: int, file: int, types: int, count: u32, flags: i32) -> int
extern fn LLVMDIBuilderCreateFunction(dib: int, scope: int, name: string, name_len: int, linkage_name: string, linkage_name_len: int, file: int, line: u32, ty: int, is_local: i32, is_definition: i32, scope_line: u32, flags: i32, optimized: i32) -> int
extern fn LLVMSetSubprogram(func: int, subprogram: int)
extern fn LLVMDIBuilderCreateDebugLocation(ctx: int, line: u32, column: u32, scope: int, inlined_at: int) -> int
extern fn LLVMSetCurrentDebugLocation2(b: int, loc: int)
extern fn LLVMGetCurrentDebugLocation2(b: int) -> int
extern fn LLVMDIBuilderCreateBasicType(dib: int, name: string, name_len: int, bits: int, encoding: u32, flags: i32) -> int
extern fn LLVMDIBuilderCreatePointerType(dib: int, pointee: int, bits: int, align_bits: u32, address_space: u32, name: string, name_len: int) -> int
extern fn LLVMDIBuilderCreateMemberType(dib: int, scope: int, name: string, name_len: int, file: int, line: u32, bits: int, align_bits: u32, offset_bits: int, flags: i32, ty: int) -> int
extern fn LLVMDIBuilderCreateReplaceableCompositeType(dib: int, tag: u32, name: string, name_len: int, scope: int, file: int, line: u32, runtime_lang: u32, bits: int, align_bits: u32, flags: i32, unique_id: string, unique_id_len: int) -> int
extern fn LLVMDIBuilderCreateStructType(dib: int, scope: int, name: string, name_len: int, file: int, line: u32, bits: int, align_bits: u32, flags: i32, derived_from: int, elements: int, count: u32, runtime_lang: u32, vtable_holder: int, unique_id: string, unique_id_len: int) -> int
extern fn LLVMMetadataReplaceAllUsesWith(temporary: int, replacement: int)
extern fn LLVMDIBuilderCreateAutoVariable(dib: int, scope: int, name: string, name_len: int, file: int, line: u32, ty: int, always_preserve: i32, flags: i32, align_bits: u32) -> int
extern fn LLVMDIBuilderCreateParameterVariable(dib: int, scope: int, name: string, name_len: int, arg: u32, file: int, line: u32, ty: int, always_preserve: i32, flags: i32) -> int
extern fn LLVMDIBuilderCreateExpression(dib: int, ops: int, count: int) -> int
extern fn LLVMDIBuilderInsertDeclareRecordAtEnd(dib: int, storage: int, variable: int, expr: int, loc: int, block: int) -> int
extern fn LLVMGetModuleDataLayout(m: int) -> int
extern fn LLVMABISizeOfType(target_data: int, ty: int) -> int
extern fn LLVMOffsetOfElement(target_data: int, struct_ty: int, element: u32) -> int
extern fn LLVMIsOpaqueStruct(ty: int) -> i32
extern fn LLVMCountStructElementTypes(ty: int) -> u32

-- Passes
extern fn LLVMCreatePassBuilderOptions() -> int
extern fn LLVMRunPasses(m: int, passes: string, tm: int, options: int) -> int
extern fn LLVMDisposePassBuilderOptions(options: int)
extern fn LLVMGetErrorMessage(err: int) -> int
extern fn LLVMDisposeErrorMessage(message: int)

-- Types
extern fn LLVMInt1TypeInContext(ctx: int) -> int
extern fn LLVMInt8TypeInContext(ctx: int) -> int
extern fn LLVMInt16TypeInContext(ctx: int) -> int
extern fn LLVMInt32TypeInContext(ctx: int) -> int
extern fn LLVMInt64TypeInContext(ctx: int) -> int
extern fn LLVMFloatTypeInContext(ctx: int) -> int
extern fn LLVMDoubleTypeInContext(ctx: int) -> int
extern fn LLVMVoidTypeInContext(ctx: int) -> int
extern fn LLVMPointerTypeInContext(ctx: int, address_space: u32) -> int
extern fn LLVMArrayType2(elem: int, count: int) -> int
extern fn LLVMVectorType(elem: int, count: u32) -> int
extern fn LLVMStructTypeInContext(ctx: int, fields: int, count: u32, is_packed: i32) -> int
extern fn LLVMStructCreateNamed(ctx: int, name: string) -> int
extern fn LLVMStructSetBody(struct_ty: int, fields: int, count: u32, is_packed: i32)
extern fn LLVMGetStructName(ty: int) -> int
extern fn LLVMFunctionType(ret: int, params: int, count: u32, is_var_arg: i32) -> int
extern fn LLVMCountParamTypes(fn_ty: int) -> u32
extern fn LLVMGetParamTypes(fn_ty: int, dest: int)
extern fn LLVMIsFunctionVarArg(fn_ty: int) -> i32
extern fn LLVMGetTypeKind(ty: int) -> i32
extern fn LLVMGetIntTypeWidth(ty: int) -> u32
extern fn LLVMGetTypeContext(ty: int) -> int
extern fn LLVMPrintTypeToString(ty: int) -> int
extern fn LLVMSizeOf(ty: int) -> int

-- Values
extern fn LLVMConstInt(ty: int, value: int, sign_extend: i32) -> int
extern fn LLVMConstReal(ty: int, value: float) -> int
extern fn LLVMConstNull(ty: int) -> int
extern fn LLVMGetUndef(ty: int) -> int
extern fn LLVMTypeOf(value: int) -> int
extern fn LLVMSetInitializer(global: int, init: int)
extern fn LLVMSetGlobalConstant(global: int, is_const: i32)
extern fn LLVMSetLinkage(global: int, linkage: i32)
extern fn LLVMGetParam(func: int, index: u32) -> int
extern fn LLVMSetValueName2(value: int, name: string, name_len: int)
extern fn LLVMGetValueName2(value: int, length: int) -> int
extern fn LLVMGetEnumAttributeKindForName(name: string, name_len: int) -> u32
extern fn LLVMCreateEnumAttribute(ctx: int, kind: u32, value: int) -> int
extern fn LLVMAddAttributeAtIndex(func: int, index: u32, attr: int)
extern fn LLVMAddCallSiteAttribute(call: int, index: u32, attr: int)
extern fn LLVMGetNumArgOperands(call: int) -> u32
extern fn LLVMGetOperand(value: int, index: u32) -> int

-- Blocks and the builder
extern fn LLVMAppendBasicBlockInContext(ctx: int, func: int, name: string) -> int
extern fn LLVMGetBasicBlockParent(block: int) -> int
extern fn LLVMGetEntryBasicBlock(func: int) -> int
extern fn LLVMGetFirstInstruction(block: int) -> int
extern fn LLVMGetNextInstruction(inst: int) -> int
extern fn LLVMIsAAllocaInst(value: int) -> int
extern fn LLVMCreateBuilderInContext(ctx: int) -> int
extern fn LLVMDisposeBuilder(b: int)
extern fn LLVMPositionBuilderAtEnd(b: int, block: int)
extern fn LLVMPositionBuilderBefore(b: int, inst: int)
extern fn LLVMGetInsertBlock(b: int) -> int

-- Instructions
extern fn LLVMBuildAdd(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildSub(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildMul(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildSDiv(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildUDiv(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildSRem(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildURem(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildFAdd(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildFSub(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildFMul(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildFDiv(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildFRem(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildAnd(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildOr(b: int, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildNeg(b: int, value: int, name: string) -> int
extern fn LLVMBuildFNeg(b: int, value: int, name: string) -> int
extern fn LLVMBuildNot(b: int, value: int, name: string) -> int
extern fn LLVMBuildFPCast(b: int, value: int, dest_ty: int, name: string) -> int
extern fn LLVMBuildSIToFP(b: int, value: int, dest_ty: int, name: string) -> int
extern fn LLVMBuildUIToFP(b: int, value: int, dest_ty: int, name: string) -> int
extern fn LLVMBuildFPToSI(b: int, value: int, dest_ty: int, name: string) -> int
extern fn LLVMBuildTrunc(b: int, value: int, dest_ty: int, name: string) -> int
extern fn LLVMBuildSExt(b: int, value: int, dest_ty: int, name: string) -> int
extern fn LLVMBuildIntToPtr(b: int, value: int, dest_ty: int, name: string) -> int
extern fn LLVMBuildPtrToInt(b: int, value: int, dest_ty: int, name: string) -> int
extern fn LLVMBuildIntCast2(b: int, value: int, dest_ty: int, is_signed: i32, name: string) -> int
extern fn LLVMBuildICmp(b: int, pred: i32, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildFCmp(b: int, pred: i32, lhs: int, rhs: int, name: string) -> int
extern fn LLVMBuildAlloca(b: int, ty: int, name: string) -> int
extern fn LLVMBuildLoad2(b: int, ty: int, ptr: int, name: string) -> int
extern fn LLVMBuildStore(b: int, value: int, ptr: int) -> int
extern fn LLVMBuildInBoundsGEP2(b: int, ty: int, ptr: int, indices: int, count: u32, name: string) -> int
extern fn LLVMBuildStructGEP2(b: int, ty: int, ptr: int, index: u32, name: string) -> int
extern fn LLVMBuildExtractValue(b: int, agg: int, index: u32, name: string) -> int
extern fn LLVMBuildInsertValue(b: int, agg: int, value: int, index: u32, name: string) -> int
extern fn LLVMBuildInsertElement(b: int, vec: int, elem: int, index: int, name: string) -> int
extern fn LLVMBuildExtractElement(b: int, vec: int, index: int, name: string) -> int
extern fn LLVMBuildBr(b: int, dest: int) -> int
extern fn LLVMBuildCondBr(b: int, cond: int, then_block: int, else_block: int) -> int
extern fn LLVMBuildRet(b: int, value: int) -> int
extern fn LLVMBuildRetVoid(b: int) -> int
extern fn LLVMBuildUnreachable(b: int) -> int
extern fn LLVMBuildCall2(b: int, fn_ty: int, func: int, args: int, count: u32, name: string) -> int
extern fn LLVMBuildPhi(b: int, ty: int, name: string) -> int
extern fn LLVMAddIncoming(phi: int, values: int, blocks: int, count: u32)
extern fn LLVMBuildGlobalStringPtr(b: int, text: string, name: string) -> int

-- Targets. LLVMInitializeAllTargets and its like are inline functions
-- of the C headers with no symbol to call, so each target is
-- initialized by its own functions.
extern fn LLVMInitializeX86TargetInfo()
extern fn LLVMInitializeX86Target()
extern fn LLVMInitializeX86TargetMC()
extern fn LLVMInitializeX86AsmPrinter()
extern fn LLVMInitializeX86AsmParser()
extern fn LLVMInitializeAArch64TargetInfo()
extern fn LLVMInitializeAArch64Target()
extern fn LLVMInitializeAArch64TargetMC()
extern fn LLVMInitializeAArch64AsmPrinter()
extern fn LLVMInitializeAArch64AsmParser()
extern fn LLVMGetDefaultTargetTriple() -> int
extern fn LLVMGetTargetFromTriple(triple: string, target: int, message: int) -> i32
extern fn LLVMCreateTargetMachine(target: int, triple: string, cpu: string, features: string, level: i32, reloc: i32, code_model: i32) -> int
extern fn LLVMDisposeTargetMachine(tm: int)
extern fn LLVMTargetMachineEmitToFile(tm: int, m: int, filename: string, file_type: i32, message: int) -> i32
extern fn LLVMCreateTargetDataLayout(tm: int) -> int
extern fn LLVMCopyStringRepOfTargetData(data: int) -> int

-- libc, under names of the bridge's own
extern fn bridge_strlen(s: int) -> int = "strlen"
extern fn bridge_malloc(n: int) -> int = "malloc"
extern fn bridge_free(p: int) = "free"
extern fn bridge_memcpy(dst: int, src: int, n: int) -> int = "memcpy"

-- ============================================================
-- Helpers
-- ============================================================

-- An int as an LLVMBool: 1 unless it is 0
fn bridge_bool(v: int) -> i32 {
    if v != 0 {
        return 1
    }
    return 0
}

-- The C string at address p, as a string that borrows its bytes
fn bridge_borrowed(p: int) -> string {
    unsafe {
        return string_from_raw(p, bridge_strlen(p), 0)
    }
}

-- A string that holds its own copy of the n bytes at address p
fn bridge_copy(p: int, n: int) -> string {
    if p == 0 or n <= 0 {
        return ""
    }
    unsafe {
        let buf = bridge_malloc(n + 1)
        if buf == 0 {
            panic("out of memory")
        }
        bridge_memcpy(buf, p, n)
        store_u8(buf + n, 0)
        return string_from_raw(buf, n, n + 1)
    }
}

-- ============================================================
-- Context
-- ============================================================

fn dm_llvm_context_create() -> int {
    unsafe {
        return LLVMContextCreate()
    }
}

fn dm_llvm_context_dispose(ctx: int) {
    unsafe {
        LLVMContextDispose(ctx)
    }
}

-- ============================================================
-- Module
-- ============================================================

fn dm_llvm_module_create(ctx: int, name: string) -> int {
    unsafe {
        return LLVMModuleCreateWithNameInContext(name, ctx)
    }
}

fn dm_llvm_module_dispose(m: int) {
    unsafe {
        LLVMDisposeModule(m)
    }
}

fn dm_llvm_module_set_target(m: int, triple: string) {
    unsafe {
        LLVMSetTarget(m, triple)
    }
}

fn dm_llvm_module_set_data_layout(m: int, layout: string) {
    unsafe {
        LLVMSetDataLayout(m, layout)
    }
}

fn dm_llvm_module_add_function(m: int, name: string, fn_type: int) -> int {
    unsafe {
        return LLVMAddFunction(m, name, fn_type)
    }
}

fn dm_llvm_module_add_global(m: int, ty: int, name: string) -> int {
    unsafe {
        return LLVMAddGlobal(m, ty, name)
    }
}

-- Check the module. Gives 0 when it is valid; otherwise prints what is
-- wrong with it to standard error and gives 1.
fn dm_llvm_module_verify(m: int) -> int {
    unsafe {
        let mut message = 0
        -- 2 is LLVMReturnStatusAction
        let failed = LLVMVerifyModule(m, 2, addr_of(message)) as int
        if failed != 0 and message != 0 {
            eprintln("LLVM Module verification failed:")
            eprintln(bridge_borrowed(message))
            LLVMDisposeMessage(message)
        }
        return failed
    }
}

-- Write the module as LLVM IR text. Gives 0, or 1 when the file cannot
-- be written.
fn dm_llvm_module_print_to_file(m: int, filename: string) -> int {
    unsafe {
        let mut message = 0
        let failed = LLVMPrintModuleToFile(m, filename, addr_of(message)) as int
        if failed != 0 and message != 0 {
            LLVMDisposeMessage(message)
        }
        return failed
    }
}

-- Write the module as LLVM bitcode. Gives 0, or 1 when the file cannot
-- be written.
fn dm_llvm_module_write_bitcode(m: int, filename: string) -> int {
    unsafe {
        if LLVMWriteBitcodeToFile(m, filename) != 0 {
            return 1
        }
        return 0
    }
}

-- Run the passes named in `passes`, such as "default<O2>", over the
-- module. Gives 0, or 1 after printing the error.
fn dm_llvm_run_passes(m: int, passes: string, tm: int) -> int {
    unsafe {
        let options = LLVMCreatePassBuilderOptions()
        let err = LLVMRunPasses(m, passes, tm, options)
        LLVMDisposePassBuilderOptions(options)
        if err != 0 {
            let message = LLVMGetErrorMessage(err)
            eprintln("Pass run failed: " + bridge_borrowed(message))
            LLVMDisposeErrorMessage(message)
            return 1
        }
        return 0
    }
}

-- ============================================================
-- Types
-- ============================================================

fn dm_llvm_int1_type(ctx: int) -> int {
    unsafe {
        return LLVMInt1TypeInContext(ctx)
    }
}

fn dm_llvm_int8_type(ctx: int) -> int {
    unsafe {
        return LLVMInt8TypeInContext(ctx)
    }
}

fn dm_llvm_int16_type(ctx: int) -> int {
    unsafe {
        return LLVMInt16TypeInContext(ctx)
    }
}

fn dm_llvm_int32_type(ctx: int) -> int {
    unsafe {
        return LLVMInt32TypeInContext(ctx)
    }
}

fn dm_llvm_int64_type(ctx: int) -> int {
    unsafe {
        return LLVMInt64TypeInContext(ctx)
    }
}

fn dm_llvm_float_type(ctx: int) -> int {
    unsafe {
        return LLVMFloatTypeInContext(ctx)
    }
}

fn dm_llvm_double_type(ctx: int) -> int {
    unsafe {
        return LLVMDoubleTypeInContext(ctx)
    }
}

fn dm_llvm_void_type(ctx: int) -> int {
    unsafe {
        return LLVMVoidTypeInContext(ctx)
    }
}

fn dm_llvm_pointer_type(ctx: int) -> int {
    unsafe {
        return LLVMPointerTypeInContext(ctx, 0)
    }
}

fn dm_llvm_array_type(elem: int, count: int) -> int {
    unsafe {
        return LLVMArrayType2(elem, count)
    }
}

fn dm_llvm_vector_type(elem: int, count: int) -> int {
    unsafe {
        return LLVMVectorType(elem, count as u32)
    }
}

fn dm_llvm_struct_create_named(ctx: int, name: string) -> int {
    unsafe {
        return LLVMStructCreateNamed(ctx, name)
    }
}

fn dm_llvm_get_type_kind(ty: int) -> int {
    unsafe {
        return LLVMGetTypeKind(ty) as int
    }
}

fn dm_llvm_get_int_type_width(ty: int) -> int {
    unsafe {
        return LLVMGetIntTypeWidth(ty) as int
    }
}

-- ============================================================
-- Values
-- ============================================================

fn dm_llvm_const_int(ty: int, val: int, sign_extend: int) -> int {
    unsafe {
        return LLVMConstInt(ty, val, bridge_bool(sign_extend))
    }
}

fn dm_llvm_const_real(ty: int, val: float) -> int {
    unsafe {
        return LLVMConstReal(ty, val)
    }
}

fn dm_llvm_const_null(ty: int) -> int {
    unsafe {
        return LLVMConstNull(ty)
    }
}

fn dm_llvm_set_initializer(global: int, init: int) {
    unsafe {
        LLVMSetInitializer(global, init)
    }
}

fn dm_llvm_set_global_constant(global: int, is_const: int) {
    unsafe {
        LLVMSetGlobalConstant(global, bridge_bool(is_const))
    }
}

fn dm_llvm_set_linkage(global: int, linkage: int) {
    unsafe {
        LLVMSetLinkage(global, linkage as i32)
    }
}

fn dm_llvm_get_param(func: int, index: int) -> int {
    unsafe {
        return LLVMGetParam(func, index as u32)
    }
}

fn dm_llvm_set_value_name(val: int, name: string) {
    unsafe {
        LLVMSetValueName2(val, name, len(name))
    }
}

fn dm_llvm_type_of(val: int) -> int {
    unsafe {
        return LLVMTypeOf(val)
    }
}

fn dm_llvm_get_undef(ty: int) -> int {
    unsafe {
        return LLVMGetUndef(ty)
    }
}

-- ============================================================
-- Basic blocks and the builder
-- ============================================================

fn dm_llvm_append_basic_block(ctx: int, func: int, name: string) -> int {
    unsafe {
        return LLVMAppendBasicBlockInContext(ctx, func, name)
    }
}

fn dm_llvm_create_builder(ctx: int) -> int {
    unsafe {
        return LLVMCreateBuilderInContext(ctx)
    }
}

fn dm_llvm_dispose_builder(builder: int) {
    unsafe {
        LLVMDisposeBuilder(builder)
    }
}

fn dm_llvm_position_at_end(builder: int, block: int) {
    unsafe {
        LLVMPositionBuilderAtEnd(builder, block)
    }
}

fn dm_llvm_get_insert_block(builder: int) -> int {
    unsafe {
        return LLVMGetInsertBlock(builder)
    }
}

-- ============================================================
-- Arithmetic and logic
-- ============================================================

fn dm_llvm_build_add(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildAdd(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_sub(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildSub(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_mul(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildMul(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_sdiv(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildSDiv(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_udiv(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildUDiv(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_srem(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildSRem(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_urem(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildURem(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_fadd(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildFAdd(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_fsub(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildFSub(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_fmul(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildFMul(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_fdiv(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildFDiv(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_frem(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildFRem(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_and(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildAnd(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_or(b: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildOr(b, lhs, rhs, name)
    }
}

fn dm_llvm_build_neg(b: int, val: int, name: string) -> int {
    unsafe {
        return LLVMBuildNeg(b, val, name)
    }
}

fn dm_llvm_build_fneg(b: int, val: int, name: string) -> int {
    unsafe {
        return LLVMBuildFNeg(b, val, name)
    }
}

fn dm_llvm_build_not(b: int, val: int, name: string) -> int {
    unsafe {
        return LLVMBuildNot(b, val, name)
    }
}

fn dm_llvm_build_icmp(b: int, pred: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildICmp(b, pred as i32, lhs, rhs, name)
    }
}

fn dm_llvm_build_fcmp(b: int, pred: int, lhs: int, rhs: int, name: string) -> int {
    unsafe {
        return LLVMBuildFCmp(b, pred as i32, lhs, rhs, name)
    }
}

-- ============================================================
-- Memory
-- ============================================================

-- Every alloca goes after the allocas already at the top of the
-- function's entry block, wherever the builder is. An alloca inside a
-- loop body takes new stack space on each iteration and is never
-- released before the function returns, so a long loop overflows the
-- stack; mem2reg also only promotes entry-block allocas.
fn dm_llvm_build_alloca(b: int, ty: int, name: string) -> int {
    unsafe {
        let current = LLVMGetInsertBlock(b)
        let entry = LLVMGetEntryBasicBlock(LLVMGetBasicBlockParent(current))
        let mut at = LLVMGetFirstInstruction(entry)
        let mut searching = true
        while searching {
            if at == 0 {
                searching = false
            } else if LLVMIsAAllocaInst(at) == 0 {
                searching = false
            } else {
                at = LLVMGetNextInstruction(at)
            }
        }
        -- Moving the builder before an instruction gives it that
        -- instruction's debug location: the alloca gets none, and the
        -- builder gets its own back
        let location = LLVMGetCurrentDebugLocation2(b)
        if at != 0 {
            LLVMPositionBuilderBefore(b, at)
        } else {
            LLVMPositionBuilderAtEnd(b, entry)
        }
        LLVMSetCurrentDebugLocation2(b, 0)
        let result = LLVMBuildAlloca(b, ty, name)
        LLVMPositionBuilderAtEnd(b, current)
        LLVMSetCurrentDebugLocation2(b, location)
        return result
    }
}

fn dm_llvm_build_load2(b: int, ty: int, ptr: int, name: string) -> int {
    unsafe {
        return LLVMBuildLoad2(b, ty, ptr, name)
    }
}

fn dm_llvm_build_store(b: int, val: int, ptr: int) -> int {
    unsafe {
        return LLVMBuildStore(b, val, ptr)
    }
}

-- The address of element idx of the array of `ty` at ptr
fn dm_llvm_build_gep2_one(b: int, ty: int, ptr: int, idx: int, name: string) -> int {
    unsafe {
        let mut index = idx
        return LLVMBuildInBoundsGEP2(b, ty, ptr, addr_of(index), 1, name)
    }
}

fn dm_llvm_build_struct_gep2(b: int, ty: int, ptr: int, idx: int, name: string) -> int {
    unsafe {
        return LLVMBuildStructGEP2(b, ty, ptr, idx as u32, name)
    }
}

-- Field idx of the struct value agg. When agg is not a struct the
-- result is a constant 0: llvm_gen reports that as a type error, and
-- generation goes on so that it can collect every error.
fn dm_llvm_build_extract_value(b: int, agg: int, idx: int, name: string) -> int {
    unsafe {
        -- 10 is LLVMStructTypeKind
        if LLVMGetTypeKind(LLVMTypeOf(agg)) != 10 {
            let func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b))
            let ctx = LLVMGetModuleContext(LLVMGetGlobalParent(func))
            return LLVMConstInt(LLVMInt64TypeInContext(ctx), 0, 0)
        }
        return LLVMBuildExtractValue(b, agg, idx as u32, name)
    }
}

fn dm_llvm_build_insert_value(b: int, agg: int, val: int, idx: int, name: string) -> int {
    unsafe {
        return LLVMBuildInsertValue(b, agg, val, idx as u32, name)
    }
}

-- ============================================================
-- Control flow
-- ============================================================

fn dm_llvm_build_br(b: int, dest: int) -> int {
    unsafe {
        return LLVMBuildBr(b, dest)
    }
}

fn dm_llvm_build_cond_br(b: int, cond: int, then_bb: int, else_bb: int) -> int {
    unsafe {
        return LLVMBuildCondBr(b, cond, then_bb, else_bb)
    }
}

fn dm_llvm_build_ret(b: int, val: int) -> int {
    unsafe {
        return LLVMBuildRet(b, val)
    }
}

fn dm_llvm_build_ret_void(b: int) -> int {
    unsafe {
        return LLVMBuildRetVoid(b)
    }
}

fn dm_llvm_build_unreachable(b: int) -> int {
    unsafe {
        return LLVMBuildUnreachable(b)
    }
}

fn dm_llvm_build_phi(b: int, ty: int, name: string) -> int {
    unsafe {
        return LLVMBuildPhi(b, ty, name)
    }
}

-- Add one incoming value, and the block it comes from, to a phi
fn dm_llvm_add_incoming_one(phi: int, val: int, block: int) {
    unsafe {
        let mut value = val
        let mut from = block
        LLVMAddIncoming(phi, addr_of(value), addr_of(from), 1)
    }
}

-- ============================================================
-- Casts
-- ============================================================

fn dm_llvm_build_int_cast2(b: int, val: int, dest_ty: int, is_signed: int, name: string) -> int {
    unsafe {
        return LLVMBuildIntCast2(b, val, dest_ty, bridge_bool(is_signed), name)
    }
}

fn dm_llvm_build_fp_cast(b: int, val: int, dest_ty: int, name: string) -> int {
    unsafe {
        return LLVMBuildFPCast(b, val, dest_ty, name)
    }
}

fn dm_llvm_build_si_to_fp(b: int, val: int, dest_ty: int, name: string) -> int {
    unsafe {
        return LLVMBuildSIToFP(b, val, dest_ty, name)
    }
}

fn dm_llvm_build_ui_to_fp(b: int, val: int, dest_ty: int, name: string) -> int {
    unsafe {
        return LLVMBuildUIToFP(b, val, dest_ty, name)
    }
}

fn dm_llvm_build_fp_to_si(b: int, val: int, dest_ty: int, name: string) -> int {
    unsafe {
        return LLVMBuildFPToSI(b, val, dest_ty, name)
    }
}

fn dm_llvm_build_trunc(b: int, val: int, dest_ty: int, name: string) -> int {
    unsafe {
        return LLVMBuildTrunc(b, val, dest_ty, name)
    }
}

fn dm_llvm_build_sext(b: int, val: int, dest_ty: int, name: string) -> int {
    unsafe {
        return LLVMBuildSExt(b, val, dest_ty, name)
    }
}

fn dm_llvm_build_int_to_ptr(b: int, val: int, dest_ty: int, name: string) -> int {
    unsafe {
        return LLVMBuildIntToPtr(b, val, dest_ty, name)
    }
}

fn dm_llvm_build_ptr_to_int(b: int, val: int, dest_ty: int, name: string) -> int {
    unsafe {
        return LLVMBuildPtrToInt(b, val, dest_ty, name)
    }
}

fn dm_llvm_sizeof_type(ty: int) -> int {
    unsafe {
        return LLVMSizeOf(ty)
    }
}

-- ============================================================
-- Vectors and global strings
-- ============================================================

fn dm_llvm_build_insert_element(b: int, vec: int, elem: int, idx: int, name: string) -> int {
    unsafe {
        return LLVMBuildInsertElement(b, vec, elem, idx, name)
    }
}

fn dm_llvm_build_extract_element(b: int, vec: int, idx: int, name: string) -> int {
    unsafe {
        return LLVMBuildExtractElement(b, vec, idx, name)
    }
}

fn dm_llvm_build_global_string_ptr(b: int, str: string, name: string) -> int {
    unsafe {
        return LLVMBuildGlobalStringPtr(b, str, name)
    }
}

-- ============================================================
-- Targets
-- ============================================================

fn dm_llvm_initialize_all_targets() {
    unsafe {
        LLVMInitializeX86TargetInfo()
        LLVMInitializeX86Target()
        LLVMInitializeX86TargetMC()
        LLVMInitializeX86AsmPrinter()
        LLVMInitializeX86AsmParser()
        LLVMInitializeAArch64TargetInfo()
        LLVMInitializeAArch64Target()
        LLVMInitializeAArch64TargetMC()
        LLVMInitializeAArch64AsmPrinter()
        LLVMInitializeAArch64AsmParser()
    }
}

-- Add an attribute that is only a name, such as "optnone", to a function
fn dm_llvm_add_function_attr(func: int, name: string) {
    unsafe {
        let kind = LLVMGetEnumAttributeKindForName(name, len(name))
        let ctx = LLVMGetModuleContext(LLVMGetGlobalParent(func))
        -- 4294967295 is LLVMAttributeFunctionIndex: the function itself,
        -- not one of its parameters
        LLVMAddAttributeAtIndex(func, 4294967295, LLVMCreateEnumAttribute(ctx, kind, 0))
    }
}

-- The address of a C string; free it with dm_llvm_free_c_string
fn dm_llvm_get_default_target_triple() -> int {
    unsafe {
        return LLVMGetDefaultTargetTriple()
    }
}

-- A target machine for the triple that emits position-independent code,
-- or 0 after printing the error. opt_level is 0 to 3.
fn dm_llvm_create_target_machine(triple: string, opt_level: int) -> int {
    unsafe {
        let mut target = 0
        let mut message = 0
        if LLVMGetTargetFromTriple(triple, addr_of(target), addr_of(message)) != 0 {
            if message != 0 {
                eprintln("Target error: " + bridge_borrowed(message))
                LLVMDisposeMessage(message)
            }
            return 0
        }
        -- LLVMCodeGenOptLevel numbers its levels 0 to 3 too
        let mut level = 0
        if opt_level >= 1 and opt_level <= 3 {
            level = opt_level
        }
        -- 2 is LLVMRelocPIC and 0 is LLVMCodeModelDefault
        return LLVMCreateTargetMachine(target, triple, "generic", "", level as i32, 2, 0)
    }
}

fn dm_llvm_dispose_target_machine(tm: int) {
    unsafe {
        LLVMDisposeTargetMachine(tm)
    }
}

-- Write the module as an object file (file_type 0) or as assembly.
-- Gives 0, or 1 after printing the error.
fn dm_llvm_target_machine_emit_to_file(tm: int, m: int, filename: string, file_type: int) -> int {
    unsafe {
        -- LLVMCodeGenFileType: 1 is LLVMObjectFile and 0 is LLVMAssemblyFile
        let mut llvm_file_type = 0
        if file_type == 0 {
            llvm_file_type = 1
        }
        let mut message = 0
        let failed = LLVMTargetMachineEmitToFile(tm, m, filename, llvm_file_type as i32, addr_of(message)) as int
        if failed != 0 and message != 0 {
            eprintln("Emit error: " + bridge_borrowed(message))
            LLVMDisposeMessage(message)
        }
        return failed
    }
}

-- The address of a C string; free it with dm_llvm_free_c_string
fn dm_llvm_get_target_data_layout(tm: int) -> int {
    unsafe {
        return LLVMCopyStringRepOfTargetData(LLVMCreateTargetDataLayout(tm))
    }
}

-- ============================================================
-- Enum constants
-- ============================================================

fn dm_llvm_int_eq() -> int { return 32 }  -- LLVMIntEQ
fn dm_llvm_int_ne() -> int { return 33 }  -- LLVMIntNE
fn dm_llvm_int_slt() -> int { return 40 }  -- LLVMIntSLT
fn dm_llvm_int_sle() -> int { return 41 }  -- LLVMIntSLE
fn dm_llvm_int_sgt() -> int { return 38 }  -- LLVMIntSGT
fn dm_llvm_int_sge() -> int { return 39 }  -- LLVMIntSGE

fn dm_llvm_real_oeq() -> int { return 1 }  -- LLVMRealOEQ
fn dm_llvm_real_one() -> int { return 6 }  -- LLVMRealONE
fn dm_llvm_real_olt() -> int { return 4 }  -- LLVMRealOLT
fn dm_llvm_real_ole() -> int { return 5 }  -- LLVMRealOLE
fn dm_llvm_real_ogt() -> int { return 2 }  -- LLVMRealOGT
fn dm_llvm_real_oge() -> int { return 3 }  -- LLVMRealOGE

fn dm_llvm_external_linkage() -> int { return 0 }  -- LLVMExternalLinkage
fn dm_llvm_weak_linkage() -> int { return 5 }  -- LLVMWeakAnyLinkage

-- The file types of dm_llvm_target_machine_emit_to_file
fn dm_llvm_object_file() -> int { return 0 }
fn dm_llvm_assembly_file() -> int { return 1 }

-- ============================================================
-- C strings
-- ============================================================

-- A copy of the C string at address c_str_ptr; "" when the address is 0
fn dm_llvm_c_str_to_string(c_str_ptr: int) -> string {
    unsafe {
        if c_str_ptr == 0 {
            return ""
        }
        return bridge_copy(c_str_ptr, bridge_strlen(c_str_ptr))
    }
}

-- Free a C string that LLVM allocated
fn dm_llvm_free_c_string(c_str_ptr: int) {
    unsafe {
        if c_str_ptr != 0 {
            LLVMDisposeMessage(c_str_ptr)
        }
    }
}

fn dm_llvm_get_value_name(val: int) -> string {
    unsafe {
        let mut length = 0
        let name = LLVMGetValueName2(val, addr_of(length))
        return bridge_copy(name, length)
    }
}

-- The address of a C string; free it with dm_llvm_free_c_string
fn dm_llvm_print_type_to_string(ty: int) -> int {
    unsafe {
        return LLVMPrintTypeToString(ty)
    }
}

-- The name of a struct type; "" when it has none
fn dm_llvm_get_struct_name(ty: int) -> string {
    unsafe {
        let name = LLVMGetStructName(ty)
        if name == 0 {
            return ""
        }
        return bridge_copy(name, bridge_strlen(name))
    }
}

-- ============================================================
-- The list buffer
-- LLVM takes a list of types or values as an array. The compiler
-- builds one here and then calls a function that uses it:
--   dm_llvm_buf_clear()
--   dm_llvm_buf_push(ref1)
--   dm_llvm_buf_push(ref2)
--   dm_llvm_struct_set_body_buf(ty, packed)
-- Each function that uses the list leaves it empty.
-- ============================================================

-- The most references the buffer holds
fn bridge_buf_max() -> int {
    return 256
}

-- The address of the buffer: room for bridge_buf_max() references
fn bridge_buf() -> int {
    unsafe {
        return static_addr("llvm_bridge_buf", 2048)
    }
}

-- The address of the number of references in the buffer
fn bridge_buf_count() -> int {
    unsafe {
        return static_addr("llvm_bridge_buf_count", 8)
    }
}

fn dm_llvm_buf_clear() {
    unsafe {
        store_i64(bridge_buf_count(), 0)
    }
}

fn dm_llvm_buf_push(ref: int) {
    unsafe {
        let count = load_i64(bridge_buf_count())
        if count >= bridge_buf_max() {
            panic("LLVM bridge: a list of more than 256 types or values")
        }
        store_i64(bridge_buf() + count * 8, ref)
        store_i64(bridge_buf_count(), count + 1)
    }
}

fn dm_llvm_buf_len() -> int {
    unsafe {
        return load_i64(bridge_buf_count())
    }
}

fn dm_llvm_buf_get(index: int) -> int {
    unsafe {
        return load_i64(bridge_buf() + index * 8)
    }
}

-- A struct type of the first `count` types in the list
fn dm_llvm_struct_type_buf(ctx: int, count: int, is_packed: int) -> int {
    unsafe {
        let result = LLVMStructTypeInContext(ctx, bridge_buf(), count as u32, is_packed as i32)
        store_i64(bridge_buf_count(), 0)
        return result
    }
}

-- Give a named struct type the listed types as its fields
fn dm_llvm_struct_set_body_buf(struct_ty: int, is_packed: int) {
    unsafe {
        LLVMStructSetBody(struct_ty, bridge_buf(), load_i64(bridge_buf_count()) as u32, is_packed as i32)
        store_i64(bridge_buf_count(), 0)
    }
}

-- The type of a function that takes the listed types
fn dm_llvm_function_type_buf(ret: int, is_var_arg: int) -> int {
    unsafe {
        let result = LLVMFunctionType(ret, bridge_buf(), load_i64(bridge_buf_count()) as u32, is_var_arg as i32)
        store_i64(bridge_buf_count(), 0)
        return result
    }
}

-- A C function reads a bool argument as a whole byte, so the caller has
-- to zero-extend it. LLVM leaves the upper bits of an i1 argument
-- undefined unless the call marks it zeroext.
fn bridge_call_zeroext_bools(call: int) -> int {
    unsafe {
        let zeroext = LLVMGetEnumAttributeKindForName("zeroext", 7)
        let count = LLVMGetNumArgOperands(call) as int
        let mut i = 0
        while i < count {
            let ty = LLVMTypeOf(LLVMGetOperand(call, i as u32))
            -- 8 is LLVMIntegerTypeKind
            if LLVMGetTypeKind(ty) == 8 {
                if LLVMGetIntTypeWidth(ty) == 1 {
                    LLVMAddCallSiteAttribute(call, (i + 1) as u32, LLVMCreateEnumAttribute(LLVMGetTypeContext(ty), zeroext, 0))
                }
            }
            i = i + 1
        }
        return call
    }
}

-- A call of func, whose type is fn_ty, with the listed values
fn dm_llvm_build_call2_buf(b: int, fn_ty: int, func: int, name: string) -> int {
    unsafe {
        let call = LLVMBuildCall2(b, fn_ty, func, bridge_buf(), load_i64(bridge_buf_count()) as u32, name)
        store_i64(bridge_buf_count(), 0)
        return bridge_call_zeroext_bools(call)
    }
}

-- ============================================================
-- Function types
-- ============================================================

fn dm_llvm_count_param_types(fn_ty: int) -> int {
    unsafe {
        return LLVMCountParamTypes(fn_ty) as int
    }
}

fn dm_llvm_param_type_at(fn_ty: int, index: int) -> int {
    unsafe {
        let count = LLVMCountParamTypes(fn_ty) as int
        if index < 0 or index >= count {
            panic("LLVM bridge: a function type has no parameter " + int_to_string(index))
        }
        let types = bridge_malloc(count * 8)
        if types == 0 {
            panic("out of memory")
        }
        LLVMGetParamTypes(fn_ty, types)
        let result = load_i64(types + index * 8)
        bridge_free(types)
        return result
    }
}

fn dm_llvm_is_function_var_arg(fn_ty: int) -> int {
    unsafe {
        return LLVMIsFunctionVarArg(fn_ty) as int
    }
}

-- ============================================================
-- Debug info (DWARF)
-- ============================================================

-- A builder of debug info for the module, which is marked as having
-- debug info of the version this LLVM writes, as DWARF 4
fn dm_llvm_di_builder_create(m: int) -> int {
    unsafe {
        let i32_type = LLVMInt32TypeInContext(LLVMGetModuleContext(m))
        let version = LLVMDebugMetadataVersion() as int
        -- 1 is LLVMModuleFlagBehaviorWarning
        LLVMAddModuleFlag(m, 1, "Debug Info Version", 18, LLVMValueAsMetadata(LLVMConstInt(i32_type, version, 0)))
        LLVMAddModuleFlag(m, 1, "Dwarf Version", 13, LLVMValueAsMetadata(LLVMConstInt(i32_type, 4, 0)))
        return LLVMCreateDIBuilder(m)
    }
}

-- Complete the module's debug info and dispose of the builder
fn dm_llvm_di_builder_finalize(dib: int) {
    unsafe {
        LLVMDIBuilderFinalize(dib)
        LLVMDisposeDIBuilder(dib)
    }
}

fn dm_llvm_di_file(dib: int, name: string, directory: string) -> int {
    unsafe {
        return LLVMDIBuilderCreateFile(dib, name, len(name), directory, len(directory))
    }
}

-- The compile unit of the program whose main file is `file`. A
-- debugger treats the language as C99, the nearest it knows.
fn dm_llvm_di_compile_unit(dib: int, file: int, producer: string, optimized: int) -> int {
    unsafe {
        -- 11 is LLVMDWARFSourceLanguageC99, 1 is LLVMDWARFEmissionFull
        return LLVMDIBuilderCreateCompileUnit(dib, 11, file, producer, len(producer), bridge_bool(optimized), "", 0, 0, "", 0, 1, 0, 0, 0, "", 0, "", 0)
    }
}

-- A function defined at `line` of `file`. Its type says nothing of its
-- parameters.
fn dm_llvm_di_function(dib: int, file: int, name: string, symbol: string, line: int, optimized: int) -> int {
    unsafe {
        let ty = LLVMDIBuilderCreateSubroutineType(dib, file, 0, 0, 0)
        return LLVMDIBuilderCreateFunction(dib, file, name, len(name), symbol, len(symbol), file, line as u32, ty, 0, 1, line as u32, 0, bridge_bool(optimized))
    }
}

fn dm_llvm_set_subprogram(func: int, subprogram: int) {
    unsafe {
        LLVMSetSubprogram(func, subprogram)
    }
}

-- The place of the instructions the builder makes from now on: a line
-- and column of the function `scope`, or no place when scope is 0
fn dm_llvm_set_debug_location(b: int, ctx: int, line: int, col: int, scope: int) {
    unsafe {
        let mut loc = 0
        if scope != 0 {
            loc = LLVMDIBuilderCreateDebugLocation(ctx, line as u32, col as u32, scope, 0)
        }
        LLVMSetCurrentDebugLocation2(b, loc)
    }
}

fn dm_llvm_di_basic_type(dib: int, name: string, bits: int, encoding: int) -> int {
    unsafe {
        return LLVMDIBuilderCreateBasicType(dib, name, len(name), bits, encoding as u32, 0)
    }
}

-- A pointer to `pointee`, or to nothing in particular when it is 0
fn dm_llvm_di_pointer_type(dib: int, pointee: int, name: string) -> int {
    unsafe {
        return LLVMDIBuilderCreatePointerType(dib, pointee, 64, 0, 0, name, len(name))
    }
}

-- A field of the struct `scope`
fn dm_llvm_di_member(dib: int, scope: int, name: string, bits: int, offset_bits: int, ty: int) -> int {
    unsafe {
        return LLVMDIBuilderCreateMemberType(dib, scope, name, len(name), 0, 0, bits, 0, offset_bits, 0, ty)
    }
}

-- A struct type whose fields are not known yet: it stands for the
-- struct, in its own fields' types too, until dm_llvm_di_struct_end_buf
-- replaces it. 19 is DW_TAG_structure_type.
fn dm_llvm_di_struct_begin(dib: int, name: string, bits: int) -> int {
    unsafe {
        return LLVMDIBuilderCreateReplaceableCompositeType(dib, 19, name, len(name), 0, 0, 0, 0, bits, 0, 0, "", 0)
    }
}

-- The struct type whose fields are the listed members, which takes the
-- place of `begun`
fn dm_llvm_di_struct_end_buf(dib: int, begun: int, name: string, bits: int) -> int {
    unsafe {
        let count = load_i64(bridge_buf_count())
        let made = LLVMDIBuilderCreateStructType(dib, 0, name, len(name), 0, 0, bits, 0, 0, 0, bridge_buf(), count as u32, 0, 0, "", 0)
        store_i64(bridge_buf_count(), 0)
        LLVMMetadataReplaceAllUsesWith(begun, made)
        return made
    }
}

-- A variable of the function `scope`: its parameter number `arg`, from
-- 1, or a local when arg is 0
fn dm_llvm_di_variable(dib: int, scope: int, name: string, file: int, line: int, ty: int, arg: int) -> int {
    unsafe {
        if arg > 0 {
            return LLVMDIBuilderCreateParameterVariable(dib, scope, name, len(name), arg as u32, file, line as u32, ty, 1, 0)
        }
        return LLVMDIBuilderCreateAutoVariable(dib, scope, name, len(name), file, line as u32, ty, 1, 0, 0)
    }
}

-- Say, at the end of the builder's block, that the variable lives in
-- `storage`; with deref, that storage holds the variable's address
fn dm_llvm_di_declare(dib: int, b: int, storage: int, variable: int, ctx: int, line: int, scope: int, deref: int) {
    unsafe {
        -- 6 is DW_OP_deref
        let mut op = 6
        let mut count = 0
        if deref != 0 {
            count = 1
        }
        let expr = LLVMDIBuilderCreateExpression(dib, addr_of(op), count)
        let loc = LLVMDIBuilderCreateDebugLocation(ctx, line as u32, 0, scope, 0)
        LLVMDIBuilderInsertDeclareRecordAtEnd(dib, storage, variable, expr, loc, LLVMGetInsertBlock(b))
    }
}

fn dm_llvm_is_alloca(val: int) -> int {
    unsafe {
        if LLVMIsAAllocaInst(val) != 0 {
            return 1
        }
        return 0
    }
}

-- The size in bytes of a type in the module's data layout, which must
-- be set
fn dm_llvm_abi_size_of(m: int, ty: int) -> int {
    unsafe {
        return LLVMABISizeOfType(LLVMGetModuleDataLayout(m), ty)
    }
}

-- The offset in bytes of a struct type's field
fn dm_llvm_offset_of_element(m: int, struct_ty: int, index: int) -> int {
    unsafe {
        return LLVMOffsetOfElement(LLVMGetModuleDataLayout(m), struct_ty, index as u32)
    }
}

-- The number of fields of a struct type; 0 for a struct type with no
-- body and for a type that is no struct
fn dm_llvm_struct_field_count(ty: int) -> int {
    unsafe {
        -- 10 is LLVMStructTypeKind
        if LLVMGetTypeKind(ty) as int != 10 {
            return 0
        }
        if LLVMIsOpaqueStruct(ty) as int != 0 {
            return 0
        }
        return LLVMCountStructElementTypes(ty) as int
    }
}
