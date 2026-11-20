/*
 * LLVM-C Bridge for the Stage 5 compiler that Stage 0 builds
 *
 * Stage 0 emits C and cannot parse llvm_bridge.dm, which defines these
 * same functions in dAImond and is what Stage 5 uses when it builds
 * itself. Until Stage 0 goes, a change to one file belongs in the
 * other too.
 *
 * All LLVM-C opaque pointer types (LLVMContextRef, LLVMValueRef, etc.)
 * are cast to/from int64_t so dAImond can use them as plain `int` values.
 *
 * Naming convention: dm_llvm_<category>_<operation>
 */

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/DebugInfo.h>
#include <llvm-c/Transforms/PassBuilder.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* ================================================================
 * Helper macros for pointer <-> int64_t conversion
 * ================================================================ */

#define TO_PTR(type, val)    ((type)(uintptr_t)(val))
#define TO_INT(ptr)          ((int64_t)(uintptr_t)(ptr))

/* ================================================================
 * Context
 * ================================================================ */

int64_t dm_llvm_context_create(void) {
    return TO_INT(LLVMContextCreate());
}

void dm_llvm_context_dispose(int64_t ctx) {
    LLVMContextDispose(TO_PTR(LLVMContextRef, ctx));
}

/* ================================================================
 * Module
 * ================================================================ */

int64_t dm_llvm_module_create(int64_t ctx, const char* name) {
    return TO_INT(LLVMModuleCreateWithNameInContext(name, TO_PTR(LLVMContextRef, ctx)));
}

void dm_llvm_module_dispose(int64_t mod) {
    LLVMDisposeModule(TO_PTR(LLVMModuleRef, mod));
}

void dm_llvm_module_set_target(int64_t mod, const char* triple) {
    LLVMSetTarget(TO_PTR(LLVMModuleRef, mod), triple);
}

void dm_llvm_module_set_data_layout(int64_t mod, const char* layout) {
    LLVMSetDataLayout(TO_PTR(LLVMModuleRef, mod), layout);
}

int64_t dm_llvm_module_add_function(int64_t mod, const char* name, int64_t fn_type) {
    return TO_INT(LLVMAddFunction(TO_PTR(LLVMModuleRef, mod), name,
                                   TO_PTR(LLVMTypeRef, fn_type)));
}

int64_t dm_llvm_module_add_global(int64_t mod, int64_t ty, const char* name) {
    return TO_INT(LLVMAddGlobal(TO_PTR(LLVMModuleRef, mod),
                                 TO_PTR(LLVMTypeRef, ty), name));
}

int64_t dm_llvm_module_verify(int64_t mod) {
    char* err_msg = NULL;
    int result = LLVMVerifyModule(TO_PTR(LLVMModuleRef, mod),
                                   LLVMReturnStatusAction, &err_msg);
    if (result != 0 && err_msg) {
        fprintf(stderr, "LLVM Module verification failed:\n%s\n", err_msg);
        LLVMDisposeMessage(err_msg);
    }
    return (int64_t)result;
}

/* Returns a C string (caller must call dm_llvm_dispose_message) */
int64_t dm_llvm_module_print_to_file(int64_t mod, const char* filename) {
    char* err_msg = NULL;
    int result = LLVMPrintModuleToFile(TO_PTR(LLVMModuleRef, mod), filename, &err_msg);
    if (result != 0 && err_msg) {
        LLVMDisposeMessage(err_msg);
    }
    return (int64_t)result;
}

/* Write the module as LLVM bitcode. Returns 0, or 1 when the file cannot be written. */
int64_t dm_llvm_module_write_bitcode(int64_t mod, const char* filename) {
    return LLVMWriteBitcodeToFile(TO_PTR(LLVMModuleRef, mod), filename) != 0 ? 1 : 0;
}

/* Pass manager */
int64_t dm_llvm_run_passes(int64_t mod, const char* passes, int64_t tm) {
    LLVMPassBuilderOptionsRef opts = LLVMCreatePassBuilderOptions();
    LLVMErrorRef err = LLVMRunPasses(TO_PTR(LLVMModuleRef, mod), passes,
                                      TO_PTR(LLVMTargetMachineRef, tm), opts);
    LLVMDisposePassBuilderOptions(opts);
    if (err != NULL) {
        char* msg = LLVMGetErrorMessage(err);
        fprintf(stderr, "Pass run failed: %s\n", msg);
        LLVMDisposeErrorMessage(msg);
        return 1;
    }
    return 0;
}

/* ================================================================
 * Types
 * ================================================================ */

int64_t dm_llvm_int1_type(int64_t ctx) {
    return TO_INT(LLVMInt1TypeInContext(TO_PTR(LLVMContextRef, ctx)));
}

int64_t dm_llvm_int8_type(int64_t ctx) {
    return TO_INT(LLVMInt8TypeInContext(TO_PTR(LLVMContextRef, ctx)));
}

int64_t dm_llvm_int16_type(int64_t ctx) {
    return TO_INT(LLVMInt16TypeInContext(TO_PTR(LLVMContextRef, ctx)));
}

int64_t dm_llvm_int32_type(int64_t ctx) {
    return TO_INT(LLVMInt32TypeInContext(TO_PTR(LLVMContextRef, ctx)));
}

int64_t dm_llvm_int64_type(int64_t ctx) {
    return TO_INT(LLVMInt64TypeInContext(TO_PTR(LLVMContextRef, ctx)));
}

int64_t dm_llvm_float_type(int64_t ctx) {
    return TO_INT(LLVMFloatTypeInContext(TO_PTR(LLVMContextRef, ctx)));
}

int64_t dm_llvm_double_type(int64_t ctx) {
    return TO_INT(LLVMDoubleTypeInContext(TO_PTR(LLVMContextRef, ctx)));
}

int64_t dm_llvm_void_type(int64_t ctx) {
    return TO_INT(LLVMVoidTypeInContext(TO_PTR(LLVMContextRef, ctx)));
}

int64_t dm_llvm_pointer_type(int64_t ctx) {
    return TO_INT(LLVMPointerTypeInContext(TO_PTR(LLVMContextRef, ctx), 0));
}

int64_t dm_llvm_array_type(int64_t elem, int64_t count) {
    return TO_INT(LLVMArrayType2(TO_PTR(LLVMTypeRef, elem), (uint64_t)count));
}

int64_t dm_llvm_vector_type(int64_t elem, int64_t count) {
    return TO_INT(LLVMVectorType(TO_PTR(LLVMTypeRef, elem), (unsigned)count));
}

/* Struct types - fields passed as array of int64_t */
int64_t dm_llvm_struct_create_named(int64_t ctx, const char* name) {
    return TO_INT(LLVMStructCreateNamed(TO_PTR(LLVMContextRef, ctx), name));
}

int64_t dm_llvm_get_type_kind(int64_t ty) {
    return (int64_t)LLVMGetTypeKind(TO_PTR(LLVMTypeRef, ty));
}

int64_t dm_llvm_get_int_type_width(int64_t ty) {
    return (int64_t)LLVMGetIntTypeWidth(TO_PTR(LLVMTypeRef, ty));
}

/* ================================================================
 * Values / Constants
 * ================================================================ */

int64_t dm_llvm_const_int(int64_t ty, int64_t val, int64_t sign_extend) {
    return TO_INT(LLVMConstInt(TO_PTR(LLVMTypeRef, ty), (uint64_t)val, sign_extend ? 1 : 0));
}

int64_t dm_llvm_const_real(int64_t ty, double val) {
    return TO_INT(LLVMConstReal(TO_PTR(LLVMTypeRef, ty), val));
}

int64_t dm_llvm_const_null(int64_t ty) {
    return TO_INT(LLVMConstNull(TO_PTR(LLVMTypeRef, ty)));
}

void dm_llvm_set_initializer(int64_t global, int64_t init) {
    LLVMSetInitializer(TO_PTR(LLVMValueRef, global), TO_PTR(LLVMValueRef, init));
}

void dm_llvm_set_global_constant(int64_t global, int64_t is_const) {
    LLVMSetGlobalConstant(TO_PTR(LLVMValueRef, global), is_const ? 1 : 0);
}

void dm_llvm_set_linkage(int64_t global, int64_t linkage) {
    LLVMSetLinkage(TO_PTR(LLVMValueRef, global), (LLVMLinkage)linkage);
}

int64_t dm_llvm_get_param(int64_t func, int64_t index) {
    return TO_INT(LLVMGetParam(TO_PTR(LLVMValueRef, func), (unsigned)index));
}

void dm_llvm_set_value_name(int64_t val, const char* name) {
    LLVMSetValueName2(TO_PTR(LLVMValueRef, val), name, strlen(name));
}

int64_t dm_llvm_type_of(int64_t val) {
    return TO_INT(LLVMTypeOf(TO_PTR(LLVMValueRef, val)));
}

int64_t dm_llvm_get_undef(int64_t ty) {
    return TO_INT(LLVMGetUndef(TO_PTR(LLVMTypeRef, ty)));
}

/* ================================================================
 * Basic Blocks
 * ================================================================ */

int64_t dm_llvm_append_basic_block(int64_t ctx, int64_t func, const char* name) {
    return TO_INT(LLVMAppendBasicBlockInContext(
        TO_PTR(LLVMContextRef, ctx),
        TO_PTR(LLVMValueRef, func), name));
}

/* ================================================================
 * Builder
 * ================================================================ */

int64_t dm_llvm_create_builder(int64_t ctx) {
    return TO_INT(LLVMCreateBuilderInContext(TO_PTR(LLVMContextRef, ctx)));
}

void dm_llvm_dispose_builder(int64_t builder) {
    LLVMDisposeBuilder(TO_PTR(LLVMBuilderRef, builder));
}

void dm_llvm_position_at_end(int64_t builder, int64_t block) {
    LLVMPositionBuilderAtEnd(TO_PTR(LLVMBuilderRef, builder),
                              TO_PTR(LLVMBasicBlockRef, block));
}

int64_t dm_llvm_get_insert_block(int64_t builder) {
    return TO_INT(LLVMGetInsertBlock(TO_PTR(LLVMBuilderRef, builder)));
}

/* Arithmetic */
int64_t dm_llvm_build_add(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildAdd(TO_PTR(LLVMBuilderRef, b),
                                TO_PTR(LLVMValueRef, lhs),
                                TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_sub(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildSub(TO_PTR(LLVMBuilderRef, b),
                                TO_PTR(LLVMValueRef, lhs),
                                TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_mul(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildMul(TO_PTR(LLVMBuilderRef, b),
                                TO_PTR(LLVMValueRef, lhs),
                                TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_sdiv(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildSDiv(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_udiv(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildUDiv(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_urem(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildURem(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_srem(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildSRem(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_neg(int64_t b, int64_t val, const char* name) {
    return TO_INT(LLVMBuildNeg(TO_PTR(LLVMBuilderRef, b),
                                TO_PTR(LLVMValueRef, val), name));
}

int64_t dm_llvm_build_fadd(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildFAdd(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_fsub(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildFSub(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_fmul(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildFMul(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_fdiv(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildFDiv(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_frem(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildFRem(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_fneg(int64_t b, int64_t val, const char* name) {
    return TO_INT(LLVMBuildFNeg(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, val), name));
}

/* Comparison */
int64_t dm_llvm_build_icmp(int64_t b, int64_t pred, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildICmp(TO_PTR(LLVMBuilderRef, b),
                                 (LLVMIntPredicate)pred,
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_fcmp(int64_t b, int64_t pred, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildFCmp(TO_PTR(LLVMBuilderRef, b),
                                 (LLVMRealPredicate)pred,
                                 TO_PTR(LLVMValueRef, lhs),
                                 TO_PTR(LLVMValueRef, rhs), name));
}

/* Memory */
/* Every alloca goes after the allocas already at the top of the function's
 * entry block, wherever the builder is. An alloca inside a loop body takes
 * new stack space on each iteration and is never released before the
 * function returns, so a long loop overflows the stack; mem2reg also only
 * promotes entry-block allocas. */
int64_t dm_llvm_build_alloca(int64_t b, int64_t ty, const char* name) {
    LLVMBuilderRef builder = TO_PTR(LLVMBuilderRef, b);
    LLVMBasicBlockRef cur = LLVMGetInsertBlock(builder);
    LLVMBasicBlockRef entry = LLVMGetEntryBasicBlock(LLVMGetBasicBlockParent(cur));
    LLVMValueRef at = LLVMGetFirstInstruction(entry);
    while (at && LLVMIsAAllocaInst(at)) at = LLVMGetNextInstruction(at);
    /* Moving the builder before an instruction gives it that
     * instruction's debug location: the alloca gets none, and the
     * builder gets its own back */
    LLVMMetadataRef location = LLVMGetCurrentDebugLocation2(builder);
    if (at) LLVMPositionBuilderBefore(builder, at);
    else LLVMPositionBuilderAtEnd(builder, entry);
    LLVMSetCurrentDebugLocation2(builder, NULL);
    LLVMValueRef result = LLVMBuildAlloca(builder, TO_PTR(LLVMTypeRef, ty), name);
    LLVMPositionBuilderAtEnd(builder, cur);
    LLVMSetCurrentDebugLocation2(builder, location);
    return TO_INT(result);
}

int64_t dm_llvm_build_load2(int64_t b, int64_t ty, int64_t ptr, const char* name) {
    return TO_INT(LLVMBuildLoad2(TO_PTR(LLVMBuilderRef, b),
                                  TO_PTR(LLVMTypeRef, ty),
                                  TO_PTR(LLVMValueRef, ptr), name));
}

int64_t dm_llvm_build_store(int64_t b, int64_t val, int64_t ptr) {
    return TO_INT(LLVMBuildStore(TO_PTR(LLVMBuilderRef, b),
                                  TO_PTR(LLVMValueRef, val),
                                  TO_PTR(LLVMValueRef, ptr)));
}

int64_t dm_llvm_build_gep2_one(int64_t b, int64_t ty, int64_t ptr, int64_t idx, const char* name) {
    LLVMValueRef index = TO_PTR(LLVMValueRef, idx);
    return TO_INT(LLVMBuildInBoundsGEP2(TO_PTR(LLVMBuilderRef, b), TO_PTR(LLVMTypeRef, ty),
                                        TO_PTR(LLVMValueRef, ptr), &index, 1, name));
}

int64_t dm_llvm_build_struct_gep2(int64_t b, int64_t ty, int64_t ptr, int64_t idx, const char* name) {
    return TO_INT(LLVMBuildStructGEP2(TO_PTR(LLVMBuilderRef, b),
                                       TO_PTR(LLVMTypeRef, ty),
                                       TO_PTR(LLVMValueRef, ptr),
                                       (unsigned)idx, name));
}

int64_t dm_llvm_build_extract_value(int64_t b, int64_t agg, int64_t idx, const char* name) {
    LLVMValueRef agg_val = TO_PTR(LLVMValueRef, agg);
    LLVMTypeRef agg_ty = LLVMTypeOf(agg_val);
    LLVMTypeKind tk = LLVMGetTypeKind(agg_ty);
    if (tk != LLVMStructTypeKind) {
        /* llvm_gen reports this as a type error; return a dummy so
         * generation can continue and collect every error. */
        LLVMBasicBlockRef bb = LLVMGetInsertBlock(TO_PTR(LLVMBuilderRef, b));
        LLVMValueRef fn = LLVMGetBasicBlockParent(bb);
        return TO_INT(LLVMConstInt(LLVMInt64TypeInContext(
            LLVMGetModuleContext(LLVMGetGlobalParent(fn))), 0, 0));
    }
    return TO_INT(LLVMBuildExtractValue(TO_PTR(LLVMBuilderRef, b),
                                         agg_val,
                                         (unsigned)idx, name));
}

int64_t dm_llvm_build_insert_value(int64_t b, int64_t agg, int64_t val, int64_t idx, const char* name) {
    return TO_INT(LLVMBuildInsertValue(TO_PTR(LLVMBuilderRef, b),
                                        TO_PTR(LLVMValueRef, agg),
                                        TO_PTR(LLVMValueRef, val),
                                        (unsigned)idx, name));
}

/* Control flow */
int64_t dm_llvm_build_br(int64_t b, int64_t dest) {
    return TO_INT(LLVMBuildBr(TO_PTR(LLVMBuilderRef, b),
                               TO_PTR(LLVMBasicBlockRef, dest)));
}

int64_t dm_llvm_build_cond_br(int64_t b, int64_t cond, int64_t then_bb, int64_t else_bb) {
    return TO_INT(LLVMBuildCondBr(TO_PTR(LLVMBuilderRef, b),
                                   TO_PTR(LLVMValueRef, cond),
                                   TO_PTR(LLVMBasicBlockRef, then_bb),
                                   TO_PTR(LLVMBasicBlockRef, else_bb)));
}

int64_t dm_llvm_build_ret(int64_t b, int64_t val) {
    return TO_INT(LLVMBuildRet(TO_PTR(LLVMBuilderRef, b),
                                TO_PTR(LLVMValueRef, val)));
}

int64_t dm_llvm_build_ret_void(int64_t b) {
    return TO_INT(LLVMBuildRetVoid(TO_PTR(LLVMBuilderRef, b)));
}

int64_t dm_llvm_build_unreachable(int64_t b) {
    return TO_INT(LLVMBuildUnreachable(TO_PTR(LLVMBuilderRef, b)));
}

/* Function calls */
/* A C function reads a bool argument as a whole byte, so the caller has to
 * zero-extend it. LLVM leaves the upper bits of an i1 argument undefined
 * unless the call marks it zeroext. */
static LLVMValueRef dm_call_zeroext_bools(LLVMValueRef call) {
    unsigned zeroext = LLVMGetEnumAttributeKindForName("zeroext", 7);
    unsigned n = LLVMGetNumArgOperands(call);
    for (unsigned i = 0; i < n; i++) {
        LLVMTypeRef ty = LLVMTypeOf(LLVMGetOperand(call, i));
        if (LLVMGetTypeKind(ty) == LLVMIntegerTypeKind && LLVMGetIntTypeWidth(ty) == 1) {
            LLVMAddCallSiteAttribute(call, i + 1,
                LLVMCreateEnumAttribute(LLVMGetTypeContext(ty), zeroext, 0));
        }
    }
    return call;
}

/* Phi */
int64_t dm_llvm_build_phi(int64_t b, int64_t ty, const char* name) {
    return TO_INT(LLVMBuildPhi(TO_PTR(LLVMBuilderRef, b),
                                TO_PTR(LLVMTypeRef, ty), name));
}

/* Casts */
int64_t dm_llvm_build_int_cast2(int64_t b, int64_t val, int64_t dest_ty,
                                 int64_t is_signed, const char* name) {
    return TO_INT(LLVMBuildIntCast2(TO_PTR(LLVMBuilderRef, b),
                                     TO_PTR(LLVMValueRef, val),
                                     TO_PTR(LLVMTypeRef, dest_ty),
                                     is_signed ? 1 : 0, name));
}

int64_t dm_llvm_build_fp_cast(int64_t b, int64_t val, int64_t dest_ty, const char* name) {
    return TO_INT(LLVMBuildFPCast(TO_PTR(LLVMBuilderRef, b),
                                   TO_PTR(LLVMValueRef, val),
                                   TO_PTR(LLVMTypeRef, dest_ty), name));
}

int64_t dm_llvm_build_si_to_fp(int64_t b, int64_t val, int64_t dest_ty, const char* name) {
    return TO_INT(LLVMBuildSIToFP(TO_PTR(LLVMBuilderRef, b),
                                   TO_PTR(LLVMValueRef, val),
                                   TO_PTR(LLVMTypeRef, dest_ty), name));
}

int64_t dm_llvm_build_ui_to_fp(int64_t b, int64_t val, int64_t dest_ty, const char* name) {
    return TO_INT(LLVMBuildUIToFP(TO_PTR(LLVMBuilderRef, b),
                                   TO_PTR(LLVMValueRef, val),
                                   TO_PTR(LLVMTypeRef, dest_ty), name));
}

int64_t dm_llvm_build_fp_to_si(int64_t b, int64_t val, int64_t dest_ty, const char* name) {
    return TO_INT(LLVMBuildFPToSI(TO_PTR(LLVMBuilderRef, b),
                                   TO_PTR(LLVMValueRef, val),
                                   TO_PTR(LLVMTypeRef, dest_ty), name));
}

int64_t dm_llvm_build_trunc(int64_t b, int64_t val, int64_t dest_ty, const char* name) {
    return TO_INT(LLVMBuildTrunc(TO_PTR(LLVMBuilderRef, b),
                                  TO_PTR(LLVMValueRef, val),
                                  TO_PTR(LLVMTypeRef, dest_ty), name));
}

int64_t dm_llvm_build_sext(int64_t b, int64_t val, int64_t dest_ty, const char* name) {
    return TO_INT(LLVMBuildSExt(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMValueRef, val),
                                 TO_PTR(LLVMTypeRef, dest_ty), name));
}

int64_t dm_llvm_build_int_to_ptr(int64_t b, int64_t val, int64_t dest_ty, const char* name) {
    return TO_INT(LLVMBuildIntToPtr(TO_PTR(LLVMBuilderRef, b),
                                     TO_PTR(LLVMValueRef, val),
                                     TO_PTR(LLVMTypeRef, dest_ty), name));
}

int64_t dm_llvm_build_ptr_to_int(int64_t b, int64_t val, int64_t dest_ty, const char* name) {
    return TO_INT(LLVMBuildPtrToInt(TO_PTR(LLVMBuilderRef, b),
                                     TO_PTR(LLVMValueRef, val),
                                     TO_PTR(LLVMTypeRef, dest_ty), name));
}

int64_t dm_llvm_sizeof_type(int64_t ty) {
    return TO_INT(LLVMSizeOf(TO_PTR(LLVMTypeRef, ty)));
}

/* Logical */
int64_t dm_llvm_build_and(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildAnd(TO_PTR(LLVMBuilderRef, b),
                                TO_PTR(LLVMValueRef, lhs),
                                TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_or(int64_t b, int64_t lhs, int64_t rhs, const char* name) {
    return TO_INT(LLVMBuildOr(TO_PTR(LLVMBuilderRef, b),
                               TO_PTR(LLVMValueRef, lhs),
                               TO_PTR(LLVMValueRef, rhs), name));
}

int64_t dm_llvm_build_not(int64_t b, int64_t val, const char* name) {
    return TO_INT(LLVMBuildNot(TO_PTR(LLVMBuilderRef, b),
                                TO_PTR(LLVMValueRef, val), name));
}

/* SIMD / Vector */
int64_t dm_llvm_build_insert_element(int64_t b, int64_t vec, int64_t elem, int64_t idx, const char* name) {
    return TO_INT(LLVMBuildInsertElement(TO_PTR(LLVMBuilderRef, b),
                                          TO_PTR(LLVMValueRef, vec),
                                          TO_PTR(LLVMValueRef, elem),
                                          TO_PTR(LLVMValueRef, idx), name));
}

int64_t dm_llvm_build_extract_element(int64_t b, int64_t vec, int64_t idx, const char* name) {
    return TO_INT(LLVMBuildExtractElement(TO_PTR(LLVMBuilderRef, b),
                                           TO_PTR(LLVMValueRef, vec),
                                           TO_PTR(LLVMValueRef, idx), name));
}

/* Global strings */
int64_t dm_llvm_build_global_string_ptr(int64_t b, const char* str, const char* name) {
    return TO_INT(LLVMBuildGlobalStringPtr(TO_PTR(LLVMBuilderRef, b), str, name));
}

/* ================================================================
 * Target
 * ================================================================ */

void dm_llvm_initialize_x86(void) {
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmPrinter();
    LLVMInitializeX86AsmParser();
}

void dm_llvm_initialize_aarch64(void) {
    LLVMInitializeAArch64TargetInfo();
    LLVMInitializeAArch64Target();
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeAArch64AsmPrinter();
    LLVMInitializeAArch64AsmParser();
}

void dm_llvm_initialize_all_targets(void) {
    dm_llvm_initialize_x86();
    dm_llvm_initialize_aarch64();
}

/* Add an attribute that is only a name, such as "optnone", to a function */
void dm_llvm_add_function_attr(int64_t func, const char* name) {
    LLVMValueRef f = TO_PTR(LLVMValueRef, func);
    unsigned kind = LLVMGetEnumAttributeKindForName(name, strlen(name));
    LLVMAddAttributeAtIndex(f, LLVMAttributeFunctionIndex,
        LLVMCreateEnumAttribute(LLVMGetModuleContext(LLVMGetGlobalParent(f)), kind, 0));
}

/* Returns a C string owned by LLVM (caller must free with dm_llvm_dispose_message) */
int64_t dm_llvm_get_default_target_triple(void) {
    return TO_INT(LLVMGetDefaultTargetTriple());
}

int64_t dm_llvm_create_target_machine(const char* triple, int64_t opt_level) {
    LLVMTargetRef target = NULL;
    char* err_msg = NULL;
    if (LLVMGetTargetFromTriple(triple, &target, &err_msg) != 0) {
        if (err_msg) {
            fprintf(stderr, "Target error: %s\n", err_msg);
            LLVMDisposeMessage(err_msg);
        }
        return 0;
    }

    LLVMCodeGenOptLevel llvm_opt;
    switch (opt_level) {
        case 0: llvm_opt = LLVMCodeGenLevelNone; break;
        case 1: llvm_opt = LLVMCodeGenLevelLess; break;
        case 2: llvm_opt = LLVMCodeGenLevelDefault; break;
        case 3: llvm_opt = LLVMCodeGenLevelAggressive; break;
        default: llvm_opt = LLVMCodeGenLevelNone; break;
    }

    LLVMTargetMachineRef tm = LLVMCreateTargetMachine(
        target, triple, "generic", "",
        llvm_opt, LLVMRelocPIC, LLVMCodeModelDefault);
    return TO_INT(tm);
}

void dm_llvm_dispose_target_machine(int64_t tm) {
    LLVMDisposeTargetMachine(TO_PTR(LLVMTargetMachineRef, tm));
}

int64_t dm_llvm_target_machine_emit_to_file(int64_t tm, int64_t mod,
                                             const char* filename, int64_t file_type) {
    char* err_msg = NULL;
    LLVMCodeGenFileType ft = (file_type == 0) ? LLVMObjectFile : LLVMAssemblyFile;
    int result = LLVMTargetMachineEmitToFile(
        TO_PTR(LLVMTargetMachineRef, tm),
        TO_PTR(LLVMModuleRef, mod),
        (char*)filename, ft, &err_msg);
    if (result != 0 && err_msg) {
        fprintf(stderr, "Emit error: %s\n", err_msg);
        LLVMDisposeMessage(err_msg);
    }
    return (int64_t)result;
}

int64_t dm_llvm_get_target_data_layout(int64_t tm) {
    LLVMTargetDataRef dl = LLVMCreateTargetDataLayout(TO_PTR(LLVMTargetMachineRef, tm));
    return TO_INT(LLVMCopyStringRepOfTargetData(dl));
}

/* ================================================================
 * Enum Constants (returned as int64_t)
 * ================================================================ */

/* Integer comparison predicates */
int64_t dm_llvm_int_eq(void) { return (int64_t)LLVMIntEQ; }
int64_t dm_llvm_int_ne(void) { return (int64_t)LLVMIntNE; }
int64_t dm_llvm_int_slt(void) { return (int64_t)LLVMIntSLT; }
int64_t dm_llvm_int_sle(void) { return (int64_t)LLVMIntSLE; }
int64_t dm_llvm_int_sgt(void) { return (int64_t)LLVMIntSGT; }
int64_t dm_llvm_int_sge(void) { return (int64_t)LLVMIntSGE; }

/* Float comparison predicates */
int64_t dm_llvm_real_oeq(void) { return (int64_t)LLVMRealOEQ; }
int64_t dm_llvm_real_one(void) { return (int64_t)LLVMRealONE; }
int64_t dm_llvm_real_olt(void) { return (int64_t)LLVMRealOLT; }
int64_t dm_llvm_real_ole(void) { return (int64_t)LLVMRealOLE; }
int64_t dm_llvm_real_ogt(void) { return (int64_t)LLVMRealOGT; }
int64_t dm_llvm_real_oge(void) { return (int64_t)LLVMRealOGE; }

/* Linkage */
int64_t dm_llvm_external_linkage(void) { return (int64_t)LLVMExternalLinkage; }
int64_t dm_llvm_weak_linkage(void) { return (int64_t)LLVMWeakAnyLinkage; }

/* File types */
int64_t dm_llvm_object_file(void) { return 0; }
int64_t dm_llvm_assembly_file(void) { return 1; }

/* ================================================================
 * String helper: convert C string pointer to dAImond dm_string
 * ================================================================ */

/* Returns a strdup'd copy of the C string pointed to by c_str_ptr.
 * Must strdup because dm_string_from_cstr doesn't copy, and the original
 * LLVM string will be freed by dm_llvm_free_c_string. */
const char* dm_llvm_c_str_to_string(int64_t c_str_ptr) {
    const char* s = TO_PTR(const char*, c_str_ptr);
    return s ? strdup(s) : "";
}

void dm_llvm_free_c_string(int64_t c_str_ptr) {
    char* s = TO_PTR(char*, c_str_ptr);
    if (s) {
        LLVMDisposeMessage(s);
    }
}

/* ================================================================
 * Buffer-based API for passing arrays to LLVM functions
 *
 * dAImond has no raw pointer type, so we use a shared buffer:
 *   dm_llvm_buf_clear()
 *   dm_llvm_buf_push(ref1)
 *   dm_llvm_buf_push(ref2)
 *   dm_llvm_struct_set_body_buf(ty, packed)
 * ================================================================ */

#define DM_LLVM_BUF_MAX 256
static int64_t dm_llvm_buf[DM_LLVM_BUF_MAX];
static int dm_llvm_buf_count = 0;

void dm_llvm_buf_clear(void) {
    dm_llvm_buf_count = 0;
}

void dm_llvm_buf_push(int64_t ref) {
    if (dm_llvm_buf_count < DM_LLVM_BUF_MAX) {
        dm_llvm_buf[dm_llvm_buf_count++] = ref;
    }
}

/* Read access to the buffer and to function types, so the generator can
 * check call arguments against parameter types before building a call. */
int64_t dm_llvm_buf_len(void) {
    return dm_llvm_buf_count;
}

int64_t dm_llvm_buf_get(int64_t index) {
    return dm_llvm_buf[index];
}

int64_t dm_llvm_count_param_types(int64_t fn_ty) {
    return (int64_t)LLVMCountParamTypes(TO_PTR(LLVMTypeRef, fn_ty));
}

int64_t dm_llvm_param_type_at(int64_t fn_ty, int64_t index) {
    LLVMTypeRef types[DM_LLVM_BUF_MAX];
    LLVMGetParamTypes(TO_PTR(LLVMTypeRef, fn_ty), types);
    return TO_INT(types[index]);
}

int64_t dm_llvm_is_function_var_arg(int64_t fn_ty) {
    return (int64_t)LLVMIsFunctionVarArg(TO_PTR(LLVMTypeRef, fn_ty));
}

const char* dm_llvm_get_value_name(int64_t val) {
    size_t len = 0;
    const char* name = LLVMGetValueName2(TO_PTR(LLVMValueRef, val), &len);
    return strndup(name ? name : "", len);
}

/* Returns a C string; free it with dm_llvm_free_c_string. */
int64_t dm_llvm_print_type_to_string(int64_t ty) {
    return TO_INT(LLVMPrintTypeToString(TO_PTR(LLVMTypeRef, ty)));
}

const char* dm_llvm_get_struct_name(int64_t ty) {
    const char* name = LLVMGetStructName(TO_PTR(LLVMTypeRef, ty));
    return strdup(name ? name : "");
}

int64_t dm_llvm_struct_type_buf(int64_t ctx, int64_t count, int64_t is_packed) {
    LLVMTypeRef types[DM_LLVM_BUF_MAX];
    for (int i = 0; i < dm_llvm_buf_count && i < count; i++) {
        types[i] = TO_PTR(LLVMTypeRef, dm_llvm_buf[i]);
    }
    int64_t result = TO_INT(LLVMStructTypeInContext(
        TO_PTR(LLVMContextRef, ctx), types, (unsigned)count, (LLVMBool)is_packed));
    dm_llvm_buf_count = 0;
    return result;
}

void dm_llvm_struct_set_body_buf(int64_t struct_ty, int64_t is_packed) {
    LLVMTypeRef types[DM_LLVM_BUF_MAX];
    for (int i = 0; i < dm_llvm_buf_count; i++) {
        types[i] = TO_PTR(LLVMTypeRef, dm_llvm_buf[i]);
    }
    LLVMStructSetBody(TO_PTR(LLVMTypeRef, struct_ty), types, dm_llvm_buf_count, (LLVMBool)is_packed);
    dm_llvm_buf_count = 0;
}

int64_t dm_llvm_function_type_buf(int64_t ret, int64_t is_var_arg) {
    LLVMTypeRef types[DM_LLVM_BUF_MAX];
    for (int i = 0; i < dm_llvm_buf_count; i++) {
        types[i] = TO_PTR(LLVMTypeRef, dm_llvm_buf[i]);
    }
    int64_t result = TO_INT(LLVMFunctionType(
        TO_PTR(LLVMTypeRef, ret), types, dm_llvm_buf_count, (LLVMBool)is_var_arg));
    dm_llvm_buf_count = 0;
    return result;
}

int64_t dm_llvm_build_call2_buf(int64_t b, int64_t fn_ty, int64_t func, const char* name) {
    LLVMValueRef args[DM_LLVM_BUF_MAX];
    for (int i = 0; i < dm_llvm_buf_count; i++) {
        args[i] = TO_PTR(LLVMValueRef, dm_llvm_buf[i]);
    }
    int64_t result = TO_INT(dm_call_zeroext_bools(LLVMBuildCall2(
        TO_PTR(LLVMBuilderRef, b), TO_PTR(LLVMTypeRef, fn_ty),
        TO_PTR(LLVMValueRef, func), args, dm_llvm_buf_count, name)));
    dm_llvm_buf_count = 0;
    return result;
}

void dm_llvm_add_incoming_one(int64_t phi, int64_t val, int64_t block) {
    LLVMValueRef vals[1] = { TO_PTR(LLVMValueRef, val) };
    LLVMBasicBlockRef blocks[1] = { TO_PTR(LLVMBasicBlockRef, block) };
    LLVMAddIncoming(TO_PTR(LLVMValueRef, phi), vals, blocks, 1);
}


/* ================================================================
 * Debug info (DWARF)
 * ================================================================ */

/* A builder of debug info for the module, which is marked as having
 * debug info of the version this LLVM writes, as DWARF 4. */
int64_t dm_llvm_di_builder_create(int64_t mod) {
    LLVMModuleRef m = TO_PTR(LLVMModuleRef, mod);
    LLVMTypeRef i32 = LLVMInt32TypeInContext(LLVMGetModuleContext(m));
    LLVMAddModuleFlag(m, LLVMModuleFlagBehaviorWarning, "Debug Info Version", 18,
                      LLVMValueAsMetadata(LLVMConstInt(i32, LLVMDebugMetadataVersion(), 0)));
    LLVMAddModuleFlag(m, LLVMModuleFlagBehaviorWarning, "Dwarf Version", 13,
                      LLVMValueAsMetadata(LLVMConstInt(i32, 4, 0)));
    return TO_INT(LLVMCreateDIBuilder(m));
}

/* Complete the module's debug info and dispose of the builder. */
void dm_llvm_di_builder_finalize(int64_t dib) {
    LLVMDIBuilderFinalize(TO_PTR(LLVMDIBuilderRef, dib));
    LLVMDisposeDIBuilder(TO_PTR(LLVMDIBuilderRef, dib));
}

int64_t dm_llvm_di_file(int64_t dib, const char* name, const char* directory) {
    return TO_INT(LLVMDIBuilderCreateFile(TO_PTR(LLVMDIBuilderRef, dib),
                                          name, strlen(name), directory, strlen(directory)));
}

/* The compile unit of the program whose main file is `file`. A
 * debugger treats the language as C99, the nearest it knows. */
int64_t dm_llvm_di_compile_unit(int64_t dib, int64_t file, const char* producer, int64_t optimized) {
    return TO_INT(LLVMDIBuilderCreateCompileUnit(
        TO_PTR(LLVMDIBuilderRef, dib), LLVMDWARFSourceLanguageC99,
        TO_PTR(LLVMMetadataRef, file), producer, strlen(producer), (LLVMBool)optimized,
        "", 0, 0, "", 0, LLVMDWARFEmissionFull, 0, 0, 0, "", 0, "", 0));
}

/* A function defined at `line` of `file`. Its type says nothing of
 * its parameters. */
int64_t dm_llvm_di_function(int64_t dib, int64_t file, const char* name, const char* symbol,
                            int64_t line, int64_t optimized) {
    LLVMDIBuilderRef b = TO_PTR(LLVMDIBuilderRef, dib);
    LLVMMetadataRef f = TO_PTR(LLVMMetadataRef, file);
    LLVMMetadataRef ty = LLVMDIBuilderCreateSubroutineType(b, f, NULL, 0, LLVMDIFlagZero);
    return TO_INT(LLVMDIBuilderCreateFunction(b, f, name, strlen(name), symbol, strlen(symbol),
                                              f, (unsigned)line, ty, 0, 1, (unsigned)line,
                                              LLVMDIFlagZero, (LLVMBool)optimized));
}

void dm_llvm_set_subprogram(int64_t func, int64_t subprogram) {
    LLVMSetSubprogram(TO_PTR(LLVMValueRef, func), TO_PTR(LLVMMetadataRef, subprogram));
}

/* The place of the instructions the builder makes from now on: a line
 * and column of the function `scope`, or no place when scope is 0. */
void dm_llvm_set_debug_location(int64_t b, int64_t ctx, int64_t line, int64_t col, int64_t scope) {
    LLVMMetadataRef loc = NULL;
    if (scope != 0) {
        loc = LLVMDIBuilderCreateDebugLocation(TO_PTR(LLVMContextRef, ctx), (unsigned)line,
                                               (unsigned)col, TO_PTR(LLVMMetadataRef, scope), NULL);
    }
    LLVMSetCurrentDebugLocation2(TO_PTR(LLVMBuilderRef, b), loc);
}

int64_t dm_llvm_di_basic_type(int64_t dib, const char* name, int64_t bits, int64_t encoding) {
    return TO_INT(LLVMDIBuilderCreateBasicType(TO_PTR(LLVMDIBuilderRef, dib), name, strlen(name),
                                               (uint64_t)bits, (LLVMDWARFTypeEncoding)encoding,
                                               LLVMDIFlagZero));
}

/* A pointer to `pointee`, or to nothing in particular when it is 0 */
int64_t dm_llvm_di_pointer_type(int64_t dib, int64_t pointee, const char* name) {
    return TO_INT(LLVMDIBuilderCreatePointerType(TO_PTR(LLVMDIBuilderRef, dib),
                                                 TO_PTR(LLVMMetadataRef, pointee), 64, 0, 0,
                                                 name, strlen(name)));
}

/* A field of the struct `scope` */
int64_t dm_llvm_di_member(int64_t dib, int64_t scope, const char* name, int64_t bits,
                          int64_t offset_bits, int64_t ty) {
    return TO_INT(LLVMDIBuilderCreateMemberType(TO_PTR(LLVMDIBuilderRef, dib),
                                                TO_PTR(LLVMMetadataRef, scope), name, strlen(name),
                                                NULL, 0, (uint64_t)bits, 0, (uint64_t)offset_bits,
                                                LLVMDIFlagZero, TO_PTR(LLVMMetadataRef, ty)));
}

/* A struct type whose fields are not known yet: it stands for the
 * struct, in its own fields' types too, until dm_llvm_di_struct_end_buf
 * replaces it. 0x13 is DW_TAG_structure_type. */
int64_t dm_llvm_di_struct_begin(int64_t dib, const char* name, int64_t bits) {
    return TO_INT(LLVMDIBuilderCreateReplaceableCompositeType(
        TO_PTR(LLVMDIBuilderRef, dib), 0x13, name, strlen(name), NULL, NULL, 0, 0,
        (uint64_t)bits, 0, LLVMDIFlagZero, "", 0));
}

/* The struct type whose fields are the listed members, which takes
 * the place of `begun` */
int64_t dm_llvm_di_struct_end_buf(int64_t dib, int64_t begun, const char* name, int64_t bits) {
    LLVMMetadataRef members[DM_LLVM_BUF_MAX];
    for (int i = 0; i < dm_llvm_buf_count; i++) {
        members[i] = TO_PTR(LLVMMetadataRef, dm_llvm_buf[i]);
    }
    LLVMMetadataRef made = LLVMDIBuilderCreateStructType(
        TO_PTR(LLVMDIBuilderRef, dib), NULL, name, strlen(name), NULL, 0, (uint64_t)bits, 0,
        LLVMDIFlagZero, NULL, members, (unsigned)dm_llvm_buf_count, 0, NULL, "", 0);
    dm_llvm_buf_count = 0;
    LLVMMetadataReplaceAllUsesWith(TO_PTR(LLVMMetadataRef, begun), made);
    return TO_INT(made);
}

/* A variable of the function `scope`: its parameter number `arg`,
 * from 1, or a local when arg is 0 */
int64_t dm_llvm_di_variable(int64_t dib, int64_t scope, const char* name, int64_t file,
                            int64_t line, int64_t ty, int64_t arg) {
    LLVMDIBuilderRef b = TO_PTR(LLVMDIBuilderRef, dib);
    if (arg > 0) {
        return TO_INT(LLVMDIBuilderCreateParameterVariable(
            b, TO_PTR(LLVMMetadataRef, scope), name, strlen(name), (unsigned)arg,
            TO_PTR(LLVMMetadataRef, file), (unsigned)line, TO_PTR(LLVMMetadataRef, ty), 1,
            LLVMDIFlagZero));
    }
    return TO_INT(LLVMDIBuilderCreateAutoVariable(
        b, TO_PTR(LLVMMetadataRef, scope), name, strlen(name), TO_PTR(LLVMMetadataRef, file),
        (unsigned)line, TO_PTR(LLVMMetadataRef, ty), 1, LLVMDIFlagZero, 0));
}

/* Say, at the end of the builder's block, that the variable lives in
 * `storage`; with deref, that storage holds the variable's address.
 * 0x06 is DW_OP_deref. */
void dm_llvm_di_declare(int64_t dib, int64_t b, int64_t storage, int64_t variable, int64_t ctx,
                        int64_t line, int64_t scope, int64_t deref) {
    LLVMDIBuilderRef builder = TO_PTR(LLVMDIBuilderRef, dib);
    uint64_t ops[1] = { 0x06 };
    LLVMMetadataRef expr = LLVMDIBuilderCreateExpression(builder, ops, deref ? 1 : 0);
    LLVMMetadataRef loc = LLVMDIBuilderCreateDebugLocation(
        TO_PTR(LLVMContextRef, ctx), (unsigned)line, 0, TO_PTR(LLVMMetadataRef, scope), NULL);
    LLVMDIBuilderInsertDeclareRecordAtEnd(builder, TO_PTR(LLVMValueRef, storage),
                                          TO_PTR(LLVMMetadataRef, variable), expr, loc,
                                          LLVMGetInsertBlock(TO_PTR(LLVMBuilderRef, b)));
}

int64_t dm_llvm_is_alloca(int64_t val) {
    return LLVMIsAAllocaInst(TO_PTR(LLVMValueRef, val)) != NULL;
}

/* The size in bytes of a type in the module's data layout, which must
 * be set */
int64_t dm_llvm_abi_size_of(int64_t mod, int64_t ty) {
    return (int64_t)LLVMABISizeOfType(LLVMGetModuleDataLayout(TO_PTR(LLVMModuleRef, mod)),
                                      TO_PTR(LLVMTypeRef, ty));
}

/* The offset in bytes of a struct type's field */
int64_t dm_llvm_offset_of_element(int64_t mod, int64_t struct_ty, int64_t index) {
    return (int64_t)LLVMOffsetOfElement(LLVMGetModuleDataLayout(TO_PTR(LLVMModuleRef, mod)),
                                        TO_PTR(LLVMTypeRef, struct_ty), (unsigned)index);
}

/* The number of fields of a struct type; 0 for a struct type with no
 * body and for a type that is no struct */
int64_t dm_llvm_struct_field_count(int64_t ty) {
    LLVMTypeRef t = TO_PTR(LLVMTypeRef, ty);
    if (LLVMGetTypeKind(t) != LLVMStructTypeKind || LLVMIsOpaqueStruct(t)) return 0;
    return LLVMCountStructElementTypes(t);
}
