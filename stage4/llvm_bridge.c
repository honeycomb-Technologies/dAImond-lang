/*
 * LLVM-C Bridge for dAImond Stage 4
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
#include <llvm-c/Transforms/PassBuilder.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* Include the dAImond runtime for dm_string interop */
#include "../stage0/runtime/daimond_runtime.h"

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

int64_t dm_llvm_module_get_named_function(int64_t mod, const char* name) {
    LLVMValueRef f = LLVMGetNamedFunction(TO_PTR(LLVMModuleRef, mod), name);
    return TO_INT(f); /* 0 if not found */
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
int64_t dm_llvm_module_print_to_string(int64_t mod) {
    return TO_INT(LLVMPrintModuleToString(TO_PTR(LLVMModuleRef, mod)));
}

int64_t dm_llvm_module_print_to_file(int64_t mod, const char* filename) {
    char* err_msg = NULL;
    int result = LLVMPrintModuleToFile(TO_PTR(LLVMModuleRef, mod), filename, &err_msg);
    if (result != 0 && err_msg) {
        LLVMDisposeMessage(err_msg);
    }
    return (int64_t)result;
}

int64_t dm_llvm_module_write_bitcode(int64_t mod, const char* filename) {
    return (int64_t)LLVMWriteBitcodeToFile(TO_PTR(LLVMModuleRef, mod), filename);
}

void dm_llvm_dispose_message(int64_t msg) {
    if (msg != 0) {
        LLVMDisposeMessage(TO_PTR(char*, msg));
    }
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
int64_t dm_llvm_struct_type(int64_t ctx, int64_t* fields, int64_t field_count, int64_t is_packed) {
    return TO_INT(LLVMStructTypeInContext(
        TO_PTR(LLVMContextRef, ctx),
        (LLVMTypeRef*)(uintptr_t)fields,
        (unsigned)field_count,
        is_packed ? 1 : 0));
}

int64_t dm_llvm_struct_create_named(int64_t ctx, const char* name) {
    return TO_INT(LLVMStructCreateNamed(TO_PTR(LLVMContextRef, ctx), name));
}

void dm_llvm_struct_set_body(int64_t struct_ty, int64_t* fields, int64_t field_count, int64_t is_packed) {
    LLVMStructSetBody(TO_PTR(LLVMTypeRef, struct_ty),
                       (LLVMTypeRef*)(uintptr_t)fields,
                       (unsigned)field_count,
                       is_packed ? 1 : 0);
}

int64_t dm_llvm_struct_get_type_at_index(int64_t struct_ty, int64_t index) {
    return TO_INT(LLVMStructGetTypeAtIndex(TO_PTR(LLVMTypeRef, struct_ty), (unsigned)index));
}

int64_t dm_llvm_get_type_kind(int64_t ty) {
    return (int64_t)LLVMGetTypeKind(TO_PTR(LLVMTypeRef, ty));
}

int64_t dm_llvm_get_int_type_width(int64_t ty) {
    return (int64_t)LLVMGetIntTypeWidth(TO_PTR(LLVMTypeRef, ty));
}

int64_t dm_llvm_function_type(int64_t ret, int64_t* params, int64_t param_count, int64_t is_var_arg) {
    return TO_INT(LLVMFunctionType(
        TO_PTR(LLVMTypeRef, ret),
        (LLVMTypeRef*)(uintptr_t)params,
        (unsigned)param_count,
        is_var_arg ? 1 : 0));
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

int64_t dm_llvm_const_string(int64_t ctx, const char* str, int64_t len, int64_t null_terminate) {
    return TO_INT(LLVMConstStringInContext(
        TO_PTR(LLVMContextRef, ctx), str, (unsigned)len,
        null_terminate ? 0 : 1));
}

int64_t dm_llvm_const_struct(int64_t ctx, int64_t* values, int64_t count, int64_t is_packed) {
    return TO_INT(LLVMConstStructInContext(
        TO_PTR(LLVMContextRef, ctx),
        (LLVMValueRef*)(uintptr_t)values,
        (unsigned)count,
        is_packed ? 1 : 0));
}

int64_t dm_llvm_const_array(int64_t elem_ty, int64_t* values, int64_t count) {
    return TO_INT(LLVMConstArray2(
        TO_PTR(LLVMTypeRef, elem_ty),
        (LLVMValueRef*)(uintptr_t)values,
        (uint64_t)count));
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
int64_t dm_llvm_build_alloca(int64_t b, int64_t ty, const char* name) {
    return TO_INT(LLVMBuildAlloca(TO_PTR(LLVMBuilderRef, b),
                                   TO_PTR(LLVMTypeRef, ty), name));
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

int64_t dm_llvm_build_gep2(int64_t b, int64_t ty, int64_t ptr,
                            int64_t* indices, int64_t num_indices, const char* name) {
    return TO_INT(LLVMBuildGEP2(TO_PTR(LLVMBuilderRef, b),
                                 TO_PTR(LLVMTypeRef, ty),
                                 TO_PTR(LLVMValueRef, ptr),
                                 (LLVMValueRef*)(uintptr_t)indices,
                                 (unsigned)num_indices, name));
}

int64_t dm_llvm_build_struct_gep2(int64_t b, int64_t ty, int64_t ptr, int64_t idx, const char* name) {
    return TO_INT(LLVMBuildStructGEP2(TO_PTR(LLVMBuilderRef, b),
                                       TO_PTR(LLVMTypeRef, ty),
                                       TO_PTR(LLVMValueRef, ptr),
                                       (unsigned)idx, name));
}

int64_t dm_llvm_build_extract_value(int64_t b, int64_t agg, int64_t idx, const char* name) {
    return TO_INT(LLVMBuildExtractValue(TO_PTR(LLVMBuilderRef, b),
                                         TO_PTR(LLVMValueRef, agg),
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
int64_t dm_llvm_build_call2(int64_t b, int64_t fn_ty, int64_t func,
                             int64_t* args, int64_t arg_count, const char* name) {
    return TO_INT(LLVMBuildCall2(TO_PTR(LLVMBuilderRef, b),
                                  TO_PTR(LLVMTypeRef, fn_ty),
                                  TO_PTR(LLVMValueRef, func),
                                  (LLVMValueRef*)(uintptr_t)args,
                                  (unsigned)arg_count, name));
}

/* Phi */
int64_t dm_llvm_build_phi(int64_t b, int64_t ty, const char* name) {
    return TO_INT(LLVMBuildPhi(TO_PTR(LLVMBuilderRef, b),
                                TO_PTR(LLVMTypeRef, ty), name));
}

void dm_llvm_add_incoming(int64_t phi, int64_t* values, int64_t* blocks, int64_t count) {
    LLVMAddIncoming(TO_PTR(LLVMValueRef, phi),
                     (LLVMValueRef*)(uintptr_t)values,
                     (LLVMBasicBlockRef*)(uintptr_t)blocks,
                     (unsigned)count);
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

int64_t dm_llvm_build_bit_cast(int64_t b, int64_t val, int64_t dest_ty, const char* name) {
    return TO_INT(LLVMBuildBitCast(TO_PTR(LLVMBuilderRef, b),
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

int64_t dm_llvm_build_shuffle_vector(int64_t b, int64_t v1, int64_t v2, int64_t mask, const char* name) {
    return TO_INT(LLVMBuildShuffleVector(TO_PTR(LLVMBuilderRef, b),
                                          TO_PTR(LLVMValueRef, v1),
                                          TO_PTR(LLVMValueRef, v2),
                                          TO_PTR(LLVMValueRef, mask), name));
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
int64_t dm_llvm_internal_linkage(void) { return (int64_t)LLVMInternalLinkage; }
int64_t dm_llvm_private_linkage(void) { return (int64_t)LLVMPrivateLinkage; }

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
    int64_t result = TO_INT(LLVMBuildCall2(
        TO_PTR(LLVMBuilderRef, b), TO_PTR(LLVMTypeRef, fn_ty),
        TO_PTR(LLVMValueRef, func), args, dm_llvm_buf_count, name));
    dm_llvm_buf_count = 0;
    return result;
}

int64_t dm_llvm_const_struct_buf(int64_t ctx, int64_t is_packed) {
    LLVMValueRef vals[DM_LLVM_BUF_MAX];
    for (int i = 0; i < dm_llvm_buf_count; i++) {
        vals[i] = TO_PTR(LLVMValueRef, dm_llvm_buf[i]);
    }
    int64_t result = TO_INT(LLVMConstStructInContext(
        TO_PTR(LLVMContextRef, ctx), vals, dm_llvm_buf_count, (LLVMBool)is_packed));
    dm_llvm_buf_count = 0;
    return result;
}

int64_t dm_llvm_const_array_buf(int64_t elem_ty) {
    LLVMValueRef vals[DM_LLVM_BUF_MAX];
    for (int i = 0; i < dm_llvm_buf_count; i++) {
        vals[i] = TO_PTR(LLVMValueRef, dm_llvm_buf[i]);
    }
    int64_t result = TO_INT(LLVMConstArray2(
        TO_PTR(LLVMTypeRef, elem_ty), vals, dm_llvm_buf_count));
    dm_llvm_buf_count = 0;
    return result;
}

void dm_llvm_add_incoming_one(int64_t phi, int64_t val, int64_t block) {
    LLVMValueRef vals[1] = { TO_PTR(LLVMValueRef, val) };
    LLVMBasicBlockRef blocks[1] = { TO_PTR(LLVMBasicBlockRef, block) };
    LLVMAddIncoming(TO_PTR(LLVMValueRef, phi), vals, blocks, 1);
}
