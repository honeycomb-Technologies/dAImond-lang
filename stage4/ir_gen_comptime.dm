module ir_gen_comptime

import ast
import ir
import ir_builder
import ir_gen

-- ============================================================
-- Compile-Time Expression Evaluation
-- Evaluates expressions at compile time, producing IRValues
-- directly without emitting IR instructions.
-- Port of stage3/src/ir_gen.zig comptime evaluation logic.
-- ============================================================

-- Maximum recursion depth for comptime function calls
fn COMPTIME_MAX_DEPTH() -> int { return 64 }

-- ============================================================
-- MAIN COMPTIME DISPATCHER
-- ============================================================

-- Evaluate an expression at compile time, returning an IRValue.
-- Returns ir_val_int(0) for unsupported expression kinds.
fn eval_comptime_expr(gen: IRGenerator, e: Expr) -> IRValue {
    -- Integer literal
    if e.kind == EXPR_LITERAL_INT() {
        return ir_val_int(e.int_val)
    }

    -- Float literal
    if e.kind == EXPR_LITERAL_FLOAT() {
        return ir_val_float(e.float_val)
    }

    -- Bool literal
    if e.kind == EXPR_LITERAL_BOOL() {
        return ir_val_bool(e.bool_val)
    }

    -- String literal
    if e.kind == EXPR_LITERAL_STRING() {
        return ir_val_string(e.str_val)
    }

    -- Binary expression
    if e.kind == EXPR_BINARY() {
        return eval_binary_comptime(gen, e)
    }

    -- Unary expression
    if e.kind == EXPR_UNARY() {
        return eval_unary_comptime(gen, e)
    }

    -- Identifier: look up in module globals
    if e.kind == EXPR_IDENTIFIER() {
        return eval_identifier_comptime(gen, e.name)
    }

    -- If expression
    if e.kind == EXPR_IF() {
        return eval_if_comptime(gen, e)
    }

    -- Block expression
    if e.kind == EXPR_BLOCK() {
        return eval_block_comptime(gen, e)
    }

    -- Function call
    if e.kind == EXPR_FUNCTION_CALL() {
        return eval_call_comptime(gen, e, 0)
    }

    -- Match expression
    if e.kind == EXPR_MATCH() {
        return eval_match_comptime(gen, e)
    }

    -- Grouped expression (parenthesized)
    if e.kind == EXPR_GROUPED() {
        let inner = *e.operand
        return eval_comptime_expr(gen, inner)
    }

    -- Comptime expression wrapper (e.g., `comptime expr`)
    if e.kind == EXPR_COMPTIME() {
        let inner = *e.operand
        return eval_comptime_expr(gen, inner)
    }

    -- Unsupported expression kind
    return ir_val_int(0)
}

-- ============================================================
-- IDENTIFIER LOOKUP
-- ============================================================

-- Look up an identifier in the module's global constants.
fn eval_identifier_comptime(gen: IRGenerator, name: string) -> IRValue {
    let mut i = 0
    while i < gen.ir_mod.globals.len() {
        let gl = gen.ir_mod.globals[i]
        if gl.name == name and gl.has_init {
            return gl.init_value
        }
        i = i + 1
    }
    return ir_val_int(0)
}

-- ============================================================
-- BINARY EXPRESSION EVALUATION
-- ============================================================

-- Evaluate a binary expression at compile time.
-- Supports integer arithmetic, float arithmetic, boolean logic,
-- comparison operators, string concatenation, and string comparison.
fn eval_binary_comptime(gen: IRGenerator, e: Expr) -> IRValue {
    let lhs = eval_comptime_expr(gen, *e.left)
    let rhs = eval_comptime_expr(gen, *e.right)
    let op = e.op

    -- Integer arithmetic
    if lhs.kind == VAL_CONST_INT() and rhs.kind == VAL_CONST_INT() {
        let a = lhs.int_val
        let b = rhs.int_val

        if op == BINOP_ADD() { return ir_val_int(a + b) }
        if op == BINOP_SUB() { return ir_val_int(a - b) }
        if op == BINOP_MUL() { return ir_val_int(a * b) }
        if op == BINOP_DIV() {
            if b != 0 {
                return ir_val_int(a / b)
            }
            return ir_val_int(0)
        }
        if op == BINOP_MOD() {
            if b != 0 {
                return ir_val_int(a % b)
            }
            return ir_val_int(0)
        }

        -- Integer comparisons
        if op == BINOP_EQ() { return ir_val_bool(a == b) }
        if op == BINOP_NE() { return ir_val_bool(a != b) }
        if op == BINOP_LT() { return ir_val_bool(a < b) }
        if op == BINOP_LE() { return ir_val_bool(a <= b) }
        if op == BINOP_GT() { return ir_val_bool(a > b) }
        if op == BINOP_GE() { return ir_val_bool(a >= b) }
    }

    -- Float arithmetic
    if lhs.kind == VAL_CONST_FLOAT() and rhs.kind == VAL_CONST_FLOAT() {
        let a = lhs.float_val
        let b = rhs.float_val

        if op == BINOP_ADD() { return ir_val_float(a + b) }
        if op == BINOP_SUB() { return ir_val_float(a - b) }
        if op == BINOP_MUL() { return ir_val_float(a * b) }
        if op == BINOP_DIV() {
            return ir_val_float(a / b)
        }

        -- Float comparisons
        if op == BINOP_EQ() { return ir_val_bool(a == b) }
        if op == BINOP_NE() { return ir_val_bool(a != b) }
        if op == BINOP_LT() { return ir_val_bool(a < b) }
        if op == BINOP_LE() { return ir_val_bool(a <= b) }
        if op == BINOP_GT() { return ir_val_bool(a > b) }
        if op == BINOP_GE() { return ir_val_bool(a >= b) }
    }

    -- Mixed int/float: promote int to float
    if lhs.kind == VAL_CONST_INT() and rhs.kind == VAL_CONST_FLOAT() {
        let a = lhs.int_val
        let b = rhs.float_val
        if op == BINOP_ADD() { return ir_val_float(a + b) }
        if op == BINOP_SUB() { return ir_val_float(a - b) }
        if op == BINOP_MUL() { return ir_val_float(a * b) }
    }

    if lhs.kind == VAL_CONST_FLOAT() and rhs.kind == VAL_CONST_INT() {
        let a = lhs.float_val
        let b = rhs.int_val
        if op == BINOP_ADD() { return ir_val_float(a + b) }
        if op == BINOP_SUB() { return ir_val_float(a - b) }
        if op == BINOP_MUL() { return ir_val_float(a * b) }
    }

    -- Boolean logic
    if lhs.kind == VAL_CONST_BOOL() and rhs.kind == VAL_CONST_BOOL() {
        let a = lhs.bool_val
        let b = rhs.bool_val

        if op == BINOP_AND() { return ir_val_bool(a and b) }
        if op == BINOP_OR() { return ir_val_bool(a or b) }
        if op == BINOP_EQ() { return ir_val_bool(a == b) }
        if op == BINOP_NE() { return ir_val_bool(a != b) }
    }

    -- String concatenation
    if lhs.kind == VAL_CONST_STRING() and rhs.kind == VAL_CONST_STRING() {
        let a = lhs.str_val
        let b = rhs.str_val

        if op == BINOP_ADD() { return ir_val_string(a + b) }
        if op == BINOP_EQ() { return ir_val_bool(a == b) }
        if op == BINOP_NE() { return ir_val_bool(a != b) }
    }

    return ir_val_int(0)
}

-- ============================================================
-- UNARY EXPRESSION EVALUATION
-- ============================================================

-- Evaluate a unary expression at compile time.
-- Supports negation of int/float and logical not of bool.
fn eval_unary_comptime(gen: IRGenerator, e: Expr) -> IRValue {
    let operand_val = eval_comptime_expr(gen, *e.operand)

    if e.op == UNOP_NEG() {
        if operand_val.kind == VAL_CONST_INT() {
            return ir_val_int(0 - operand_val.int_val)
        }
        if operand_val.kind == VAL_CONST_FLOAT() {
            return ir_val_float(0.0 - operand_val.float_val)
        }
    }

    if e.op == UNOP_NOT() {
        if operand_val.kind == VAL_CONST_BOOL() {
            if operand_val.bool_val {
                return ir_val_bool(false)
            } else {
                return ir_val_bool(true)
            }
        }
    }

    return ir_val_int(0)
}

-- ============================================================
-- IF EXPRESSION EVALUATION
-- ============================================================

-- Evaluate an if expression at compile time.
-- The condition must evaluate to a boolean. The appropriate branch
-- is then evaluated and its value returned.
fn eval_if_comptime(gen: IRGenerator, e: Expr) -> IRValue {
    let cond = eval_comptime_expr(gen, *e.condition)

    -- Condition must be a boolean
    if cond.kind == VAL_CONST_BOOL() {
        if cond.bool_val {
            return eval_comptime_expr(gen, *e.then_branch)
        } else if e.has_else {
            return eval_comptime_expr(gen, *e.else_branch)
        }
    }

    -- If condition is a truthy int (non-zero)
    if cond.kind == VAL_CONST_INT() {
        if cond.int_val != 0 {
            return eval_comptime_expr(gen, *e.then_branch)
        } else if e.has_else {
            return eval_comptime_expr(gen, *e.else_branch)
        }
    }

    return ir_val_int(0)
}

-- ============================================================
-- BLOCK EXPRESSION EVALUATION
-- ============================================================

-- Evaluate a block expression at compile time.
-- This is simplified: we skip statement evaluation and only
-- evaluate the result expression if present.
-- For full comptime block evaluation, local variable tracking
-- would need to be implemented.
fn eval_block_comptime(gen: IRGenerator, e: Expr) -> IRValue {
    -- If the block has a result expression, evaluate it
    if e.has_result {
        return eval_comptime_expr(gen, *e.result)
    }

    return ir_val_int(0)
}

-- ============================================================
-- FUNCTION CALL EVALUATION
-- ============================================================

-- Evaluate a function call at compile time.
-- Looks up the function in the generator's declaration list,
-- substitutes parameters, and evaluates the body.
-- Supports recursion up to COMPTIME_MAX_DEPTH.
fn eval_call_comptime(gen: IRGenerator, e: Expr, depth: int) -> IRValue {
    -- Prevent infinite recursion
    if depth >= COMPTIME_MAX_DEPTH() {
        return ir_val_int(0)
    }

    -- Get the function name from the callee expression
    let callee = *e.callee
    if callee.kind != EXPR_IDENTIFIER() {
        return ir_val_int(0)
    }
    let fn_name = callee.name

    -- Evaluate all argument expressions
    let mut arg_values: List[IRValue] = []
    let mut i = 0
    while i < e.args.len() {
        let val = eval_comptime_expr(gen, e.args[i])
        arg_values.push(val)
        i = i + 1
    }

    -- Look up the function declaration in source
    let mut found_idx = -1
    i = 0
    while i < gen.declarations.len() {
        let decl = gen.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.name == fn_name {
                found_idx = i
            }
        }
        i = i + 1
    }

    if found_idx < 0 {
        return ir_val_int(0)
    }

    let decl = gen.declarations[found_idx]
    let fd = *decl.func_decl

    -- Simple case: function with a single return statement
    -- For more complex functions, we would need a full comptime interpreter
    if fd.body.len() == 1 {
        let stmt = fd.body[0]
        if stmt.kind == STMT_RETURN() and stmt.has_ret_value {
            -- Create a modified generator with parameter values as globals
            let mut g = gen
            i = 0
            while i < fd.params.len() and i < arg_values.len() {
                let param_name = fd.params[i].name
                let mut global = ir_global_new(param_name, "i64")
                global.init_value = arg_values[i]
                global.has_init = true
                global.is_const = true
                g.ir_mod.globals.push(global)
                i = i + 1
            }
            let result = eval_comptime_expr(g, *stmt.ret_value)
            return result
        }
    }

    -- Multi-statement function: evaluate sequentially
    -- Track local comptime variables via temporary globals
    let mut g = gen

    -- Bind parameters as globals
    i = 0
    while i < fd.params.len() and i < arg_values.len() {
        let param_name = fd.params[i].name
        let mut global = ir_global_new(param_name, "i64")
        global.init_value = arg_values[i]
        global.has_init = true
        global.is_const = true
        g.ir_mod.globals.push(global)
        i = i + 1
    }

    -- Evaluate body statements looking for return
    i = 0
    while i < fd.body.len() {
        let stmt = fd.body[i]

        -- Handle return statements
        if stmt.kind == STMT_RETURN() and stmt.has_ret_value {
            return eval_comptime_expr(g, *stmt.ret_value)
        }

        -- Handle let bindings (add to globals for subsequent expressions)
        if stmt.kind == STMT_LET() {
            let val_expr = *stmt.let_value
            if val_expr.kind != EXPR_LITERAL_NULL() {
                let val = eval_comptime_expr(g, val_expr)
                let mut global = ir_global_new(stmt.let_name, "i64")
                global.init_value = val
                global.has_init = true
                global.is_const = true
                g.ir_mod.globals.push(global)
            }
        }

        -- Handle if statements with returns
        if stmt.kind == STMT_IF() {
            let cond = eval_comptime_expr(g, *stmt.if_cond)
            if cond.kind == VAL_CONST_BOOL() {
                if cond.bool_val {
                    let branch_val = eval_comptime_stmts(g, stmt.if_then, depth)
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                } else if stmt.has_else_branch {
                    let branch_val = eval_comptime_stmts(g, stmt.if_else, depth)
                    if branch_val.kind != VAL_UNDEF() {
                        return branch_val
                    }
                }
            }
        }

        i = i + 1
    }

    return ir_val_int(0)
}

-- ============================================================
-- STATEMENT LIST EVALUATION (for comptime if branches)
-- ============================================================

-- Evaluate a list of statements at compile time, looking for a return.
-- Returns ir_val_undef() if no return is found.
fn eval_comptime_stmts(gen: IRGenerator, stmts: List[Stmt], depth: int) -> IRValue {
    let mut g = gen
    let mut i = 0
    while i < stmts.len() {
        let stmt = stmts[i]

        if stmt.kind == STMT_RETURN() and stmt.has_ret_value {
            return eval_comptime_expr(g, *stmt.ret_value)
        }

        if stmt.kind == STMT_LET() {
            let val_expr = *stmt.let_value
            if val_expr.kind != EXPR_LITERAL_NULL() {
                let val = eval_comptime_expr(g, val_expr)
                let mut global = ir_global_new(stmt.let_name, "i64")
                global.init_value = val
                global.has_init = true
                global.is_const = true
                g.ir_mod.globals.push(global)
            }
        }

        i = i + 1
    }
    return ir_val_undef()
}

-- ============================================================
-- MATCH EXPRESSION EVALUATION
-- ============================================================

-- Evaluate a match expression at compile time.
-- Evaluates the scrutinee, then checks each arm's pattern.
-- For literal patterns, compares values. For wildcard/identifier,
-- always matches. Returns the body expression of the first match.
fn eval_match_comptime(gen: IRGenerator, e: Expr) -> IRValue {
    let scrutinee_val = eval_comptime_expr(gen, *e.scrutinee)

    let mut i = 0
    while i < e.match_arms.len() {
        let arm = e.match_arms[i]
        let pat = arm.pattern

        -- Check if pattern matches
        let matches = comptime_pattern_matches(scrutinee_val, pat)

        if matches {
            -- If guard present, evaluate it
            if arm.has_guard {
                let guard_val = eval_comptime_expr(gen, *arm.guard)
                if guard_val.kind == VAL_CONST_BOOL() and guard_val.bool_val == false {
                    -- Guard failed, try next arm
                    i = i + 1
                    continue
                }
            }

            -- Evaluate the arm body
            if arm.is_expr_body {
                return eval_comptime_expr(gen, *arm.body_expr)
            }

            -- Statement body: look for return
            let body_val = eval_comptime_stmts(gen, arm.body, 0)
            if body_val.kind != VAL_UNDEF() {
                return body_val
            }
            return ir_val_int(0)
        }

        i = i + 1
    }

    return ir_val_int(0)
}

-- ============================================================
-- PATTERN MATCHING FOR COMPTIME
-- ============================================================

-- Check if a comptime value matches a pattern.
fn comptime_pattern_matches(val: IRValue, pat: Pattern) -> bool {
    -- Wildcard always matches
    if pat.kind == PAT_WILDCARD() {
        return true
    }

    -- Identifier pattern always matches (acts as binding)
    if pat.kind == PAT_IDENTIFIER() {
        return true
    }

    -- Literal pattern: compare values
    if pat.kind == PAT_LITERAL() {
        let lit_expr = *pat.literal_expr
        let lit_val = comptime_literal_to_value(lit_expr)
        return comptime_values_equal(val, lit_val)
    }

    -- Other patterns (enum, struct) not supported at comptime
    return false
}

-- Convert a literal expression to an IRValue for pattern comparison.
fn comptime_literal_to_value(e: Expr) -> IRValue {
    if e.kind == EXPR_LITERAL_INT() {
        return ir_val_int(e.int_val)
    }
    if e.kind == EXPR_LITERAL_FLOAT() {
        return ir_val_float(e.float_val)
    }
    if e.kind == EXPR_LITERAL_BOOL() {
        return ir_val_bool(e.bool_val)
    }
    if e.kind == EXPR_LITERAL_STRING() {
        return ir_val_string(e.str_val)
    }
    return ir_val_int(0)
}

-- ============================================================
-- VALUE COMPARISON
-- ============================================================

-- Compare two IRValues for equality at compile time.
-- Returns true if both values are of the same kind and equal.
fn comptime_values_equal(a: IRValue, b: IRValue) -> bool {
    -- Different kinds are never equal
    if a.kind != b.kind {
        return false
    }

    if a.kind == VAL_CONST_INT() {
        return a.int_val == b.int_val
    }

    if a.kind == VAL_CONST_FLOAT() {
        return a.float_val == b.float_val
    }

    if a.kind == VAL_CONST_BOOL() {
        return a.bool_val == b.bool_val
    }

    if a.kind == VAL_CONST_STRING() {
        return a.str_val == b.str_val
    }

    -- Instruction refs: compare by ID
    if a.kind == VAL_INST_REF() {
        return a.ref_id == b.ref_id
    }

    return false
}

-- ============================================================
-- COMPTIME RESULT TYPE INFERENCE
-- ============================================================

-- Infer the IR type string for a comptime-evaluated value.
fn comptime_value_type(val: IRValue) -> string {
    if val.kind == VAL_CONST_INT() { return "i64" }
    if val.kind == VAL_CONST_FLOAT() { return "f64" }
    if val.kind == VAL_CONST_BOOL() { return "bool" }
    if val.kind == VAL_CONST_STRING() { return "string" }
    return "i64"
}
