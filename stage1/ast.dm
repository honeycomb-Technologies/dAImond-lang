module ast

-- ============================================================
-- AST Node Kind Constants
-- ============================================================

-- Expression kinds
fn EXPR_INT_LIT() -> int { return 1 }
fn EXPR_FLOAT_LIT() -> int { return 2 }
fn EXPR_BOOL_LIT() -> int { return 3 }
fn EXPR_STRING_LIT() -> int { return 4 }
fn EXPR_IDENT() -> int { return 5 }
fn EXPR_BINARY() -> int { return 6 }
fn EXPR_UNARY() -> int { return 7 }
fn EXPR_CALL() -> int { return 8 }
fn EXPR_FIELD_ACCESS() -> int { return 9 }
fn EXPR_INDEX() -> int { return 10 }
fn EXPR_IF() -> int { return 11 }
fn EXPR_BLOCK() -> int { return 12 }
fn EXPR_LIST_LIT() -> int { return 13 }
fn EXPR_STRUCT_LIT() -> int { return 14 }
fn EXPR_METHOD_CALL() -> int { return 15 }
fn EXPR_ENUM_VARIANT() -> int { return 16 }
fn EXPR_MATCH() -> int { return 17 }
fn EXPR_LAMBDA() -> int { return 18 }

-- Statement kinds
fn STMT_LET() -> int { return 30 }
fn STMT_RETURN() -> int { return 31 }
fn STMT_EXPR() -> int { return 32 }
fn STMT_ASSIGN() -> int { return 33 }
fn STMT_IF() -> int { return 34 }
fn STMT_WHILE() -> int { return 35 }
fn STMT_FOR() -> int { return 36 }
fn STMT_BREAK() -> int { return 37 }
fn STMT_CONTINUE() -> int { return 38 }
fn STMT_MATCH() -> int { return 39 }
fn STMT_LOOP() -> int { return 40 }

-- Declaration kinds
fn DECL_FUNCTION() -> int { return 50 }
fn DECL_STRUCT() -> int { return 51 }
fn DECL_ENUM() -> int { return 52 }
fn DECL_IMPL() -> int { return 53 }
fn DECL_TRAIT() -> int { return 54 }
fn DECL_IMPORT() -> int { return 55 }
fn DECL_CONST() -> int { return 56 }

-- Type expression kinds
fn TYPE_NAMED() -> int { return 70 }
fn TYPE_GENERIC() -> int { return 71 }
fn TYPE_FUNC() -> int { return 72 }
fn TYPE_REF() -> int { return 73 }

-- Pattern kinds
fn PAT_WILDCARD() -> int { return 0 }
fn PAT_IDENT() -> int { return 1 }
fn PAT_LITERAL() -> int { return 2 }
fn PAT_VARIANT() -> int { return 3 }
fn PAT_STRUCT() -> int { return 4 }

-- ============================================================
-- Core AST Nodes
-- ============================================================

-- Expression node (flat struct, kind determines which fields are used)
struct Expr {
    kind: int,
    int_val: int,
    float_val: float,
    bool_val: bool,
    str_val: string,
    name: string,
    op: string,
    left: Box[Expr],
    right: Box[Expr],
    args: List[Expr],
    object: Box[Expr],
    field: string,
    condition: Box[Expr],
    then_branch: List[Stmt],
    else_branch: List[Stmt],
    stmts: List[Stmt],
    field_names: List[string],
    field_values: List[Expr],
    type_name: string,
    match_arms: List[MatchArm],
    param_names: List[string],
    generic_args: List[TypeExpr],
    line: int,
    col: int
}

struct Stmt {
    kind: int,
    name: string,
    is_mutable: bool,
    type_ann: TypeExpr,
    has_type_ann: bool,
    value: Expr,
    has_value: bool,
    return_val: Expr,
    expr: Expr,
    target: Expr,
    assign_val: Expr,
    condition: Expr,
    then_body: List[Stmt],
    else_body: List[Stmt],
    has_else: bool,
    while_cond: Expr,
    loop_body: List[Stmt],
    for_var: string,
    for_iter: Expr,
    for_body: List[Stmt],
    match_expr: Expr,
    match_arms: List[MatchArm],
    line: int,
    col: int
}

struct MatchArm {
    pattern: Pattern,
    body: List[Stmt],
    is_expr: bool,
    expr: Expr,
    line: int,
    col: int
}

struct Pattern {
    kind: int,
    name: string,
    int_val: int,
    str_val: string,
    bool_val: bool,
    variant_type: string,
    variant_name: string,
    fields: List[string],
    sub_patterns: List[Pattern],
    line: int,
    col: int
}

struct TypeExpr {
    kind: int,
    name: string,
    generic_args: List[TypeExpr],
    is_mutable: bool
}

struct Param {
    name: string,
    type_ann: TypeExpr,
    line: int,
    col: int
}

struct FieldDef {
    name: string,
    type_ann: TypeExpr,
    line: int,
    col: int
}

struct VariantDef {
    name: string,
    has_payload: bool,
    payload_type: TypeExpr,
    fields: List[FieldDef],
    line: int,
    col: int
}

struct Decl {
    kind: int,
    name: string,
    params: List[Param],
    return_type: TypeExpr,
    has_return_type: bool,
    body: List[Stmt],
    generic_params: List[string],
    is_method: bool,
    has_self: bool,
    self_is_mut: bool,
    fields: List[FieldDef],
    variants: List[VariantDef],
    impl_type: string,
    methods: List[Decl],
    trait_name: string,
    trait_methods: List[Decl],
    import_path: string,
    import_names: List[string],
    line: int,
    col: int
}

struct SourceFile {
    module_name: string,
    declarations: List[Decl]
}

-- ============================================================
-- Constructor helpers
-- ============================================================

fn empty_type_expr() -> TypeExpr {
    let ga: List[TypeExpr] = []
    return TypeExpr { kind: 0, name: "", generic_args: ga, is_mutable: false }
}

fn named_type(name: string) -> TypeExpr {
    let ga: List[TypeExpr] = []
    return TypeExpr { kind: TYPE_NAMED(), name: name, generic_args: ga, is_mutable: false }
}

fn generic_type(name: string, args: List[TypeExpr]) -> TypeExpr {
    return TypeExpr { kind: TYPE_GENERIC(), name: name, generic_args: args, is_mutable: false }
}

fn null_expr() -> Expr {
    let ea: List[Expr] = []
    let sa: List[Stmt] = []
    let na: List[string] = []
    let ma: List[MatchArm] = []
    let ta: List[TypeExpr] = []
    return Expr {
        kind: 0, int_val: 0, float_val: 0.0, bool_val: false,
        str_val: "", name: "", op: "",
        left: Box_null(), right: Box_null(),
        args: ea, object: Box_null(), field: "",
        condition: Box_null(),
        then_branch: sa, else_branch: sa,
        stmts: sa, field_names: na, field_values: ea,
        type_name: "", match_arms: ma, param_names: na,
        generic_args: ta, line: 0, col: 0
    }
}

fn null_stmt() -> Stmt {
    let sa: List[Stmt] = []
    let ma: List[MatchArm] = []
    let ne = null_expr()
    return Stmt {
        kind: 0, name: "", is_mutable: false,
        type_ann: empty_type_expr(), has_type_ann: false,
        value: ne, has_value: false,
        return_val: ne, expr: ne,
        target: ne, assign_val: ne,
        condition: ne, then_body: sa, else_body: sa, has_else: false,
        while_cond: ne, loop_body: sa,
        for_var: "", for_iter: ne, for_body: sa,
        match_expr: ne, match_arms: ma,
        line: 0, col: 0
    }
}

fn null_pattern() -> Pattern {
    let fs: List[string] = []
    let sp: List[Pattern] = []
    return Pattern {
        kind: PAT_WILDCARD(), name: "", int_val: 0, str_val: "",
        bool_val: false, variant_type: "", variant_name: "",
        fields: fs, sub_patterns: sp, line: 0, col: 0
    }
}

fn null_match_arm() -> MatchArm {
    let sa: List[Stmt] = []
    return MatchArm {
        pattern: null_pattern(), body: sa,
        is_expr: false, expr: null_expr(),
        line: 0, col: 0
    }
}

fn null_decl() -> Decl {
    let pa: List[Param] = []
    let sa: List[Stmt] = []
    let ga: List[string] = []
    let fa: List[FieldDef] = []
    let va: List[VariantDef] = []
    let da: List[Decl] = []
    let na: List[string] = []
    return Decl {
        kind: 0, name: "",
        params: pa, return_type: empty_type_expr(), has_return_type: false,
        body: sa, generic_params: ga,
        is_method: false, has_self: false, self_is_mut: false,
        fields: fa, variants: va,
        impl_type: "", methods: da,
        trait_name: "", trait_methods: da,
        import_path: "", import_names: na,
        line: 0, col: 0
    }
}

-- Expression constructors
fn make_int_lit(val: int, line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_INT_LIT()
    e.int_val = val
    e.line = line
    e.col = col
    return e
}

fn make_float_lit(val: float, line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_FLOAT_LIT()
    e.float_val = val
    e.line = line
    e.col = col
    return e
}

fn make_bool_lit(val: bool, line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_BOOL_LIT()
    e.bool_val = val
    e.line = line
    e.col = col
    return e
}

fn make_string_lit(val: string, line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_STRING_LIT()
    e.str_val = val
    e.line = line
    e.col = col
    return e
}

fn make_ident(name: string, line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_IDENT()
    e.name = name
    e.line = line
    e.col = col
    return e
}

fn make_binary(left: Expr, op: string, right: Expr, line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_BINARY()
    e.left = Box_new(left)
    e.op = op
    e.right = Box_new(right)
    e.line = line
    e.col = col
    return e
}

fn make_unary(op: string, operand: Expr, line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_UNARY()
    e.op = op
    e.right = Box_new(operand)
    e.line = line
    e.col = col
    return e
}

fn make_call(callee_name: string, call_args: List[Expr], line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_CALL()
    e.name = callee_name
    e.args = call_args
    e.line = line
    e.col = col
    return e
}

fn make_method_call(obj: Expr, method: string, call_args: List[Expr], line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_METHOD_CALL()
    e.object = Box_new(obj)
    e.field = method
    e.args = call_args
    e.line = line
    e.col = col
    return e
}

fn make_field_access(obj: Expr, field_name: string, line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_FIELD_ACCESS()
    e.object = Box_new(obj)
    e.field = field_name
    e.line = line
    e.col = col
    return e
}

fn make_index(obj: Expr, idx: Expr, line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_INDEX()
    e.object = Box_new(obj)
    e.right = Box_new(idx)
    e.line = line
    e.col = col
    return e
}

fn make_list_lit(elements: List[Expr], line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_LIST_LIT()
    e.args = elements
    e.line = line
    e.col = col
    return e
}

fn make_struct_lit(type_nm: string, names: List[string], values: List[Expr], line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_STRUCT_LIT()
    e.type_name = type_nm
    e.field_names = names
    e.field_values = values
    e.line = line
    e.col = col
    return e
}

fn make_enum_variant(type_nm: string, variant: string, call_args: List[Expr], line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_ENUM_VARIANT()
    e.type_name = type_nm
    e.name = variant
    e.args = call_args
    e.line = line
    e.col = col
    return e
}

fn make_if_expr(cond: Expr, then_stmts: List[Stmt], else_stmts: List[Stmt], line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_IF()
    e.condition = Box_new(cond)
    e.then_branch = then_stmts
    e.else_branch = else_stmts
    e.line = line
    e.col = col
    return e
}

fn make_block_expr(block_stmts: List[Stmt], line: int, col: int) -> Expr {
    let mut e = null_expr()
    e.kind = EXPR_BLOCK()
    e.stmts = block_stmts
    e.line = line
    e.col = col
    return e
}
