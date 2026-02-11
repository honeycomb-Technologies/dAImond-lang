module ast

-- ============================================================
-- AST Node Definitions for dAImond Stage 4
-- Uses integer kind tags (not tagged unions) following Stage 1 pattern
-- ============================================================

-- ============================================================
-- EXPRESSION KINDS
-- ============================================================
fn EXPR_LITERAL_INT() -> int { return 1 }
fn EXPR_LITERAL_FLOAT() -> int { return 2 }
fn EXPR_LITERAL_STRING() -> int { return 3 }
fn EXPR_LITERAL_BOOL() -> int { return 4 }
fn EXPR_LITERAL_NULL() -> int { return 5 }
fn EXPR_IDENTIFIER() -> int { return 10 }
fn EXPR_BINARY() -> int { return 20 }
fn EXPR_UNARY() -> int { return 21 }
fn EXPR_FIELD_ACCESS() -> int { return 30 }
fn EXPR_INDEX_ACCESS() -> int { return 31 }
fn EXPR_METHOD_CALL() -> int { return 32 }
fn EXPR_FUNCTION_CALL() -> int { return 40 }
fn EXPR_STRUCT_LITERAL() -> int { return 41 }
fn EXPR_ENUM_LITERAL() -> int { return 42 }
fn EXPR_ARRAY_LITERAL() -> int { return 43 }
fn EXPR_IF() -> int { return 50 }
fn EXPR_MATCH() -> int { return 51 }
fn EXPR_BLOCK() -> int { return 52 }
fn EXPR_LAMBDA() -> int { return 60 }
fn EXPR_PIPELINE() -> int { return 70 }
fn EXPR_ERROR_PROPAGATE() -> int { return 71 }
fn EXPR_RANGE() -> int { return 72 }
fn EXPR_CAST() -> int { return 80 }
fn EXPR_STRING_INTERP() -> int { return 90 }
fn EXPR_GROUPED() -> int { return 91 }
fn EXPR_COMPTIME() -> int { return 92 }
fn EXPR_AWAIT() -> int { return 93 }

-- ============================================================
-- STATEMENT KINDS
-- ============================================================
fn STMT_LET() -> int { return 1 }
fn STMT_RETURN() -> int { return 2 }
fn STMT_IF() -> int { return 3 }
fn STMT_MATCH() -> int { return 4 }
fn STMT_FOR() -> int { return 5 }
fn STMT_WHILE() -> int { return 6 }
fn STMT_LOOP() -> int { return 7 }
fn STMT_BREAK() -> int { return 8 }
fn STMT_CONTINUE() -> int { return 9 }
fn STMT_REGION() -> int { return 10 }
fn STMT_EXPRESSION() -> int { return 11 }
fn STMT_ASSIGNMENT() -> int { return 12 }

-- ============================================================
-- DECLARATION KINDS
-- ============================================================
fn DECL_FUNCTION() -> int { return 1 }
fn DECL_STRUCT() -> int { return 2 }
fn DECL_ENUM() -> int { return 3 }
fn DECL_TRAIT() -> int { return 4 }
fn DECL_IMPL() -> int { return 5 }
fn DECL_CONST() -> int { return 6 }

-- ============================================================
-- BINARY OPERATOR KINDS
-- ============================================================
fn BINOP_ADD() -> int { return 1 }
fn BINOP_SUB() -> int { return 2 }
fn BINOP_MUL() -> int { return 3 }
fn BINOP_DIV() -> int { return 4 }
fn BINOP_MOD() -> int { return 5 }
fn BINOP_EQ() -> int { return 10 }
fn BINOP_NE() -> int { return 11 }
fn BINOP_LT() -> int { return 12 }
fn BINOP_LE() -> int { return 13 }
fn BINOP_GT() -> int { return 14 }
fn BINOP_GE() -> int { return 15 }
fn BINOP_AND() -> int { return 20 }
fn BINOP_OR() -> int { return 21 }
fn BINOP_BIT_AND() -> int { return 30 }
fn BINOP_BIT_OR() -> int { return 31 }
fn BINOP_BIT_XOR() -> int { return 32 }
fn BINOP_SHL() -> int { return 33 }
fn BINOP_SHR() -> int { return 34 }

-- ============================================================
-- UNARY OPERATOR KINDS
-- ============================================================
fn UNOP_NEG() -> int { return 1 }
fn UNOP_NOT() -> int { return 2 }
fn UNOP_BIT_NOT() -> int { return 3 }

-- ============================================================
-- ASSIGNMENT OPERATOR KINDS
-- ============================================================
fn ASSIGN_EQ() -> int { return 0 }
fn ASSIGN_ADD() -> int { return 1 }
fn ASSIGN_SUB() -> int { return 2 }
fn ASSIGN_MUL() -> int { return 3 }
fn ASSIGN_DIV() -> int { return 4 }
fn ASSIGN_MOD() -> int { return 5 }

-- ============================================================
-- TYPE EXPRESSION KINDS
-- ============================================================
fn TYPE_NAMED() -> int { return 1 }
fn TYPE_FUNCTION() -> int { return 2 }
fn TYPE_ARRAY() -> int { return 3 }
fn TYPE_POINTER() -> int { return 4 }
fn TYPE_OPTION() -> int { return 5 }
fn TYPE_RESULT() -> int { return 6 }
fn TYPE_TRAIT_OBJECT() -> int { return 7 }
fn TYPE_INFER() -> int { return 8 }

-- ============================================================
-- PATTERN KINDS
-- ============================================================
fn PAT_LITERAL() -> int { return 1 }
fn PAT_IDENTIFIER() -> int { return 2 }
fn PAT_WILDCARD() -> int { return 3 }
fn PAT_ENUM_VARIANT() -> int { return 4 }
fn PAT_STRUCT() -> int { return 5 }

-- ============================================================
-- AST NODE STRUCTS
-- ============================================================

-- Expression node
struct Expr {
    kind: int,
    line: int,
    col: int,
    -- Literal values
    int_val: int,
    float_val: float,
    str_val: string,
    bool_val: bool,
    -- Identifier name
    name: string,
    -- Binary/Unary operator
    op: int,
    -- Children (Box for tree structure)
    left: Box[Expr],
    right: Box[Expr],
    operand: Box[Expr],
    -- Function call / method call
    callee: Box[Expr],
    args: List[Expr],
    -- Field access / index
    object: Box[Expr],
    field: string,
    index: Box[Expr],
    -- Method name
    method: string,
    -- Generic type arguments (as strings)
    generic_args: List[string],
    -- Struct literal
    type_name: string,
    field_names: List[string],
    field_values: List[Expr],
    -- Enum literal
    enum_name: string,
    variant_name: string,
    payload: List[Expr],
    -- Array literal
    elements: List[Expr],
    -- If expression
    condition: Box[Expr],
    then_branch: Box[Expr],
    else_branch: Box[Expr],
    has_else: bool,
    -- Match expression
    scrutinee: Box[Expr],
    match_arms: List[MatchArm],
    -- Block expression
    stmts: List[Stmt],
    result: Box[Expr],
    has_result: bool,
    -- Lambda
    lambda_params: List[LambdaParam],
    lambda_ret_type: string,
    lambda_body: Box[Expr],
    -- Cast target type
    cast_type: string,
    -- String interpolation parts (alternating literal and expr)
    interp_parts: List[Expr],
    interp_is_literal: List[bool],
    -- Pipeline right side
    pipe_right: Box[Expr]
}

fn expr_new(kind: int, line: int, col: int) -> Expr {
    return Expr {
        kind: kind, line: line, col: col,
        int_val: 0, float_val: 0.0, str_val: "", bool_val: false,
        name: "", op: 0,
        left: Box_null(), right: Box_null(), operand: Box_null(),
        callee: Box_null(), args: [],
        object: Box_null(), field: "", index: Box_null(),
        method: "", generic_args: [],
        type_name: "", field_names: [], field_values: [],
        enum_name: "", variant_name: "", payload: [],
        elements: [],
        condition: Box_null(), then_branch: Box_null(), else_branch: Box_null(), has_else: false,
        scrutinee: Box_null(), match_arms: [],
        stmts: [], result: Box_null(), has_result: false,
        lambda_params: [], lambda_ret_type: "", lambda_body: Box_null(),
        cast_type: "",
        interp_parts: [], interp_is_literal: [],
        pipe_right: Box_null()
    }
}

fn expr_int(val: int, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_LITERAL_INT(), line, col)
    e.int_val = val
    return e
}

fn expr_float(val: float, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_LITERAL_FLOAT(), line, col)
    e.float_val = val
    return e
}

fn expr_string(val: string, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_LITERAL_STRING(), line, col)
    e.str_val = val
    return e
}

fn expr_bool(val: bool, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_LITERAL_BOOL(), line, col)
    e.bool_val = val
    return e
}

fn expr_ident(name: string, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_IDENTIFIER(), line, col)
    e.name = name
    return e
}

fn expr_binary(op: int, left: Expr, right: Expr, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_BINARY(), line, col)
    e.op = op
    e.left = Box_new(left)
    e.right = Box_new(right)
    return e
}

fn expr_unary(op: int, operand: Expr, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_UNARY(), line, col)
    e.op = op
    e.operand = Box_new(operand)
    return e
}

fn expr_call(callee: Expr, args: List[Expr], line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_FUNCTION_CALL(), line, col)
    e.callee = Box_new(callee)
    e.args = args
    return e
}

fn expr_method_call(object: Expr, method: string, args: List[Expr], line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_METHOD_CALL(), line, col)
    e.object = Box_new(object)
    e.method = method
    e.args = args
    return e
}

fn expr_field_access(object: Expr, field: string, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_FIELD_ACCESS(), line, col)
    e.object = Box_new(object)
    e.field = field
    return e
}

fn expr_index_access(object: Expr, index: Expr, line: int, col: int) -> Expr {
    let mut e = expr_new(EXPR_INDEX_ACCESS(), line, col)
    e.object = Box_new(object)
    e.index = Box_new(index)
    return e
}

-- Statement node
struct Stmt {
    kind: int,
    line: int,
    col: int,
    -- Let binding
    let_name: string,
    let_type: string,
    let_value: Box[Expr],
    let_is_mut: bool,
    -- Return
    ret_value: Box[Expr],
    has_ret_value: bool,
    -- If statement
    if_cond: Box[Expr],
    if_then: List[Stmt],
    if_else: List[Stmt],
    has_else_branch: bool,
    -- For loop
    for_var: string,
    for_iter: Box[Expr],
    for_body: List[Stmt],
    -- While loop
    while_cond: Box[Expr],
    while_body: List[Stmt],
    -- Loop
    loop_body: List[Stmt],
    -- Match statement
    match_expr: Box[Expr],
    match_stmt_arms: List[MatchArm],
    -- Region
    region_name: string,
    region_body: List[Stmt],
    -- Expression statement
    expr: Box[Expr],
    -- Assignment
    assign_target: Box[Expr],
    assign_op: int,
    assign_value: Box[Expr]
}

fn stmt_new(kind: int, line: int, col: int) -> Stmt {
    return Stmt {
        kind: kind, line: line, col: col,
        let_name: "", let_type: "", let_value: Box_null(), let_is_mut: false,
        ret_value: Box_null(), has_ret_value: false,
        if_cond: Box_null(), if_then: [], if_else: [], has_else_branch: false,
        for_var: "", for_iter: Box_null(), for_body: [],
        while_cond: Box_null(), while_body: [],
        loop_body: [],
        match_expr: Box_null(), match_stmt_arms: [],
        region_name: "", region_body: [],
        expr: Box_null(),
        assign_target: Box_null(), assign_op: 0, assign_value: Box_null()
    }
}

-- Match arm
struct MatchArm {
    pattern: Pattern,
    body: List[Stmt],
    body_expr: Box[Expr],
    is_expr_body: bool,
    guard: Box[Expr],
    has_guard: bool
}

fn match_arm_new(pattern: Pattern) -> MatchArm {
    return MatchArm {
        pattern: pattern,
        body: [],
        body_expr: Box_null(),
        is_expr_body: false,
        guard: Box_null(),
        has_guard: false
    }
}

-- Lambda parameter
struct LambdaParam {
    name: string,
    type_name: string
}

fn lambda_param_new(name: string, type_name: string) -> LambdaParam {
    return LambdaParam { name: name, type_name: type_name }
}

-- Pattern node
struct Pattern {
    kind: int,
    -- Literal pattern
    literal_expr: Box[Expr],
    -- Identifier pattern
    name: string,
    is_mut: bool,
    -- Enum variant pattern
    enum_type: string,
    variant: string,
    bindings: List[string],
    -- Struct pattern
    struct_type: string,
    field_patterns: List[FieldPattern]
}

fn pattern_new(kind: int) -> Pattern {
    return Pattern {
        kind: kind,
        literal_expr: Box_null(),
        name: "", is_mut: false,
        enum_type: "", variant: "", bindings: [],
        struct_type: "", field_patterns: []
    }
}

fn pattern_wildcard() -> Pattern {
    return pattern_new(PAT_WILDCARD())
}

fn pattern_ident(name: string) -> Pattern {
    let mut p = pattern_new(PAT_IDENTIFIER())
    p.name = name
    return p
}

fn pattern_enum_variant(enum_type: string, variant: string, bindings: List[string]) -> Pattern {
    let mut p = pattern_new(PAT_ENUM_VARIANT())
    p.enum_type = enum_type
    p.variant = variant
    p.bindings = bindings
    return p
}

-- Field pattern for struct destructuring
struct FieldPattern {
    name: string,
    binding: string
}

fn field_pattern_new(name: string, binding: string) -> FieldPattern {
    return FieldPattern { name: name, binding: binding }
}

-- ============================================================
-- DECLARATION STRUCTS
-- ============================================================

-- Function declaration
struct FunctionDecl {
    name: string,
    params: List[FuncParam],
    return_type: string,
    generic_params: List[string],
    body: List[Stmt],
    effects: List[string],
    is_extern: bool,
    is_async: bool,
    is_comptime: bool,
    line: int,
    col: int
}

fn func_decl_new(name: string, line: int, col: int) -> FunctionDecl {
    return FunctionDecl {
        name: name,
        params: [],
        return_type: "void",
        generic_params: [],
        body: [],
        effects: [],
        is_extern: false,
        is_async: false,
        is_comptime: false,
        line: line,
        col: col
    }
}

-- Function parameter
struct FuncParam {
    name: string,
    type_name: string,
    is_mut: bool
}

fn func_param_new(name: string, type_name: string) -> FuncParam {
    return FuncParam { name: name, type_name: type_name, is_mut: false }
}

-- Struct declaration
struct StructDecl {
    name: string,
    fields: List[StructField],
    generic_params: List[string],
    line: int,
    col: int
}

fn struct_decl_new(name: string, line: int, col: int) -> StructDecl {
    return StructDecl { name: name, fields: [], generic_params: [], line: line, col: col }
}

-- Struct field
struct StructField {
    name: string,
    type_name: string
}

fn struct_field_new(name: string, type_name: string) -> StructField {
    return StructField { name: name, type_name: type_name }
}

-- Enum declaration
struct EnumDecl {
    name: string,
    variants: List[EnumVariant],
    generic_params: List[string],
    line: int,
    col: int
}

fn enum_decl_new(name: string, line: int, col: int) -> EnumDecl {
    return EnumDecl { name: name, variants: [], generic_params: [], line: line, col: col }
}

-- Enum variant
struct EnumVariant {
    name: string,
    payload_types: List[string],
    has_payload: bool
}

fn enum_variant_new(name: string) -> EnumVariant {
    return EnumVariant { name: name, payload_types: [], has_payload: false }
}

-- Trait declaration
struct TraitDecl {
    name: string,
    methods: List[FunctionDecl],
    generic_params: List[string],
    line: int,
    col: int
}

fn trait_decl_new(name: string, line: int, col: int) -> TraitDecl {
    return TraitDecl { name: name, methods: [], generic_params: [], line: line, col: col }
}

-- Impl block
struct ImplBlock {
    trait_name: string,
    target_type: string,
    methods: List[FunctionDecl],
    generic_params: List[string],
    line: int,
    col: int
}

fn impl_block_new(target: string, line: int, col: int) -> ImplBlock {
    return ImplBlock {
        trait_name: "", target_type: target, methods: [],
        generic_params: [], line: line, col: col
    }
}

-- Const declaration
struct ConstDecl {
    name: string,
    type_name: string,
    value: Box[Expr],
    is_comptime: bool,
    line: int,
    col: int
}

fn const_decl_new(name: string, line: int, col: int) -> ConstDecl {
    return ConstDecl {
        name: name, type_name: "", value: Box_null(),
        is_comptime: false, line: line, col: col
    }
}

-- Top-level declaration (wraps any of the above)
struct Declaration {
    kind: int,
    func_decl: Box[FunctionDecl],
    struct_decl: Box[StructDecl],
    enum_decl: Box[EnumDecl],
    trait_decl: Box[TraitDecl],
    impl_block: Box[ImplBlock],
    const_decl: Box[ConstDecl]
}

fn decl_function(fd: FunctionDecl) -> Declaration {
    return Declaration {
        kind: DECL_FUNCTION(),
        func_decl: Box_new(fd),
        struct_decl: Box_null(),
        enum_decl: Box_null(),
        trait_decl: Box_null(),
        impl_block: Box_null(),
        const_decl: Box_null()
    }
}

fn decl_struct(sd: StructDecl) -> Declaration {
    return Declaration {
        kind: DECL_STRUCT(),
        func_decl: Box_null(),
        struct_decl: Box_new(sd),
        enum_decl: Box_null(),
        trait_decl: Box_null(),
        impl_block: Box_null(),
        const_decl: Box_null()
    }
}

fn decl_enum(ed: EnumDecl) -> Declaration {
    return Declaration {
        kind: DECL_ENUM(),
        func_decl: Box_null(),
        struct_decl: Box_null(),
        enum_decl: Box_new(ed),
        trait_decl: Box_null(),
        impl_block: Box_null(),
        const_decl: Box_null()
    }
}

fn decl_trait(td: TraitDecl) -> Declaration {
    return Declaration {
        kind: DECL_TRAIT(),
        func_decl: Box_null(),
        struct_decl: Box_null(),
        enum_decl: Box_null(),
        trait_decl: Box_new(td),
        impl_block: Box_null(),
        const_decl: Box_null()
    }
}

fn decl_impl(ib: ImplBlock) -> Declaration {
    return Declaration {
        kind: DECL_IMPL(),
        func_decl: Box_null(),
        struct_decl: Box_null(),
        enum_decl: Box_null(),
        trait_decl: Box_null(),
        impl_block: Box_new(ib),
        const_decl: Box_null()
    }
}

fn decl_const(cd: ConstDecl) -> Declaration {
    return Declaration {
        kind: DECL_CONST(),
        func_decl: Box_null(),
        struct_decl: Box_null(),
        enum_decl: Box_null(),
        trait_decl: Box_null(),
        impl_block: Box_null(),
        const_decl: Box_new(cd)
    }
}

-- Import declaration
struct ImportDecl {
    path: string,
    line: int,
    col: int
}

fn import_decl_new(path: string, line: int, col: int) -> ImportDecl {
    return ImportDecl { path: path, line: line, col: col }
}

-- Source file (root AST node)
struct SourceFile {
    module_name: string,
    imports: List[ImportDecl],
    declarations: List[Declaration]
}

fn source_file_new(name: string) -> SourceFile {
    return SourceFile { module_name: name, imports: [], declarations: [] }
}
