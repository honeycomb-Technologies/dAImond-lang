module ast

-- ============================================================
-- AST node definitions
-- A node's kind is an integer tag, and it has the fields of every kind
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
fn UNOP_DEREF() -> int { return 4 }

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

-- ============================================================
-- AST NODE STRUCTS
-- ============================================================

-- EXPRESSION IDS: every expression has a number of its own, in the
-- order the expressions are made, whichever file they come from. The
-- checker records the type it finds for an expression at its number,
-- and code generation reads it there (checked_type in ir_gen.dm). A
-- copy of an expression has the number of the original.
fn expr_next_id() -> int {
    unsafe {
        let counter = static_addr("dm_expr_ids", 8)
        let id = load_i64(counter)
        store_i64(counter, id + 1)
        return id
    }
}

-- How many expressions have been made
fn expr_id_count() -> int {
    unsafe {
        return load_i64(static_addr("dm_expr_ids", 8))
    }
}

-- Expression node
struct Expr {
    kind: int,
    -- See EXPRESSION IDS
    id: int,
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
    -- Cast target was written as a reference type (&T / &mut T); the
    -- '&' itself is erased from cast_type by parse_type
    cast_is_ref: bool,
    -- String interpolation parts (alternating literal and expr)
    interp_parts: List[Expr],
    interp_is_literal: List[bool]
}

fn expr_new(kind: int, line: int, col: int) -> Expr {
    return Expr {
        kind: kind, id: expr_next_id(), line: line, col: col,
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
        cast_type: "", cast_is_ref: false,
        interp_parts: [], interp_is_literal: []
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
    -- An `if` statement as the expression it is where a block that
    -- ends in it is used as a value (see stmt_value_expr). It shares
    -- the condition and the statements of if_cond, if_then and if_else.
    if_expr: Box[Expr],
    has_if_expr: bool,
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

-- Whether a block that ends in the statement has a value: an
-- expression, a `match`, or an `if` with an `else`
fn stmt_gives_value(s: Stmt) -> bool {
    if s.kind == STMT_EXPRESSION() or s.kind == STMT_MATCH() { return true }
    return s.kind == STMT_IF() and s.has_else_branch and s.has_if_expr
}

-- The expression that gives a block that ends in the statement its
-- value (see stmt_gives_value)
fn stmt_value_expr(s: Stmt) -> Expr {
    if s.kind == STMT_MATCH() { return *s.match_expr }
    if s.kind == STMT_IF() { return *s.if_expr }
    return *s.expr
}

-- The `if` statement that an `if` expression is where its value is
-- not used
fn if_stmt_of(e: Expr) -> Stmt {
    let mut s = stmt_new(STMT_IF(), e.line, e.col)
    s.if_cond = e.condition
    let then_block = *e.then_branch
    s.if_then = then_block.stmts
    if e.has_else {
        s.has_else_branch = true
        let other = *e.else_branch
        if other.kind == EXPR_IF() {
            -- else if: one statement in the else block
            s.if_else.push(if_stmt_of(other))
        } else {
            s.if_else = other.stmts
        }
    }
    s.if_expr = Box_new(e)
    s.has_if_expr = true
    return s
}

fn stmt_new(kind: int, line: int, col: int) -> Stmt {
    return Stmt {
        kind: kind, line: line, col: col,
        let_name: "", let_type: "", let_value: Box_null(), let_is_mut: false,
        ret_value: Box_null(), has_ret_value: false,
        if_cond: Box_null(), if_then: [], if_else: [], has_else_branch: false,
        if_expr: Box_null(), has_if_expr: false,
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
    has_guard: bool,
    -- The arm is what an `if let` without an `else` does when the
    -- pattern is not matched: nothing (parse_if_let)
    no_else: bool
}

fn match_arm_new(pattern: Pattern) -> MatchArm {
    return MatchArm {
        pattern: pattern,
        body: [],
        body_expr: Box_null(),
        is_expr_body: false,
        guard: Box_null(),
        has_guard: false,
        no_else: false
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
    -- What must also hold for the pattern to match: `$lit == 1` for
    -- each literal among a variant pattern's values, as Color.Blue(1)
    -- has. The parser makes them part of the arm's guard.
    tests: List[Expr]
}

fn pattern_new(kind: int) -> Pattern {
    return Pattern {
        kind: kind,
        literal_expr: Box_null(),
        name: "", is_mut: false,
        enum_type: "", variant: "", bindings: [], tests: []
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

-- ============================================================
-- DECLARATION STRUCTS
-- ============================================================

-- Function declaration
struct FunctionDecl {
    name: string,
    params: List[FuncParam],
    return_type: string,
    generic_params: List[string],
    -- The traits each type parameter must implement, joined with "+":
    -- "Show+Named" for T in fn f[T: Show + Named]; "" for none
    generic_bounds: List[string],
    body: List[Stmt],
    effects: List[string],
    is_extern: bool,
    -- An extern fn declared with a trailing `...`, like C's printf
    is_variadic: bool,
    -- The C symbol an extern fn is bound to with `= "symbol"`; "" binds
    -- the function's own name
    link_name: string,
    is_async: bool,
    is_comptime: bool,
    -- The ids of the expressions of the function: from expr_from up
    -- to, and not including, expr_to
    expr_from: int,
    expr_to: int,
    line: int,
    col: int
}

fn func_decl_new(name: string, line: int, col: int) -> FunctionDecl {
    return FunctionDecl {
        name: name,
        params: [],
        return_type: "void",
        generic_params: [],
        generic_bounds: [],
        body: [],
        effects: [],
        is_extern: false,
        is_variadic: false,
        link_name: "",
        is_async: false,
        is_comptime: false,
        expr_from: 0,
        expr_to: 0,
        line: line,
        col: col
    }
}

-- Function parameter
struct FuncParam {
    name: string,
    type_name: string,
    is_mut: bool,
    -- Parameter type was written as a reference type (&T / &mut T)
    is_ref: bool
}

fn func_param_new(name: string, type_name: string) -> FuncParam {
    return FuncParam { name: name, type_name: type_name, is_mut: false, is_ref: false }
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
    declarations: List[Declaration],
    -- The syntax errors the parser found, in source order
    errors: List[string],
    -- The names of the files the declarations come from, at their
    -- numbers (see LINES IN FILES in token.dm). Empty until the
    -- imports are resolved.
    files: List[string]
}

fn source_file_new(name: string) -> SourceFile {
    return SourceFile { module_name: name, imports: [], declarations: [], errors: [], files: [] }
}

-- ============================================================
-- NAMES IN EXPRESSIONS
-- ============================================================

-- The names in the expression, added to `found`. With calls_only, the
-- names it calls as `name(...)`; otherwise every name it mentions: the
-- variables it reads and the functions it calls by name, including the
-- names the expression itself declares (a `let`, a loop variable, a
-- lambda's parameter). A name is added each time it occurs.
fn names_in_expr(e: Expr, found: List[string], calls_only: bool) -> List[string] {
    let mut names = found
    let k = e.kind
    if k == EXPR_IDENTIFIER() {
        if calls_only == false {
            names.push(e.name)
        }
        return names
    }
    if k == EXPR_BINARY() or k == EXPR_RANGE() {
        names = names_in_expr(*e.left, names, calls_only)
        return names_in_expr(*e.right, names, calls_only)
    }
    if k == EXPR_UNARY() or k == EXPR_ERROR_PROPAGATE() or k == EXPR_CAST() or k == EXPR_GROUPED() or k == EXPR_COMPTIME() or k == EXPR_AWAIT() {
        return names_in_expr(*e.operand, names, calls_only)
    }
    if k == EXPR_FIELD_ACCESS() {
        return names_in_expr(*e.object, names, calls_only)
    }
    if k == EXPR_INDEX_ACCESS() {
        names = names_in_expr(*e.object, names, calls_only)
        return names_in_expr(*e.index, names, calls_only)
    }
    if k == EXPR_METHOD_CALL() {
        names = names_in_expr(*e.object, names, calls_only)
        return names_in_exprs(e.args, names, calls_only)
    }
    if k == EXPR_FUNCTION_CALL() {
        let callee = *e.callee
        if calls_only and callee.kind == EXPR_IDENTIFIER() {
            names.push(callee.name)
        }
        names = names_in_expr(callee, names, calls_only)
        return names_in_exprs(e.args, names, calls_only)
    }
    if k == EXPR_STRUCT_LITERAL() {
        return names_in_exprs(e.field_values, names, calls_only)
    }
    if k == EXPR_ENUM_LITERAL() {
        return names_in_exprs(e.payload, names, calls_only)
    }
    if k == EXPR_ARRAY_LITERAL() {
        return names_in_exprs(e.elements, names, calls_only)
    }
    if k == EXPR_STRING_INTERP() {
        return names_in_exprs(e.interp_parts, names, calls_only)
    }
    if k == EXPR_IF() {
        names = names_in_expr(*e.condition, names, calls_only)
        names = names_in_expr(*e.then_branch, names, calls_only)
        if e.has_else {
            names = names_in_expr(*e.else_branch, names, calls_only)
        }
        return names
    }
    if k == EXPR_MATCH() {
        names = names_in_expr(*e.scrutinee, names, calls_only)
        let mut i = 0
        while i < e.match_arms.len() {
            let arm = e.match_arms[i]
            if arm.has_guard {
                names = names_in_expr(*arm.guard, names, calls_only)
            }
            if arm.is_expr_body {
                names = names_in_expr(*arm.body_expr, names, calls_only)
            } else {
                names = names_in_stmts(arm.body, names, calls_only)
            }
            i = i + 1
        }
        return names
    }
    if k == EXPR_BLOCK() {
        names = names_in_stmts(e.stmts, names, calls_only)
        if e.has_result {
            names = names_in_expr(*e.result, names, calls_only)
        }
        return names
    }
    if k == EXPR_LAMBDA() {
        return names_in_expr(*e.lambda_body, names, calls_only)
    }
    return names
}

fn names_in_exprs(exprs: List[Expr], found: List[string], calls_only: bool) -> List[string] {
    let mut names = found
    let mut i = 0
    while i < exprs.len() {
        names = names_in_expr(exprs[i], names, calls_only)
        i = i + 1
    }
    return names
}

-- names_in_expr for every expression in the statements
fn names_in_stmts(stmts: List[Stmt], found: List[string], calls_only: bool) -> List[string] {
    let mut names = found
    let mut i = 0
    while i < stmts.len() {
        let s = stmts[i]
        let k = s.kind
        if k == STMT_LET() {
            names = names_in_expr(*s.let_value, names, calls_only)
        } else if k == STMT_RETURN() {
            if s.has_ret_value {
                names = names_in_expr(*s.ret_value, names, calls_only)
            }
        } else if k == STMT_IF() {
            names = names_in_expr(*s.if_cond, names, calls_only)
            names = names_in_stmts(s.if_then, names, calls_only)
            if s.has_else_branch {
                names = names_in_stmts(s.if_else, names, calls_only)
            }
        } else if k == STMT_MATCH() {
            names = names_in_expr(*s.match_expr, names, calls_only)
        } else if k == STMT_FOR() {
            names = names_in_expr(*s.for_iter, names, calls_only)
            names = names_in_stmts(s.for_body, names, calls_only)
        } else if k == STMT_WHILE() {
            names = names_in_expr(*s.while_cond, names, calls_only)
            names = names_in_stmts(s.while_body, names, calls_only)
        } else if k == STMT_LOOP() {
            names = names_in_stmts(s.loop_body, names, calls_only)
        } else if k == STMT_REGION() {
            names = names_in_stmts(s.region_body, names, calls_only)
        } else if k == STMT_EXPRESSION() {
            names = names_in_expr(*s.expr, names, calls_only)
        } else if k == STMT_ASSIGNMENT() {
            names = names_in_expr(*s.assign_target, names, calls_only)
            names = names_in_expr(*s.assign_value, names, calls_only)
        }
        i = i + 1
    }
    return names
}

-- The expression the value of a branch comes from: the last one of a
-- block, without its parentheses
fn branch_value_expr(e: Expr) -> Expr {
    let mut cur = e
    let mut going = true
    while going {
        going = false
        if cur.kind == EXPR_GROUPED() {
            cur = *cur.operand
            going = true
        } else if cur.kind == EXPR_BLOCK() {
            if cur.has_result {
                cur = *cur.result
                going = true
            } else if cur.stmts.len() > 0 {
                let last = cur.stmts[cur.stmts.len() - 1]
                if last.kind == STMT_EXPRESSION() {
                    cur = *last.expr
                    going = true
                }
            }
        }
    }
    return cur
}

-- Every name the expression mentions (see names_in_expr)
fn expr_names(e: Expr) -> List[string] {
    let none: List[string] = []
    return names_in_expr(e, none, false)
}

-- ============================================================
-- SHADOWED NAMES
-- IR generation keeps a function's variables by name, in maps that
-- know nothing of scopes: a `let x` in a block took the place of an
-- outer `x` for the rest of the function, and so did a loop's
-- variable, a lambda's parameter and a name that a match arm binds.
-- So before IR is generated, a variable that is declared while
-- another of its name is in scope gets a name of its own, `x$1`,
-- and so does each mention of it. What is in scope where is what
-- the checker says (ck_scope, ck_block, ck_for, ck_match and
-- ck_lambda in check.dm). A copy of an expression keeps its id, so
-- the types the checker recorded still belong to it. source_name
-- (token.dm) gives the name as it is written.
-- ============================================================

struct Scopes {
    -- The variables in scope, the innermost last: each one's name
    -- in the source, and its name now
    names: List[string],
    now: List[string],
    -- How many variables of the function have a new name
    renamed: int
}

fn scopes_new() -> Scopes {
    return Scopes { names: [], now: [], renamed: 0 }
}

-- Where the innermost variable of the name is in s.names, or -1
fn sc_find(s: Scopes, name: string) -> int {
    let mut i = s.names.len() - 1
    while i >= 0 {
        if s.names[i] == name { return i }
        i = i - 1
    }
    return -1
}

-- The name a mention of `name` has now
fn sc_name(s: Scopes, name: string) -> string {
    let at = sc_find(s, name)
    if at < 0 { return name }
    return s.now[at]
}

-- Declare a variable; sc_name gives its name from here on
fn sc_declare(s0: Scopes, name: string) -> Scopes {
    let mut s = s0
    let mut now = name
    if name != "_" and sc_find(s, name) >= 0 {
        s.renamed = s.renamed + 1
        now = name + "$" + int_to_string(s.renamed)
    }
    s.names.push(name)
    s.now.push(now)
    return s
}

-- Leave a scope: the variables declared since s.names had `mark`
-- names are gone
fn sc_leave(s0: Scopes, mark: int) -> Scopes {
    let mut s = s0
    while s.names.len() > mark {
        s.names.pop()
        s.now.pop()
    }
    return s
}

struct ScopedExpr {
    s: Scopes,
    e: Expr
}

struct ScopedExprs {
    s: Scopes,
    exprs: List[Expr]
}

struct ScopedStmts {
    s: Scopes,
    stmts: List[Stmt]
}

-- ---- Whether a function shadows any variable: a walk that builds
-- ---- nothing, since most functions shadow none

fn sc_scan_exprs(s0: Scopes, exprs: List[Expr]) -> Scopes {
    let mut s = s0
    let mut i = 0
    while i < exprs.len() {
        s = sc_scan_expr(s, exprs[i])
        i = i + 1
    }
    return s
}

fn sc_scan_scope(s0: Scopes, stmts: List[Stmt]) -> Scopes {
    let mark = s0.names.len()
    return sc_leave(sc_scan_stmts(s0, stmts), mark)
}

fn sc_scan_pattern(s0: Scopes, p: Pattern) -> Scopes {
    let mut s = s0
    if p.kind == PAT_IDENTIFIER() { return sc_declare(s, p.name) }
    if p.kind == PAT_ENUM_VARIANT() {
        let mut i = 0
        while i < p.bindings.len() {
            s = sc_declare(s, p.bindings[i])
            i = i + 1
        }
    }
    return s
}

fn sc_scan_expr(s0: Scopes, e: Expr) -> Scopes {
    let mut s = s0
    let k = e.kind
    if k == EXPR_BINARY() or k == EXPR_RANGE() {
        return sc_scan_expr(sc_scan_expr(s, *e.left), *e.right)
    }
    if k == EXPR_UNARY() or k == EXPR_ERROR_PROPAGATE() or k == EXPR_CAST() or k == EXPR_GROUPED() or k == EXPR_COMPTIME() or k == EXPR_AWAIT() {
        return sc_scan_expr(s, *e.operand)
    }
    if k == EXPR_FIELD_ACCESS() { return sc_scan_expr(s, *e.object) }
    if k == EXPR_INDEX_ACCESS() {
        return sc_scan_expr(sc_scan_expr(s, *e.object), *e.index)
    }
    if k == EXPR_METHOD_CALL() {
        return sc_scan_exprs(sc_scan_expr(s, *e.object), e.args)
    }
    if k == EXPR_FUNCTION_CALL() {
        return sc_scan_exprs(sc_scan_expr(s, *e.callee), e.args)
    }
    if k == EXPR_STRUCT_LITERAL() { return sc_scan_exprs(s, e.field_values) }
    if k == EXPR_ENUM_LITERAL() { return sc_scan_exprs(s, e.payload) }
    if k == EXPR_ARRAY_LITERAL() { return sc_scan_exprs(s, e.elements) }
    if k == EXPR_STRING_INTERP() { return sc_scan_exprs(s, e.interp_parts) }
    if k == EXPR_IF() {
        s = sc_scan_expr(s, *e.condition)
        s = sc_scan_expr(s, *e.then_branch)
        if e.has_else { s = sc_scan_expr(s, *e.else_branch) }
        return s
    }
    if k == EXPR_MATCH() {
        s = sc_scan_expr(s, *e.scrutinee)
        let mut i = 0
        while i < e.match_arms.len() {
            let arm = e.match_arms[i]
            let mark = s.names.len()
            s = sc_scan_pattern(s, arm.pattern)
            if arm.has_guard { s = sc_scan_expr(s, *arm.guard) }
            if arm.is_expr_body {
                s = sc_scan_expr(s, *arm.body_expr)
            } else {
                s = sc_scan_stmts(s, arm.body)
            }
            s = sc_leave(s, mark)
            i = i + 1
        }
        return s
    }
    if k == EXPR_BLOCK() {
        let mark = s.names.len()
        s = sc_scan_stmts(s, e.stmts)
        if e.has_result { s = sc_scan_expr(s, *e.result) }
        return sc_leave(s, mark)
    }
    if k == EXPR_LAMBDA() {
        let mark = s.names.len()
        let mut i = 0
        while i < e.lambda_params.len() {
            s = sc_declare(s, e.lambda_params[i].name)
            i = i + 1
        }
        return sc_leave(sc_scan_expr(s, *e.lambda_body), mark)
    }
    return s
}

fn sc_scan_stmts(s0: Scopes, stmts: List[Stmt]) -> Scopes {
    let mut s = s0
    let mut i = 0
    while i < stmts.len() {
        let st = stmts[i]
        let k = st.kind
        if k == STMT_LET() {
            s = sc_declare(sc_scan_expr(s, *st.let_value), st.let_name)
        } else if k == STMT_RETURN() {
            if st.has_ret_value { s = sc_scan_expr(s, *st.ret_value) }
        } else if k == STMT_IF() {
            if st.has_if_expr {
                s = sc_scan_expr(s, *st.if_expr)
            } else {
                s = sc_scan_scope(sc_scan_expr(s, *st.if_cond), st.if_then)
                if st.has_else_branch { s = sc_scan_scope(s, st.if_else) }
            }
        } else if k == STMT_MATCH() {
            s = sc_scan_expr(s, *st.match_expr)
        } else if k == STMT_FOR() {
            s = sc_scan_expr(s, *st.for_iter)
            let mark = s.names.len()
            s = sc_leave(sc_scan_stmts(sc_declare(s, st.for_var), st.for_body), mark)
        } else if k == STMT_WHILE() {
            s = sc_scan_scope(sc_scan_expr(s, *st.while_cond), st.while_body)
        } else if k == STMT_LOOP() {
            s = sc_scan_scope(s, st.loop_body)
        } else if k == STMT_REGION() {
            s = sc_scan_scope(s, st.region_body)
        } else if k == STMT_EXPRESSION() {
            s = sc_scan_expr(s, *st.expr)
        } else if k == STMT_ASSIGNMENT() {
            s = sc_scan_expr(sc_scan_expr(s, *st.assign_target), *st.assign_value)
        }
        i = i + 1
    }
    return s
}

-- ---- The same walk, building the function with the new names

fn sc_exprs(s0: Scopes, exprs: List[Expr]) -> ScopedExprs {
    let mut s = s0
    let mut out: List[Expr] = []
    let mut i = 0
    while i < exprs.len() {
        let r = sc_expr(s, exprs[i])
        s = r.s
        out.push(r.e)
        i = i + 1
    }
    return ScopedExprs { s: s, exprs: out }
}

fn sc_scope(s0: Scopes, stmts: List[Stmt]) -> ScopedStmts {
    let mark = s0.names.len()
    let r = sc_stmts(s0, stmts)
    return ScopedStmts { s: sc_leave(r.s, mark), stmts: r.stmts }
}

fn sc_expr(s0: Scopes, e: Expr) -> ScopedExpr {
    let mut s = s0
    let mut out = e
    let k = e.kind
    if k == EXPR_IDENTIFIER() {
        out.name = sc_name(s, e.name)
    } else if k == EXPR_BINARY() or k == EXPR_RANGE() {
        let l = sc_expr(s, *e.left)
        let r = sc_expr(l.s, *e.right)
        s = r.s
        out.left = Box_new(l.e)
        out.right = Box_new(r.e)
    } else if k == EXPR_UNARY() or k == EXPR_ERROR_PROPAGATE() or k == EXPR_CAST() or k == EXPR_GROUPED() or k == EXPR_COMPTIME() or k == EXPR_AWAIT() {
        let r = sc_expr(s, *e.operand)
        s = r.s
        out.operand = Box_new(r.e)
    } else if k == EXPR_FIELD_ACCESS() {
        let r = sc_expr(s, *e.object)
        s = r.s
        out.object = Box_new(r.e)
    } else if k == EXPR_INDEX_ACCESS() {
        let o = sc_expr(s, *e.object)
        let r = sc_expr(o.s, *e.index)
        s = r.s
        out.object = Box_new(o.e)
        out.index = Box_new(r.e)
    } else if k == EXPR_METHOD_CALL() {
        let o = sc_expr(s, *e.object)
        let r = sc_exprs(o.s, e.args)
        s = r.s
        out.object = Box_new(o.e)
        out.args = r.exprs
    } else if k == EXPR_FUNCTION_CALL() {
        let c = sc_expr(s, *e.callee)
        let r = sc_exprs(c.s, e.args)
        s = r.s
        out.callee = Box_new(c.e)
        out.args = r.exprs
    } else if k == EXPR_STRUCT_LITERAL() {
        let r = sc_exprs(s, e.field_values)
        s = r.s
        out.field_values = r.exprs
    } else if k == EXPR_ENUM_LITERAL() {
        let r = sc_exprs(s, e.payload)
        s = r.s
        out.payload = r.exprs
    } else if k == EXPR_ARRAY_LITERAL() {
        let r = sc_exprs(s, e.elements)
        s = r.s
        out.elements = r.exprs
    } else if k == EXPR_STRING_INTERP() {
        let r = sc_exprs(s, e.interp_parts)
        s = r.s
        out.interp_parts = r.exprs
    } else if k == EXPR_IF() {
        let c = sc_expr(s, *e.condition)
        let t = sc_expr(c.s, *e.then_branch)
        s = t.s
        out.condition = Box_new(c.e)
        out.then_branch = Box_new(t.e)
        if e.has_else {
            let other = sc_expr(s, *e.else_branch)
            s = other.s
            out.else_branch = Box_new(other.e)
        }
    } else if k == EXPR_MATCH() {
        let scrutinee = sc_expr(s, *e.scrutinee)
        s = scrutinee.s
        out.scrutinee = Box_new(scrutinee.e)
        let mut arms: List[MatchArm] = []
        let mut i = 0
        while i < e.match_arms.len() {
            let mut arm = e.match_arms[i]
            let mark = s.names.len()
            let mut pattern = arm.pattern
            if pattern.kind == PAT_IDENTIFIER() {
                s = sc_declare(s, pattern.name)
                pattern.name = s.now[s.now.len() - 1]
            } else if pattern.kind == PAT_ENUM_VARIANT() {
                let mut bindings: List[string] = []
                let mut bi = 0
                while bi < pattern.bindings.len() {
                    s = sc_declare(s, pattern.bindings[bi])
                    bindings.push(s.now[s.now.len() - 1])
                    bi = bi + 1
                }
                pattern.bindings = bindings
            }
            arm.pattern = pattern
            if arm.has_guard {
                let guard = sc_expr(s, *arm.guard)
                s = guard.s
                arm.guard = Box_new(guard.e)
            }
            if arm.is_expr_body {
                let body = sc_expr(s, *arm.body_expr)
                s = body.s
                arm.body_expr = Box_new(body.e)
            } else {
                let body = sc_stmts(s, arm.body)
                s = body.s
                arm.body = body.stmts
            }
            s = sc_leave(s, mark)
            arms.push(arm)
            i = i + 1
        }
        out.match_arms = arms
    } else if k == EXPR_BLOCK() {
        let mark = s.names.len()
        let body = sc_stmts(s, e.stmts)
        s = body.s
        out.stmts = body.stmts
        if e.has_result {
            let r = sc_expr(s, *e.result)
            s = r.s
            out.result = Box_new(r.e)
        }
        s = sc_leave(s, mark)
    } else if k == EXPR_LAMBDA() {
        let mark = s.names.len()
        let mut params: List[LambdaParam] = []
        let mut i = 0
        while i < e.lambda_params.len() {
            let mut lp = e.lambda_params[i]
            s = sc_declare(s, lp.name)
            lp.name = s.now[s.now.len() - 1]
            params.push(lp)
            i = i + 1
        }
        out.lambda_params = params
        let body = sc_expr(s, *e.lambda_body)
        out.lambda_body = Box_new(body.e)
        s = sc_leave(body.s, mark)
    }
    return ScopedExpr { s: s, e: out }
}

fn sc_stmts(s0: Scopes, stmts: List[Stmt]) -> ScopedStmts {
    let mut s = s0
    let mut result: List[Stmt] = []
    let mut i = 0
    while i < stmts.len() {
        let st = stmts[i]
        let mut out = st
        let k = st.kind
        if k == STMT_LET() {
            -- The value is found before the variable exists:
            -- `let x = x + 1` reads the x before it
            let value = sc_expr(s, *st.let_value)
            s = sc_declare(value.s, st.let_name)
            out.let_value = Box_new(value.e)
            out.let_name = s.now[s.now.len() - 1]
        } else if k == STMT_RETURN() {
            if st.has_ret_value {
                let value = sc_expr(s, *st.ret_value)
                s = value.s
                out.ret_value = Box_new(value.e)
            }
        } else if k == STMT_IF() {
            if st.has_if_expr {
                -- The statement's parts are those of its expression
                let whole = sc_expr(s, *st.if_expr)
                s = whole.s
                out = if_stmt_of(whole.e)
                out.line = st.line
                out.col = st.col
            } else {
                let cond = sc_expr(s, *st.if_cond)
                let then_part = sc_scope(cond.s, st.if_then)
                s = then_part.s
                out.if_cond = Box_new(cond.e)
                out.if_then = then_part.stmts
                if st.has_else_branch {
                    let else_part = sc_scope(s, st.if_else)
                    s = else_part.s
                    out.if_else = else_part.stmts
                }
            }
        } else if k == STMT_MATCH() {
            let matched = sc_expr(s, *st.match_expr)
            s = matched.s
            out.match_expr = Box_new(matched.e)
        } else if k == STMT_FOR() {
            let iter = sc_expr(s, *st.for_iter)
            s = iter.s
            out.for_iter = Box_new(iter.e)
            let mark = s.names.len()
            s = sc_declare(s, st.for_var)
            out.for_var = s.now[s.now.len() - 1]
            let body = sc_stmts(s, st.for_body)
            out.for_body = body.stmts
            s = sc_leave(body.s, mark)
        } else if k == STMT_WHILE() {
            let cond = sc_expr(s, *st.while_cond)
            let body = sc_scope(cond.s, st.while_body)
            s = body.s
            out.while_cond = Box_new(cond.e)
            out.while_body = body.stmts
        } else if k == STMT_LOOP() {
            let body = sc_scope(s, st.loop_body)
            s = body.s
            out.loop_body = body.stmts
        } else if k == STMT_REGION() {
            let body = sc_scope(s, st.region_body)
            s = body.s
            out.region_body = body.stmts
        } else if k == STMT_EXPRESSION() {
            let value = sc_expr(s, *st.expr)
            s = value.s
            out.expr = Box_new(value.e)
        } else if k == STMT_ASSIGNMENT() {
            let target = sc_expr(s, *st.assign_target)
            let value = sc_expr(target.s, *st.assign_value)
            s = value.s
            out.assign_target = Box_new(target.e)
            out.assign_value = Box_new(value.e)
        }
        result.push(out)
        i = i + 1
    }
    return ScopedStmts { s: s, stmts: result }
}

-- The function with a name of its own for each variable that shadows
-- another; the function itself when it shadows none
fn unshadowed_function(fd: FunctionDecl) -> FunctionDecl {
    if fd.is_extern { return fd }
    let mut s = scopes_new()
    let mut i = 0
    while i < fd.params.len() {
        s.names.push(fd.params[i].name)
        s.now.push(fd.params[i].name)
        i = i + 1
    }
    let scanned = sc_scan_stmts(s, fd.body)
    if scanned.renamed == 0 { return fd }
    let mut params = scopes_new()
    i = 0
    while i < fd.params.len() {
        params.names.push(fd.params[i].name)
        params.now.push(fd.params[i].name)
        i = i + 1
    }
    let body = sc_stmts(params, fd.body)
    let mut out = fd
    out.body = body.stmts
    return out
}

fn unshadowed_functions(fns: List[FunctionDecl]) -> List[FunctionDecl] {
    let mut out: List[FunctionDecl] = []
    let mut i = 0
    while i < fns.len() {
        out.push(unshadowed_function(fns[i]))
        i = i + 1
    }
    return out
}

-- The declarations with every function, method and constant as
-- unshadowed_function gives it (see SHADOWED NAMES)
fn unshadowed_declarations(decls: List[Declaration]) -> List[Declaration] {
    let mut out: List[Declaration] = []
    let mut i = 0
    while i < decls.len() {
        let mut d = decls[i]
        if d.kind == DECL_FUNCTION() {
            d.func_decl = Box_new(unshadowed_function(*d.func_decl))
        } else if d.kind == DECL_IMPL() {
            let mut block = *d.impl_block
            block.methods = unshadowed_functions(block.methods)
            d.impl_block = Box_new(block)
        } else if d.kind == DECL_TRAIT() {
            let mut td = *d.trait_decl
            td.methods = unshadowed_functions(td.methods)
            d.trait_decl = Box_new(td)
        } else if d.kind == DECL_CONST() {
            let mut cd = *d.const_decl
            let scanned = sc_scan_expr(scopes_new(), *cd.value)
            if scanned.renamed > 0 {
                let value = sc_expr(scopes_new(), *cd.value)
                cd.value = Box_new(value.e)
                d.const_decl = Box_new(cd)
            }
        }
        out.push(d)
        i = i + 1
    }
    return out
}

-- ============================================================
-- COPIES WITH IDS OF THEIR OWN
-- A copy of a function whose expressions are numbered anew, so that
-- the checker records the types of the copy's expressions apart from
-- the original's: a trait's method with a body becomes a method of
-- each type that implements the trait and does not define it
-- (TRAITS in main_split.dm).
-- ============================================================

fn fresh_exprs(exprs: List[Expr]) -> List[Expr] {
    let mut out: List[Expr] = []
    let mut i = 0
    while i < exprs.len() {
        out.push(fresh_expr(exprs[i]))
        i = i + 1
    }
    return out
}

fn fresh_expr(e: Expr) -> Expr {
    let mut out = e
    out.id = expr_next_id()
    let k = e.kind
    if k == EXPR_BINARY() or k == EXPR_RANGE() {
        out.left = Box_new(fresh_expr(*e.left))
        out.right = Box_new(fresh_expr(*e.right))
    } else if k == EXPR_UNARY() or k == EXPR_ERROR_PROPAGATE() or k == EXPR_CAST() or k == EXPR_GROUPED() or k == EXPR_COMPTIME() or k == EXPR_AWAIT() {
        out.operand = Box_new(fresh_expr(*e.operand))
    } else if k == EXPR_FIELD_ACCESS() {
        out.object = Box_new(fresh_expr(*e.object))
    } else if k == EXPR_INDEX_ACCESS() {
        out.object = Box_new(fresh_expr(*e.object))
        out.index = Box_new(fresh_expr(*e.index))
    } else if k == EXPR_METHOD_CALL() {
        out.object = Box_new(fresh_expr(*e.object))
        out.args = fresh_exprs(e.args)
    } else if k == EXPR_FUNCTION_CALL() {
        out.callee = Box_new(fresh_expr(*e.callee))
        out.args = fresh_exprs(e.args)
    } else if k == EXPR_STRUCT_LITERAL() {
        out.field_values = fresh_exprs(e.field_values)
    } else if k == EXPR_ENUM_LITERAL() {
        out.payload = fresh_exprs(e.payload)
    } else if k == EXPR_ARRAY_LITERAL() {
        out.elements = fresh_exprs(e.elements)
    } else if k == EXPR_STRING_INTERP() {
        out.interp_parts = fresh_exprs(e.interp_parts)
    } else if k == EXPR_IF() {
        out.condition = Box_new(fresh_expr(*e.condition))
        out.then_branch = Box_new(fresh_expr(*e.then_branch))
        if e.has_else {
            out.else_branch = Box_new(fresh_expr(*e.else_branch))
        }
    } else if k == EXPR_MATCH() {
        out.scrutinee = Box_new(fresh_expr(*e.scrutinee))
        let mut arms: List[MatchArm] = []
        let mut i = 0
        while i < e.match_arms.len() {
            let mut arm = e.match_arms[i]
            let mut pattern = arm.pattern
            if pattern.kind == PAT_LITERAL() {
                pattern.literal_expr = Box_new(fresh_expr(*pattern.literal_expr))
            }
            arm.pattern = pattern
            if arm.has_guard {
                arm.guard = Box_new(fresh_expr(*arm.guard))
            }
            if arm.is_expr_body {
                arm.body_expr = Box_new(fresh_expr(*arm.body_expr))
            } else {
                arm.body = fresh_stmts(arm.body)
            }
            arms.push(arm)
            i = i + 1
        }
        out.match_arms = arms
    } else if k == EXPR_BLOCK() {
        out.stmts = fresh_stmts(e.stmts)
        if e.has_result {
            out.result = Box_new(fresh_expr(*e.result))
        }
    } else if k == EXPR_LAMBDA() {
        out.lambda_body = Box_new(fresh_expr(*e.lambda_body))
    }
    return out
}

fn fresh_stmts(stmts: List[Stmt]) -> List[Stmt] {
    let mut result: List[Stmt] = []
    let mut i = 0
    while i < stmts.len() {
        let st = stmts[i]
        let mut out = st
        let k = st.kind
        if k == STMT_LET() {
            out.let_value = Box_new(fresh_expr(*st.let_value))
        } else if k == STMT_RETURN() {
            if st.has_ret_value {
                out.ret_value = Box_new(fresh_expr(*st.ret_value))
            }
        } else if k == STMT_IF() {
            if st.has_if_expr {
                out = if_stmt_of(fresh_expr(*st.if_expr))
                out.line = st.line
                out.col = st.col
            } else {
                out.if_cond = Box_new(fresh_expr(*st.if_cond))
                out.if_then = fresh_stmts(st.if_then)
                if st.has_else_branch {
                    out.if_else = fresh_stmts(st.if_else)
                }
            }
        } else if k == STMT_MATCH() {
            out.match_expr = Box_new(fresh_expr(*st.match_expr))
        } else if k == STMT_FOR() {
            out.for_iter = Box_new(fresh_expr(*st.for_iter))
            out.for_body = fresh_stmts(st.for_body)
        } else if k == STMT_WHILE() {
            out.while_cond = Box_new(fresh_expr(*st.while_cond))
            out.while_body = fresh_stmts(st.while_body)
        } else if k == STMT_LOOP() {
            out.loop_body = fresh_stmts(st.loop_body)
        } else if k == STMT_REGION() {
            out.region_body = fresh_stmts(st.region_body)
        } else if k == STMT_EXPRESSION() {
            out.expr = Box_new(fresh_expr(*st.expr))
        } else if k == STMT_ASSIGNMENT() {
            out.assign_target = Box_new(fresh_expr(*st.assign_target))
            out.assign_value = Box_new(fresh_expr(*st.assign_value))
        }
        result.push(out)
        i = i + 1
    }
    return result
}

-- The function with expressions of its own
fn fresh_function(fd: FunctionDecl) -> FunctionDecl {
    let mut out = fd
    out.expr_from = expr_id_count()
    out.body = fresh_stmts(fd.body)
    out.expr_to = expr_id_count()
    return out
}
