module compiler

import token

-- ============================================================
-- PARSER
-- ============================================================

struct Parser {
    tokens: List[Token],
    pos: int,
    errors: List[string]
}

fn parser_new(tokens: List[Token]) -> Parser {
    let errs: List[string] = []
    return Parser { tokens: tokens, pos: 0, errors: errs }
}

fn p_cur(p: Parser) -> Token {
    if p.pos < p.tokens.len() {
        return p.tokens[p.pos]
    }
    return token_new(TK_EOF(), "", 0, 0)
}

fn p_peek(p: Parser) -> int {
    return p_cur(p).kind
}

fn p_advance(p: Parser) -> Parser {
    let mut pp = p
    pp.pos = pp.pos + 1
    return pp
}

fn p_skip_newlines(p: Parser) -> Parser {
    let mut pp = p
    while p_peek(pp) == TK_NEWLINE() {
        pp = p_advance(pp)
    }
    return pp
}

fn p_expect(p: Parser, kind: int) -> Parser {
    let mut pp = p
    pp = p_skip_newlines(pp)
    if p_peek(pp) == kind {
        return p_advance(pp)
    }
    let tok = p_cur(pp)
    let msg = "Error at " + int_to_string(tok.line) + ":" + int_to_string(tok.col) + ": expected " + token_kind_name(kind) + " but got " + token_kind_name(tok.kind)
    pp.errors.push(msg)
    return pp
}

fn p_error(p: Parser, msg: string) -> Parser {
    let mut pp = p
    let tok = p_cur(pp)
    let full = "Error at " + int_to_string(tok.line) + ":" + int_to_string(tok.col) + ": " + msg
    pp.errors.push(full)
    return pp
}

-- ============================================================
-- PARSE TYPE EXPRESSIONS
-- ============================================================

struct TypeExpr {
    kind: int,
    name: string,
    generic_args: List[TypeExpr],
    is_mutable: bool
}

fn empty_type() -> TypeExpr {
    let ga: List[TypeExpr] = []
    return TypeExpr { kind: 0, name: "", generic_args: ga, is_mutable: false }
}

struct TypeResult {
    p: Parser,
    ty: TypeExpr
}

fn parse_type(p: Parser) -> TypeResult {
    let mut pp = p_skip_newlines(p)
    let tok = p_cur(pp)
    if tok.kind != TK_IDENT() and tok.kind != TK_SELF() {
        return TypeResult { p: pp, ty: empty_type() }
    }
    let name = tok.value
    pp = p_advance(pp)
    -- Check for generic args [T, U]
    let ga: List[TypeExpr] = []
    if p_peek(pp) == TK_LBRACKET() {
        pp = p_advance(pp)
        let mut first = true
        let mut type_done = false
        while type_done == false {
            if p_peek(pp) == TK_RBRACKET() {
                type_done = true
            } else {
                if first == false {
                    pp = p_expect(pp, TK_COMMA())
                }
                first = false
                let inner = parse_type(pp)
                pp = inner.p
                ga.push(inner.ty)
            }
        }
        pp = p_expect(pp, TK_RBRACKET())
    }
    let ty = TypeExpr { kind: 70, name: name, generic_args: ga, is_mutable: false }
    return TypeResult { p: pp, ty: ty }
}

-- ============================================================
-- PARSE EXPRESSIONS (Pratt parser)
-- ============================================================

struct ExprResult {
    p: Parser,
    kind: int,
    int_val: int,
    float_val: float,
    bool_val: bool,
    str_val: string,
    name: string,
    op: string,
    left_idx: int,
    right_idx: int,
    args_start: int,
    args_count: int,
    line: int,
    col: int
}

-- We store expressions in a flat pool since Box[Expr] doesn't work well
-- Instead, we'll generate C code directly from token stream
-- This avoids the need for a complex AST

-- For Stage 1 v0.1, we use a simpler approach:
-- Parse directly to C code string output (one-pass compilation)

-- ============================================================
-- SIMPLE ONE-PASS COMPILER
-- Parse and generate C code simultaneously
-- ============================================================

struct Compiler {
    tokens: List[Token],
    pos: int,
    output: string,
    indent: int,
    errors: List[string],
    -- Track struct names for type generation
    struct_names: List[string],
    fn_names: List[string],
    fn_ret_types: List[string],
    fn_sigs: List[string],
    struct_defs: List[string],
    fn_defs: List[string],
    -- Track which variables are strings for operator dispatch
    str_vars: List[string],
    -- Accumulated list type definitions (built during parsing)
    list_type_defs: string,
    -- Track list variable element types: "|dm_var=elem_type|..."
    list_elem_types: string,
    -- Track all variable types: "|dm_var=c_type|..."
    var_types: string,
    -- Track struct field types: "|dm_StructName.fieldName=c_type|..."
    struct_fields: string,
    -- Track enum variant info: "|EnumName.Variant=unit|" or "|EnumName.Variant=tuple:type1,type2|"
    enum_variants: string,
    -- Track enum names (separate from struct_names)
    enum_names: List[string],
    -- Track generated Option/Result type defs to avoid duplicates
    option_type_defs: string,
    -- Counter for match expression temporaries
    match_counter: int,
    -- Expected type context for Some/None/Ok/Err inference
    expected_type: string,
    -- Current function return type for return statement context
    current_fn_ret_type: string,
    -- Counter for lambda lifted functions
    lambda_counter: int,
    -- Accumulated lambda function definitions (lifted to file scope)
    lambda_defs: string,
    -- Track generic function token ranges: "|fn_name=start:end|"
    generic_fn_tokens: string,
    -- Track already-monomorphized generic functions: "|mangled_name|"
    monomorphized_fns: string,
    -- Counter for unique for-loop iterator variables
    for_counter: int,
    -- Counter for unique try-operator temporaries
    try_counter: int
}

fn compiler_new(tokens: List[Token]) -> Compiler {
    let errs: List[string] = []
    let sn: List[string] = []
    let fnames: List[string] = []
    let frets: List[string] = []
    let fsigs: List[string] = []
    let sdefs: List[string] = []
    let fdefs: List[string] = []
    let sv: List[string] = []
    let en: List[string] = []
    return Compiler {
        tokens: tokens, pos: 0, output: "", indent: 0,
        errors: errs, struct_names: sn, fn_names: fnames,
        fn_ret_types: frets,
        fn_sigs: fsigs, struct_defs: sdefs, fn_defs: fdefs,
        str_vars: sv, list_type_defs: "",
        list_elem_types: "",
        var_types: "",
        struct_fields: "",
        enum_variants: "",
        enum_names: en,
        option_type_defs: "",
        match_counter: 0,
        expected_type: "",
        current_fn_ret_type: "",
        lambda_counter: 0,
        lambda_defs: "",
        generic_fn_tokens: "",
        monomorphized_fns: "",
        for_counter: 0,
        try_counter: 0
    }
}

fn lookup_struct_field_type(c: Compiler, struct_type: string, field_name: string) -> string {
    let marker = "|" + struct_type + "." + field_name + "="
    let pos = string_find(c.struct_fields, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.struct_fields, start, len(c.struct_fields) - start)
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, 0, end_pos)
}

fn is_enum_name(c: Compiler, name: string) -> bool {
    let mut i = 0
    while i < c.enum_names.len() {
        let cur = "" + c.enum_names[i]
        if cur == name { return true }
        i = i + 1
    }
    return false
}

fn lookup_enum_variant(c: Compiler, enum_name: string, variant_name: string) -> string {
    let marker = "|" + enum_name + "." + variant_name + "="
    let pos = string_find(c.enum_variants, marker)
    if pos < 0 { return "" }
    let start = pos + len(marker)
    let rest = substr(c.enum_variants, start, len(c.enum_variants) - start)
    let end_pos = string_find(rest, "|")
    if end_pos < 0 { return "" }
    return substr(rest, 0, end_pos)
}

-- Check if a field access like "dm_var.field" is a string
fn field_access_is_string(code: string, c: Compiler) -> bool {
    let dot_pos = string_find(code, ".")
    if dot_pos < 0 { return false }
    let var_name = substr(code, 0, dot_pos)
    let field_name = substr(code, dot_pos + 1, len(code) - dot_pos - 1)
    -- Look up the variable's type
    let var_type = lookup_var_type(c, var_name)
    if var_type == "" { return false }
    -- Look up the field's type in that struct
    let field_type = lookup_struct_field_type(c, var_type, field_name)
    if field_type == "dm_string" { return true }
    return false
}

-- Check if a generated C code expression is known to produce a string
fn str_list_contains(list: List[string], val: string) -> bool {
    let mut i = 0
    while i < list.len() {
        let cur = "" + list[i]
        if cur == val { return true }
        i = i + 1
    }
    return false
}

fn code_is_string(code: string, c: Compiler) -> bool {
    if starts_with(code, "dm_string_from_cstr(") { return true }
    if starts_with(code, "dm_string_concat(") { return true }
    if starts_with(code, "dm_int_to_string(") { return true }
    if starts_with(code, "dm_bool_to_string(") { return true }
    if starts_with(code, "dm_float_to_string(") { return true }
    if starts_with(code, "dm_string_substr(") { return true }
    if starts_with(code, "dm_char_at(") { return true }
    if starts_with(code, "dm_file_read(") { return true }
    if starts_with(code, "dm_args_get(") { return true }
    if starts_with(code, "dm_string_replace(") { return true }
    if starts_with(code, "dm_string_trim(") { return true }
    if starts_with(code, "dm_string_to_upper(") { return true }
    if starts_with(code, "dm_string_to_lower(") { return true }
    -- Check if it's a known string variable
    if str_list_contains(c.str_vars, code) { return true }
    -- Check var_types for dm_string
    let vt = lookup_var_type(c, code)
    if vt == "dm_string" { return true }
    -- Check struct field access: dm_var.field
    if field_access_is_string(code, c) { return true }
    return false
}

fn c_cur(c: Compiler) -> Token {
    if c.pos < c.tokens.len() {
        return c.tokens[c.pos]
    }
    return token_new(TK_EOF(), "", 0, 0)
}

fn c_peek(c: Compiler) -> int {
    return c_cur(c).kind
}

fn c_advance(c: Compiler) -> Compiler {
    let mut cc = c
    cc.pos = cc.pos + 1
    return cc
}

fn c_skip_nl(c: Compiler) -> Compiler {
    let mut cc = c
    while c_peek(cc) == TK_NEWLINE() {
        cc = c_advance(cc)
    }
    return cc
}

fn c_expect(c: Compiler, kind: int) -> Compiler {
    let mut cc = c_skip_nl(c)
    if c_peek(cc) == kind {
        return c_advance(cc)
    }
    let tok = c_cur(cc)
    let msg = "Error at " + int_to_string(tok.line) + ":" + int_to_string(tok.col) + ": expected " + token_kind_name(kind) + " but got " + token_kind_name(tok.kind)
    cc.errors.push(msg)
    return cc
}

fn c_error(c: Compiler, msg: string) -> Compiler {
    let mut cc = c
    let tok = c_cur(cc)
    let full = "Error at " + int_to_string(tok.line) + ":" + int_to_string(tok.col) + ": " + msg
    cc.errors.push(full)
    return cc
}

fn indent_str(level: int) -> string {
    let mut s = ""
    let mut i = 0
    while i < level {
        s = s + "    "
        i = i + 1
    }
    return s
}

fn dm_mangle(name: string) -> string {
    return "dm_" + name
}

fn map_dm_type(name: string) -> string {
    if name == "int" { return "int64_t" }
    if name == "float" { return "double" }
    if name == "bool" { return "bool" }
    if name == "string" { return "dm_string" }
    if name == "void" { return "void" }
    return "dm_" + name
}
