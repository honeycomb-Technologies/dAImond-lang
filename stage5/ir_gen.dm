module ir_gen

import ast
import ir
import ir_builder

-- ============================================================
-- IR Generator Core
-- Central struct and initialization, type mapping
-- ============================================================

-- Struct info for tracking registered structs
struct StructInfo {
    name: string,
    field_names: List[string],
    -- IR type names (lowered)
    field_types: List[string],
    -- Source type names as written, e.g. List[Stmt] or Map[string, int]
    field_src_types: List[string]
}

fn struct_info_new(name: string) -> StructInfo {
    return StructInfo { name: name, field_names: [], field_types: [], field_src_types: [] }
}

-- Enum info for tracking registered enums
struct EnumInfo {
    name: string,
    variant_names: List[string],
    variant_tags: List[int],
    variant_payload_types: List[string],
    variant_has_payload: List[bool],
    -- The source types of the values each variant carries, joined with
    -- "|"; "" for a variant that carries none
    variant_payloads: List[string]
}

fn enum_info_new(name: string) -> EnumInfo {
    return EnumInfo {
        name: name, variant_names: [], variant_tags: [],
        variant_payload_types: [], variant_has_payload: [],
        variant_payloads: []
    }
}

-- "1 value" or "N values"
fn values_text(n: int) -> string {
    if n == 1 { return "1 value" }
    return int_to_string(n) + " values"
}

-- The number of a variant by its name, or -1
fn enum_variant_index(info: EnumInfo, name: string) -> int {
    let mut i = 0
    while i < info.variant_names.len() {
        if info.variant_names[i] == name { return i }
        i = i + 1
    }
    return -1
}

-- The number of values a variant carries
fn enum_payload_count(info: EnumInfo, variant: int) -> int {
    let joined = info.variant_payloads[variant]
    if joined == "" { return 0 }
    return string_split(joined, "|").len()
}

-- The source type of value number `k` of a variant
fn enum_payload_type(info: EnumInfo, variant: int, k: int) -> string {
    let types = string_split(info.variant_payloads[variant], "|")
    if k < types.len() { return types[k] }
    return ""
}

-- The field of the enum's struct that holds a variant's first value.
-- The struct is the tag, then the values of each variant in turn.
fn enum_payload_field(info: EnumInfo, variant: int) -> int {
    let mut field = 1
    let mut i = 0
    while i < variant {
        field = field + enum_payload_count(info, i)
        i = i + 1
    }
    return field
}

-- List element kind for typed list operations
fn LIST_ELEM_INT() -> int { return 0 }
fn LIST_ELEM_FLOAT() -> int { return 1 }
fn LIST_ELEM_STRING() -> int { return 2 }
fn LIST_ELEM_OTHER() -> int { return 3 }


-- ============================================================
-- IR GENERATOR STRUCT
-- ============================================================

struct IRGenerator {
    -- IR module being built
    ir_mod: IRModule,
    -- IR builder for current function
    builder: IRBuilder,
    -- Variable name -> alloca instruction ID
    variable_map: Map[string, int],
    -- Variable name -> type name
    variable_types: Map[string, string],
    -- Struct name -> StructInfo
    struct_defs: Map[string, StructInfo],
    -- Variable name -> list element kind
    list_elem_kinds: Map[string, int],
    -- Variable name -> list element type name (for struct-typed lists)
    list_elem_types: Map[string, string],
    -- Generic function name -> declaration index in source
    generic_fn_decls: Map[string, int],
    -- Mangled monomorphization name -> generated flag
    mono_generated: Map[string, bool],
    -- Type param name -> concrete type name
    type_substitutions: Map[string, string],
    -- The id of the `match` that is a statement, while it is
    -- generated: its arms' values go nowhere (see match_slot_type in
    -- ir_gen_expr.dm). -1 otherwise.
    unused_match: int,
    -- Method name -> struct type name
    method_struct_map: Map[string, string],
    -- The functions of impl blocks that take no self, by mangled name
    -- ("Point_new"): they are called on the type, as Point.new(...)
    static_fns: Map[string, bool],
    -- Variable name -> struct type name
    var_struct_types: Map[string, string],
    -- Function name -> pipe-delimited mut param flags
    mut_param_fns: Map[string, string],
    -- Enum name -> EnumInfo
    enum_defs: Map[string, EnumInfo],
    -- Variable name -> enum type name
    var_enum_types: Map[string, string],
    -- Variable name -> box inner type name
    box_inner_types: Map[string, string],
    -- "StructName:TraitName" -> pipe-delimited method list
    trait_impl_methods: Map[string, string],
    -- User extern function names
    user_extern_fns: Map[string, bool],
    -- Function name -> return type (IR type string)
    fn_return_types: Map[string, string],
    -- "StructName.field" -> list elem kind
    field_list_elem_kinds: Map[string, int],
    -- "StructName.field" -> list elem type name
    field_list_elem_types: Map[string, string],
    -- Variable name -> true if it's a mut pointer (e.g. mut self)
    mut_ptr_vars: Map[string, bool],
    -- Variable name -> param index for mut pointer params (no alloca created)
    param_ptr_map: Map[string, int],
    -- Label counter for unique block names
    label_counter: int,
    -- Lambda counter
    lambda_counter: int,
    -- Break/continue targets
    break_target: string,
    continue_target: string,
    has_break_target: bool,
    has_continue_target: bool,
    -- Current return type name
    current_return_type: string,
    -- Current impl type name
    current_impl_type: string,
    has_impl_type: bool,
    -- Source declarations for comptime resolution
    declarations: List[Declaration],
    -- Completed blocks that need to be added to the function
    completed_blocks: List[IRBasicBlock],
    -- Variable name -> full type annotation (e.g., "Result[int, string]")
    var_type_annotations: Map[string, string],
    -- Known user-defined function names (for undefined function detection)
    known_functions: Map[string, bool],
    -- Variable name -> true if declared with a fixed-size array type [T; N]
    -- (normalized to a runtime list; bounds panics say "(array)", not "(list)")
    array_var_names: Map[string, bool],
    -- Compile-time safety checks (Stage 0 Phase 2 rules, per-function state)
    -- Depth of enclosing unsafe { } blocks (extern-call/deref fence)
    unsafe_depth: int,
    -- Depth of enclosing named region { } blocks (region escape rule)
    region_depth: int,
    -- Variable name -> true if it holds a reference of extern/cast provenance
    tainted_vars: Map[string, bool],
    -- Variable name -> true if its type is a reference type (&T)
    ref_vars: Map[string, bool],
    -- Variable name -> true if it holds a region-born value
    region_born_vars: Map[string, bool],
    -- Declaration-order log of variable names for the current function
    declared_vars: List[string],
    -- Stack of declared_vars lengths at named-region entry points
    region_marks: List[int],
    -- All source-declared extern fn names (unsafe fence; includes dm_*)
    all_extern_fns: Map[string, bool],
    -- Functions defined by the precompiled runtime object that the
    -- program is linked with: they are declared here, not generated
    linked_fns: Map[string, bool],
    -- Expressions IR generation could not resolve, reported as compile
    -- errors after IR generation instead of being lowered to placeholders
    type_errors: List[string],
    -- The type the checker found for each expression, at the
    -- expression's id (see CHECKED TYPES). Empty when nothing was checked.
    checked_types: List[string],
    -- The type arguments the checker found for each call of a generic
    -- function, at the call's id, joined with "|"
    checked_type_args: List[string],
    -- What a method call calls where it is no method, at the call's
    -- id: "call:f" for x.f(a), which is f(x, a)
    checked_targets: List[string],
    -- The checker of the program, for the instances of generic
    -- functions (see INSTANCES in check.dm); none when has_checker is
    -- false
    checker: Box[Checker],
    has_checker: bool,
    -- -g: say where each variable lives (OP_DEBUG_VAR)
    debug_info: bool,
    -- Declared source return types of functions and impl methods
    -- (methods keyed Type_method), as written in the source
    fn_src_return_types: Map[string, string],
    -- Function name -> its parameters' source types, joined with "|"
    fn_src_param_types: Map[string, string],
    -- Source type of each variable in the current function, as written
    -- or as resolved by static_type; reset per function
    var_src_types: Map[string, string],
    -- Generic struct name -> index of its StructDecl in declarations
    generic_struct_decls: Map[string, int],
    -- Generic struct name -> "|"-joined type parameter names
    generic_struct_params: Map[string, string],
    -- Instance struct name (e.g. Table_string_i64) -> generic struct name
    generic_instances: Map[string, string],
    -- Instance struct name -> "|"-joined source type arguments
    generic_instance_args: Map[string, string]
}

fn ir_generator_new() -> IRGenerator {
    return IRGenerator {
        ir_mod: ir_module_new(),
        builder: ir_builder_new(),
        variable_map: Map_new(),
        variable_types: Map_new(),
        struct_defs: Map_new(),
        list_elem_kinds: Map_new(),
        list_elem_types: Map_new(),
        generic_fn_decls: Map_new(),
        mono_generated: Map_new(),
        type_substitutions: Map_new(),
        unused_match: -1,
        method_struct_map: Map_new(),
        static_fns: Map_new(),
        var_struct_types: Map_new(),
        mut_param_fns: Map_new(),
        enum_defs: Map_new(),
        var_enum_types: Map_new(),
        box_inner_types: Map_new(),
        trait_impl_methods: Map_new(),
        user_extern_fns: Map_new(),
        fn_return_types: Map_new(),
        field_list_elem_kinds: Map_new(),
        field_list_elem_types: Map_new(),
        mut_ptr_vars: Map_new(),
        param_ptr_map: Map_new(),
        label_counter: 0,
        lambda_counter: 0,
        break_target: "",
        continue_target: "",
        has_break_target: false,
        has_continue_target: false,
        current_return_type: "void",
        current_impl_type: "",
        has_impl_type: false,
        declarations: [],
        completed_blocks: [],
        var_type_annotations: Map_new(),
        known_functions: Map_new(),
        array_var_names: Map_new(),
        unsafe_depth: 0,
        region_depth: 0,
        tainted_vars: Map_new(),
        ref_vars: Map_new(),
        region_born_vars: Map_new(),
        declared_vars: [],
        region_marks: [],
        all_extern_fns: Map_new(),
        linked_fns: Map_new(),
        type_errors: [],
        checked_types: [],
        checked_type_args: [],
        checked_targets: [],
        checker: Box_null(),
        has_checker: false,
        debug_info: false,
        fn_src_return_types: Map_new(),
        fn_src_param_types: Map_new(),
        var_src_types: Map_new(),
        generic_struct_decls: Map_new(),
        generic_struct_params: Map_new(),
        generic_instances: Map_new(),
        generic_instance_args: Map_new()
    }
}

-- Record a compile error for an expression IR generation cannot resolve.
-- Generation continues with a placeholder so every error is reported.
-- The source types of a function's parameters, joined with "|"
fn param_src_types(fd: FunctionDecl) -> string {
    let mut joined = ""
    let mut i = 0
    while i < fd.params.len() {
        if i > 0 {
            joined = joined + "|"
        }
        joined = joined + fd.params[i].type_name
        i = i + 1
    }
    return joined
}

fn ir_type_error(gen: IRGenerator, msg: string, line: int) -> IRGenerator {
    let mut g = gen
    let text = error_at(g.ir_mod.files, msg, line)
    -- Not what the checker reported already for this instance of a
    -- generic function ("... at line 7 (in first[Point])")
    let mut i = 0
    while i < g.type_errors.len() {
        if starts_with(g.type_errors[i], text + " (in ") { return g }
        i = i + 1
    }
    g.type_errors.push(text)
    return g
}

-- ============================================================
-- LABEL GENERATION
-- ============================================================

fn gen_next_label(gen: IRGenerator, prefix: string) -> string {
    return prefix + "_" + int_to_string(gen.label_counter)
}

fn gen_advance_label(gen: IRGenerator) -> IRGenerator {
    let mut g = gen
    g.label_counter = g.label_counter + 1
    return g
}

fn gen_next_lambda_name(gen: IRGenerator) -> string {
    return "__lambda_" + int_to_string(gen.lambda_counter)
}

fn gen_advance_lambda(gen: IRGenerator) -> IRGenerator {
    let mut g = gen
    g.lambda_counter = g.lambda_counter + 1
    return g
}

-- ============================================================
-- TYPE MAPPING: AST type names -> IR type names
-- ============================================================

fn map_type_name(name: string) -> string {
    -- Primitive types
    if name == "int" { return "i64" }
    if name == "i8" { return "i8" }
    if name == "i16" { return "i16" }
    if name == "i32" { return "i32" }
    if name == "i64" { return "i64" }
    if name == "u8" { return "u8" }
    if name == "u16" { return "u16" }
    if name == "u32" { return "u32" }
    if name == "u64" { return "u64" }
    if name == "float" { return "f64" }
    if name == "f32" { return "f32" }
    if name == "f64" { return "f64" }
    if name == "bool" { return "bool" }
    if name == "string" { return "string" }
    if name == "void" { return "void" }
    -- SIMD types
    if name == "f32x4" { return "f32x4" }
    if name == "f32x8" { return "f32x8" }
    if name == "f64x2" { return "f64x2" }
    if name == "f64x4" { return "f64x4" }
    if name == "i32x4" { return "i32x4" }
    if name == "i32x8" { return "i32x8" }
    if name == "i64x2" { return "i64x2" }
    if name == "i64x4" { return "i64x4" }
    -- A function value is the address of its closure (see FUNCTION
    -- VALUES in ir_gen_expr.dm)
    if starts_with(name, "fn(") {
        return "ptr"
    }
    -- Default: return as-is (struct name, enum name, etc.)
    return name
}

-- ============================================================
-- GENERIC STRUCT INSTANCES
-- Each use of a generic struct with concrete type arguments,
-- such as Table[string, int], is an ordinary struct named
-- Table_string_i64 with the arguments substituted into its
-- fields. canon_type rewrites source types to those names;
-- ensure_type_instances (ir_gen_decl.dm) registers them.
-- ============================================================

-- Split "A, B[C, D], E" at its top-level commas
fn split_type_args(inner: string) -> List[string] {
    let mut out: List[string] = []
    let mut depth = 0
    let mut start = 0
    let mut i = 0
    while i < len(inner) {
        let c = char_at(inner, i)
        if c == "[" or c == "(" { depth = depth + 1 }
        if c == "]" or c == ")" { depth = depth - 1 }
        if c == "," and depth == 0 {
            out.push(string_trim(substr(inner, start, i - start)))
            start = i + 1
        }
        i = i + 1
    }
    let last = string_trim(substr(inner, start, len(inner) - start))
    if len(last) > 0 { out.push(last) }
    return out
}

-- A type argument as part of an instance name: List[int] -> List_i64
fn mangle_type_arg(t: string) -> string {
    -- Every function type is a pointer in the IR, and each is a type
    -- argument of its own: fn(int) -> bool is fn$int$to$bool
    if starts_with(t, "fn(") {
        let a = string_replace(string_replace(string_replace(t, " -> ", "$to$"), ", ", "$"), ",", "$")
        let b = string_replace(string_replace(string_replace(string_replace(a, "(", "$"), ")", ""), "[", "$"), "]", "")
        return string_replace(b, " ", "")
    }
    let m = map_type_name(t)
    return string_replace(string_replace(string_replace(string_replace(m, ", ", "_"), ",", "_"), "[", "_"), "]", "")
}

fn generic_instance_name(base: string, args: List[string]) -> string {
    let mut name = base
    let mut i = 0
    while i < args.len() {
        name = name + "_" + mangle_type_arg(args[i])
        i = i + 1
    }
    return name
}

-- Whether the name is a generic enum's, such as Opt for enum Opt[T]
fn is_generic_enum(gen: IRGenerator, name: string) -> bool {
    if gen.generic_struct_decls.contains(name) == false { return false }
    let decl = gen.declarations[gen.generic_struct_decls.get(name)]
    return decl.kind == DECL_ENUM()
}

-- A source type with type parameters substituted and the instances of
-- generic structs and enums named: Table[K, List[V]] -> Table_string_List_i64
fn canon_type(gen: IRGenerator, t: string) -> string {
    if gen.type_substitutions.contains(t) {
        return gen.type_substitutions.get(t)
    }
    let bracket = string_find(t, "[")
    if bracket <= 0 or ends_with(t, "]") == false {
        return t
    }
    let base = substr(t, 0, bracket)
    let raw_args = split_type_args(substr(t, bracket + 1, len(t) - bracket - 2))
    let mut args: List[string] = []
    let mut i = 0
    while i < raw_args.len() {
        args.push(canon_type(gen, raw_args[i]))
        i = i + 1
    }
    if gen.generic_struct_params.contains(base) {
        return generic_instance_name(base, args)
    }
    let mut out = base + "["
    i = 0
    while i < args.len() {
        if i > 0 { out = out + ", " }
        out = out + args[i]
        i = i + 1
    }
    return out + "]"
}

-- Substitute an instance's type arguments into a type written inside
-- its generic struct or impl: K -> string, Self -> Table_string_i64
fn instance_type(gen: IRGenerator, instance: string, t: string) -> string {
    if gen.generic_instances.contains(instance) == false {
        return canon_type(gen, t)
    }
    let mut g = gen
    g.type_substitutions = instance_subs(gen, instance)
    return canon_type(g, t)
}

-- Type parameter -> argument map of a generic struct instance
fn instance_subs(gen: IRGenerator, instance: string) -> Map[string, string] {
    let mut subs: Map[string, string] = Map_new()
    let base = gen.generic_instances.get(instance)
    let params = string_split(gen.generic_struct_params.get(base), "|")
    let args = string_split(gen.generic_instance_args.get(instance), "|")
    let mut i = 0
    while i < params.len() and i < args.len() {
        subs.insert(params[i], args[i])
        i = i + 1
    }
    subs.insert("Self", instance)
    subs.insert(base, instance)
    return subs
}

-- ============================================================
-- CHECKED TYPES
-- The checker (check.dm) finds the type of every expression
-- before code is generated, and records it at the expression's
-- id. It knows what the functions here cannot work out from an
-- expression alone: the type of None or Ok(x) from the place
-- the value goes to, the type of an `if` or a `match` from all
-- its branches, the result of a call of a generic function.
--
-- The body of a generic function, and of a method of a generic
-- struct or enum, is compiled once for each set of type
-- arguments: the checker checks it again for each, just before
-- the instance is generated, and its types are in force while
-- the instance is (INSTANCES in check.dm). It records nothing
-- for an expression made here, during code generation, which has
-- an id it never saw. And it does not keep the widths of numbers
-- apart where a value goes to a place: every integer type fits
-- every other, and so do f32 and float. So the type of a number
-- is worked out here (infer_expr_type), where a value is
-- converted to the type of its place (convert_int_value), and
-- checked_ir_type gives none for one. The types of a list
-- literal's elements, of a struct literal of a generic struct
-- and of the function of a generic type that is called on the
-- type are read from the checker alone (array_literal_elem_type,
-- struct_literal_type, generic_static_type in ir_gen_expr.dm).
-- ============================================================

-- The type the checker found for the expression, in source form
-- with generic structs as their instances (canon_type): "string",
-- "Option[int]", "Pair_i64_string". "" when it found none, or one
-- with a part it does not know.
fn checked_type(gen: IRGenerator, e: Expr) -> string {
    if e.id < 0 or e.id >= gen.checked_types.len() { return "" }
    let t = gen.checked_types[e.id]
    if t == "" or string_contains(t, "?") { return "" }
    return canon_type(gen, t)
}

-- What the type parameters of the generic function stand for in the
-- call, as the checker found them from the call's arguments: T is
-- "P" in first(ps) for fn first[T](xs: List[T]) and ps: List[P].
-- Empty when it did not find them all.
fn checked_type_args(gen: IRGenerator, call: Expr) -> List[string] {
    let none: List[string] = []
    if call.id < 0 or call.id >= gen.checked_type_args.len() { return none }
    let joined = gen.checked_type_args[call.id]
    if joined == "" { return none }
    return string_split(joined, "|")
}

-- What the checker found the method call to call, where it is no
-- method: "call:f" for x.f(a), which is f(x, a). "" for a method.
fn checked_target(gen: IRGenerator, call: Expr) -> string {
    if call.id < 0 or call.id >= gen.checked_targets.len() { return "" }
    let marks = gen.checked_targets[call.id]
    if starts_with(marks, "call:") == false { return "" }
    let end = string_find(marks, ";")
    if end < 0 { return marks }
    return substr(marks, 0, end)
}

-- "Dyn$Shape|Circle" for an expression whose value, a Circle, goes
-- where a `dyn Shape` is expected, as the checker found (ck_settle);
-- "" for any other expression
fn checked_dyn(gen: IRGenerator, e: Expr) -> string {
    if e.id < 0 or e.id >= gen.checked_targets.len() { return "" }
    let marks = gen.checked_targets[e.id]
    let at = string_find(marks, "dyn:")
    if at < 0 { return "" }
    return substr(marks, at + 4, len(marks) - at - 4)
}

-- The IR type of the expression's value as the checker found it. ""
-- when it found none, for a number (see CHECKED TYPES), and for the
-- types that have another form in the IR than their name.
fn checked_ir_type(gen: IRGenerator, e: Expr) -> string {
    let t = checked_type(gen, e)
    if t == "" or t == "void" or starts_with(t, "[") or starts_with(t, "Future[") { return "" }
    let ir_type = map_type_name(t)
    if is_integer_type(ir_type) or is_float_type(ir_type) { return "" }
    return ir_type
}

-- The type of the value of an `if`, a `match` or a block: the
-- checker's, and `inferred` where it gives none, as in the body of a
-- generic function. `inferred` is what is found here from the first
-- branch, before the branch's own variables are declared: a name the
-- branch binds may still have the type an earlier binding of that
-- name left (`Some(v)` in one match after `Some(v)` in another), and
-- the type is "i64" where nothing is known. For a number the
-- checker's type is the one the branches meet at (int_meet_type):
-- `match o { Some(x) => x, None => 0 }` with o: Option[i32] is an
-- i32. Each branch's value is converted to it where it is stored
-- (store_branch_value in ir_gen_expr.dm).
fn checked_slot_type(gen: IRGenerator, e: Expr, inferred: string) -> string {
    let checked = checked_ir_type(gen, e)
    if checked != "" { return checked }
    let number = map_type_name(checked_type(gen, e))
    if is_integer_type(number) or is_float_type(number) { return number }
    return inferred
}

fn is_integer_type(name: string) -> bool {
    if name == "i8" or name == "i16" or name == "i32" or name == "i64" { return true }
    if name == "u8" or name == "u16" or name == "u32" or name == "u64" { return true }
    return false
}

fn is_float_type(name: string) -> bool {
    return name == "f32" or name == "f64"
}

fn is_signed_type(name: string) -> bool {
    return name == "i8" or name == "i16" or name == "i32" or name == "i64"
}

fn is_unsigned_type(name: string) -> bool {
    return name == "u8" or name == "u16" or name == "u32" or name == "u64"
}

fn type_bit_width(name: string) -> int {
    if name == "i8" or name == "u8" { return 8 }
    if name == "i16" or name == "u16" { return 16 }
    if name == "i32" or name == "u32" { return 32 }
    if name == "i64" or name == "u64" { return 64 }
    if name == "f32" { return 32 }
    if name == "f64" { return 64 }
    return 64
}

-- The one type two integers of types a and b meet at, as the two
-- operands of an operator or as two branches that give one value: an
-- integer literal takes the other's type, as in `fd + 1` with fd: i32
-- and in `match o { Some(x) => x, None => 0 }`; otherwise the narrower
-- one widens, and of two types of one width it is the first. The
-- types are in source or IR form. The checker types an expression by
-- this and code generation converts the values by it.
fn int_meet_type(a: string, a_literal: bool, b: string, b_literal: bool) -> string {
    if a == b { return a }
    if is_float_name(a) and is_float_name(b) { return float_meet_type(a, a_literal, b, b_literal) }
    if b_literal and a_literal == false { return a }
    if a_literal and b_literal == false { return b }
    if type_bit_width(b) > type_bit_width(a) { return b }
    return a
}

-- The type two floats of different types meet at, in a binary
-- operator: a literal takes the other operand's type, as in `x / 2.0`
-- with x: f32; otherwise the f32 widens. For the names of the IR
-- ("f64") and of the checker ("float").
fn float_meet_type(a: string, a_literal: bool, b: string, b_literal: bool) -> string {
    if a == b { return a }
    if b_literal and a_literal == false { return a }
    if a_literal and b_literal == false { return b }
    if a == "f32" { return b }
    return a
}

-- A float type by its name in the IR or in the checker
fn is_float_name(t: string) -> bool {
    return t == "f32" or t == "f64" or t == "float"
}

-- Whether the two types are both integers or both floats, by their
-- names in the IR or in the checker: numbers that meet at one type
-- (int_meet_type)
fn numbers_meet(a: string, b: string) -> bool {
    if is_float_name(a) { return is_float_name(b) }
    if is_float_name(b) { return false }
    return (a == "int" or is_integer_type(a)) and (b == "int" or is_integer_type(b))
}

-- An integer or a float constant (is_int_literal_expr, is_float_literal_expr)
fn is_number_literal_expr(e: Expr) -> bool {
    return is_int_literal_expr(e) or is_float_literal_expr(e)
}

-- Whether the expression is a float constant: a literal, one negated
-- or in parentheses, or the sum, difference, product or quotient of two
fn is_float_literal_expr(e: Expr) -> bool {
    if e.kind == EXPR_LITERAL_FLOAT() { return true }
    if e.kind == EXPR_GROUPED() { return is_float_literal_expr(*e.operand) }
    if e.kind == EXPR_UNARY() and e.op == UNOP_NEG() { return is_float_literal_expr(*e.operand) }
    if e.kind == EXPR_BINARY() and (e.op == BINOP_ADD() or e.op == BINOP_SUB() or e.op == BINOP_MUL() or e.op == BINOP_DIV()) {
        return is_float_literal_expr(*e.left) and is_float_literal_expr(*e.right)
    }
    return false
}

fn is_simd_type(name: string) -> bool {
    if name == "f32x4" or name == "f32x8" { return true }
    if name == "f64x2" or name == "f64x4" { return true }
    if name == "i32x4" or name == "i32x8" { return true }
    if name == "i64x2" or name == "i64x4" { return true }
    return false
}

-- ============================================================
-- DEFAULT VALUE FOR TYPE
-- ============================================================

fn default_value_for_type(type_name: string) -> IRValue {
    if type_name == "i64" or type_name == "i32" or type_name == "i16" or type_name == "i8" {
        return ir_val_int(0)
    }
    if type_name == "u64" or type_name == "u32" or type_name == "u16" or type_name == "u8" {
        return ir_val_int(0)
    }
    if type_name == "f64" or type_name == "f32" {
        return ir_val_float(0.0)
    }
    if type_name == "bool" {
        return ir_val_bool(false)
    }
    if type_name == "string" {
        return ir_val_string("")
    }
    return ir_val_int(0)
}

-- ============================================================
-- PARSE TYPE STRING
-- Handles composite types like List[int], Option[string], etc.
-- Returns the IR type name
-- ============================================================

fn parse_type_to_ir(type_str: string) -> string {
    -- Check for generic types
    if starts_with(type_str, "List[") {
        return "list"
    }
    if starts_with(type_str, "Box[") {
        return "ptr"
    }
    if starts_with(type_str, "Option[") {
        return "option"
    }
    if starts_with(type_str, "Result[") {
        return "result"
    }
    if starts_with(type_str, "Future[") {
        return "future"
    }
    return map_type_name(type_str)
}

-- Extract inner type from "List[T]", "Option[T]", "Box[T]", etc.
fn extract_inner_type(type_str: string, prefix: string) -> string {
    let prefix_len = len(prefix)
    let total_len = len(type_str)
    if total_len <= prefix_len + 1 {
        return "i64"
    }
    -- Extract between prefix and closing ]
    return substr(type_str, prefix_len, total_len - prefix_len - 1)
}

-- Determine list element kind from type name
fn classify_list_elem(elem_type: string) -> int {
    if elem_type == "int" or elem_type == "i64" { return LIST_ELEM_INT() }
    if elem_type == "float" or elem_type == "f64" { return LIST_ELEM_FLOAT() }
    if elem_type == "string" or elem_type == "String" { return LIST_ELEM_STRING() }
    return LIST_ELEM_OTHER()
}

-- ============================================================
-- COMPILE-TIME SAFETY CHECKS (Stage 0 Phase 2 rules)
-- Cast legality, unsafe fence, region escape. There is no
-- separate checker pass: the checks run inside this typed IR
-- walk, where variable types, struct definitions, and the
-- extern registry already live. A violation prints a
-- source-located diagnostic and exits before LLVM emission.
-- The rules of the Zig compiler's checker (inferCast, unsafe fence
-- provenance, REGION ESCAPE RULE).
-- ============================================================

-- Unwrap parenthesized (grouped) expressions
fn safety_unwrap_grouped(e: Expr) -> Expr {
    let mut cur = e
    while cur.kind == EXPR_GROUPED() {
        cur = *cur.operand
    }
    return cur
}

-- Normalize a source or IR type string for cast classification:
-- composite containers collapse to their erased IR kinds
fn safety_normalize_type(t: string) -> string {
    if starts_with(t, "dm_list_") { return "list" }
    return parse_type_to_ir(t)
}

fn safety_is_numeric_type(t: string) -> bool {
    return is_integer_type(t) or is_float_type(t)
}

-- Human-readable type name for cast diagnostics (Stage 0 spellings)
fn safety_display_name(t: string) -> string {
    let m = map_type_name(t)
    if m == "i64" { return "int" }
    if m == "u64" { return "uint" }
    if m == "f64" { return "float" }
    return t
}

-- Cast legality, mirroring Stage 0 inferCast: same-type casts and
-- numeric <-> numeric casts are legal, `as dyn Trait` is what the
-- checker allowed, and every other combination is a compile error.
-- Reference/Box identity is judged on the tracked inner types;
-- a Box erased to a bare "ptr" with unknown inner type is
-- unprovable and passes, as do "void" inference gaps.
fn check_cast_legal(gen: IRGenerator, operand: Expr, from_type: string, cast_type: string, cast_is_ref: bool, line: int) {
    if starts_with(cast_type, DYN_PREFIX()) { return }

    -- Source provenance for plain-variable operands
    let mut src_box_inner = ""
    let mut src_is_ref = false
    let inner = safety_unwrap_grouped(operand)
    if inner.kind == EXPR_IDENTIFIER() {
        if gen.box_inner_types.contains(inner.name) {
            src_box_inner = gen.box_inner_types.get(inner.name)
        }
        if gen.ref_vars.contains(inner.name) and gen.ref_vars.get(inner.name) {
            src_is_ref = true
        }
    }
    if inner.kind == EXPR_CAST() and inner.cast_is_ref {
        src_is_ref = true
    }

    let src_ir = safety_normalize_type(from_type)
    let dst_ir = safety_normalize_type(cast_type)
    let src_numeric = safety_is_numeric_type(src_ir)
    let dst_numeric = safety_is_numeric_type(dst_ir)
    let dst_is_box = starts_with(cast_type, "Box[")

    let mut legal = false
    if cast_is_ref {
        -- Target &T: only a reference to the same pointee type may cast
        if src_is_ref and src_ir == dst_ir { legal = true }
    } else if dst_is_box {
        let dst_inner = extract_inner_type(cast_type, "Box[")
        if src_box_inner != "" and src_box_inner == dst_inner { legal = true }
        if src_ir == "ptr" and src_box_inner == "" { legal = true }
    } else if src_is_ref {
        -- &T to a non-reference type forges a pointer/integer
        legal = false
    } else if src_box_inner != "" {
        -- Box[T] to a non-Box type forges a pointer/integer
        legal = false
    } else if src_numeric and dst_numeric {
        legal = true
    } else if src_ir == dst_ir {
        legal = true
    } else if src_ir == "void" or dst_ir == "void" {
        legal = true
    }

    if legal == false {
        let mut src_disp = safety_display_name(from_type)
        if src_box_inner != "" { src_disp = "Box[" + src_box_inner + "]" }
        if src_is_ref { src_disp = "&" + safety_display_name(from_type) }
        let mut dst_disp = safety_display_name(cast_type)
        if cast_is_ref { dst_disp = "&" + safety_display_name(cast_type) }
        eprintln(error_at(gen.ir_mod.files, "invalid cast from " + src_disp + " to " + dst_disp + ": only numeric to numeric casts are allowed", line))
        exit(1)
    }
}

-- Unsafe fence provenance (name-based taint tracking, mirroring
-- Stage 0): an extern call result or a cast to a reference type is
-- a taint source; taint propagates through plain copies.
fn expr_is_taint_source(gen: IRGenerator, e: Expr) -> bool {
    let inner = safety_unwrap_grouped(e)
    if inner.kind == EXPR_FUNCTION_CALL() {
        let callee = safety_unwrap_grouped(*inner.callee)
        if callee.kind == EXPR_IDENTIFIER() {
            return gen.all_extern_fns.contains(callee.name) and gen.all_extern_fns.get(callee.name)
        }
        return false
    }
    if inner.kind == EXPR_CAST() {
        return inner.cast_is_ref
    }
    return false
}

fn expr_is_tainted_ref(gen: IRGenerator, e: Expr) -> bool {
    let inner = safety_unwrap_grouped(e)
    if inner.kind == EXPR_IDENTIFIER() {
        return gen.tainted_vars.contains(inner.name) and gen.tainted_vars.get(inner.name)
    }
    return expr_is_taint_source(gen, inner)
}

-- True if the expression's value has a reference type (&T) as far as
-- the erased type strings can tell: a cast to &T or a known &T var
fn expr_is_ref_value(gen: IRGenerator, e: Expr) -> bool {
    let inner = safety_unwrap_grouped(e)
    if inner.kind == EXPR_IDENTIFIER() {
        return gen.ref_vars.contains(inner.name) and gen.ref_vars.get(inner.name)
    }
    if inner.kind == EXPR_CAST() {
        return inner.cast_is_ref
    }
    return false
}

-- Unsafe fence: calling an `extern fn` requires an unsafe block
fn check_extern_fence(gen: IRGenerator, fn_name: string, line: int) {
    if gen.unsafe_depth == 0 and gen.all_extern_fns.contains(fn_name) and gen.all_extern_fns.get(fn_name) {
        eprintln(error_at(gen.ir_mod.files, "extern function call '" + fn_name + "' requires unsafe block", line))
        exit(1)
    }
}

-- Region escape rule (name-based region-born tracking, mirroring
-- Stage 0): string concat/interpolation results, List/Map literals
-- and constructors (List_new/Map_new), Box_new, and values bound
-- from those inside a named region must not escape it. Declaration
-- scopes are approximated by the declared_vars log: a name is
-- "outside" the innermost region when its last recorded
-- declaration (parameter, let, or for variable) precedes the
-- region-entry mark. Match-arm bindings are not logged.
fn var_declared_outside_innermost_region(gen: IRGenerator, name: string) -> bool {
    if gen.region_marks.len() == 0 { return false }
    let mark = gen.region_marks[gen.region_marks.len() - 1]
    let mut last = -1
    let mut i = 0
    while i < gen.declared_vars.len() {
        if gen.declared_vars[i] == name { last = i }
        i = i + 1
    }
    if last < 0 { return false }
    return last < mark
}

-- Base identifier of a field/index chain (a.b[i].c -> a), else ""
fn root_identifier_name(e: Expr) -> string {
    let mut cur = safety_unwrap_grouped(e)
    let mut cont = true
    while cont {
        if cur.kind == EXPR_IDENTIFIER() {
            return cur.name
        } else if cur.kind == EXPR_FIELD_ACCESS() or cur.kind == EXPR_INDEX_ACCESS() {
            cur = safety_unwrap_grouped(*cur.object)
        } else {
            cont = false
        }
    }
    return ""
}

fn expr_is_region_born(gen: IRGenerator, e: Expr) -> bool {
    if gen.region_depth == 0 { return false }
    let inner = safety_unwrap_grouped(e)
    if inner.kind == EXPR_IDENTIFIER() {
        let born = gen.region_born_vars.contains(inner.name) and gen.region_born_vars.get(inner.name)
        return born and var_declared_outside_innermost_region(gen, inner.name) == false
    }
    if inner.kind == EXPR_STRING_INTERP() { return true }
    if inner.kind == EXPR_ARRAY_LITERAL() { return true }
    if inner.kind == EXPR_BINARY() {
        if inner.op != BINOP_ADD() { return false }
        if infer_expr_type(gen, inner) == "string" { return true }
        let bl = *inner.left
        let br = *inner.right
        if safety_unwrap_grouped(bl).kind == EXPR_LITERAL_STRING() { return true }
        if safety_unwrap_grouped(br).kind == EXPR_LITERAL_STRING() { return true }
        return expr_is_region_born(gen, bl) or expr_is_region_born(gen, br)
    }
    if inner.kind == EXPR_FUNCTION_CALL() {
        let callee = safety_unwrap_grouped(*inner.callee)
        if callee.kind == EXPR_IDENTIFIER() {
            return callee.name == "List_new" or callee.name == "Map_new" or callee.name == "Box_new"
        }
        return false
    }
    if inner.kind == EXPR_COMPTIME() or inner.kind == EXPR_CAST() {
        return expr_is_region_born(gen, *inner.operand)
    }
    if inner.kind == EXPR_BLOCK() {
        if inner.has_result { return expr_is_region_born(gen, *inner.result) }
        if inner.stmts.len() > 0 {
            let last_stmt = inner.stmts[inner.stmts.len() - 1]
            if last_stmt.kind == STMT_EXPRESSION() {
                return expr_is_region_born(gen, *last_stmt.expr)
            }
        }
        return false
    }
    if inner.kind == EXPR_IF() {
        if expr_is_region_born(gen, *inner.then_branch) { return true }
        if inner.has_else { return expr_is_region_born(gen, *inner.else_branch) }
        return false
    }
    return false
}

-- Record fence provenance for `name` bound/assigned from `value`
fn safety_track_assignment(gen: IRGenerator, name: string, value: Expr) -> IRGenerator {
    let mut g = gen
    g.tainted_vars.insert(name, expr_is_tainted_ref(g, value))
    g.ref_vars.insert(name, expr_is_ref_value(g, value))
    g.region_born_vars.insert(name, expr_is_region_born(g, value))
    return g
}

-- ============================================================
-- BUILTIN NAME MAPPING
-- Maps dAImond builtin names to C runtime function names
-- ============================================================

fn map_builtin_name(name: string) -> string {
    if name == "print" { return "dm_print" }
    if name == "println" { return "dm_println" }
    if name == "eprint" { return "dm_eprint" }
    if name == "eprintln" { return "dm_eprintln" }
    if name == "panic" { return "dm_panic" }
    if name == "exit" { return "dm_libc_exit" }
    if name == "len" { return "dm_string_len" }
    if name == "char_at" { return "dm_char_at" }
    if name == "substr" { return "dm_substr" }
    if name == "int_to_string" { return "dm_int_to_string" }
    if name == "float_to_string" { return "dm_float_to_string" }
    if name == "bool_to_string" { return "dm_bool_to_string" }
    if name == "parse_int" { return "dm_parse_int" }
    if name == "parse_float" { return "dm_parse_float" }
    if name == "string_contains" { return "dm_string_contains" }
    if name == "string_find" { return "dm_string_find" }
    if name == "starts_with" { return "dm_starts_with" }
    if name == "ends_with" { return "dm_ends_with" }
    if name == "string_trim" { return "dm_string_trim" }
    if name == "string_replace" { return "dm_string_replace" }
    if name == "string_to_upper" { return "dm_string_to_upper" }
    if name == "string_to_lower" { return "dm_string_to_lower" }
    if name == "string_split" { return "dm_string_split" }
    if name == "file_read" { return "dm_file_read" }
    if name == "file_write" { return "dm_file_write" }
    if name == "file_append" { return "dm_file_append" }
    if name == "file_exists" { return "dm_file_exists" }
    if name == "read_line" { return "dm_read_line" }
    if name == "args_len" { return "dm_args_len" }
    if name == "args_get" { return "dm_args_get" }
    if name == "mem_mark" { return "dm_mem_mark" }
    if name == "mem_sweep" { return "dm_mem_sweep" }
    if name == "system" { return "dm_system" }
    if name == "run_command" or name == "run_command_io" { return "dm_run_command" }
    if name == "assert" { return "dm_assert" }
    if name == "assert_eq" { return "dm_assert_eq" }
    -- Character classification
    if name == "is_alpha" { return "dm_is_alpha" }
    if name == "is_digit" { return "dm_is_digit" }
    if name == "is_whitespace" { return "dm_is_whitespace" }
    if name == "is_alnum" { return "dm_is_alnum" }
    -- Character/string conversion
    if name == "char_to_string" { return "dm_char_to_string" }
    if name == "string_to_int" { return "dm_parse_int" }
    -- Path utilities
    if name == "path_dirname" { return "dm_path_dirname" }
    if name == "path_basename" { return "dm_path_basename" }
    if name == "path_extension" { return "dm_path_extension" }
    if name == "path_stem" { return "dm_path_stem" }
    if name == "path_join" { return "dm_path_join" }
    -- Filesystem builtins
    if name == "fs_getcwd" { return "dm_fs_getcwd" }
    if name == "fs_readdir" { return "dm_fs_readdir" }
    if name == "fs_rename" { return "dm_fs_rename" }
    if name == "fs_mkdir" { return "dm_fs_mkdir" }
    if name == "fs_remove" { return "dm_fs_remove" }
    -- OS builtins
    if name == "os_getenv" or name == "env_get" { return "dm_os_getenv" }
    -- Terminal and time builtins
    if name == "chr" { return "dm_chr" }
    if name == "flush" { return "dm_flush" }
    if name == "read_key" { return "dm_read_key" }
    if name == "read_key_nb" { return "dm_read_key_nb" }
    if name == "term_width" { return "dm_term_width" }
    if name == "term_height" { return "dm_term_height" }
    if name == "time_ms" { return "dm_time_ms" }
    if name == "sleep_ms" { return "dm_sleep_ms" }
    -- Network and thread builtins
    if name == "tcp_listen" { return "dm_tcp_listen" }
    if name == "tcp_accept" { return "dm_tcp_accept" }
    if name == "tcp_connect" { return "dm_tcp_connect" }
    if name == "tcp_read" { return "dm_tcp_read" }
    if name == "tcp_write" { return "dm_tcp_write" }
    if name == "tcp_close" { return "dm_tcp_close" }
    if name == "thread_spawn" { return "dm_thread_spawn" }
    if name == "thread_join" { return "dm_thread_join" }
    if name == "mutex_new" { return "dm_mutex_new" }
    if name == "mutex_lock" { return "dm_mutex_lock" }
    if name == "mutex_unlock" { return "dm_mutex_unlock" }
    if name == "mutex_destroy" { return "dm_mutex_destroy" }
    return name
}

-- ============================================================
-- RAW MEMORY PRIMITIVES
-- Builtins for code that manages memory itself, such as the runtime.
-- Each one needs an unsafe block. Addresses are ints.
--   load_T(addr) and store_T(addr, v), where T is u8, i8, u16, i16,
--     u32, i32, i64, f32 or f64. Integer loads widen to int (zero- or
--     sign-extended by T); float loads widen to float. Stores narrow v.
--   addr_of(place): the address of a variable, field or list element
--   size_of[T](): the size of type T in bytes
--   string_data(s): the address of a string's bytes, which a NUL byte
--     follows unless string_from_raw made the string without one
--   string_from_raw(ptr, len, cap): the string at ptr; cap 0 marks
--     memory the string does not own
--   symbol_addr("name"): the address of the data symbol `name`, such
--     as a C library's global variable
--   static_addr("name", size): the address of `size` bytes that belong
--     to the program, are zero when it starts and last until it ends.
--     Every use of one name gives the same address.
-- ============================================================

-- The memory type a load_T or store_T builtin accesses, or ""
fn raw_mem_type(name: string) -> string {
    let mut t = ""
    if starts_with(name, "load_") {
        t = substr(name, 5, len(name) - 5)
    } else if starts_with(name, "store_") {
        t = substr(name, 6, len(name) - 6)
    } else {
        return ""
    }
    if t == "u8" or t == "i8" or t == "u16" or t == "i16" or t == "u32" or t == "i32" { return t }
    if t == "i64" or t == "f32" or t == "f64" { return t }
    return ""
}

fn is_raw_builtin(name: string) -> bool {
    if raw_mem_type(name) != "" { return true }
    return name == "addr_of" or name == "size_of" or name == "string_data" or name == "string_from_raw" or name == "symbol_addr" or name == "static_addr"
}

fn raw_builtin_return_type(name: string) -> string {
    if starts_with(name, "store_") { return "void" }
    if name == "string_from_raw" { return "string" }
    if name == "load_f32" or name == "load_f64" { return "f64" }
    return "i64"
}

-- "no arguments", "1 argument", "2 arguments"
fn arguments_text(count: int) -> string {
    if count == 0 { return "no arguments" }
    if count == 1 { return "1 argument" }
    return int_to_string(count) + " arguments"
}

-- The effect a function must declare to call the builtin, or ""
fn builtin_required_effect(name: string) -> string {
    if name == "file_read" or name == "file_write" or name == "file_append" or name == "file_exists" { return "FileSystem" }
    if name == "println" or name == "print" or name == "eprintln" or name == "eprint" or name == "read_line" { return "Console" }
    if name == "read_key" or name == "read_key_nb" { return "Console" }
    if name == "args_get" or name == "args_len" { return "IO" }
    if name == "flush" or name == "term_width" or name == "term_height" or name == "time_ms" or name == "sleep_ms" { return "IO" }
    if name == "system" or name == "exit" or name == "run_command" or name == "run_command_io" { return "Process" }
    return ""
}

fn is_builtin_fn(name: string) -> bool {
    if name == "print" or name == "println" { return true }
    if name == "eprint" or name == "eprintln" { return true }
    if name == "panic" or name == "exit" { return true }
    if name == "len" or name == "char_at" or name == "substr" { return true }
    if name == "int_to_string" or name == "float_to_string" or name == "bool_to_string" { return true }
    if name == "parse_int" or name == "parse_float" { return true }
    if name == "string_contains" or name == "string_find" { return true }
    if name == "starts_with" or name == "ends_with" { return true }
    if name == "string_trim" or name == "string_replace" { return true }
    if name == "string_to_upper" or name == "string_to_lower" { return true }
    if name == "string_split" { return true }
    if name == "file_read" or name == "file_write" or name == "file_append" or name == "file_exists" { return true }
    if name == "read_line" { return true }
    if name == "args_len" or name == "args_get" { return true }
    if name == "mem_mark" or name == "mem_sweep" { return true }
    if name == "system" { return true }
    if name == "run_command" or name == "run_command_io" { return true }
    if name == "assert" or name == "assert_eq" { return true }
    if name == "Box_new" or name == "Box_null" { return true }
    if name == "Map_new" { return true }
    if name == "byte_at" or name == "hash_key" { return true }
    if is_raw_builtin(name) { return true }
    if name == "is_alpha" or name == "is_digit" or name == "is_whitespace" or name == "is_alnum" { return true }
    if name == "char_to_string" { return true }
    if name == "string_to_int" { return true }
    if name == "path_dirname" or name == "path_basename" or name == "path_extension" or name == "path_stem" { return true }
    if name == "path_join" { return true }
    if name == "fs_getcwd" or name == "fs_readdir" or name == "fs_rename" { return true }
    if name == "fs_mkdir" or name == "fs_remove" { return true }
    if name == "os_getenv" or name == "env_get" { return true }
    if name == "chr" or name == "flush" or name == "read_key" or name == "read_key_nb" { return true }
    if name == "term_width" or name == "term_height" or name == "time_ms" or name == "sleep_ms" { return true }
    if name == "tcp_listen" or name == "tcp_accept" or name == "tcp_connect" { return true }
    if name == "tcp_read" or name == "tcp_write" or name == "tcp_close" { return true }
    if name == "thread_spawn" or name == "thread_join" { return true }
    if name == "mutex_new" or name == "mutex_lock" or name == "mutex_unlock" or name == "mutex_destroy" { return true }
    return false
}

-- Get the return type of a builtin function
fn builtin_return_type(name: string) -> string {
    if name == "print" or name == "println" { return "void" }
    if name == "eprint" or name == "eprintln" { return "void" }
    if name == "panic" { return "void" }
    if name == "exit" { return "void" }
    if name == "len" { return "i64" }
    if name == "byte_at" or name == "hash_key" { return "i64" }
    if is_raw_builtin(name) { return raw_builtin_return_type(name) }
    if name == "Box_new" or name == "Box_null" { return "ptr" }
    if name == "char_at" { return "string" }
    if name == "substr" { return "string" }
    if name == "int_to_string" { return "string" }
    if name == "float_to_string" { return "string" }
    if name == "bool_to_string" { return "string" }
    if name == "parse_int" { return "i64" }
    if name == "parse_float" { return "f64" }
    if name == "string_contains" { return "bool" }
    if name == "string_find" { return "i64" }
    if name == "starts_with" or name == "ends_with" { return "bool" }
    if name == "string_trim" { return "string" }
    if name == "string_replace" { return "string" }
    if name == "string_to_upper" or name == "string_to_lower" { return "string" }
    if name == "string_split" { return "list" }
    if name == "file_read" { return "string" }
    if name == "file_write" or name == "file_append" { return "void" }
    if name == "file_exists" { return "bool" }
    if name == "read_line" { return "string" }
    if name == "args_len" { return "i64" }
    if name == "args_get" { return "string" }
    if name == "mem_mark" { return "i64" }
    if name == "mem_sweep" { return "void" }
    if name == "system" { return "i64" }
    if name == "run_command" or name == "run_command_io" { return "i64" }
    if name == "assert" or name == "assert_eq" { return "void" }
    -- Character classification
    if name == "is_alpha" or name == "is_digit" or name == "is_whitespace" or name == "is_alnum" { return "bool" }
    -- Character/string conversion
    if name == "char_to_string" { return "string" }
    if name == "string_to_int" { return "i64" }
    -- Path utilities
    if name == "path_dirname" or name == "path_basename" or name == "path_extension" or name == "path_stem" { return "string" }
    if name == "path_join" { return "string" }
    -- Filesystem builtins
    if name == "fs_getcwd" { return "string" }
    if name == "fs_readdir" { return "string" }
    if name == "fs_rename" { return "i64" }
    if name == "fs_mkdir" { return "i64" }
    if name == "fs_remove" { return "i64" }
    -- OS builtins
    if name == "os_getenv" or name == "env_get" { return "string" }
    -- Terminal and time builtins
    if name == "chr" { return "string" }
    if name == "flush" or name == "sleep_ms" { return "void" }
    if name == "read_key" or name == "read_key_nb" { return "i64" }
    if name == "term_width" or name == "term_height" or name == "time_ms" { return "i64" }
    -- Network and thread builtins
    if name == "tcp_listen" or name == "tcp_accept" or name == "tcp_connect" or name == "tcp_write" { return "i64" }
    if name == "tcp_read" { return "string" }
    if name == "thread_spawn" or name == "mutex_new" { return "i64" }
    return "void"
}

-- ============================================================
-- GENERATE MODULE: Entry point for IR generation from AST
-- ============================================================

fn generate_module(gen: IRGenerator, parsed: SourceFile) -> IRGenerator {
    let mut g = gen
    -- IR generation knows a function's variables by name
    let mut source = parsed
    source.declarations = unshadowed_declarations(parsed.declarations)
    g.declarations = source.declarations
    g.ir_mod.files = source.files

    -- Phase 1: Register all struct and enum definitions. Generic structs
    -- and enums are recorded first; each use with type arguments
    -- registers an instance (ensure_type_instances).
    let mut i = 0
    while i < source.declarations.len() {
        let decl = source.declarations[i]
        let mut generic_name = ""
        let mut generic_params: List[string] = []
        if decl.kind == DECL_STRUCT() {
            let sd = *decl.struct_decl
            generic_name = sd.name
            generic_params = sd.generic_params
        } else if decl.kind == DECL_ENUM() {
            let ed = *decl.enum_decl
            generic_name = ed.name
            generic_params = ed.generic_params
        }
        if generic_params.len() > 0 {
            g.generic_struct_decls.insert(generic_name, i)
            let mut params = ""
            let mut pi = 0
            while pi < generic_params.len() {
                if pi > 0 { params = params + "|" }
                params = params + generic_params[pi]
                pi = pi + 1
            }
            g.generic_struct_params.insert(generic_name, params)
        }
        i = i + 1
    }
    i = 0
    while i < source.declarations.len() {
        let decl = source.declarations[i]
        if decl.kind == DECL_STRUCT() {
            let sd = *decl.struct_decl
            if sd.generic_params.len() == 0 {
                g = register_struct(g, sd)
            }
        } else if decl.kind == DECL_ENUM() {
            let ed = *decl.enum_decl
            if ed.generic_params.len() == 0 {
                g = register_enum(g, ed)
            }
        }
        i = i + 1
    }

    -- Phase 2: Register all function signatures (forward declarations)
    i = 0
    while i < source.declarations.len() {
        let decl = source.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            g.known_functions.insert(fd.name, true)
            if fd.is_extern {
                g.all_extern_fns.insert(fd.name, true)
            }
            if fd.generic_params.len() > 0 {
                -- Store generic function for later monomorphization
                g.generic_fn_decls.insert(fd.name, i)
            }
            -- Generic struct instances in the signature
            let mut ret_src = fd.return_type
            if fd.generic_params.len() == 0 {
                g = ensure_type_instances(g, ret_src, fd.line)
                ret_src = canon_type(g, ret_src)
                let mut si = 0
                while si < fd.params.len() {
                    g = ensure_type_instances(g, fd.params[si].type_name, fd.line)
                    si = si + 1
                }
            }
            -- Track return type for all functions
            let ret = if len(ret_src) > 0 {
                parse_type_to_ir(ret_src)
            } else {
                "void"
            }
            g.fn_return_types.insert(fd.name, ret)
            g.fn_src_return_types.insert(fd.name, ret_src)
            g.fn_src_param_types.insert(fd.name, param_src_types(fd))
            -- Mut params change how callers pass arguments, so calls that
            -- precede the declaration must know about them too
            let mut mut_flags = ""
            let mut pi = 0
            while pi < fd.params.len() {
                if pi > 0 { mut_flags = mut_flags + "|" }
                if fd.params[pi].is_mut {
                    mut_flags = mut_flags + "1"
                } else {
                    mut_flags = mut_flags + "0"
                }
                pi = pi + 1
            }
            if string_contains(mut_flags, "1") {
                g.mut_param_fns.insert(fd.name, mut_flags)
            }
        } else if decl.kind == DECL_IMPL() {
            let ib = *decl.impl_block
            g = register_impl_methods(g, ib)
            -- Track return types for impl methods
            let mut mi = 0
            while mi < ib.methods.len() {
                let method = ib.methods[mi]
                let method_name = ib.target_type + "_" + method.name
                let mut msrc = if method.return_type == "Self" { ib.target_type } else { method.return_type }
                if ib.generic_params.len() == 0 {
                    g = ensure_type_instances(g, msrc, method.line)
                    msrc = canon_type(g, msrc)
                    let mut si = 0
                    while si < method.params.len() {
                        g = ensure_type_instances(g, method.params[si].type_name, method.line)
                        si = si + 1
                    }
                }
                let mret = if len(msrc) > 0 {
                    parse_type_to_ir(msrc)
                } else {
                    "void"
                }
                g.fn_return_types.insert(method_name, mret)
                g.fn_src_return_types.insert(method_name, msrc)
                g.fn_src_param_types.insert(method_name, param_src_types(method))
                mi = mi + 1
            }
        }
        i = i + 1
    }

    -- Phase 3: Declare runtime builtin functions
    g = declare_runtime_functions(g)

    -- The generic instances among the types of the program's expressions
    g = ensure_checked_instances(g, 0, g.checked_types.len())

    -- Phase 4: Generate IR for all declarations
    i = 0
    while i < source.declarations.len() {
        let decl = source.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.generic_params.len() == 0 {
                g = generate_function(g, fd)
            }
        } else if decl.kind == DECL_IMPL() {
            let ib = *decl.impl_block
            g = generate_impl_block(g, ib)
        } else if decl.kind == DECL_CONST() {
            let cd = *decl.const_decl
            g = generate_constant(g, cd)
        }
        i = i + 1
    }

    return g
}

-- ============================================================
-- FORWARD DECLARATIONS (stubs - implemented in ir_gen_decl.dm)
-- ============================================================

-- These will be defined in the split module files
-- Using the import concatenation system, they are visible

-- register_struct: from ir_gen_decl.dm
-- register_enum: from ir_gen_decl.dm
-- register_impl_methods: from ir_gen_decl.dm
-- declare_runtime_functions: from ir_gen_decl.dm
-- generate_function: from ir_gen_decl.dm
-- generate_impl_block: from ir_gen_decl.dm
-- generate_constant: from ir_gen_decl.dm
