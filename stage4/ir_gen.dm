module ir_gen

import ast
import ir
import ir_builder

-- ============================================================
-- IR Generator Core
-- Central struct and initialization, type mapping
-- Port of stage3/src/ir_gen.zig IRGenerator struct
-- ============================================================

-- Struct info for tracking registered structs
struct StructInfo {
    name: string,
    field_names: List[string],
    field_types: List[string]
}

fn struct_info_new(name: string) -> StructInfo {
    return StructInfo { name: name, field_names: [], field_types: [] }
}

-- Enum info for tracking registered enums
struct EnumInfo {
    name: string,
    variant_names: List[string],
    variant_tags: List[int],
    variant_payload_types: List[string],
    variant_has_payload: List[bool]
}

fn enum_info_new(name: string) -> EnumInfo {
    return EnumInfo {
        name: name, variant_names: [], variant_tags: [],
        variant_payload_types: [], variant_has_payload: []
    }
}

-- List element kind for typed list operations
fn LIST_ELEM_INT() -> int { return 0 }
fn LIST_ELEM_FLOAT() -> int { return 1 }
fn LIST_ELEM_STRING() -> int { return 2 }
fn LIST_ELEM_OTHER() -> int { return 3 }

-- Map key/value type combinations
fn MAP_KIND_STRING_INT() -> int { return 0 }
fn MAP_KIND_INT_STRING() -> int { return 1 }

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
    -- Variable name -> lambda function name
    fn_ptr_vars: Map[string, string],
    -- Lambda name -> pipe-delimited capture list
    lambda_captures: Map[string, string],
    -- Method name -> struct type name
    method_struct_map: Map[string, string],
    -- Variable name -> struct type name
    var_struct_types: Map[string, string],
    -- Function name -> pipe-delimited mut param flags
    mut_param_fns: Map[string, string],
    -- Enum name -> EnumInfo
    enum_defs: Map[string, EnumInfo],
    -- Variable name -> enum type name
    var_enum_types: Map[string, string],
    -- Variable name -> map kind
    map_var_kinds: Map[string, int],
    -- Variable name -> box inner type name
    box_inner_types: Map[string, string],
    -- Variable name -> concrete struct type for dyn Trait
    dyn_var_concrete: Map[string, string],
    -- Variable name -> trait name for dyn Trait
    dyn_var_traits: Map[string, string],
    -- "StructName:TraitName" -> pipe-delimited method list
    trait_impl_methods: Map[string, string],
    -- User extern function names
    user_extern_fns: Map[string, bool],
    -- "StructName.field" -> list elem kind
    field_list_elem_kinds: Map[string, int],
    -- "StructName.field" -> list elem type name
    field_list_elem_types: Map[string, string],
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
    completed_blocks: List[IRBasicBlock]
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
        fn_ptr_vars: Map_new(),
        lambda_captures: Map_new(),
        method_struct_map: Map_new(),
        var_struct_types: Map_new(),
        mut_param_fns: Map_new(),
        enum_defs: Map_new(),
        var_enum_types: Map_new(),
        map_var_kinds: Map_new(),
        box_inner_types: Map_new(),
        dyn_var_concrete: Map_new(),
        dyn_var_traits: Map_new(),
        trait_impl_methods: Map_new(),
        user_extern_fns: Map_new(),
        field_list_elem_kinds: Map_new(),
        field_list_elem_types: Map_new(),
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
        completed_blocks: []
    }
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
    -- Default: return as-is (struct name, enum name, etc.)
    return name
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
    if starts_with(type_str, "Map[") {
        return "map"
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
    if starts_with(type_str, "dyn ") {
        return "dyn"
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
    if elem_type == "string" { return LIST_ELEM_STRING() }
    return LIST_ELEM_OTHER()
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
    if name == "exit" { return "dm_exit" }
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
    if name == "system" { return "dm_system" }
    if name == "assert" { return "dm_assert" }
    if name == "assert_eq" { return "dm_assert_eq" }
    return name
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
    if name == "system" { return true }
    if name == "assert" or name == "assert_eq" { return true }
    if name == "Box_new" or name == "Box_null" { return true }
    if name == "Map_new" { return true }
    return false
}

-- Get the return type of a builtin function
fn builtin_return_type(name: string) -> string {
    if name == "print" or name == "println" { return "void" }
    if name == "eprint" or name == "eprintln" { return "void" }
    if name == "panic" { return "void" }
    if name == "exit" { return "void" }
    if name == "len" { return "i64" }
    if name == "char_at" { return "string" }
    if name == "substr" { return "string" }
    if name == "int_to_string" { return "string" }
    if name == "float_to_string" { return "string" }
    if name == "bool_to_string" { return "string" }
    if name == "parse_int" { return "i64" }
    if name == "parse_float" { return "f64" }
    if name == "string_contains" or name == "string_find" { return "i64" }
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
    if name == "system" { return "i64" }
    if name == "assert" or name == "assert_eq" { return "void" }
    return "void"
}

-- ============================================================
-- GENERATE MODULE: Entry point for IR generation from AST
-- ============================================================

fn generate_module(gen: IRGenerator, source: SourceFile) -> IRGenerator {
    let mut g = gen
    g.declarations = source.declarations

    -- Phase 1: Register all struct and enum definitions
    let mut i = 0
    while i < source.declarations.len() {
        let decl = source.declarations[i]
        if decl.kind == DECL_STRUCT() {
            let sd = *decl.struct_decl
            g = register_struct(g, sd)
        } else if decl.kind == DECL_ENUM() {
            let ed = *decl.enum_decl
            g = register_enum(g, ed)
        }
        i = i + 1
    }

    -- Phase 2: Register all function signatures (forward declarations)
    i = 0
    while i < source.declarations.len() {
        let decl = source.declarations[i]
        if decl.kind == DECL_FUNCTION() {
            let fd = *decl.func_decl
            if fd.generic_params.len() > 0 {
                -- Store generic function for later monomorphization
                g.generic_fn_decls.insert(fd.name, i)
            }
        } else if decl.kind == DECL_IMPL() {
            let ib = *decl.impl_block
            g = register_impl_methods(g, ib)
        }
        i = i + 1
    }

    -- Phase 3: Declare runtime builtin functions
    g = declare_runtime_functions(g)

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
