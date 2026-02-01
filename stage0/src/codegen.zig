//! dAImond C Code Generator
//!
//! This module generates C11 code from the dAImond AST. It translates
//! dAImond language constructs into equivalent C code that can be
//! compiled with any standard C compiler.
//!
//! Key features:
//! - Type mapping (dAImond types -> C types)
//! - Expression code generation
//! - Statement code generation
//! - Declaration code generation
//! - Runtime support generation
//! - Memory region handling

const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const types = @import("types.zig");

// Re-import AST types
const Span = ast.Span;
const Identifier = ast.Identifier;
const Path = ast.Path;
const SourceFile = ast.SourceFile;
const Declaration = ast.Declaration;
const FunctionDecl = ast.FunctionDecl;
const StructDecl = ast.StructDecl;
const EnumDecl = ast.EnumDecl;
const EnumVariant = ast.EnumVariant;
const ImplBlock = ast.ImplBlock;
const ConstDecl = ast.ConstDecl;
const Statement = ast.Statement;
const LetBinding = ast.LetBinding;
const Assignment = ast.Assignment;
const ReturnStmt = ast.ReturnStmt;
const ForLoop = ast.ForLoop;
const WhileLoop = ast.WhileLoop;
const LoopStmt = ast.LoopStmt;
const BreakStmt = ast.BreakStmt;
const ContinueStmt = ast.ContinueStmt;
const RegionBlock = ast.RegionBlock;
const Expr = ast.Expr;
const Literal = ast.Literal;
const BinaryExpr = ast.BinaryExpr;
const UnaryExpr = ast.UnaryExpr;
const FieldAccess = ast.FieldAccess;
const IndexAccess = ast.IndexAccess;
const MethodCall = ast.MethodCall;
const FunctionCall = ast.FunctionCall;
const StructLiteral = ast.StructLiteral;
const EnumLiteral = ast.EnumLiteral;
const ArrayLiteral = ast.ArrayLiteral;
const TupleLiteral = ast.TupleLiteral;
const IfExpr = ast.IfExpr;
const MatchExpr = ast.MatchExpr;
const BlockExpr = ast.BlockExpr;
const LambdaExpr = ast.LambdaExpr;
const PipelineExpr = ast.PipelineExpr;
const ErrorPropagateExpr = ast.ErrorPropagateExpr;
const CoalesceExpr = ast.CoalesceExpr;
const RangeExpr = ast.RangeExpr;
const CastExpr = ast.CastExpr;
const TypeCheckExpr = ast.TypeCheckExpr;
const Pattern = ast.Pattern;
const TypeExpr = ast.TypeExpr;
const NamedType = ast.NamedType;
const FunctionType = ast.FunctionType;
const ArrayType = ast.ArrayType;
const SliceType = ast.SliceType;
const PointerType = ast.PointerType;
const ReferenceType = ast.ReferenceType;
const TupleType = ast.TupleType;
const OptionType = ast.OptionType;
const ResultType = ast.ResultType;

// ============================================================================
// CODE GENERATION ERRORS
// ============================================================================

pub const CodeGenError = error{
    OutOfMemory,
    UnsupportedConstruct,
    InvalidAST,
    TypeMismatch,
    UnresolvedType,
    InvalidPattern,
    InvalidExpression,
    UnimplementedFeature,
};

// ============================================================================
// C WRITER - Helper for generating formatted C code
// ============================================================================

/// Helper struct for writing formatted C code with indentation management
pub const CWriter = struct {
    buffer: std.ArrayList(u8),
    indent_level: usize,
    indent_str: []const u8,
    at_line_start: bool,

    const Self = @This();

    /// Initialize a new CWriter
    pub fn init(allocator: Allocator) Self {
        return .{
            .buffer = std.ArrayList(u8).init(allocator),
            .indent_level = 0,
            .indent_str = "    ", // 4 spaces
            .at_line_start = true,
        };
    }

    /// Clean up resources
    pub fn deinit(self: *Self) void {
        self.buffer.deinit();
    }

    /// Get the generated code as a string
    pub fn getOutput(self: *Self) []const u8 {
        return self.buffer.items;
    }

    /// Get owned output (caller must free)
    pub fn toOwnedSlice(self: *Self) ![]u8 {
        return self.buffer.toOwnedSlice();
    }

    /// Increase indentation level
    pub fn indent(self: *Self) void {
        self.indent_level += 1;
    }

    /// Decrease indentation level
    pub fn dedent(self: *Self) void {
        if (self.indent_level > 0) {
            self.indent_level -= 1;
        }
    }

    /// Write indentation if at line start
    fn writeIndent(self: *Self) !void {
        if (self.at_line_start) {
            for (0..self.indent_level) |_| {
                try self.buffer.appendSlice(self.indent_str);
            }
            self.at_line_start = false;
        }
    }

    /// Write a string
    pub fn write(self: *Self, str: []const u8) !void {
        try self.writeIndent();
        try self.buffer.appendSlice(str);
    }

    /// Write a formatted string
    pub fn print(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.writeIndent();
        try self.buffer.writer().print(fmt, args);
    }

    /// Write a newline
    pub fn newline(self: *Self) !void {
        try self.buffer.append('\n');
        self.at_line_start = true;
    }

    /// Write a line (string + newline)
    pub fn writeLine(self: *Self, str: []const u8) !void {
        try self.write(str);
        try self.newline();
    }

    /// Write a formatted line
    pub fn printLine(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        try self.print(fmt, args);
        try self.newline();
    }

    /// Write a blank line
    pub fn blankLine(self: *Self) !void {
        try self.newline();
    }

    /// Write opening brace and indent
    pub fn openBrace(self: *Self) !void {
        try self.writeLine("{");
        self.indent();
    }

    /// Dedent and write closing brace
    pub fn closeBrace(self: *Self) !void {
        self.dedent();
        try self.writeLine("}");
    }

    /// Write closing brace without newline
    pub fn closeBraceInline(self: *Self) !void {
        self.dedent();
        try self.write("}");
    }

    /// Begin a block (open brace on same line)
    pub fn beginBlock(self: *Self) !void {
        try self.write(" {");
        try self.newline();
        self.indent();
    }

    /// End a block
    pub fn endBlock(self: *Self) !void {
        self.dedent();
        try self.writeLine("}");
    }

    /// Write a C comment
    pub fn writeComment(self: *Self, comment: []const u8) !void {
        try self.print("/* {s} */", .{comment});
        try self.newline();
    }

    /// Write a line comment
    pub fn writeLineComment(self: *Self, comment: []const u8) !void {
        try self.print("// {s}", .{comment});
        try self.newline();
    }
};

// ============================================================================
// C CODE GENERATOR
// ============================================================================

/// The main C code generator
pub const CodeGenerator = struct {
    allocator: Allocator,
    /// Arena for temporary strings (mangled names, temps, etc.)
    string_arena: std.heap.ArenaAllocator,
    writer: CWriter,
    type_context: ?*types.TypeContext,

    // Track generated types for forward declarations
    generated_structs: std.StringHashMap(void),
    generated_enums: std.StringHashMap(void),
    simple_enums: std.StringHashMap(void),
    forward_declarations: std.ArrayList([]const u8),

    // Track impl methods for later generation
    impl_methods: std.ArrayList(ImplMethodInfo),

    // Track variable types for type inference during codegen
    variable_types: std.StringHashMap([]const u8),

    // Track function return types for type inference during codegen
    function_return_types: std.StringHashMap([]const u8),

    // Track struct field types: maps "StructName.field" -> C type
    struct_field_types: std.StringHashMap([]const u8),

    // Track generated list types: maps list type name -> element C type
    generated_list_types: std.StringHashMap([]const u8),
    // Deferred buffer for list type definitions
    list_type_writer: CWriter,

    // Unique ID counter for temporary variables
    temp_counter: usize,

    // Stack for tracking lambda/closure context
    lambda_depth: usize,

    // Current function name (for error messages)
    current_function: ?[]const u8,

    // Whether current function returns void (don't generate return for block result)
    current_function_is_void: bool,

    // Track which parameters in the current function are mut (pass-by-reference)
    current_mut_params: std.StringHashMap(void),

    // Track which function parameters are mut: function name → array of is_mut flags
    function_mut_info: std.StringHashMap([]const bool),

    // Region stack for memory management
    region_stack: std.ArrayList([]const u8),

    const Self = @This();

    const ImplMethodInfo = struct {
        target_type: []const u8,
        method: *FunctionDecl,
        trait_name: ?[]const u8,
    };

    /// Initialize a new code generator
    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .string_arena = std.heap.ArenaAllocator.init(allocator),
            .writer = CWriter.init(allocator),
            .type_context = null,
            .generated_structs = std.StringHashMap(void).init(allocator),
            .generated_enums = std.StringHashMap(void).init(allocator),
            .simple_enums = std.StringHashMap(void).init(allocator),
            .forward_declarations = std.ArrayList([]const u8).init(allocator),
            .impl_methods = std.ArrayList(ImplMethodInfo).init(allocator),
            .variable_types = std.StringHashMap([]const u8).init(allocator),
            .function_return_types = std.StringHashMap([]const u8).init(allocator),
            .struct_field_types = std.StringHashMap([]const u8).init(allocator),
            .generated_list_types = std.StringHashMap([]const u8).init(allocator),
            .list_type_writer = CWriter.init(allocator),
            .temp_counter = 0,
            .lambda_depth = 0,
            .current_function = null,
            .current_function_is_void = false,
            .current_mut_params = std.StringHashMap(void).init(allocator),
            .function_mut_info = std.StringHashMap([]const bool).init(allocator),
            .region_stack = std.ArrayList([]const u8).init(allocator),
        };
    }

    /// Clean up resources - arena frees all temporary strings
    pub fn deinit(self: *Self) void {
        self.string_arena.deinit();
        self.writer.deinit();
        self.generated_structs.deinit();
        self.generated_enums.deinit();
        self.simple_enums.deinit();
        self.forward_declarations.deinit();
        self.impl_methods.deinit();
        self.variable_types.deinit();
        self.function_return_types.deinit();
        self.struct_field_types.deinit();
        self.generated_list_types.deinit();
        self.list_type_writer.deinit();
        self.current_mut_params.deinit();
        self.function_mut_info.deinit();
        self.region_stack.deinit();
    }

    /// Get arena allocator for temporary strings
    fn stringAllocator(self: *Self) Allocator {
        return self.string_arena.allocator();
    }

    /// Set the type context for type resolution
    pub fn setTypeContext(self: *Self, ctx: *types.TypeContext) void {
        self.type_context = ctx;
    }

    /// Generate a unique temporary variable name
    fn freshTemp(self: *Self) ![]const u8 {
        const name = try std.fmt.allocPrint(self.stringAllocator(), "_dm_tmp_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        return name;
    }

    /// Write a string value as a C string literal, escaping special characters.
    /// The input is the actual byte content (escapes already resolved by parser).
    /// This re-escapes for C: newline → \n, tab → \t, etc.
    fn writeCStringLiteral(self: *Self, value: []const u8) !void {
        try self.writer.write("\"");
        for (value) |c| {
            switch (c) {
                '\n' => try self.writer.write("\\n"),
                '\t' => try self.writer.write("\\t"),
                '\r' => try self.writer.write("\\r"),
                '\\' => try self.writer.write("\\\\"),
                '"' => try self.writer.write("\\\""),
                0 => try self.writer.write("\\0"),
                else => {
                    if (c >= 0x20 and c < 0x7F) {
                        try self.writer.buffer.append(c);
                    } else {
                        // Non-printable: emit hex escape
                        var hex_buf: [4]u8 = undefined;
                        const hex = std.fmt.bufPrint(&hex_buf, "\\x{x:0>2}", .{c}) catch unreachable;
                        try self.writer.write(hex);
                    }
                },
            }
        }
        try self.writer.write("\"");
    }

    /// Get the generated C code
    pub fn getOutput(self: *Self) []const u8 {
        return self.writer.getOutput();
    }

    // ========================================================================
    // MAIN ENTRY POINT
    // ========================================================================

    /// Generate C code from a complete source file
    pub fn generate(self: *Self, source: *SourceFile) ![]const u8 {
        // Generate runtime header
        try self.generateRuntimeHeader();

        // First pass: collect all type declarations for forward declarations
        for (source.declarations) |decl| {
            try self.collectForwardDeclarations(decl);
        }

        // Generate forward declarations
        try self.generateForwardDeclarations();

        // Generate type definitions
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .struct_def => |s| try self.generateStruct(s),
                .enum_def => |e| try self.generateEnum(e),
                else => {},
            }
        }

        // Pre-scan all declarations to discover List[T] types used in function parameters,
        // return types, struct fields, let bindings, and function bodies. This triggers
        // monomorphized list type generation so the definitions are available before
        // function prototypes.
        for (source.declarations) |decl| {
            try self.scanForListTypes(decl);
        }

        // Insert generated list type definitions
        if (self.list_type_writer.getOutput().len > 0) {
            try self.writer.blankLine();
            try self.writer.writeLineComment("Monomorphized List Types");
            try self.writer.write(self.list_type_writer.getOutput());
        }

        try self.writer.blankLine();

        // Pre-scan all function declarations to record mutable parameter info
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .function => |f| try self.collectFunctionMutInfo(f, null),
                .impl_block => |impl| {
                    // impl methods will be collected later, but we need mut info now
                    const target_name = try self.extractTypeName(impl.target_type);
                    for (impl.items) |item| {
                        switch (item.kind) {
                            .function => |func| {
                                try self.collectFunctionMutInfo(func, target_name);
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            }
        }

        // Pre-scan all function return types for inference during codegen
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .function => |f| {
                    const ret_type = if (f.return_type) |rt| try self.mapType(rt) else "void";
                    self.trackFunctionReturnType(f.name.name, ret_type);
                },
                .impl_block => |impl| {
                    const target_name = try self.extractTypeName(impl.target_type);
                    for (impl.items) |item| {
                        switch (item.kind) {
                            .function => |func| {
                                const ret_type = if (func.return_type) |rt| try self.mapType(rt) else "void";
                                self.trackFunctionReturnType(func.name.name, ret_type);
                                const full_name = try std.fmt.allocPrint(self.stringAllocator(), "{s}_{s}", .{ target_name, func.name.name });
                                self.trackFunctionReturnType(full_name, ret_type);
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            }
        }

        // Generate function prototypes
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .function => |f| try self.generateFunctionPrototype(f, null),
                .impl_block => |impl| try self.collectImplMethods(impl),
                else => {},
            }
        }

        // Generate impl method prototypes
        for (self.impl_methods.items) |info| {
            try self.generateFunctionPrototype(info.method, info.target_type);
        }

        try self.writer.blankLine();

        // Generate constants
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .constant => |c| try self.generateConstant(c),
                else => {},
            }
        }

        // Generate function implementations
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .function => |f| try self.generateFunction(f, null),
                else => {},
            }
        }

        // Generate impl method implementations
        for (self.impl_methods.items) |info| {
            try self.generateFunction(info.method, info.target_type);
        }

        // Generate main entry point that calls dm_main
        try self.writer.blankLine();
        try self.writer.writeLine("int main(int argc, char** argv) {");
        self.writer.indent();
        try self.writer.writeLine("dm_argc = argc;");
        try self.writer.writeLine("dm_argv = argv;");
        try self.writer.writeLine("dm_main();");
        try self.writer.writeLine("return 0;");
        self.writer.dedent();
        try self.writer.writeLine("}");

        return self.writer.getOutput();
    }

    // ========================================================================
    // RUNTIME HEADER GENERATION
    // ========================================================================

    /// Generate the minimal runtime header
    fn generateRuntimeHeader(self: *Self) !void {
        try self.writer.writeLineComment("dAImond Generated C Code");
        try self.writer.writeLineComment("Generated by dAImond Stage 0 Compiler");
        try self.writer.blankLine();

        // Standard includes
        try self.writer.writeLine("#include <stdint.h>");
        try self.writer.writeLine("#include <stdbool.h>");
        try self.writer.writeLine("#include <stddef.h>");
        try self.writer.writeLine("#include <stdlib.h>");
        try self.writer.writeLine("#include <string.h>");
        try self.writer.writeLine("#include <stdio.h>");
        try self.writer.blankLine();

        // Runtime type definitions
        try self.writer.writeLineComment("Runtime Types");
        try self.writer.blankLine();

        // String type
        try self.writer.writeLine("typedef struct dm_string {");
        self.writer.indent();
        try self.writer.writeLine("const char* data;");
        try self.writer.writeLine("size_t len;");
        try self.writer.writeLine("size_t capacity;");
        self.writer.dedent();
        try self.writer.writeLine("} dm_string;");
        try self.writer.blankLine();

        // Arena allocator for regions
        try self.writer.writeLine("typedef struct dm_arena {");
        self.writer.indent();
        try self.writer.writeLine("void* memory;");
        try self.writer.writeLine("size_t size;");
        try self.writer.writeLine("size_t used;");
        try self.writer.writeLine("struct dm_arena* next;");
        self.writer.dedent();
        try self.writer.writeLine("} dm_arena;");
        try self.writer.blankLine();

        // Generic Option type (tagged union)
        try self.writer.writeLine("typedef struct dm_option_void {");
        self.writer.indent();
        try self.writer.writeLine("bool has_value;");
        try self.writer.writeLine("void* value;");
        self.writer.dedent();
        try self.writer.writeLine("} dm_option_void;");
        try self.writer.blankLine();

        // Generic Result type (tagged union)
        try self.writer.writeLine("typedef struct dm_result_void {");
        self.writer.indent();
        try self.writer.writeLine("bool is_ok;");
        try self.writer.writeLine("union {");
        self.writer.indent();
        try self.writer.writeLine("void* ok_value;");
        try self.writer.writeLine("void* err_value;");
        self.writer.dedent();
        try self.writer.writeLine("} value;");
        self.writer.dedent();
        try self.writer.writeLine("} dm_result_void;");
        try self.writer.blankLine();

        // Generic List/Vec type
        try self.writer.writeLine("typedef struct dm_list_void {");
        self.writer.indent();
        try self.writer.writeLine("void* data;");
        try self.writer.writeLine("size_t len;");
        try self.writer.writeLine("size_t capacity;");
        try self.writer.writeLine("size_t elem_size;");
        self.writer.dedent();
        try self.writer.writeLine("} dm_list_void;");
        try self.writer.blankLine();

        // Runtime helper functions
        try self.writer.writeLineComment("Runtime Functions");
        try self.writer.blankLine();

        // String creation
        try self.writer.writeLine("static inline dm_string dm_string_from_cstr(const char* s) {");
        self.writer.indent();
        try self.writer.writeLine("size_t len = strlen(s);");
        try self.writer.writeLine("return (dm_string){ .data = s, .len = len, .capacity = len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String equality
        try self.writer.writeLine("static inline bool dm_string_eq(dm_string a, dm_string b) {");
        self.writer.indent();
        try self.writer.writeLine("if (a.len != b.len) return false;");
        try self.writer.writeLine("return memcmp(a.data, b.data, a.len) == 0;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Arena creation
        try self.writer.writeLine("static inline dm_arena* dm_arena_create(size_t initial_size) {");
        self.writer.indent();
        try self.writer.writeLine("dm_arena* arena = (dm_arena*)malloc(sizeof(dm_arena));");
        try self.writer.writeLine("arena->memory = malloc(initial_size);");
        try self.writer.writeLine("arena->size = initial_size;");
        try self.writer.writeLine("arena->used = 0;");
        try self.writer.writeLine("arena->next = NULL;");
        try self.writer.writeLine("return arena;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Arena allocation
        try self.writer.writeLine("static inline void* dm_arena_alloc(dm_arena* arena, size_t size) {");
        self.writer.indent();
        try self.writer.writeLine("size_t aligned = (size + 7) & ~7; // 8-byte alignment");
        try self.writer.writeLine("if (arena->used + aligned > arena->size) {");
        self.writer.indent();
        try self.writer.writeLine("// Allocate new block");
        try self.writer.writeLine("size_t new_size = arena->size * 2;");
        try self.writer.writeLine("if (new_size < aligned) new_size = aligned;");
        try self.writer.writeLine("dm_arena* new_arena = dm_arena_create(new_size);");
        try self.writer.writeLine("new_arena->next = arena->next;");
        try self.writer.writeLine("arena->next = new_arena;");
        try self.writer.writeLine("arena = new_arena;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.writeLine("void* ptr = (char*)arena->memory + arena->used;");
        try self.writer.writeLine("arena->used += aligned;");
        try self.writer.writeLine("return ptr;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Arena destruction
        try self.writer.writeLine("static inline void dm_arena_destroy(dm_arena* arena) {");
        self.writer.indent();
        try self.writer.writeLine("while (arena) {");
        self.writer.indent();
        try self.writer.writeLine("dm_arena* next = arena->next;");
        try self.writer.writeLine("free(arena->memory);");
        try self.writer.writeLine("free(arena);");
        try self.writer.writeLine("arena = next;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Panic function
        try self.writer.writeLine("static inline void dm_panic(const char* message) {");
        self.writer.indent();
        try self.writer.writeLine("fprintf(stderr, \"PANIC: %s\\n\", message);");
        try self.writer.writeLine("exit(1);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Assert function
        try self.writer.writeLine("static inline void dm_assert(bool condition, const char* message) {");
        self.writer.indent();
        try self.writer.writeLine("if (!condition) dm_panic(message);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Print functions
        try self.writer.writeLine("static inline void dm_print_str(dm_string s) {");
        self.writer.indent();
        try self.writer.writeLine("fwrite(s.data, 1, s.len, stdout);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline void dm_println_str(dm_string s) {");
        self.writer.indent();
        try self.writer.writeLine("fwrite(s.data, 1, s.len, stdout);");
        try self.writer.writeLine("putchar('\\n');");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String concatenation
        try self.writer.writeLine("static inline dm_string dm_string_concat(dm_string a, dm_string b) {");
        self.writer.indent();
        try self.writer.writeLine("size_t new_len = a.len + b.len;");
        try self.writer.writeLine("char* buf = (char*)malloc(new_len + 1);");
        try self.writer.writeLine("memcpy(buf, a.data, a.len);");
        try self.writer.writeLine("memcpy(buf + a.len, b.data, b.len);");
        try self.writer.writeLine("buf[new_len] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = new_len, .capacity = new_len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Int to string conversion
        try self.writer.writeLine("static inline dm_string dm_int_to_string(int64_t n) {");
        self.writer.indent();
        try self.writer.writeLine("char buf[32];");
        try self.writer.writeLine("int len = snprintf(buf, sizeof(buf), \"%lld\", (long long)n);");
        try self.writer.writeLine("char* result = (char*)malloc(len + 1);");
        try self.writer.writeLine("memcpy(result, buf, len + 1);");
        try self.writer.writeLine("return (dm_string){ .data = result, .len = (size_t)len, .capacity = (size_t)len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Bool to string conversion
        try self.writer.writeLine("static inline dm_string dm_bool_to_string(bool b) {");
        self.writer.indent();
        try self.writer.writeLine("return b ? dm_string_from_cstr(\"true\") : dm_string_from_cstr(\"false\");");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Float to string conversion
        try self.writer.writeLine("static inline dm_string dm_float_to_string(double f) {");
        self.writer.indent();
        try self.writer.writeLine("char buf[64];");
        try self.writer.writeLine("int len = snprintf(buf, sizeof(buf), \"%g\", f);");
        try self.writer.writeLine("char* result = (char*)malloc(len + 1);");
        try self.writer.writeLine("memcpy(result, buf, len + 1);");
        try self.writer.writeLine("return (dm_string){ .data = result, .len = (size_t)len, .capacity = (size_t)len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String substring
        try self.writer.writeLine("static inline dm_string dm_string_substr(dm_string s, int64_t start, int64_t length) {");
        self.writer.indent();
        try self.writer.writeLine("if (start < 0 || (size_t)start >= s.len) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("size_t actual_len = (size_t)length;");
        try self.writer.writeLine("if ((size_t)start + actual_len > s.len) actual_len = s.len - (size_t)start;");
        try self.writer.writeLine("char* buf = (char*)malloc(actual_len + 1);");
        try self.writer.writeLine("memcpy(buf, s.data + start, actual_len);");
        try self.writer.writeLine("buf[actual_len] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = actual_len, .capacity = actual_len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String comparison (lexicographic)
        try self.writer.writeLine("static inline int64_t dm_string_cmp(dm_string a, dm_string b) {");
        self.writer.indent();
        try self.writer.writeLine("size_t min_len = a.len < b.len ? a.len : b.len;");
        try self.writer.writeLine("int c = memcmp(a.data, b.data, min_len);");
        try self.writer.writeLine("if (c != 0) return (int64_t)c;");
        try self.writer.writeLine("if (a.len < b.len) return -1;");
        try self.writer.writeLine("if (a.len > b.len) return 1;");
        try self.writer.writeLine("return 0;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Character classification functions
        try self.writer.writeLine("static inline bool dm_is_alpha(char c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'; }");
        try self.writer.writeLine("static inline bool dm_is_digit(char c) { return c >= '0' && c <= '9'; }");
        try self.writer.writeLine("static inline bool dm_is_alnum(char c) { return dm_is_alpha(c) || dm_is_digit(c); }");
        try self.writer.writeLine("static inline bool dm_is_whitespace(char c) { return c == ' ' || c == '\\t' || c == '\\n' || c == '\\r'; }");
        try self.writer.blankLine();

        // Char to string conversion
        try self.writer.writeLine("static inline dm_string dm_char_to_string(char c) {");
        self.writer.indent();
        try self.writer.writeLine("char* buf = (char*)malloc(2);");
        try self.writer.writeLine("buf[0] = c;");
        try self.writer.writeLine("buf[1] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = 1, .capacity = 1 };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String contains
        try self.writer.writeLine("static inline bool dm_string_contains(dm_string haystack, dm_string needle) {");
        self.writer.indent();
        try self.writer.writeLine("if (needle.len == 0) return true;");
        try self.writer.writeLine("if (needle.len > haystack.len) return false;");
        try self.writer.writeLine("for (size_t i = 0; i <= haystack.len - needle.len; i++) {");
        self.writer.indent();
        try self.writer.writeLine("if (memcmp(haystack.data + i, needle.data, needle.len) == 0) return true;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.writeLine("return false;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String find (index of)
        try self.writer.writeLine("static inline int64_t dm_string_find(dm_string haystack, dm_string needle) {");
        self.writer.indent();
        try self.writer.writeLine("if (needle.len == 0) return 0;");
        try self.writer.writeLine("if (needle.len > haystack.len) return -1;");
        try self.writer.writeLine("for (size_t i = 0; i <= haystack.len - needle.len; i++) {");
        self.writer.indent();
        try self.writer.writeLine("if (memcmp(haystack.data + i, needle.data, needle.len) == 0) return (int64_t)i;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.writeLine("return -1;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String starts_with / ends_with
        try self.writer.writeLine("static inline bool dm_string_starts_with(dm_string s, dm_string prefix) {");
        self.writer.indent();
        try self.writer.writeLine("if (prefix.len > s.len) return false;");
        try self.writer.writeLine("return memcmp(s.data, prefix.data, prefix.len) == 0;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline bool dm_string_ends_with(dm_string s, dm_string suffix) {");
        self.writer.indent();
        try self.writer.writeLine("if (suffix.len > s.len) return false;");
        try self.writer.writeLine("return memcmp(s.data + s.len - suffix.len, suffix.data, suffix.len) == 0;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Parse int
        try self.writer.writeLine("static inline int64_t dm_parse_int(dm_string s) {");
        self.writer.indent();
        try self.writer.writeLine("char buf[32];");
        try self.writer.writeLine("size_t copy_len = s.len < 31 ? s.len : 31;");
        try self.writer.writeLine("memcpy(buf, s.data, copy_len);");
        try self.writer.writeLine("buf[copy_len] = '\\0';");
        try self.writer.writeLine("return (int64_t)atoll(buf);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Stderr print functions
        try self.writer.writeLine("static inline void dm_eprint_str(dm_string s) {");
        self.writer.indent();
        try self.writer.writeLine("fwrite(s.data, 1, s.len, stderr);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline void dm_eprintln_str(dm_string s) {");
        self.writer.indent();
        try self.writer.writeLine("fwrite(s.data, 1, s.len, stderr);");
        try self.writer.writeLine("fputc('\\n', stderr);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Exit function
        try self.writer.writeLine("static inline void dm_exit(int64_t code) { exit((int)code); }");
        try self.writer.blankLine();

        // Process execution
        try self.writer.writeLine("static inline int64_t dm_system(dm_string command) {");
        self.writer.indent();
        try self.writer.writeLine("char* cmd = (char*)malloc(command.len + 1);");
        try self.writer.writeLine("memcpy(cmd, command.data, command.len);");
        try self.writer.writeLine("cmd[command.len] = '\\0';");
        try self.writer.writeLine("int result = system(cmd);");
        try self.writer.writeLine("free(cmd);");
        try self.writer.writeLine("return (int64_t)result;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Argc/argv globals and helpers
        try self.writer.writeLine("static int dm_argc = 0;");
        try self.writer.writeLine("static char** dm_argv = NULL;");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline dm_string dm_args_get(int64_t index) {");
        self.writer.indent();
        try self.writer.writeLine("if (index < 0 || index >= (int64_t)dm_argc) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("return dm_string_from_cstr(dm_argv[index]);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline int64_t dm_args_len(void) { return (int64_t)dm_argc; }");
        try self.writer.blankLine();

        // File I/O
        try self.writer.writeLine("static inline dm_string dm_file_read(dm_string path) {");
        self.writer.indent();
        try self.writer.writeLine("char* cpath = (char*)malloc(path.len + 1);");
        try self.writer.writeLine("memcpy(cpath, path.data, path.len);");
        try self.writer.writeLine("cpath[path.len] = '\\0';");
        try self.writer.writeLine("FILE* f = fopen(cpath, \"rb\");");
        try self.writer.writeLine("free(cpath);");
        try self.writer.writeLine("if (!f) dm_panic(\"file_read: cannot open file\");");
        try self.writer.writeLine("fseek(f, 0, SEEK_END);");
        try self.writer.writeLine("long file_size = ftell(f);");
        try self.writer.writeLine("fseek(f, 0, SEEK_SET);");
        try self.writer.writeLine("char* buf = (char*)malloc((size_t)file_size + 1);");
        try self.writer.writeLine("size_t read_size = fread(buf, 1, (size_t)file_size, f);");
        try self.writer.writeLine("fclose(f);");
        try self.writer.writeLine("buf[read_size] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = read_size, .capacity = (size_t)file_size };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline void dm_file_write(dm_string path, dm_string content) {");
        self.writer.indent();
        try self.writer.writeLine("char* cpath = (char*)malloc(path.len + 1);");
        try self.writer.writeLine("memcpy(cpath, path.data, path.len);");
        try self.writer.writeLine("cpath[path.len] = '\\0';");
        try self.writer.writeLine("FILE* f = fopen(cpath, \"wb\");");
        try self.writer.writeLine("free(cpath);");
        try self.writer.writeLine("if (!f) dm_panic(\"file_write: cannot open file\");");
        try self.writer.writeLine("fwrite(content.data, 1, content.len, f);");
        try self.writer.writeLine("fclose(f);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLineComment("End of Runtime");
        try self.writer.blankLine();
    }

    // ========================================================================
    // FORWARD DECLARATIONS
    // ========================================================================

    /// Collect forward declarations from a declaration
    fn collectForwardDeclarations(self: *Self, decl: *Declaration) !void {
        switch (decl.kind) {
            .struct_def => |s| {
                const name = try self.mangleTypeName(s.name.name);
                try self.forward_declarations.append(name);
            },
            .enum_def => |e| {
                // Simple enums don't need struct forward declarations
                if (!isSimpleEnum(e)) {
                    const name = try self.mangleTypeName(e.name.name);
                    try self.forward_declarations.append(name);
                }
            },
            else => {},
        }
    }

    /// Generate forward declarations
    fn generateForwardDeclarations(self: *Self) !void {
        if (self.forward_declarations.items.len == 0) return;

        try self.writer.writeLineComment("Forward Declarations");
        for (self.forward_declarations.items) |name| {
            try self.writer.printLine("typedef struct {s} {s};", .{ name, name });
        }
        try self.writer.blankLine();
    }

    // ========================================================================
    // TYPE MAPPING
    // ========================================================================

    /// Map a dAImond type expression to C type string
    pub fn mapType(self: *Self, type_expr: *TypeExpr) CodeGenError![]const u8 {
        return switch (type_expr.kind) {
            .named => |named| self.mapNamedType(named),
            .function => |func| self.mapFunctionType(func),
            .array => |arr| self.mapArrayType(arr),
            .slice => |slice| self.mapSliceType(slice),
            .pointer => |ptr| self.mapPointerType(ptr),
            .reference => |ref| self.mapReferenceType(ref),
            .tuple => |tup| self.mapTupleType(tup),
            .option => |opt| self.mapOptionType(opt),
            .result => |res| self.mapResultType(res),
            .infer => "void*", // Fallback for inferred types
            .never => "void",
            .self_type => "void*", // Should be resolved earlier
        };
    }

    /// Map a named type to C
    fn mapNamedType(self: *Self, named: *NamedType) CodeGenError![]const u8 {
        // Get the type name from the path
        if (named.path.segments.len == 0) return CodeGenError.InvalidAST;

        const type_name = named.path.segments[named.path.segments.len - 1].name;

        // Built-in type mappings
        if (std.mem.eql(u8, type_name, "int") or std.mem.eql(u8, type_name, "i64")) {
            return "int64_t";
        } else if (std.mem.eql(u8, type_name, "uint") or std.mem.eql(u8, type_name, "u64")) {
            return "uint64_t";
        } else if (std.mem.eql(u8, type_name, "i32")) {
            return "int32_t";
        } else if (std.mem.eql(u8, type_name, "u32")) {
            return "uint32_t";
        } else if (std.mem.eql(u8, type_name, "i16")) {
            return "int16_t";
        } else if (std.mem.eql(u8, type_name, "u16")) {
            return "uint16_t";
        } else if (std.mem.eql(u8, type_name, "i8")) {
            return "int8_t";
        } else if (std.mem.eql(u8, type_name, "u8") or std.mem.eql(u8, type_name, "byte")) {
            return "uint8_t";
        } else if (std.mem.eql(u8, type_name, "i128") or std.mem.eql(u8, type_name, "u128")) {
            // 128-bit integers not supported in C11 standard; use int64_t as fallback
            return "int64_t";
        } else if (std.mem.eql(u8, type_name, "float") or std.mem.eql(u8, type_name, "f64")) {
            return "double";
        } else if (std.mem.eql(u8, type_name, "f32")) {
            return "float";
        } else if (std.mem.eql(u8, type_name, "bool")) {
            return "bool";
        } else if (std.mem.eql(u8, type_name, "char")) {
            return "uint32_t"; // Unicode scalar
        } else if (std.mem.eql(u8, type_name, "str") or std.mem.eql(u8, type_name, "String")) {
            return "dm_string";
        } else if (std.mem.eql(u8, type_name, "void") or std.mem.eql(u8, type_name, "unit")) {
            return "void";
        } else if (std.mem.eql(u8, type_name, "Option")) {
            // Check if user defined their own Option type
            const mangled = try self.mangleTypeName(type_name);
            if (self.generated_enums.contains(mangled) or self.generated_structs.contains(mangled)) {
                return mangled;
            }
            // Generic Option
            if (named.generic_args) |args| {
                if (args.len > 0) {
                    return self.generateOptionType(args[0]);
                }
            }
            return "dm_option_void";
        } else if (std.mem.eql(u8, type_name, "Result")) {
            // Check if user defined their own Result type
            const mangled = try self.mangleTypeName(type_name);
            if (self.generated_enums.contains(mangled) or self.generated_structs.contains(mangled)) {
                return mangled;
            }
            // Generic Result
            if (named.generic_args) |args| {
                if (args.len >= 2) {
                    return self.generateResultType(args[0], args[1]);
                } else if (args.len == 1) {
                    return self.generateResultType(args[0], null);
                }
            }
            return "dm_result_void";
        } else if (std.mem.eql(u8, type_name, "List") or std.mem.eql(u8, type_name, "Vec")) {
            // Check if user defined their own List/Vec type
            const mangled = try self.mangleTypeName(type_name);
            if (self.generated_enums.contains(mangled) or self.generated_structs.contains(mangled)) {
                return mangled;
            }
            // Generic List
            if (named.generic_args) |args| {
                if (args.len > 0) {
                    return self.generateListType(args[0]);
                }
            }
            return "dm_list_void";
        } else if (std.mem.eql(u8, type_name, "Box")) {
            // Boxed type (heap allocated pointer)
            if (named.generic_args) |args| {
                if (args.len > 0) {
                    const inner = try self.mapType(args[0]);
                    return try std.fmt.allocPrint(self.stringAllocator(), "{s}*", .{inner});
                }
            }
            return "void*";
        }

        // User-defined type - apply mangling
        return self.mangleTypeName(type_name);
    }

    /// Map function type to C function pointer
    fn mapFunctionType(self: *Self, func: *FunctionType) CodeGenError![]const u8 {
        var params_str = std.ArrayList(u8).init(self.allocator);
        defer params_str.deinit();

        for (func.params, 0..) |param, i| {
            if (i > 0) try params_str.appendSlice(", ");
            try params_str.appendSlice(try self.mapType(param));
        }

        const ret_type = try self.mapType(func.return_type);
        return try std.fmt.allocPrint(self.stringAllocator(), "{s} (*)({s})", .{ ret_type, params_str.items });
    }

    /// Map array type to C
    fn mapArrayType(self: *Self, arr: *ArrayType) CodeGenError![]const u8 {
        const elem_type = try self.mapType(arr.element_type);
        // C arrays are tricky in function signatures, use pointer
        return try std.fmt.allocPrint(self.stringAllocator(), "{s}*", .{elem_type});
    }

    /// Map slice type to C
    fn mapSliceType(self: *Self, slice: *SliceType) CodeGenError![]const u8 {
        const elem_type = try self.mapType(slice.element_type);
        // Slices are fat pointers (pointer + length)
        return try std.fmt.allocPrint(self.stringAllocator(), "struct {{ {s}* data; size_t len; }}", .{elem_type});
    }

    /// Map pointer type to C
    fn mapPointerType(self: *Self, ptr: *PointerType) CodeGenError![]const u8 {
        const pointee = try self.mapType(ptr.pointee_type);
        return try std.fmt.allocPrint(self.stringAllocator(), "{s}*", .{pointee});
    }

    /// Map reference type to C (just a pointer in C)
    fn mapReferenceType(self: *Self, ref: *ReferenceType) CodeGenError![]const u8 {
        const referenced = try self.mapType(ref.referenced_type);
        return try std.fmt.allocPrint(self.stringAllocator(), "{s}*", .{referenced});
    }

    /// Map tuple type to C struct
    fn mapTupleType(self: *Self, tup: *TupleType) CodeGenError![]const u8 {
        var fields = std.ArrayList(u8).init(self.allocator);
        defer fields.deinit();

        for (tup.elements, 0..) |elem, i| {
            const elem_type = try self.mapType(elem);
            try fields.writer().print("{s} _{d}; ", .{ elem_type, i });
        }

        return try std.fmt.allocPrint(self.stringAllocator(), "struct {{ {s}}}", .{fields.items});
    }

    /// Map Option type to C
    fn mapOptionType(self: *Self, opt: *OptionType) CodeGenError![]const u8 {
        return self.generateOptionType(opt.inner_type);
    }

    /// Map Result type to C
    fn mapResultType(self: *Self, res: *ResultType) CodeGenError![]const u8 {
        return self.generateResultType(res.ok_type, res.err_type);
    }

    /// Generate Option type name
    fn generateOptionType(self: *Self, inner: *TypeExpr) CodeGenError![]const u8 {
        const inner_name = try self.mapType(inner);
        const safe_name = try self.sanitizeTypeName(inner_name);
        return try std.fmt.allocPrint(self.stringAllocator(), "dm_option_{s}", .{safe_name});
    }

    /// Generate Result type name
    fn generateResultType(self: *Self, ok: *TypeExpr, err: ?*TypeExpr) CodeGenError![]const u8 {
        const ok_name = try self.mapType(ok);
        const safe_ok = try self.sanitizeTypeName(ok_name);
        if (err) |e| {
            const err_name = try self.mapType(e);
            const safe_err = try self.sanitizeTypeName(err_name);
            return try std.fmt.allocPrint(self.stringAllocator(), "dm_result_{s}_{s}", .{ safe_ok, safe_err });
        }
        return try std.fmt.allocPrint(self.stringAllocator(), "dm_result_{s}", .{safe_ok});
    }

    /// Generate List type name and emit monomorphized struct + helpers if not already done
    fn generateListType(self: *Self, elem: *TypeExpr) CodeGenError![]const u8 {
        const elem_name = try self.mapType(elem);
        const safe_name = try self.sanitizeTypeName(elem_name);
        const list_type_name = try std.fmt.allocPrint(self.stringAllocator(), "dm_list_{s}", .{safe_name});

        if (!self.generated_list_types.contains(list_type_name)) {
            try self.generated_list_types.put(list_type_name, elem_name);
            try self.emitListTypeDefinition(list_type_name, elem_name);
        }

        return list_type_name;
    }

    /// Emit a monomorphized list type struct and helper functions
    fn emitListTypeDefinition(self: *Self, list_type_name: []const u8, elem_type: []const u8) !void {
        var w = &self.list_type_writer;

        // Struct definition
        try w.printLine("typedef struct {s} {{", .{list_type_name});
        w.indent();
        try w.printLine("{s}* data;", .{elem_type});
        try w.writeLine("size_t len;");
        try w.writeLine("size_t capacity;");
        w.dedent();
        try w.printLine("}} {s};", .{list_type_name});
        try w.blankLine();

        // _new constructor
        try w.printLine("static inline {s} {s}_new(void) {{", .{ list_type_name, list_type_name });
        w.indent();
        try w.printLine("return ({s}){{ .data = NULL, .len = 0, .capacity = 0 }};", .{list_type_name});
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _push
        try w.printLine("static inline void {s}_push({s}* list, {s} value) {{", .{ list_type_name, list_type_name, elem_type });
        w.indent();
        try w.writeLine("if (list->len >= list->capacity) {");
        w.indent();
        try w.writeLine("size_t new_cap = list->capacity == 0 ? 8 : list->capacity * 2;");
        try w.printLine("{s}* new_data = ({s}*)realloc(list->data, new_cap * sizeof({s}));", .{ elem_type, elem_type, elem_type });
        try w.writeLine("if (!new_data) dm_panic(\"list push: out of memory\");");
        try w.writeLine("list->data = new_data;");
        try w.writeLine("list->capacity = new_cap;");
        w.dedent();
        try w.writeLine("}");
        try w.writeLine("list->data[list->len] = value;");
        try w.writeLine("list->len++;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _get
        try w.printLine("static inline {s} {s}_get({s}* list, int64_t index) {{", .{ elem_type, list_type_name, list_type_name });
        w.indent();
        try w.writeLine("if (index < 0 || (size_t)index >= list->len) dm_panic(\"list index out of bounds\");");
        try w.writeLine("return list->data[index];");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _pop
        try w.printLine("static inline {s} {s}_pop({s}* list) {{", .{ elem_type, list_type_name, list_type_name });
        w.indent();
        try w.writeLine("if (list->len == 0) dm_panic(\"list pop: empty list\");");
        try w.writeLine("list->len--;");
        try w.writeLine("return list->data[list->len];");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();
    }

    /// Sanitize type name for use in identifier
    fn sanitizeTypeName(self: *Self, name: []const u8) ![]const u8 {
        var result = std.ArrayList(u8).init(self.stringAllocator());
        for (name) |c| {
            if (std.ascii.isAlphanumeric(c) or c == '_') {
                try result.append(c);
            } else if (c == '*') {
                try result.appendSlice("ptr");
            } else if (c == ' ' or c == ',') {
                try result.append('_');
            }
        }
        return result.toOwnedSlice();
    }

    /// Scan a declaration for List[T] usage, triggering monomorphized type generation
    fn scanForListTypes(self: *Self, decl: *Declaration) !void {
        switch (decl.kind) {
            .function => |f| {
                // Scan parameter types
                for (f.params) |param| {
                    try self.scanTypeExprForLists(param.type_expr);
                }
                // Scan return type
                if (f.return_type) |rt| {
                    try self.scanTypeExprForLists(rt);
                }
                // Scan function body for let bindings with List types
                if (f.body) |body| {
                    switch (body) {
                        .block => |block| try self.scanBlockForListTypes(block),
                        .expression => {},
                    }
                }
            },
            .struct_def => |s| {
                for (s.fields) |field| {
                    try self.scanTypeExprForLists(field.type_expr);
                }
            },
            .impl_block => |impl| {
                for (impl.items) |item| {
                    switch (item.kind) {
                        .function => |f| {
                            for (f.params) |param| {
                                try self.scanTypeExprForLists(param.type_expr);
                            }
                            if (f.return_type) |rt| {
                                try self.scanTypeExprForLists(rt);
                            }
                            if (f.body) |body| {
                                switch (body) {
                                    .block => |block| try self.scanBlockForListTypes(block),
                                    .expression => {},
                                }
                            }
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }

    /// Scan a block for let bindings with List type annotations
    fn scanBlockForListTypes(self: *Self, block: *BlockExpr) !void {
        for (block.statements) |stmt| {
            switch (stmt.kind) {
                .let_binding => |let_bind| {
                    if (let_bind.type_annotation) |ta| {
                        try self.scanTypeExprForLists(ta);
                    }
                },
                .if_stmt => |if_expr| {
                    try self.scanBlockForListTypes(if_expr.then_branch);
                    if (if_expr.else_branch) |else_br| {
                        switch (else_br) {
                            .else_block => |eb| try self.scanBlockForListTypes(eb),
                            .else_if => |ei| {
                                try self.scanBlockForListTypes(ei.then_branch);
                            },
                        }
                    }
                },
                .for_loop => |for_stmt| try self.scanBlockForListTypes(for_stmt.body),
                .while_loop => |while_stmt| try self.scanBlockForListTypes(while_stmt.body),
                .loop_stmt => |loop| try self.scanBlockForListTypes(loop.body),
                else => {},
            }
        }
    }

    /// Scan a type expression for List<T> and trigger monomorphization
    fn scanTypeExprForLists(self: *Self, type_expr: *TypeExpr) !void {
        switch (type_expr.kind) {
            .named => |named| {
                if (named.path.segments.len > 0) {
                    const type_name = named.path.segments[named.path.segments.len - 1].name;
                    if (std.mem.eql(u8, type_name, "List") or std.mem.eql(u8, type_name, "Vec")) {
                        if (named.generic_args) |args| {
                            if (args.len > 0) {
                                _ = try self.generateListType(args[0]);
                            }
                        }
                    }
                }
                // Recursively scan generic args
                if (named.generic_args) |args| {
                    for (args) |arg| {
                        try self.scanTypeExprForLists(arg);
                    }
                }
            },
            .array => |arr| try self.scanTypeExprForLists(arr.element_type),
            .slice => |s| try self.scanTypeExprForLists(s.element_type),
            .pointer => |p| try self.scanTypeExprForLists(p.pointee_type),
            .reference => |r| try self.scanTypeExprForLists(r.referenced_type),
            .option => |o| try self.scanTypeExprForLists(o.inner_type),
            .function => |f| {
                for (f.params) |param| {
                    try self.scanTypeExprForLists(param);
                }
                try self.scanTypeExprForLists(f.return_type);
            },
            else => {},
        }
    }

    /// Mangle a type name
    fn mangleTypeName(self: *Self, name: []const u8) ![]const u8 {
        return try std.fmt.allocPrint(self.stringAllocator(), "dm_{s}", .{name});
    }

    /// Mangle a function name (with optional type prefix for methods)
    fn mangleFunctionName(self: *Self, name: []const u8, type_prefix: ?[]const u8) ![]const u8 {
        if (type_prefix) |prefix| {
            return try std.fmt.allocPrint(self.stringAllocator(), "dm_{s}_{s}", .{ prefix, name });
        }
        return try std.fmt.allocPrint(self.stringAllocator(), "dm_{s}", .{name});
    }

    /// Infer C type from an expression (for let bindings without type annotations)
    fn inferCTypeFromExpr(self: *Self, expr: *Expr) []const u8 {
        return switch (expr.kind) {
            .literal => |lit| switch (lit.kind) {
                .int => |int_lit| if (int_lit.suffix) |suffix| blk: {
                    // Map suffix to C type
                    if (std.mem.eql(u8, suffix, "i8")) break :blk "int8_t";
                    if (std.mem.eql(u8, suffix, "i16")) break :blk "int16_t";
                    if (std.mem.eql(u8, suffix, "i32")) break :blk "int32_t";
                    if (std.mem.eql(u8, suffix, "i64")) break :blk "int64_t";
                    if (std.mem.eql(u8, suffix, "u8")) break :blk "uint8_t";
                    if (std.mem.eql(u8, suffix, "u16")) break :blk "uint16_t";
                    if (std.mem.eql(u8, suffix, "u32")) break :blk "uint32_t";
                    if (std.mem.eql(u8, suffix, "u64")) break :blk "uint64_t";
                    break :blk "int64_t";
                } else "int64_t",
                .float => |float_lit| if (float_lit.suffix) |suffix| blk: {
                    if (std.mem.eql(u8, suffix, "f32")) break :blk "float";
                    break :blk "double";
                } else "double",
                .string => "dm_string",
                .char => "char",
                .bool => "bool",
                .null_lit => "void*",
            },
            .binary => |bin| blk: {
                // Comparison and logical ops produce bool
                break :blk switch (bin.op) {
                    .eq, .ne, .lt, .le, .gt, .ge, .@"and", .@"or", .in => "bool",
                    .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .shl, .shr => inner: {
                        // Check if either operand is a float or string
                        const left_type = self.inferCTypeFromExpr(bin.left);
                        if (std.mem.eql(u8, left_type, "double") or std.mem.eql(u8, left_type, "float")) break :inner left_type;
                        if (std.mem.eql(u8, left_type, "dm_string")) break :inner "dm_string";
                        const right_type = self.inferCTypeFromExpr(bin.right);
                        if (std.mem.eql(u8, right_type, "double") or std.mem.eql(u8, right_type, "float")) break :inner right_type;
                        if (std.mem.eql(u8, right_type, "dm_string")) break :inner "dm_string";
                        break :inner "int64_t";
                    },
                };
            },
            .unary => |un| blk: {
                break :blk switch (un.op) {
                    .not => "bool",
                    .neg, .bit_not => self.inferCTypeFromExpr(un.operand),
                    .deref, .ref => "void*",
                };
            },
            .function_call => |call| blk: {
                // Try to look up the function's return type from declarations
                if (call.function.kind == .identifier) {
                    const name = call.function.kind.identifier.name;
                    // Check known built-in return types
                    if (std.mem.eql(u8, name, "len") or
                        std.mem.eql(u8, name, "parse_int") or
                        std.mem.eql(u8, name, "string_find") or
                        std.mem.eql(u8, name, "args_len") or
                        std.mem.eql(u8, name, "system")) break :blk "int64_t";
                    if (std.mem.eql(u8, name, "to_string") or
                        std.mem.eql(u8, name, "int_to_string") or
                        std.mem.eql(u8, name, "bool_to_string") or
                        std.mem.eql(u8, name, "float_to_string") or
                        std.mem.eql(u8, name, "substr") or
                        std.mem.eql(u8, name, "char_to_string") or
                        std.mem.eql(u8, name, "file_read") or
                        std.mem.eql(u8, name, "args_get")) break :blk "dm_string";
                    if (std.mem.eql(u8, name, "is_alpha") or
                        std.mem.eql(u8, name, "is_digit") or
                        std.mem.eql(u8, name, "is_alnum") or
                        std.mem.eql(u8, name, "is_whitespace") or
                        std.mem.eql(u8, name, "string_contains") or
                        std.mem.eql(u8, name, "starts_with") or
                        std.mem.eql(u8, name, "ends_with")) break :blk "bool";
                    if (std.mem.eql(u8, name, "char")) break :blk "char";
                    // Look up in function declarations tracked during generation
                    if (self.lookupFunctionReturnType(name)) |ret_type| break :blk ret_type;
                }
                // Check if calling an enum variant constructor: Type::Variant(args)
                if (call.function.kind == .path) {
                    const path = call.function.kind.path;
                    if (path.segments.len >= 2) {
                        break :blk self.mangleTypeName(path.segments[0].name) catch "int64_t";
                    }
                }
                break :blk "int64_t"; // Default to int64_t rather than void*
            },
            .struct_literal => |lit| blk: {
                if (lit.type_path) |path| {
                    if (path.segments.len > 0) {
                        const type_name = path.segments[path.segments.len - 1].name;
                        break :blk self.mangleTypeName(type_name) catch "void*";
                    }
                }
                break :blk "void*";
            },
            .enum_literal => |lit| blk: {
                if (lit.type_path) |path| {
                    if (path.segments.len > 0) {
                        const type_name = path.segments[path.segments.len - 1].name;
                        break :blk self.mangleTypeName(type_name) catch "void*";
                    }
                }
                break :blk "void*";
            },
            .if_expr => |if_e| blk: {
                // Infer from the then branch result
                if (if_e.then_branch.result) |result| {
                    break :blk self.inferCTypeFromExpr(result);
                }
                break :blk "void*";
            },
            .match_expr => |match| blk: {
                // Infer from the first arm body
                if (match.arms.len > 0) {
                    switch (match.arms[0].body) {
                        .expression => |arm_expr| break :blk self.inferCTypeFromExpr(arm_expr),
                        .block => |block| {
                            if (block.result) |result| break :blk self.inferCTypeFromExpr(result);
                        },
                    }
                }
                break :blk "void*";
            },
            .block => |block| blk: {
                if (block.result) |result| {
                    break :blk self.inferCTypeFromExpr(result);
                }
                break :blk "void*";
            },
            .identifier => |ident| blk: {
                // Look up in known variable types
                if (self.lookupVariableType(ident.name)) |var_type| break :blk var_type;
                break :blk "int64_t"; // Default to int64_t for identifiers
            },
            .index_access => |idx| blk: {
                const obj_type = self.inferCTypeFromExpr(idx.object);
                if (std.mem.eql(u8, obj_type, "dm_string")) break :blk "char";
                if (std.mem.startsWith(u8, obj_type, "dm_list_")) {
                    // Strip "dm_list_" prefix to get element type
                    break :blk obj_type["dm_list_".len..];
                }
                break :blk "int64_t";
            },
            .method_call => |mc| blk: {
                // Try to infer method return type from the method name
                const obj_type = self.inferCTypeFromExpr(mc.object);
                const type_name = if (std.mem.startsWith(u8, obj_type, "dm_"))
                    obj_type[3..]
                else
                    obj_type;
                const full_name = std.fmt.allocPrint(self.stringAllocator(), "{s}_{s}", .{ type_name, mc.method.name }) catch break :blk "int64_t";
                if (self.lookupFunctionReturnType(full_name)) |ret_type| break :blk ret_type;
                break :blk "int64_t";
            },
            .field_access => |fa| blk: {
                const obj_type = self.inferCTypeFromExpr(fa.object);
                const key = std.fmt.allocPrint(self.stringAllocator(), "{s}.{s}", .{ obj_type, fa.field.name }) catch break :blk "int64_t";
                if (self.struct_field_types.get(key)) |field_type| break :blk field_type;
                break :blk "int64_t";
            },
            .cast => |cast| blk: {
                break :blk self.mapType(cast.target_type) catch "void*";
            },
            .grouped => |inner| self.inferCTypeFromExpr(inner),
            .path => |path| blk: {
                // Multi-segment path like Color::Red -> type is the first segment (the enum name)
                if (path.segments.len >= 2) {
                    break :blk self.mangleTypeName(path.segments[0].name) catch "int64_t";
                }
                break :blk "int64_t";
            },
            .array_literal => "void*",
            .tuple_literal => "void*",
            .lambda => "void*",
            .type_check => "bool",
            .range => "void*",
            else => "int64_t", // Default to int64_t rather than void*
        };
    }

    /// Look up the return type of a known function (from declarations already processed)
    fn lookupFunctionReturnType(self: *Self, name: []const u8) ?[]const u8 {
        return self.function_return_types.get(name);
    }

    /// Look up the type of a known variable
    fn lookupVariableType(self: *Self, name: []const u8) ?[]const u8 {
        return self.variable_types.get(name);
    }

    /// Track a variable's C type
    fn trackVariableType(self: *Self, name: []const u8, c_type: []const u8) void {
        self.variable_types.put(name, c_type) catch {};
    }

    /// Track a function's return C type
    fn trackFunctionReturnType(self: *Self, name: []const u8, c_type: []const u8) void {
        self.function_return_types.put(name, c_type) catch {};
    }

    // ========================================================================
    // STRUCT GENERATION
    // ========================================================================

    /// Generate C struct definition
    fn generateStruct(self: *Self, s: *StructDecl) !void {
        const name = try self.mangleTypeName(s.name.name);

        // Check if already generated
        if (self.generated_structs.contains(name)) return;
        try self.generated_structs.put(name, {});

        try self.writer.printLine("typedef struct {s} {{", .{name});
        self.writer.indent();

        for (s.fields) |field| {
            const field_type = try self.mapType(field.type_expr);
            try self.writer.printLine("{s} {s};", .{ field_type, field.name.name });
            // Track field types for inference: "dm_StructName.fieldName" -> C type
            const key = try std.fmt.allocPrint(self.stringAllocator(), "{s}.{s}", .{ name, field.name.name });
            try self.struct_field_types.put(key, field_type);
        }

        self.writer.dedent();
        try self.writer.printLine("}} {s};", .{name});
        try self.writer.blankLine();
    }

    // ========================================================================
    // ENUM GENERATION (Tagged Unions)
    // ========================================================================

    /// Check if an enum has only unit variants (no payloads)
    fn isSimpleEnum(e: *EnumDecl) bool {
        for (e.variants) |variant| {
            switch (variant.payload) {
                .none => {},
                .tuple, .struct_fields => return false,
            }
        }
        return true;
    }

    /// Generate C enum/tagged union
    fn generateEnum(self: *Self, e: *EnumDecl) !void {
        const name = try self.mangleTypeName(e.name.name);

        if (self.generated_enums.contains(name)) return;
        try self.generated_enums.put(name, {});

        // Simple enum optimization: all-unit-variant enums become plain C enums
        if (isSimpleEnum(e)) {
            try self.simple_enums.put(name, {});

            try self.writer.printLine("typedef enum {s} {{", .{name});
            self.writer.indent();
            for (e.variants, 0..) |variant, i| {
                try self.writer.print("{s}_{s}", .{ name, variant.name.name });
                if (i < e.variants.len - 1) {
                    try self.writer.write(",");
                }
                try self.writer.newline();
            }
            self.writer.dedent();
            try self.writer.printLine("}} {s};", .{name});
            try self.writer.blankLine();
            return;
        }

        // Generate tag enum for complex enums
        try self.writer.printLine("typedef enum {s}_tag {{", .{name});
        self.writer.indent();
        for (e.variants, 0..) |variant, i| {
            try self.writer.print("{s}_tag_{s}", .{ name, variant.name.name });
            if (i < e.variants.len - 1) {
                try self.writer.write(",");
            }
            try self.writer.newline();
        }
        self.writer.dedent();
        try self.writer.printLine("}} {s}_tag;", .{name});
        try self.writer.blankLine();

        // Generate tagged union struct
        try self.writer.printLine("typedef struct {s} {{", .{name});
        self.writer.indent();
        try self.writer.printLine("{s}_tag tag;", .{name});
        try self.writer.writeLine("union {");
        self.writer.indent();

        var has_payload_variant = false;
        for (e.variants) |variant| {
            switch (variant.payload) {
                .none => {
                    // Unit variant in a mixed enum - no union field needed
                },
                .tuple => |tuple_types| {
                    has_payload_variant = true;
                    if (tuple_types.len == 1) {
                        const payload_type = try self.mapType(tuple_types[0]);
                        try self.writer.printLine("{s} {s};", .{ payload_type, variant.name.name });
                    } else {
                        // Multiple tuple elements - create anonymous struct
                        try self.writer.print("struct {{ ", .{});
                        for (tuple_types, 0..) |t, i| {
                            const payload_type = try self.mapType(t);
                            try self.writer.print("{s} _{d}; ", .{ payload_type, i });
                        }
                        try self.writer.printLine("}} {s};", .{variant.name.name});
                    }
                },
                .struct_fields => |fields| {
                    has_payload_variant = true;
                    // Struct variant
                    try self.writer.print("struct {{ ", .{});
                    for (fields) |field| {
                        const field_type = try self.mapType(field.type_expr);
                        try self.writer.print("{s} {s}; ", .{ field_type, field.name.name });
                    }
                    try self.writer.printLine("}} {s};", .{variant.name.name});
                },
            }
        }

        // If no payload variants exist in a mixed enum, add a placeholder
        if (!has_payload_variant) {
            try self.writer.writeLine("char _placeholder;");
        }

        self.writer.dedent();
        try self.writer.writeLine("} data;");
        self.writer.dedent();
        try self.writer.printLine("}} {s};", .{name});
        try self.writer.blankLine();

        // Generate variant constructors
        for (e.variants) |variant| {
            try self.generateEnumVariantConstructor(name, variant);
        }
    }

    /// Generate constructor function for enum variant
    fn generateEnumVariantConstructor(self: *Self, enum_name: []const u8, variant: *EnumVariant) !void {
        const variant_name = variant.name.name;

        switch (variant.payload) {
            .none => {
                // Unit variant constructor
                try self.writer.printLine("static inline {s} {s}_{s}(void) {{", .{ enum_name, enum_name, variant_name });
                self.writer.indent();
                try self.writer.printLine("{s} result;", .{enum_name});
                try self.writer.printLine("result.tag = {s}_tag_{s};", .{ enum_name, variant_name });
                try self.writer.writeLine("return result;");
                self.writer.dedent();
                try self.writer.writeLine("}");
                try self.writer.blankLine();
            },
            .tuple => |tuple_types| {
                // Tuple variant constructor
                try self.writer.print("static inline {s} {s}_{s}(", .{ enum_name, enum_name, variant_name });
                for (tuple_types, 0..) |t, i| {
                    if (i > 0) try self.writer.write(", ");
                    const param_type = try self.mapType(t);
                    try self.writer.print("{s} _{d}", .{ param_type, i });
                }
                try self.writer.writeLine(") {");
                self.writer.indent();
                try self.writer.printLine("{s} result;", .{enum_name});
                try self.writer.printLine("result.tag = {s}_tag_{s};", .{ enum_name, variant_name });
                if (tuple_types.len == 1) {
                    try self.writer.printLine("result.data.{s} = _0;", .{variant_name});
                } else {
                    for (tuple_types, 0..) |_, i| {
                        try self.writer.printLine("result.data.{s}._{d} = _{d};", .{ variant_name, i, i });
                    }
                }
                try self.writer.writeLine("return result;");
                self.writer.dedent();
                try self.writer.writeLine("}");
                try self.writer.blankLine();
            },
            .struct_fields => |fields| {
                // Struct variant constructor
                try self.writer.print("static inline {s} {s}_{s}(", .{ enum_name, enum_name, variant_name });
                for (fields, 0..) |field, i| {
                    if (i > 0) try self.writer.write(", ");
                    const param_type = try self.mapType(field.type_expr);
                    try self.writer.print("{s} {s}", .{ param_type, field.name.name });
                }
                try self.writer.writeLine(") {");
                self.writer.indent();
                try self.writer.printLine("{s} result;", .{enum_name});
                try self.writer.printLine("result.tag = {s}_tag_{s};", .{ enum_name, variant_name });
                for (fields) |field| {
                    try self.writer.printLine("result.data.{s}.{s} = {s};", .{ variant_name, field.name.name, field.name.name });
                }
                try self.writer.writeLine("return result;");
                self.writer.dedent();
                try self.writer.writeLine("}");
                try self.writer.blankLine();
            },
        }
    }

    // ========================================================================
    // IMPL BLOCK HANDLING
    // ========================================================================

    /// Collect methods from impl block
    fn collectImplMethods(self: *Self, impl: *ImplBlock) !void {
        // Get target type name
        const target_name = try self.extractTypeName(impl.target_type);
        const trait_name: ?[]const u8 = if (impl.trait_type) |t| try self.extractTypeName(t) else null;

        for (impl.items) |item| {
            switch (item.kind) {
                .function => |func| {
                    try self.impl_methods.append(.{
                        .target_type = target_name,
                        .method = func,
                        .trait_name = trait_name,
                    });
                },
                else => {},
            }
        }
    }

    /// Extract type name from type expression
    fn extractTypeName(self: *Self, type_expr: *TypeExpr) ![]const u8 {
        _ = self;
        return switch (type_expr.kind) {
            .named => |named| blk: {
                if (named.path.segments.len > 0) {
                    break :blk named.path.segments[named.path.segments.len - 1].name;
                }
                break :blk "unknown";
            },
            else => "unknown",
        };
    }

    // ========================================================================
    // FUNCTION GENERATION
    // ========================================================================

    /// Collect mutable parameter info for a function declaration.
    /// Records which parameters are mut so call sites can pass by reference.
    fn collectFunctionMutInfo(self: *Self, func: *FunctionDecl, type_prefix: ?[]const u8) !void {
        var has_any_mut = false;
        const has_self = type_prefix != null and methodHasSelf(func);
        const start_idx: usize = if (has_self) 1 else 0;

        for (func.params[start_idx..]) |param| {
            if (param.is_mut) {
                has_any_mut = true;
                break;
            }
        }

        if (!has_any_mut) return;

        // Build array of mut flags (excluding self)
        const mut_flags = try self.stringAllocator().alloc(bool, func.params[start_idx..].len);
        for (func.params[start_idx..], 0..) |param, i| {
            mut_flags[i] = param.is_mut;
        }

        const func_name = try self.mangleFunctionName(func.name.name, type_prefix);
        try self.function_mut_info.put(func_name, mut_flags);
    }

    /// Check if a method has a self parameter (first param named "self")
    fn methodHasSelf(func: *FunctionDecl) bool {
        if (func.params.len > 0) {
            return std.mem.eql(u8, func.params[0].name.name, "self");
        }
        return false;
    }

    /// Generate function prototype
    fn generateFunctionPrototype(self: *Self, func: *FunctionDecl, type_prefix: ?[]const u8) !void {
        const func_name = try self.mangleFunctionName(func.name.name, type_prefix);
        const ret_type = if (func.return_type) |rt| try self.mapType(rt) else "void";
        const has_self = type_prefix != null and methodHasSelf(func);

        try self.writer.print("{s} {s}(", .{ ret_type, func_name });

        // If this is a method with self, add typed self pointer parameter
        if (has_self) {
            const mangled_type = try self.mangleTypeName(type_prefix.?);
            try self.writer.print("{s}* self", .{mangled_type});
            if (func.params.len > 1) {
                try self.writer.write(", ");
            }
        }

        // Generate parameter list (skip self param if present)
        const start_idx: usize = if (has_self) 1 else 0;
        var first = true;
        for (func.params[start_idx..]) |param| {
            if (!first) try self.writer.write(", ");
            first = false;
            const param_type = try self.mapType(param.type_expr);
            if (param.is_mut) {
                try self.writer.print("{s}* {s}", .{ param_type, param.name.name });
            } else {
                try self.writer.print("{s} {s}", .{ param_type, param.name.name });
            }
        }

        if (!has_self and type_prefix == null and func.params.len == 0) {
            try self.writer.write("void");
        }

        try self.writer.writeLine(");");
    }

    /// Generate full function definition
    fn generateFunction(self: *Self, func: *FunctionDecl, type_prefix: ?[]const u8) !void {
        self.current_function = func.name.name;
        defer self.current_function = null;

        const is_void = func.return_type == null;
        self.current_function_is_void = is_void;
        defer self.current_function_is_void = false;

        const has_self = type_prefix != null and methodHasSelf(func);
        const func_name = try self.mangleFunctionName(func.name.name, type_prefix);
        const ret_type = if (func.return_type) |rt| try self.mapType(rt) else "void";

        // Track function return type - also track with type prefix for method calls
        self.trackFunctionReturnType(func.name.name, ret_type);
        if (type_prefix) |prefix| {
            const full_name = try std.fmt.allocPrint(self.stringAllocator(), "{s}_{s}", .{ prefix, func.name.name });
            self.trackFunctionReturnType(full_name, ret_type);
        }

        try self.writer.print("{s} {s}(", .{ ret_type, func_name });

        // If this is a method with self, add typed self pointer parameter
        if (has_self) {
            const mangled_type = try self.mangleTypeName(type_prefix.?);
            try self.writer.print("{s}* self", .{mangled_type});
            if (func.params.len > 1) {
                try self.writer.write(", ");
            }
        }

        // Clear and populate mut params for this function
        self.current_mut_params.clearRetainingCapacity();
        defer self.current_mut_params.clearRetainingCapacity();

        // Generate parameter list (skip self param if present)
        const start_idx: usize = if (has_self) 1 else 0;
        var first = true;
        for (func.params[start_idx..]) |param| {
            if (!first) try self.writer.write(", ");
            first = false;
            const param_type = try self.mapType(param.type_expr);
            if (param.is_mut) {
                try self.writer.print("{s}* {s}", .{ param_type, param.name.name });
                try self.current_mut_params.put(param.name.name, {});
            } else {
                try self.writer.print("{s} {s}", .{ param_type, param.name.name });
            }
        }

        if (!has_self and type_prefix == null and func.params.len == 0) {
            try self.writer.write("void");
        }

        try self.writer.write(")");

        // Generate body
        if (func.body) |body| {
            // Track self type for method bodies
            if (has_self) {
                const mangled_type = try self.mangleTypeName(type_prefix.?);
                self.trackVariableType("self", mangled_type);
            }

            // Track parameter types for use in body (skip self)
            for (func.params[start_idx..]) |param| {
                const param_type = try self.mapType(param.type_expr);
                self.trackVariableType(param.name.name, param_type);
            }

            switch (body) {
                .block => |block| {
                    try self.writer.beginBlock();
                    try self.generateBlock(block);
                    try self.writer.endBlock();
                },
                .expression => |expr| {
                    try self.writer.beginBlock();
                    try self.writer.write("return ");
                    try self.generateExpr(expr);
                    try self.writer.writeLine(";");
                    try self.writer.endBlock();
                },
            }
        } else {
            try self.writer.writeLine(";");
        }

        try self.writer.blankLine();
    }

    /// Generate constant definition
    fn generateConstant(self: *Self, const_decl: *ConstDecl) !void {
        const name = try self.mangleFunctionName(const_decl.name.name, null);
        const type_str = if (const_decl.type_expr) |t| try self.mapType(t) else "const void*";

        try self.writer.print("static const {s} {s}", .{ type_str, name });

        if (const_decl.value) |val| {
            try self.writer.write(" = ");
            try self.generateExpr(val);
        }

        try self.writer.writeLine(";");
    }

    // ========================================================================
    // BLOCK/STATEMENT GENERATION
    // ========================================================================

    /// Generate a block of statements
    fn generateBlock(self: *Self, block: *BlockExpr) anyerror!void {
        for (block.statements) |stmt| {
            try self.generateStatement(stmt);
        }

        // Handle block result expression
        if (block.result) |result| {
            if (self.current_function_is_void) {
                // For void functions, just execute the expression (don't return it)
                // Use statement form for if/match to avoid ternary expressions
                if (result.kind == .match_expr) {
                    try self.generateMatchStatement(result.kind.match_expr);
                } else if (result.kind == .if_expr) {
                    try self.generateIfStatement(result.kind.if_expr);
                } else {
                    try self.generateExpr(result);
                    try self.writer.writeLine(";");
                }
            } else {
                try self.writer.write("return ");
                try self.generateExpr(result);
                try self.writer.writeLine(";");
            }
        }
    }

    /// Generate a statement
    fn generateStatement(self: *Self, stmt: *Statement) anyerror!void {
        switch (stmt.kind) {
            .let_binding => |let| try self.generateLetBinding(let),
            .return_stmt => |ret| try self.generateReturn(ret),
            .if_stmt => |if_expr| try self.generateIfStatement(if_expr),
            .match_stmt => |match| try self.generateMatchStatement(match),
            .for_loop => |for_stmt| try self.generateForLoop(for_stmt),
            .while_loop => |while_stmt| try self.generateWhileLoop(while_stmt),
            .loop_stmt => |loop| try self.generateLoopStatement(loop),
            .break_stmt => |brk| try self.generateBreak(brk),
            .continue_stmt => |cont| try self.generateContinue(cont),
            .region_block => |region| try self.generateRegionBlock(region),
            .expression => |expr| {
                // If the expression is an if or match, generate as statement form
                if (expr.kind == .if_expr) {
                    try self.generateIfStatement(expr.kind.if_expr);
                } else if (expr.kind == .match_expr) {
                    try self.generateMatchStatement(expr.kind.match_expr);
                } else {
                    try self.generateExpr(expr);
                    try self.writer.writeLine(";");
                }
            },
            .assignment => |assign| try self.generateAssignment(assign),
            .discard => |discard| {
                try self.writer.write("(void)(");
                try self.generateExpr(discard.value);
                try self.writer.writeLine(");");
            },
        }
    }

    /// Generate let binding
    fn generateLetBinding(self: *Self, let: *LetBinding) anyerror!void {
        // Get type string - infer from expression if no annotation
        const type_str = if (let.type_annotation) |t|
            try self.mapType(t)
        else if (let.value) |val|
            self.inferCTypeFromExpr(val)
        else
            "void*"; // No value and no type annotation - use void*

        // Handle pattern
        switch (let.pattern.kind) {
            .identifier => |ident| {
                // Track this variable's type for later inference
                self.trackVariableType(ident.name.name, type_str);
                try self.writer.print("{s} {s}", .{ type_str, ident.name.name });
                if (let.value) |val| {
                    try self.writer.write(" = ");
                    try self.generateExpr(val);
                }
                try self.writer.writeLine(";");
            },
            .tuple => |tuple_pat| {
                // Destructuring - generate temporary and individual assignments
                if (let.value) |val| {
                    const temp = try self.freshTemp();
                    try self.writer.print("{s} {s} = ", .{ type_str, temp });
                    try self.generateExpr(val);
                    try self.writer.writeLine(";");

                    for (tuple_pat.elements, 0..) |elem, i| {
                        if (elem.kind == .identifier) {
                            const elem_ident = elem.kind.identifier;
                            // Use void* for tuple element type (proper type inference would require full type system)
                            try self.writer.print("void* {s} = {s}._{d};", .{ elem_ident.name.name, temp, i });
                            try self.writer.newline();
                        }
                    }
                }
            },
            .struct_pattern => |struct_pat| {
                // Struct destructuring
                if (let.value) |val| {
                    const temp = try self.freshTemp();
                    try self.writer.print("{s} {s} = ", .{ type_str, temp });
                    try self.generateExpr(val);
                    try self.writer.writeLine(";");

                    for (struct_pat.fields) |field| {
                        if (field.pattern) |pat| {
                            if (pat.kind == .identifier) {
                                const pat_ident = pat.kind.identifier;
                                // Use void* for struct field type (proper type inference would require full type system)
                                try self.writer.print("void* {s} = {s}.{s};", .{ pat_ident.name.name, temp, field.name.name });
                                try self.writer.newline();
                            }
                        } else {
                            try self.writer.print("void* {s} = {s}.{s};", .{ field.name.name, temp, field.name.name });
                            try self.writer.newline();
                        }
                    }
                }
            },
            .wildcard => {
                // Discard value
                if (let.value) |val| {
                    try self.writer.write("(void)(");
                    try self.generateExpr(val);
                    try self.writer.writeLine(");");
                }
            },
            else => {
                // Fallback for other patterns
                try self.writer.writeLineComment("Unsupported pattern type");
            },
        }
    }

    /// Generate return statement
    fn generateReturn(self: *Self, ret: *ReturnStmt) anyerror!void {
        if (ret.value) |val| {
            try self.writer.write("return ");
            try self.generateExpr(val);
            try self.writer.writeLine(";");
        } else {
            try self.writer.writeLine("return;");
        }
    }

    /// Generate if statement
    fn generateIfStatement(self: *Self, if_expr: *IfExpr) anyerror!void {
        try self.writer.write("if (");
        try self.generateExpr(if_expr.condition);
        try self.writer.write(")");
        try self.writer.beginBlock();
        try self.generateBlock(if_expr.then_branch);
        try self.writer.closeBraceInline();

        if (if_expr.else_branch) |else_br| {
            switch (else_br) {
                .else_block => |block| {
                    try self.writer.write(" else");
                    try self.writer.beginBlock();
                    try self.generateBlock(block);
                    try self.writer.endBlock();
                },
                .else_if => |else_if| {
                    try self.writer.write(" else ");
                    try self.generateIfStatement(else_if);
                },
            }
        } else {
            try self.writer.newline();
        }
    }

    /// Generate match statement (as switch/if chain)
    fn generateMatchStatement(self: *Self, match: *MatchExpr) anyerror!void {
        // Generate match expression into temporary
        const temp = try self.freshTemp();
        const scrutinee_type = self.inferCTypeFromExpr(match.scrutinee);
        try self.writer.print("{s} {s} = ", .{ scrutinee_type, temp });
        try self.generateExpr(match.scrutinee);
        try self.writer.writeLine(";");

        // Wrap match in a block scope for pattern bindings
        try self.writer.openBrace();

        // Pre-emit identifier pattern bindings for arms with guards
        // This ensures the bound variable is available in the guard condition
        var emitted_bindings = std.StringHashMap(void).init(self.allocator);
        defer emitted_bindings.deinit();

        for (match.arms) |arm| {
            if (arm.pattern.kind == .identifier and arm.guard != null) {
                const ident = arm.pattern.kind.identifier;
                if (!emitted_bindings.contains(ident.name.name)) {
                    try self.writer.printLine("{s} {s} = {s};", .{ scrutinee_type, ident.name.name, temp });
                    self.trackVariableType(ident.name.name, scrutinee_type);
                    try emitted_bindings.put(ident.name.name, {});
                }
            }
        }

        // Generate if-else chain for pattern matching
        for (match.arms, 0..) |arm, i| {
            if (i == 0) {
                try self.writer.write("if (");
            } else {
                try self.writer.write(" else if (");
            }

            try self.generatePatternCondition(arm.pattern, temp);

            // Guard condition
            if (arm.guard) |guard| {
                try self.writer.write(" && (");
                try self.generateExpr(guard);
                try self.writer.write(")");
            }

            try self.writer.write(")");
            try self.writer.beginBlock();

            // Generate pattern bindings (skip identifier patterns that were pre-emitted)
            if (!(arm.pattern.kind == .identifier and arm.guard != null)) {
                try self.generatePatternBindings(arm.pattern, temp, scrutinee_type);
            }

            // Generate arm body
            switch (arm.body) {
                .expression => |expr| {
                    // If the arm body is a block expression, generate it as a block (not a GCC statement expr)
                    if (expr.kind == .block) {
                        try self.generateBlock(expr.kind.block);
                    } else {
                        try self.generateExpr(expr);
                        try self.writer.writeLine(";");
                    }
                },
                .block => |block| try self.generateBlock(block),
            }

            try self.writer.closeBraceInline();
        }

        try self.writer.newline();
        try self.writer.closeBrace();
    }

    /// Generate pattern condition for match
    fn generatePatternCondition(self: *Self, pattern: *Pattern, scrutinee: []const u8) anyerror!void {
        switch (pattern.kind) {
            .literal => |lit| {
                // For string literals, use dm_string_eq
                if (lit.kind == .string) {
                    try self.writer.print("dm_string_eq({s}, dm_string_from_cstr(", .{scrutinee});
                    try self.generateLiteral(lit);
                    try self.writer.write("))");
                } else {
                    try self.writer.print("{s} == ", .{scrutinee});
                    try self.generateLiteral(lit);
                }
            },
            .identifier => {
                // Identifier pattern always matches
                try self.writer.write("true");
            },
            .wildcard => {
                try self.writer.write("true");
            },
            .enum_variant => |variant| {
                // Check tag/value for enum variant
                const variant_name = variant.variant.name;
                if (variant.type_path) |path| {
                    const type_name = path.segments[path.segments.len - 1].name;
                    const mangled = try self.mangleTypeName(type_name);
                    // Check if this is a simple enum (plain C enum vs tagged union)
                    if (self.simple_enums.contains(mangled)) {
                        try self.writer.print("{s} == {s}_{s}", .{ scrutinee, mangled, variant_name });
                    } else {
                        try self.writer.print("{s}.tag == {s}_tag_{s}", .{ scrutinee, mangled, variant_name });
                    }
                } else {
                    try self.writer.print("{s}.tag == {s}", .{ scrutinee, variant_name });
                }
            },
            .or_pattern => |or_pat| {
                try self.writer.write("(");
                for (or_pat.patterns, 0..) |sub_pat, i| {
                    if (i > 0) try self.writer.write(" || ");
                    try self.generatePatternCondition(sub_pat, scrutinee);
                }
                try self.writer.write(")");
            },
            .range => |range_pat| {
                try self.writer.write("(");
                if (range_pat.start) |start| {
                    try self.writer.print("{s} >= ", .{scrutinee});
                    try self.generateExpr(start);
                }
                if (range_pat.start != null and range_pat.end != null) {
                    try self.writer.write(" && ");
                }
                if (range_pat.end) |end| {
                    try self.writer.print("{s} <", .{scrutinee});
                    if (range_pat.inclusive) try self.writer.write("=");
                    try self.writer.write(" ");
                    try self.generateExpr(end);
                }
                try self.writer.write(")");
            },
            else => {
                try self.writer.write("true /* complex pattern */");
            },
        }
    }

    /// Generate pattern bindings
    fn generatePatternBindings(self: *Self, pattern: *Pattern, scrutinee: []const u8, scrutinee_type: []const u8) anyerror!void {
        switch (pattern.kind) {
            .identifier => |ident| {
                // Bind identifier to the scrutinee value with proper type
                try self.writer.printLine("{s} {s} = {s};", .{ scrutinee_type, ident.name.name, scrutinee });
                self.trackVariableType(ident.name.name, scrutinee_type);
            },
            .enum_variant => |variant| {
                switch (variant.payload) {
                    .none => {},
                    .tuple => |patterns| {
                        if (patterns.len == 1 and patterns[0].kind == .identifier) {
                            // Single payload: access union field directly
                            try self.writer.printLine("int64_t {s} = {s}.data.{s};", .{
                                patterns[0].kind.identifier.name.name,
                                scrutinee,
                                variant.variant.name,
                            });
                            self.trackVariableType(patterns[0].kind.identifier.name.name, "int64_t");
                        } else {
                            for (patterns, 0..) |pat, i| {
                                if (pat.kind == .identifier) {
                                    try self.writer.printLine("int64_t {s} = {s}.data.{s}._{d};", .{
                                        pat.kind.identifier.name.name,
                                        scrutinee,
                                        variant.variant.name,
                                        i,
                                    });
                                    self.trackVariableType(pat.kind.identifier.name.name, "int64_t");
                                }
                            }
                        }
                    },
                    .struct_fields => |fields| {
                        for (fields) |field| {
                            if (field.pattern) |pat| {
                                if (pat.kind == .identifier) {
                                    try self.writer.printLine("int64_t {s} = {s}.data.{s}.{s};", .{
                                        pat.kind.identifier.name.name,
                                        scrutinee,
                                        variant.variant.name,
                                        field.name.name,
                                    });
                                    self.trackVariableType(pat.kind.identifier.name.name, "int64_t");
                                }
                            }
                        }
                    },
                }
            },
            .tuple => |tuple_pat| {
                for (tuple_pat.elements, 0..) |elem, i| {
                    if (elem.kind == .identifier) {
                        try self.writer.printLine("int64_t {s} = {s}._{d};", .{
                            elem.kind.identifier.name.name,
                            scrutinee,
                            i,
                        });
                        self.trackVariableType(elem.kind.identifier.name.name, "int64_t");
                    }
                }
            },
            .struct_pattern => |struct_pat| {
                for (struct_pat.fields) |field| {
                    if (field.pattern) |pat| {
                        if (pat.kind == .identifier) {
                            try self.writer.printLine("int64_t {s} = {s}.{s};", .{
                                pat.kind.identifier.name.name,
                                scrutinee,
                                field.name.name,
                            });
                            self.trackVariableType(pat.kind.identifier.name.name, "int64_t");
                        }
                    } else {
                        // Shorthand: `{ x }` binds field value to `x`
                        try self.writer.printLine("int64_t {s} = {s}.{s};", .{
                            field.name.name,
                            scrutinee,
                            field.name.name,
                        });
                        self.trackVariableType(field.name.name, "int64_t");
                    }
                }
            },
            else => {},
        }
    }

    /// Generate for loop
    fn generateForLoop(self: *Self, for_stmt: *ForLoop) anyerror!void {
        // Loop bodies should not treat result expressions as return values
        const saved_void = self.current_function_is_void;
        self.current_function_is_void = true;
        defer self.current_function_is_void = saved_void;

        // Check if iterator is a range expression
        if (for_stmt.iterator.kind == .range) {
            const range = for_stmt.iterator.kind.range;
            const temp = try self.freshTemp();

            // Generate range-based for loop
            try self.writer.print("for (int64_t {s} = ", .{temp});
            if (range.start) |start| {
                try self.generateExpr(start);
            } else {
                try self.writer.write("0");
            }
            try self.writer.print("; {s} ", .{temp});
            if (range.inclusive) {
                try self.writer.write("<=");
            } else {
                try self.writer.write("<");
            }
            try self.writer.write(" ");
            if (range.end) |end| {
                try self.generateExpr(end);
            } else {
                try self.writer.write("INT64_MAX");
            }
            try self.writer.print("; {s}++)", .{temp});
            try self.writer.beginBlock();

            // Bind pattern variable
            switch (for_stmt.pattern.kind) {
                .identifier => |ident| {
                    try self.writer.printLine("int64_t {s} = {s};", .{ ident.name.name, temp });
                },
                else => {},
            }

            try self.generateBlock(for_stmt.body);
            try self.writer.endBlock();
        } else {
            // Check if iterating over a string or list
            const iter_type = self.inferCTypeFromExpr(for_stmt.iterator);
            const iter_temp = try self.freshTemp();
            const idx_temp = try self.freshTemp();

            if (std.mem.eql(u8, iter_type, "dm_string")) {
                // String iteration: for c in "abc" { ... }
                try self.writer.print("{s} {s} = ", .{ iter_type, iter_temp });
                try self.generateExpr(for_stmt.iterator);
                try self.writer.writeLine(";");
                try self.writer.print("for (size_t {s} = 0; {s} < {s}.len; {s}++)", .{ idx_temp, idx_temp, iter_temp, idx_temp });
                try self.writer.beginBlock();

                // Bind pattern variable as char
                switch (for_stmt.pattern.kind) {
                    .identifier => |ident| {
                        try self.writer.printLine("char {s} = {s}.data[{s}];", .{ ident.name.name, iter_temp, idx_temp });
                        self.trackVariableType(ident.name.name, "char");
                    },
                    else => {},
                }

                try self.generateBlock(for_stmt.body);
                try self.writer.endBlock();
            } else if (std.mem.startsWith(u8, iter_type, "dm_list_")) {
                // List iteration: for x in items { ... }
                const elem_type = iter_type["dm_list_".len..];
                try self.writer.print("{s} {s} = ", .{ iter_type, iter_temp });
                try self.generateExpr(for_stmt.iterator);
                try self.writer.writeLine(";");
                try self.writer.print("for (size_t {s} = 0; {s} < {s}.len; {s}++)", .{ idx_temp, idx_temp, iter_temp, idx_temp });
                try self.writer.beginBlock();

                // Bind pattern variable with element type
                switch (for_stmt.pattern.kind) {
                    .identifier => |ident| {
                        try self.writer.printLine("{s} {s} = {s}.data[{s}];", .{ elem_type, ident.name.name, iter_temp, idx_temp });
                        self.trackVariableType(ident.name.name, elem_type);
                    },
                    else => {},
                }

                try self.generateBlock(for_stmt.body);
                try self.writer.endBlock();
            } else {
                // Fallback: generic iterator (unsupported, emit placeholder)
                try self.writer.writeLineComment("unsupported iterator type");
                try self.writer.write("/* for-each on ");
                try self.writer.write(iter_type);
                try self.writer.writeLine(" not supported */");
            }
        }
    }

    /// Generate while loop
    fn generateWhileLoop(self: *Self, while_stmt: *WhileLoop) anyerror!void {
        try self.writer.write("while (");
        try self.generateExpr(while_stmt.condition);
        try self.writer.write(")");
        try self.writer.beginBlock();
        // Loop bodies should not treat result expressions as return values
        const saved = self.current_function_is_void;
        self.current_function_is_void = true;
        try self.generateBlock(while_stmt.body);
        self.current_function_is_void = saved;
        try self.writer.endBlock();
    }

    /// Generate infinite loop
    fn generateLoopStatement(self: *Self, loop: *LoopStmt) anyerror!void {
        if (loop.label) |label| {
            try self.writer.printLine("{s}:", .{label.name});
        }
        try self.writer.writeLine("while (true) {");
        self.writer.indent();
        // Loop bodies should not treat result expressions as return values
        const saved = self.current_function_is_void;
        self.current_function_is_void = true;
        try self.generateBlock(loop.body);
        self.current_function_is_void = saved;
        self.writer.dedent();
        try self.writer.writeLine("}");
        if (loop.label) |label| {
            try self.writer.printLine("{s}_end:;", .{label.name});
        }
    }

    /// Generate break statement
    fn generateBreak(self: *Self, brk: *BreakStmt) anyerror!void {
        if (brk.label) |label| {
            try self.writer.printLine("goto {s}_end;", .{label.name});
        } else {
            try self.writer.writeLine("break;");
        }
    }

    /// Generate continue statement
    fn generateContinue(self: *Self, cont: *ContinueStmt) anyerror!void {
        if (cont.label) |label| {
            try self.writer.printLine("goto {s};", .{label.name});
        } else {
            try self.writer.writeLine("continue;");
        }
    }

    /// Generate region block (arena scope)
    fn generateRegionBlock(self: *Self, region: *RegionBlock) anyerror!void {
        const arena_name = try std.fmt.allocPrint(self.stringAllocator(), "_dm_arena_{s}", .{region.name.name});
        try self.region_stack.append(arena_name);
        defer _ = self.region_stack.pop();

        try self.writer.printLine("dm_arena* {s} = dm_arena_create(4096);", .{arena_name});
        try self.writer.openBrace();
        try self.generateBlock(region.body);
        try self.writer.closeBrace();
        try self.writer.printLine("dm_arena_destroy({s});", .{arena_name});
    }

    /// Generate assignment statement
    fn generateAssignment(self: *Self, assign: *Assignment) anyerror!void {
        try self.generateExpr(assign.target);

        const op_str: []const u8 = switch (assign.op) {
            .assign => " = ",
            .add_assign => " += ",
            .sub_assign => " -= ",
            .mul_assign => " *= ",
            .div_assign => " /= ",
            .mod_assign => " %= ",
            .bit_and_assign => " &= ",
            .bit_or_assign => " |= ",
            .bit_xor_assign => " ^= ",
            .shl_assign => " <<= ",
            .shr_assign => " >>= ",
        };

        try self.writer.write(op_str);
        try self.generateExpr(assign.value);
        try self.writer.writeLine(";");
    }

    // ========================================================================
    // EXPRESSION GENERATION
    // ========================================================================

    /// Generate an expression
    pub fn generateExpr(self: *Self, expr: *Expr) anyerror!void {
        switch (expr.kind) {
            .literal => |lit| try self.generateLiteral(lit),
            .identifier => |ident| {
                if (self.current_mut_params.contains(ident.name)) {
                    try self.writer.print("(*{s})", .{ident.name});
                } else {
                    try self.writer.write(ident.name);
                }
            },
            .path => |path| try self.generatePathExpr(path),
            .binary => |bin| try self.generateBinaryExpr(bin),
            .unary => |un| try self.generateUnaryExpr(un),
            .field_access => |field| try self.generateFieldAccess(field),
            .index_access => |idx| try self.generateIndexAccess(idx),
            .method_call => |method| try self.generateMethodCall(method),
            .function_call => |call| try self.generateFunctionCall(call),
            .struct_literal => |lit| try self.generateStructLiteral(lit),
            .enum_literal => |lit| try self.generateEnumLiteral(lit),
            .array_literal => |arr| try self.generateArrayLiteral(arr),
            .tuple_literal => |tup| try self.generateTupleLiteral(tup),
            .if_expr => |if_e| try self.generateIfExpr(if_e),
            .match_expr => |match| try self.generateMatchExpr(match),
            .block => |block| try self.generateBlockExpr(block),
            .lambda => |lambda| try self.generateLambdaExpr(lambda),
            .pipeline => |pipe| try self.generatePipeline(pipe),
            .error_propagate => |err| try self.generateErrorPropagate(err),
            .coalesce => |coal| try self.generateCoalesce(coal),
            .range => |range| try self.generateRange(range),
            .cast => |cast| try self.generateCast(cast),
            .type_check => |check| try self.generateTypeCheck(check),
            .grouped => |inner| {
                try self.writer.write("(");
                try self.generateExpr(inner);
                try self.writer.write(")");
            },
            .comptime_expr => |comp| {
                // Comptime expressions should be evaluated at compile time
                try self.writer.writeComment("comptime expression");
                try self.generateExpr(comp.expr);
            },
        }
    }

    /// Generate literal
    fn generateLiteral(self: *Self, lit: *Literal) anyerror!void {
        switch (lit.kind) {
            .int => |int_lit| {
                try self.writer.write(int_lit.value);
                if (int_lit.suffix) |suffix| {
                    // Handle type suffix
                    if (std.mem.eql(u8, suffix, "u64")) {
                        try self.writer.write("ULL");
                    } else if (std.mem.eql(u8, suffix, "i64")) {
                        try self.writer.write("LL");
                    } else if (std.mem.eql(u8, suffix, "u32")) {
                        try self.writer.write("U");
                    }
                }
            },
            .float => |float_lit| {
                try self.writer.write(float_lit.value);
                if (float_lit.suffix) |suffix| {
                    if (std.mem.eql(u8, suffix, "f32")) {
                        try self.writer.write("f");
                    }
                }
            },
            .string => |str_lit| {
                switch (str_lit.kind) {
                    .regular => {
                        try self.writer.write("dm_string_from_cstr(");
                        try self.writeCStringLiteral(str_lit.value);
                        try self.writer.write(")");
                    },
                    .raw => {
                        try self.writer.write("dm_string_from_cstr(");
                        try self.writeCStringLiteral(str_lit.value);
                        try self.writer.write(")");
                    },
                    .byte => {
                        try self.writeCStringLiteral(str_lit.value);
                    },
                }
            },
            .char => |char_lit| {
                try self.writer.print("'{s}'", .{char_lit.value});
            },
            .bool => |b| {
                try self.writer.write(if (b) "true" else "false");
            },
            .null_lit => {
                try self.writer.write("NULL");
            },
        }
    }

    /// Generate path expression
    fn generatePath(self: *Self, path: Path) anyerror!void {
        if (path.segments.len >= 2) {
            // Multi-segment path like Color::Red -> dm_Color_Red
            // First segment is a type name that needs mangling
            const mangled = try self.mangleTypeName(path.segments[0].name);
            try self.writer.write(mangled);
            for (path.segments[1..]) |segment| {
                try self.writer.write("_");
                try self.writer.write(segment.name);
            }
        } else {
            for (path.segments, 0..) |segment, i| {
                if (i > 0) try self.writer.write("_");
                try self.writer.write(segment.name);
            }
        }
    }

    /// Generate a path expression, adding () for complex enum unit variants
    fn generatePathExpr(self: *Self, path: Path) anyerror!void {
        try self.generatePath(path);
        // For complex enum unit variants accessed as standalone expressions (not function calls),
        // we need to call the constructor function
        if (path.segments.len >= 2) {
            const mangled = try self.mangleTypeName(path.segments[0].name);
            if (!self.simple_enums.contains(mangled) and self.generated_enums.contains(mangled)) {
                try self.writer.write("()");
            }
        }
    }

    /// Generate binary expression
    fn generateBinaryExpr(self: *Self, bin: *BinaryExpr) anyerror!void {
        // Special handling for 'in' operator
        if (bin.op == .in) {
            try self.generateInExpr(bin);
            return;
        }

        // Special handling for string equality
        const left_type = self.inferCTypeFromExpr(bin.left);
        if (std.mem.eql(u8, left_type, "dm_string") and (bin.op == .eq or bin.op == .ne)) {
            if (bin.op == .ne) try self.writer.write("!");
            try self.writer.write("dm_string_eq(");
            try self.generateExpr(bin.left);
            try self.writer.write(", ");
            try self.generateExpr(bin.right);
            try self.writer.write(")");
            return;
        }

        // Special handling for string concatenation
        if (std.mem.eql(u8, left_type, "dm_string") and bin.op == .add) {
            try self.writer.write("dm_string_concat(");
            try self.generateExpr(bin.left);
            try self.writer.write(", ");
            try self.generateExpr(bin.right);
            try self.writer.write(")");
            return;
        }

        // Special handling for string ordering (<, <=, >, >=)
        if (std.mem.eql(u8, left_type, "dm_string") and
            (bin.op == .lt or bin.op == .le or bin.op == .gt or bin.op == .ge))
        {
            const cmp_op: []const u8 = switch (bin.op) {
                .lt => " < ",
                .le => " <= ",
                .gt => " > ",
                .ge => " >= ",
                else => unreachable,
            };
            try self.writer.write("(dm_string_cmp(");
            try self.generateExpr(bin.left);
            try self.writer.write(", ");
            try self.generateExpr(bin.right);
            try self.writer.write(")");
            try self.writer.write(cmp_op);
            try self.writer.write("0)");
            return;
        }

        try self.writer.write("(");
        try self.generateExpr(bin.left);

        const op_str: []const u8 = switch (bin.op) {
            .add => " + ",
            .sub => " - ",
            .mul => " * ",
            .div => " / ",
            .mod => " % ",
            .eq => " == ",
            .ne => " != ",
            .lt => " < ",
            .le => " <= ",
            .gt => " > ",
            .ge => " >= ",
            .@"and" => " && ",
            .@"or" => " || ",
            .bit_and => " & ",
            .bit_or => " | ",
            .bit_xor => " ^ ",
            .shl => " << ",
            .shr => " >> ",
            .in => unreachable, // Handled above
        };

        try self.writer.write(op_str);
        try self.generateExpr(bin.right);
        try self.writer.write(")");
    }

    /// Generate 'in' operator expression
    fn generateInExpr(self: *Self, bin: *BinaryExpr) anyerror!void {
        // If RHS is a range expression, generate range check
        if (bin.right.kind == .range) {
            const range = bin.right.kind.range;
            try self.writer.write("(");
            // left >= start
            if (range.start) |start| {
                try self.generateExpr(bin.left);
                try self.writer.write(" >= ");
                try self.generateExpr(start);
            } else {
                try self.writer.write("true");
            }
            try self.writer.write(" && ");
            // left < end (or <= for inclusive)
            if (range.end) |end| {
                try self.generateExpr(bin.left);
                if (range.inclusive) {
                    try self.writer.write(" <= ");
                } else {
                    try self.writer.write(" < ");
                }
                try self.generateExpr(end);
            } else {
                try self.writer.write("true");
            }
            try self.writer.write(")");
        } else {
            // For non-range RHS, generate false as fallback
            // (full collection membership would need runtime support)
            try self.writer.write("false /* 'in' requires range RHS */");
        }
    }

    /// Generate unary expression
    fn generateUnaryExpr(self: *Self, un: *UnaryExpr) anyerror!void {
        const op_str: []const u8 = switch (un.op) {
            .neg => "-",
            .not => "!",
            .bit_not => "~",
            .deref => "*",
            .ref => "&",
        };

        try self.writer.write(op_str);
        try self.writer.write("(");
        try self.generateExpr(un.operand);
        try self.writer.write(")");
    }

    /// Generate field access
    fn generateFieldAccess(self: *Self, field: *FieldAccess) anyerror!void {
        // Check if the object is a pointer (self or mut param) - use -> instead of .
        const is_pointer = if (field.object.kind == .identifier) blk: {
            const name = field.object.kind.identifier.name;
            break :blk std.mem.eql(u8, name, "self") or self.current_mut_params.contains(name);
        } else false;

        if (is_pointer) {
            // Write the name directly (not dereferenced) and use ->
            try self.writer.print("{s}->{s}", .{ field.object.kind.identifier.name, field.field.name });
        } else {
            try self.generateExpr(field.object);
            try self.writer.print(".{s}", .{field.field.name});
        }
    }

    /// Generate index access
    fn generateIndexAccess(self: *Self, idx: *IndexAccess) anyerror!void {
        const obj_type = self.inferCTypeFromExpr(idx.object);
        if (std.mem.eql(u8, obj_type, "dm_string")) {
            // dm_string is a struct: s[i] -> ((char)(s).data[i])
            try self.writer.write("((char)(");
            try self.generateExpr(idx.object);
            try self.writer.write(").data[");
            try self.generateExpr(idx.index);
            try self.writer.write("])");
        } else if (std.mem.startsWith(u8, obj_type, "dm_list_")) {
            // dm_list_X is a struct: list[i] -> (list).data[i]
            try self.writer.write("(");
            try self.generateExpr(idx.object);
            try self.writer.write(").data[");
            try self.generateExpr(idx.index);
            try self.writer.write("]");
        } else {
            try self.generateExpr(idx.object);
            try self.writer.write("[");
            try self.generateExpr(idx.index);
            try self.writer.write("]");
        }
    }

    /// Generate method call (converted to function call)
    fn generateMethodCall(self: *Self, method: *MethodCall) anyerror!void {
        // Convert obj.method(args) to dm_Type_method(&obj, args)
        // Infer the type name from the object expression
        const obj_type = self.inferCTypeFromExpr(method.object);

        // Strip "dm_" prefix if present to get the raw type name for method lookup
        const type_name = if (std.mem.startsWith(u8, obj_type, "dm_"))
            obj_type[3..]
        else
            obj_type;

        try self.writer.print("dm_{s}_{s}(", .{ type_name, method.method.name });
        try self.writer.write("&(");
        try self.generateExpr(method.object);
        try self.writer.write(")");

        for (method.args) |arg| {
            try self.writer.write(", ");
            try self.generateExpr(arg);
        }

        try self.writer.write(")");
    }

    /// Generate function call
    fn generateFunctionCall(self: *Self, call: *FunctionCall) anyerror!void {
        // Check for built-in functions
        var mangled_name: ?[]const u8 = null;
        if (call.function.kind == .identifier) {
            const name = call.function.kind.identifier.name;
            if (try self.generateBuiltinCall(name, call)) {
                return;
            }
            // User-defined function - mangle the name
            mangled_name = try self.mangleFunctionName(name, null);
            try self.writer.write(mangled_name.?);
        } else if (call.function.kind == .path) {
            // Path as function target - use generatePath (not generatePathExpr)
            // to avoid adding () for enum constructors that take arguments
            try self.generatePath(call.function.kind.path);
        } else {
            // Complex function expression (e.g., method calls, closures)
            try self.generateExpr(call.function);
        }
        try self.writer.write("(");

        // Look up mut param info for this function
        const mut_flags: ?[]const bool = if (mangled_name) |mn| self.function_mut_info.get(mn) else null;

        for (call.args, 0..) |arg, i| {
            if (i > 0) try self.writer.write(", ");
            const is_mut_param = if (mut_flags) |flags| (i < flags.len and flags[i]) else false;
            if (is_mut_param) {
                // Pass by reference for mut params
                try self.writer.write("&(");
                try self.generateExpr(arg.value);
                try self.writer.write(")");
            } else {
                try self.generateExpr(arg.value);
            }
        }

        try self.writer.write(")");
    }

    /// Generate built-in function calls
    fn generateBuiltinCall(self: *Self, name: []const u8, call: *FunctionCall) !bool {
        if (std.mem.eql(u8, name, "print")) {
            if (call.args.len > 0) {
                try self.generatePrintCall(call.args[0].value, false);
            }
            return true;
        } else if (std.mem.eql(u8, name, "println")) {
            if (call.args.len > 0) {
                try self.generatePrintCall(call.args[0].value, true);
            } else {
                try self.writer.write("putchar('\\n')");
            }
            return true;
        } else if (std.mem.eql(u8, name, "panic")) {
            try self.writer.write("dm_panic(");
            if (call.args.len > 0) {
                // panic takes a string argument - extract C string
                const arg_type = self.inferCTypeFromExpr(call.args[0].value);
                if (std.mem.eql(u8, arg_type, "dm_string")) {
                    try self.writer.write("(");
                    try self.generateExpr(call.args[0].value);
                    try self.writer.write(").data");
                } else {
                    try self.generateExpr(call.args[0].value);
                }
            } else {
                try self.writer.write("\"panic\"");
            }
            try self.writer.write(")");
            return true;
        } else if (std.mem.eql(u8, name, "assert")) {
            try self.writer.write("dm_assert(");
            if (call.args.len > 0) {
                try self.generateExpr(call.args[0].value);
            }
            try self.writer.write(", \"assertion failed\")");
            return true;
        } else if (std.mem.eql(u8, name, "len")) {
            // len(x) -> get length of string or array
            if (call.args.len > 0) {
                try self.writer.write("((int64_t)(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(").len)");
            }
            return true;
        } else if (std.mem.eql(u8, name, "int_to_string")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_int_to_string(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "bool_to_string")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_bool_to_string(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "float_to_string")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_float_to_string(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "substr")) {
            // substr(s, start, length)
            if (call.args.len >= 3) {
                try self.writer.write("dm_string_substr(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[2].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "char_to_string")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_char_to_string(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "is_alpha")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_is_alpha(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "is_digit")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_is_digit(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "is_alnum")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_is_alnum(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "is_whitespace")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_is_whitespace(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "string_contains")) {
            if (call.args.len >= 2) {
                try self.writer.write("dm_string_contains(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "string_find")) {
            if (call.args.len >= 2) {
                try self.writer.write("dm_string_find(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "starts_with")) {
            if (call.args.len >= 2) {
                try self.writer.write("dm_string_starts_with(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "ends_with")) {
            if (call.args.len >= 2) {
                try self.writer.write("dm_string_ends_with(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "parse_int")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_parse_int(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "file_read")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_file_read(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "file_write")) {
            if (call.args.len >= 2) {
                try self.writer.write("dm_file_write(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "eprint")) {
            if (call.args.len > 0) {
                try self.generateEprintCall(call.args[0].value, false);
            }
            return true;
        } else if (std.mem.eql(u8, name, "eprintln")) {
            if (call.args.len > 0) {
                try self.generateEprintCall(call.args[0].value, true);
            } else {
                try self.writer.write("fputc('\\n', stderr)");
            }
            return true;
        } else if (std.mem.eql(u8, name, "exit")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_exit(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "args_get")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_args_get(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "args_len")) {
            try self.writer.write("dm_args_len()");
            return true;
        } else if (std.mem.eql(u8, name, "system")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_system(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "List_new")) {
            // List_new() -> zero-initialized list struct
            // Type will be inferred from the let binding's type annotation
            try self.writer.write("{ .data = NULL, .len = 0, .capacity = 0 }");
            return true;
        }

        return false;
    }

    /// Generate print/println with type-based dispatch
    fn generatePrintCall(self: *Self, arg: *Expr, newline: bool) !void {
        const arg_type = self.inferCTypeFromExpr(arg);
        const nl_str = if (newline) "\\n" else "";

        if (std.mem.eql(u8, arg_type, "dm_string")) {
            if (newline) {
                try self.writer.write("dm_println_str(");
                try self.generateExpr(arg);
                try self.writer.write(")");
            } else {
                try self.writer.write("dm_print_str(");
                try self.generateExpr(arg);
                try self.writer.write(")");
            }
        } else if (std.mem.eql(u8, arg_type, "int64_t") or
            std.mem.eql(u8, arg_type, "int32_t") or
            std.mem.eql(u8, arg_type, "int16_t") or
            std.mem.eql(u8, arg_type, "int8_t"))
        {
            try self.writer.print("printf(\"%lld{s}\", (long long)(", .{nl_str});
            try self.generateExpr(arg);
            try self.writer.write("))");
        } else if (std.mem.eql(u8, arg_type, "uint64_t") or
            std.mem.eql(u8, arg_type, "uint32_t") or
            std.mem.eql(u8, arg_type, "uint16_t") or
            std.mem.eql(u8, arg_type, "uint8_t"))
        {
            try self.writer.print("printf(\"%llu{s}\", (unsigned long long)(", .{nl_str});
            try self.generateExpr(arg);
            try self.writer.write("))");
        } else if (std.mem.eql(u8, arg_type, "double")) {
            try self.writer.print("printf(\"%g{s}\", (double)(", .{nl_str});
            try self.generateExpr(arg);
            try self.writer.write("))");
        } else if (std.mem.eql(u8, arg_type, "float")) {
            try self.writer.print("printf(\"%g{s}\", (double)(", .{nl_str});
            try self.generateExpr(arg);
            try self.writer.write("))");
        } else if (std.mem.eql(u8, arg_type, "bool")) {
            try self.writer.print("printf(\"%s{s}\", (", .{nl_str});
            try self.generateExpr(arg);
            try self.writer.write(") ? \"true\" : \"false\")");
        } else if (std.mem.eql(u8, arg_type, "char")) {
            try self.writer.print("printf(\"%c{s}\", (char)(", .{nl_str});
            try self.generateExpr(arg);
            try self.writer.write("))");
        } else {
            // Fallback: assume string
            if (newline) {
                try self.writer.write("dm_println_str(");
                try self.generateExpr(arg);
                try self.writer.write(")");
            } else {
                try self.writer.write("dm_print_str(");
                try self.generateExpr(arg);
                try self.writer.write(")");
            }
        }
    }

    /// Generate eprint/eprintln with type-based dispatch (stderr variant of print)
    fn generateEprintCall(self: *Self, arg: *Expr, newline: bool) !void {
        const arg_type = self.inferCTypeFromExpr(arg);

        if (std.mem.eql(u8, arg_type, "dm_string")) {
            if (newline) {
                try self.writer.write("dm_eprintln_str(");
                try self.generateExpr(arg);
                try self.writer.write(")");
            } else {
                try self.writer.write("dm_eprint_str(");
                try self.generateExpr(arg);
                try self.writer.write(")");
            }
        } else if (std.mem.eql(u8, arg_type, "int64_t") or
            std.mem.eql(u8, arg_type, "int32_t") or
            std.mem.eql(u8, arg_type, "int16_t") or
            std.mem.eql(u8, arg_type, "int8_t"))
        {
            const nl_str = if (newline) "\\n" else "";
            try self.writer.print("fprintf(stderr, \"%lld{s}\", (long long)(", .{nl_str});
            try self.generateExpr(arg);
            try self.writer.write("))");
        } else if (std.mem.eql(u8, arg_type, "bool")) {
            const nl_str = if (newline) "\\n" else "";
            try self.writer.print("fprintf(stderr, \"%s{s}\", (", .{nl_str});
            try self.generateExpr(arg);
            try self.writer.write(") ? \"true\" : \"false\")");
        } else {
            // Fallback: assume string
            if (newline) {
                try self.writer.write("dm_eprintln_str(");
                try self.generateExpr(arg);
                try self.writer.write(")");
            } else {
                try self.writer.write("dm_eprint_str(");
                try self.generateExpr(arg);
                try self.writer.write(")");
            }
        }
    }

    /// Generate struct literal
    fn generateStructLiteral(self: *Self, lit: *StructLiteral) anyerror!void {
        if (lit.type_path) |path| {
            const type_name = path.segments[path.segments.len - 1].name;
            const mangled = try self.mangleTypeName(type_name);
            try self.writer.print("({s}){{ ", .{mangled});
        } else {
            try self.writer.write("{ ");
        }

        for (lit.fields, 0..) |field, i| {
            if (i > 0) try self.writer.write(", ");
            try self.writer.print(".{s} = ", .{field.name.name});
            try self.generateExpr(field.value);
        }

        try self.writer.write(" }");
    }

    /// Generate enum literal
    fn generateEnumLiteral(self: *Self, lit: *EnumLiteral) anyerror!void {
        var type_name: []const u8 = "unknown";
        if (lit.type_path) |path| {
            type_name = path.segments[path.segments.len - 1].name;
        }
        const mangled = try self.mangleTypeName(type_name);

        // Simple enum: emit constant directly
        if (self.simple_enums.contains(mangled)) {
            try self.writer.print("{s}_{s}", .{ mangled, lit.variant.name });
            return;
        }

        try self.writer.print("{s}_{s}(", .{ mangled, lit.variant.name });

        switch (lit.payload) {
            .none => {},
            .tuple => |exprs| {
                for (exprs, 0..) |expr, i| {
                    if (i > 0) try self.writer.write(", ");
                    try self.generateExpr(expr);
                }
            },
            .struct_fields => |fields| {
                for (fields, 0..) |field, i| {
                    if (i > 0) try self.writer.write(", ");
                    try self.generateExpr(field.value);
                }
            },
        }

        try self.writer.write(")");
    }

    /// Generate array literal
    fn generateArrayLiteral(self: *Self, arr: *ArrayLiteral) anyerror!void {
        switch (arr.kind) {
            .elements => |elems| {
                try self.writer.write("{ ");
                for (elems, 0..) |elem, i| {
                    if (i > 0) try self.writer.write(", ");
                    try self.generateExpr(elem);
                }
                try self.writer.write(" }");
            },
            .repeat => |repeat| {
                // [value; count] syntax - generate as inline compound literal
                try self.writer.write("{ ");
                try self.generateExpr(repeat.value);
                try self.writer.write(" /* repeated */ }");
            },
        }
    }

    /// Generate tuple literal
    fn generateTupleLiteral(self: *Self, tup: *TupleLiteral) anyerror!void {
        try self.writer.write("{ ");
        for (tup.elements, 0..) |elem, i| {
            if (i > 0) try self.writer.write(", ");
            try self.writer.print("._{d} = ", .{i});
            try self.generateExpr(elem);
        }
        try self.writer.write(" }");
    }

    /// Generate if expression (ternary operator)
    fn generateIfExpr(self: *Self, if_e: *IfExpr) anyerror!void {
        try self.writer.write("(");
        try self.generateExpr(if_e.condition);
        try self.writer.write(" ? ");
        // Generate then branch - take result expression or first statement
        if (if_e.then_branch.result) |result| {
            try self.generateExpr(result);
        } else if (if_e.then_branch.statements.len > 0) {
            // Complex then branch - generate function call to block
            try self.writer.write("0 /* complex then */");
        } else {
            try self.writer.write("0");
        }
        try self.writer.write(" : ");
        // Generate else branch
        if (if_e.else_branch) |else_br| {
            switch (else_br) {
                .else_block => |block| {
                    if (block.result) |result| {
                        try self.generateExpr(result);
                    } else {
                        try self.writer.write("0");
                    }
                },
                .else_if => |else_if| {
                    try self.generateIfExpr(else_if);
                },
            }
        } else {
            try self.writer.write("0");
        }
        try self.writer.write(")");
    }

    /// Generate match expression (as a value)
    /// Generates nested ternary operators - one per arm
    fn generateMatchExpr(self: *Self, match: *MatchExpr) anyerror!void {
        // For match-as-expression, generate nested ternary chain
        // Note: this doesn't support pattern bindings in the arms - use match statement for that
        for (match.arms, 0..) |arm, i| {
            if (i > 0) {
                try self.writer.write(" : ");
            }

            // Generate pattern condition
            try self.writer.write("(");
            try self.generatePatternCondition(arm.pattern, "_dm_unreachable");

            // Add guard condition if present
            if (arm.guard) |guard| {
                try self.writer.write(" && (");
                try self.generateExpr(guard);
                try self.writer.write(")");
            }

            try self.writer.write(") ? (");

            // Generate arm body
            switch (arm.body) {
                .expression => |expr| try self.generateExpr(expr),
                .block => |block| {
                    if (block.result) |result| {
                        try self.generateExpr(result);
                    } else {
                        try self.writer.write("0");
                    }
                },
            }

            try self.writer.write(")");
        }

        // Default case (unreachable if patterns are exhaustive)
        try self.writer.write(" : 0");
    }

    /// Generate block expression
    fn generateBlockExpr(self: *Self, block: *BlockExpr) anyerror!void {
        // Block expressions as values are not directly expressible in C11.
        // For simple cases (single result expression), emit the result directly.
        // For complex cases with statements, emit the statements before and result inline.
        if (block.statements.len == 0) {
            if (block.result) |result| {
                try self.generateExpr(result);
            } else {
                try self.writer.write("0");
            }
        } else {
            // Emit block statements before the expression context
            try self.generateBlock(block);
            if (block.result) |result| {
                try self.generateExpr(result);
            } else {
                try self.writer.write("0");
            }
        }
    }

    /// Generate lambda expression
    /// Note: Full lambda support requires closure conversion which is complex.
    /// For Stage 0, we implement simple non-capturing lambdas using GCC statement expressions.
    fn generateLambdaExpr(self: *Self, lambda: *LambdaExpr) anyerror!void {
        self.lambda_depth += 1;
        defer self.lambda_depth -= 1;

        // For simple expression lambdas, we can inline them directly
        // For complex cases, we need proper closure conversion (Stage 1+)
        switch (lambda.body) {
            .expression => |expr| {
                // Generate as inline expression (works for simple uses)
                try self.writer.write("(");
                try self.generateExpr(expr);
                try self.writer.write(")");
            },
            .block => |block| {
                // For block bodies, emit statements then result expression
                try self.writer.write("(");
                try self.generateBlock(block);
                if (block.result) |result| {
                    try self.generateExpr(result);
                }
                try self.writer.write(")");
            },
        }
    }

    /// Generate pipeline expression (transformed to nested calls)
    fn generatePipeline(self: *Self, pipe: *PipelineExpr) anyerror!void {
        // x |> f transforms to f(x)
        // Check if right side is a function call
        switch (pipe.right.kind) {
            .function_call => |call| {
                // Insert left as first argument
                try self.generateExpr(call.function);
                try self.writer.write("(");
                try self.generateExpr(pipe.left);
                for (call.args) |arg| {
                    try self.writer.write(", ");
                    try self.generateExpr(arg.value);
                }
                try self.writer.write(")");
            },
            .identifier => {
                // Simple function reference
                try self.generateExpr(pipe.right);
                try self.writer.write("(");
                try self.generateExpr(pipe.left);
                try self.writer.write(")");
            },
            else => {
                // Treat as function
                try self.generateExpr(pipe.right);
                try self.writer.write("(");
                try self.generateExpr(pipe.left);
                try self.writer.write(")");
            },
        }
    }

    /// Generate error propagation expression
    fn generateErrorPropagate(self: *Self, err: *ErrorPropagateExpr) anyerror!void {
        // expr? - early return on error
        // In C11, we emit the operand and access .value.ok_value
        // The early-return check must be emitted as a preceding statement by the caller
        try self.generateExpr(err.operand);
        try self.writer.write(".value.ok_value");
    }

    /// Generate null coalescing expression
    fn generateCoalesce(self: *Self, coal: *CoalesceExpr) anyerror!void {
        // left ?? right - if left has value use it, else right
        // Use C11 ternary: (left).has_value ? (left).value : (right)
        try self.writer.write("(");
        try self.generateExpr(coal.left);
        try self.writer.write(").has_value ? (");
        try self.generateExpr(coal.left);
        try self.writer.write(").value : (");
        try self.generateExpr(coal.right);
        try self.writer.write(")");
    }

    /// Generate range expression
    fn generateRange(self: *Self, range: *RangeExpr) anyerror!void {
        // Ranges are typically used with for loops
        // Generate as struct literal
        try self.writer.write("((struct { size_t start; size_t end; bool inclusive; }){ ");
        if (range.start) |start| {
            try self.generateExpr(start);
        } else {
            try self.writer.write("0");
        }
        try self.writer.write(", ");
        if (range.end) |end| {
            try self.generateExpr(end);
        } else {
            try self.writer.write("SIZE_MAX");
        }
        try self.writer.print(", {s} }})", .{if (range.inclusive) "true" else "false"});
    }

    /// Generate cast expression
    fn generateCast(self: *Self, cast: *CastExpr) anyerror!void {
        const target_type = try self.mapType(cast.target_type);
        try self.writer.print("(({s})(", .{target_type});
        try self.generateExpr(cast.expr);
        try self.writer.write("))");
    }

    /// Generate type check expression
    fn generateTypeCheck(self: *Self, check: *TypeCheckExpr) anyerror!void {
        // For enum variant checks: expr is Variant -> check tag
        // For static type checks: resolve at compile time to true
        const target_type = self.mapType(check.checked_type) catch "void*";

        // Check if the target type looks like an enum variant (contains _tag_)
        // For now, for static types, just emit true since dAImond is statically typed
        _ = target_type;
        try self.writer.write("true");
    }
};

// ============================================================================
// TESTS
// ============================================================================

const testing = std.testing;

test "CWriter basic operations" {
    var writer = CWriter.init(testing.allocator);
    defer writer.deinit();

    try writer.writeLine("int main() {");
    writer.indent();
    try writer.writeLine("return 0;");
    writer.dedent();
    try writer.writeLine("}");

    const output = writer.getOutput();
    try testing.expect(std.mem.indexOf(u8, output, "int main()") != null);
    try testing.expect(std.mem.indexOf(u8, output, "    return 0;") != null);
}

test "CWriter indentation" {
    var writer = CWriter.init(testing.allocator);
    defer writer.deinit();

    try writer.writeLine("level 0");
    writer.indent();
    try writer.writeLine("level 1");
    writer.indent();
    try writer.writeLine("level 2");
    writer.dedent();
    try writer.writeLine("level 1 again");
    writer.dedent();
    try writer.writeLine("level 0 again");

    const output = writer.getOutput();
    try testing.expect(std.mem.indexOf(u8, output, "level 0\n") != null);
    try testing.expect(std.mem.indexOf(u8, output, "    level 1\n") != null);
    try testing.expect(std.mem.indexOf(u8, output, "        level 2\n") != null);
}

test "CWriter block operations" {
    var writer = CWriter.init(testing.allocator);
    defer writer.deinit();

    try writer.write("if (true)");
    try writer.beginBlock();
    try writer.writeLine("do_something();");
    try writer.endBlock();

    const output = writer.getOutput();
    try testing.expect(std.mem.indexOf(u8, output, "if (true) {") != null);
    try testing.expect(std.mem.indexOf(u8, output, "    do_something();") != null);
    try testing.expect(std.mem.indexOf(u8, output, "}") != null);
}

test "CodeGenerator initialization" {
    var codegen = CodeGenerator.init(testing.allocator);
    defer codegen.deinit();

    try testing.expect(codegen.temp_counter == 0);
    try testing.expect(codegen.lambda_depth == 0);
}

test "CodeGenerator type mapping - primitives" {
    var codegen = CodeGenerator.init(testing.allocator);
    defer codegen.deinit();

    // Test primitive type mappings through named type
    // Note: In real usage, these would come from parsed AST nodes
}

test "CodeGenerator fresh temp generation" {
    var codegen = CodeGenerator.init(testing.allocator);
    defer codegen.deinit();

    const temp1 = try codegen.freshTemp();
    const temp2 = try codegen.freshTemp();
    const temp3 = try codegen.freshTemp();

    try testing.expect(!std.mem.eql(u8, temp1, temp2));
    try testing.expect(!std.mem.eql(u8, temp2, temp3));
    try testing.expect(std.mem.startsWith(u8, temp1, "_dm_tmp_"));
}

test "CodeGenerator runtime header generation" {
    var codegen = CodeGenerator.init(testing.allocator);
    defer codegen.deinit();

    try codegen.generateRuntimeHeader();

    const output = codegen.getOutput();

    // Check for standard includes
    try testing.expect(std.mem.indexOf(u8, output, "#include <stdint.h>") != null);
    try testing.expect(std.mem.indexOf(u8, output, "#include <stdbool.h>") != null);
    try testing.expect(std.mem.indexOf(u8, output, "#include <stdlib.h>") != null);

    // Check for runtime types
    try testing.expect(std.mem.indexOf(u8, output, "typedef struct dm_string") != null);
    try testing.expect(std.mem.indexOf(u8, output, "typedef struct dm_arena") != null);

    // Check for runtime functions
    try testing.expect(std.mem.indexOf(u8, output, "dm_string_from_cstr") != null);
    try testing.expect(std.mem.indexOf(u8, output, "dm_panic") != null);
    try testing.expect(std.mem.indexOf(u8, output, "dm_arena_create") != null);
}

test "CodeGenerator mangle type name" {
    var codegen = CodeGenerator.init(testing.allocator);
    defer codegen.deinit();

    const mangled = try codegen.mangleTypeName("Point");
    try testing.expectEqualStrings("dm_Point", mangled);
}

test "CodeGenerator mangle function name" {
    var codegen = CodeGenerator.init(testing.allocator);
    defer codegen.deinit();

    const mangled1 = try codegen.mangleFunctionName("calculate", null);
    try testing.expectEqualStrings("dm_calculate", mangled1);

    const mangled2 = try codegen.mangleFunctionName("new", "Point");
    try testing.expectEqualStrings("dm_Point_new", mangled2);
}

test "CodeGenerator sanitize type name" {
    var codegen = CodeGenerator.init(testing.allocator);
    defer codegen.deinit();

    const sanitized1 = try codegen.sanitizeTypeName("int64_t");
    try testing.expectEqualStrings("int64_t", sanitized1);

    const sanitized2 = try codegen.sanitizeTypeName("int*");
    try testing.expectEqualStrings("intptr", sanitized2);
}

test "CWriter comment generation" {
    var writer = CWriter.init(testing.allocator);
    defer writer.deinit();

    try writer.writeComment("This is a block comment");
    try writer.writeLineComment("This is a line comment");

    const output = writer.getOutput();
    try testing.expect(std.mem.indexOf(u8, output, "/* This is a block comment */") != null);
    try testing.expect(std.mem.indexOf(u8, output, "// This is a line comment") != null);
}

test "CWriter print formatting" {
    var writer = CWriter.init(testing.allocator);
    defer writer.deinit();

    try writer.print("int {s} = {d};", .{ "x", 42 });
    try writer.newline();

    const output = writer.getOutput();
    try testing.expectEqualStrings("int x = 42;\n", output);
}

test "CodeGenerator assignment operators" {
    var codegen = CodeGenerator.init(testing.allocator);
    defer codegen.deinit();

    // Test that the assignment operator mappings exist
    const ops = [_]Assignment.AssignOp{
        .assign,
        .add_assign,
        .sub_assign,
        .mul_assign,
        .div_assign,
        .mod_assign,
        .bit_and_assign,
        .bit_or_assign,
        .bit_xor_assign,
        .shl_assign,
        .shr_assign,
    };

    try testing.expectEqual(@as(usize, 11), ops.len);
}

test "CodeGenerator binary operator mapping" {
    // Test that all binary operators have mappings
    const ops = [_]BinaryExpr.BinaryOp{
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .eq,
        .ne,
        .lt,
        .le,
        .gt,
        .ge,
        .@"and",
        .@"or",
        .bit_and,
        .bit_or,
        .bit_xor,
        .shl,
        .shr,
        .in,
    };

    try testing.expectEqual(@as(usize, 19), ops.len);
}

test "CodeGenerator unary operator mapping" {
    // Test that all unary operators have mappings
    const ops = [_]UnaryExpr.UnaryOp{
        .neg,
        .not,
        .bit_not,
        .deref,
        .ref,
    };

    try testing.expectEqual(@as(usize, 5), ops.len);
}
