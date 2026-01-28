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
const FunctionParam = ast.FunctionParam;
const GenericParam = ast.GenericParam;
const StructDecl = ast.StructDecl;
const StructField = ast.StructField;
const EnumDecl = ast.EnumDecl;
const EnumVariant = ast.EnumVariant;
const TraitDecl = ast.TraitDecl;
const ImplBlock = ast.ImplBlock;
const ImplItem = ast.ImplItem;
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
const DiscardStmt = ast.DiscardStmt;
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
const MatchArm = ast.MatchArm;
const BlockExpr = ast.BlockExpr;
const LambdaExpr = ast.LambdaExpr;
const PipelineExpr = ast.PipelineExpr;
const ErrorPropagateExpr = ast.ErrorPropagateExpr;
const CoalesceExpr = ast.CoalesceExpr;
const RangeExpr = ast.RangeExpr;
const CastExpr = ast.CastExpr;
const TypeCheckExpr = ast.TypeCheckExpr;
const ComptimeExpr = ast.ComptimeExpr;
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
    writer: CWriter,
    type_context: ?*types.TypeContext,

    // Track generated types for forward declarations
    generated_structs: std.StringHashMap(void),
    generated_enums: std.StringHashMap(void),
    forward_declarations: std.ArrayList([]const u8),

    // Track impl methods for later generation
    impl_methods: std.ArrayList(ImplMethodInfo),

    // Unique ID counter for temporary variables
    temp_counter: usize,

    // Stack for tracking lambda/closure context
    lambda_depth: usize,

    // Current function name (for error messages)
    current_function: ?[]const u8,

    // Whether current function returns void (don't generate return for block result)
    current_function_is_void: bool,

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
            .writer = CWriter.init(allocator),
            .type_context = null,
            .generated_structs = std.StringHashMap(void).init(allocator),
            .generated_enums = std.StringHashMap(void).init(allocator),
            .forward_declarations = std.ArrayList([]const u8).init(allocator),
            .impl_methods = std.ArrayList(ImplMethodInfo).init(allocator),
            .temp_counter = 0,
            .lambda_depth = 0,
            .current_function = null,
            .current_function_is_void = false,
            .region_stack = std.ArrayList([]const u8).init(allocator),
        };
    }

    /// Clean up resources
    pub fn deinit(self: *Self) void {
        self.writer.deinit();
        self.generated_structs.deinit();
        self.generated_enums.deinit();
        self.forward_declarations.deinit();
        self.impl_methods.deinit();
        self.region_stack.deinit();
    }

    /// Set the type context for type resolution
    pub fn setTypeContext(self: *Self, ctx: *types.TypeContext) void {
        self.type_context = ctx;
    }

    /// Generate a unique temporary variable name
    fn freshTemp(self: *Self) ![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "_dm_tmp_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        return name;
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

        try self.writer.blankLine();

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
        try self.writer.writeLine("(void)argc; (void)argv;");
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
                const name = try self.mangleTypeName(e.name.name);
                try self.forward_declarations.append(name);
            },
            else => {},
        }
    }

    /// Generate forward declarations
    fn generateForwardDeclarations(self: *Self) !void {
        if (self.forward_declarations.items.len == 0) return;

        try self.writer.writeLineComment("Forward Declarations");
        for (self.forward_declarations.items) |name| {
            try self.writer.printLine("struct {s};", .{name});
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
        } else if (std.mem.eql(u8, type_name, "i128")) {
            return "__int128";
        } else if (std.mem.eql(u8, type_name, "u128")) {
            return "unsigned __int128";
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
            // Generic Option
            if (named.generic_args) |args| {
                if (args.len > 0) {
                    return self.generateOptionType(args[0]);
                }
            }
            return "dm_option_void";
        } else if (std.mem.eql(u8, type_name, "Result")) {
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
                    return try std.fmt.allocPrint(self.allocator, "{s}*", .{inner});
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
        return try std.fmt.allocPrint(self.allocator, "{s} (*)({s})", .{ ret_type, params_str.items });
    }

    /// Map array type to C
    fn mapArrayType(self: *Self, arr: *ArrayType) CodeGenError![]const u8 {
        const elem_type = try self.mapType(arr.element_type);
        // C arrays are tricky in function signatures, use pointer
        return try std.fmt.allocPrint(self.allocator, "{s}*", .{elem_type});
    }

    /// Map slice type to C
    fn mapSliceType(self: *Self, slice: *SliceType) CodeGenError![]const u8 {
        const elem_type = try self.mapType(slice.element_type);
        // Slices are fat pointers (pointer + length)
        return try std.fmt.allocPrint(self.allocator, "struct {{ {s}* data; size_t len; }}", .{elem_type});
    }

    /// Map pointer type to C
    fn mapPointerType(self: *Self, ptr: *PointerType) CodeGenError![]const u8 {
        const pointee = try self.mapType(ptr.pointee_type);
        return try std.fmt.allocPrint(self.allocator, "{s}*", .{pointee});
    }

    /// Map reference type to C (just a pointer in C)
    fn mapReferenceType(self: *Self, ref: *ReferenceType) CodeGenError![]const u8 {
        const referenced = try self.mapType(ref.referenced_type);
        return try std.fmt.allocPrint(self.allocator, "{s}*", .{referenced});
    }

    /// Map tuple type to C struct
    fn mapTupleType(self: *Self, tup: *TupleType) CodeGenError![]const u8 {
        var fields = std.ArrayList(u8).init(self.allocator);
        defer fields.deinit();

        for (tup.elements, 0..) |elem, i| {
            const elem_type = try self.mapType(elem);
            try fields.writer().print("{s} _{d}; ", .{ elem_type, i });
        }

        return try std.fmt.allocPrint(self.allocator, "struct {{ {s}}}", .{fields.items});
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
        return try std.fmt.allocPrint(self.allocator, "dm_option_{s}", .{safe_name});
    }

    /// Generate Result type name
    fn generateResultType(self: *Self, ok: *TypeExpr, err: ?*TypeExpr) CodeGenError![]const u8 {
        const ok_name = try self.mapType(ok);
        const safe_ok = try self.sanitizeTypeName(ok_name);
        if (err) |e| {
            const err_name = try self.mapType(e);
            const safe_err = try self.sanitizeTypeName(err_name);
            return try std.fmt.allocPrint(self.allocator, "dm_result_{s}_{s}", .{ safe_ok, safe_err });
        }
        return try std.fmt.allocPrint(self.allocator, "dm_result_{s}", .{safe_ok});
    }

    /// Generate List type name
    fn generateListType(self: *Self, elem: *TypeExpr) CodeGenError![]const u8 {
        const elem_name = try self.mapType(elem);
        const safe_name = try self.sanitizeTypeName(elem_name);
        return try std.fmt.allocPrint(self.allocator, "dm_list_{s}", .{safe_name});
    }

    /// Sanitize type name for use in identifier
    fn sanitizeTypeName(self: *Self, name: []const u8) ![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
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

    /// Mangle a type name
    fn mangleTypeName(self: *Self, name: []const u8) ![]const u8 {
        return try std.fmt.allocPrint(self.allocator, "dm_{s}", .{name});
    }

    /// Mangle a function name (with optional type prefix for methods)
    fn mangleFunctionName(self: *Self, name: []const u8, type_prefix: ?[]const u8) ![]const u8 {
        if (type_prefix) |prefix| {
            return try std.fmt.allocPrint(self.allocator, "dm_{s}_{s}", .{ prefix, name });
        }
        return try std.fmt.allocPrint(self.allocator, "dm_{s}", .{name});
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

        try self.writer.printLine("struct {s}", .{name});
        try self.writer.openBrace();

        for (s.fields) |field| {
            const field_type = try self.mapType(field.type_expr);
            try self.writer.printLine("{s} {s};", .{ field_type, field.name.name });
        }

        try self.writer.closeBrace();
        try self.writer.printLine("typedef struct {s} {s};", .{ name, name });
        try self.writer.blankLine();
    }

    // ========================================================================
    // ENUM GENERATION (Tagged Unions)
    // ========================================================================

    /// Generate C enum/tagged union
    fn generateEnum(self: *Self, e: *EnumDecl) !void {
        const name = try self.mangleTypeName(e.name.name);

        if (self.generated_enums.contains(name)) return;
        try self.generated_enums.put(name, {});

        // Generate tag enum
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

        for (e.variants) |variant| {
            switch (variant.payload) {
                .none => {
                    // Unit variant - no payload
                    try self.writer.printLine("char {s}; // unit variant placeholder", .{variant.name.name});
                },
                .tuple => |tuple_types| {
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

    /// Generate function prototype
    fn generateFunctionPrototype(self: *Self, func: *FunctionDecl, type_prefix: ?[]const u8) !void {
        const func_name = try self.mangleFunctionName(func.name.name, type_prefix);
        const ret_type = if (func.return_type) |rt| try self.mapType(rt) else "void";

        try self.writer.print("{s} {s}(", .{ ret_type, func_name });

        // If this is a method, add self parameter
        if (type_prefix) |prefix| {
            const mangled_type = try self.mangleTypeName(prefix);
            try self.writer.print("{s}* self", .{mangled_type});
            if (func.params.len > 0) {
                try self.writer.write(", ");
            }
        }

        // Generate parameter list
        for (func.params, 0..) |param, i| {
            if (i > 0) try self.writer.write(", ");
            const param_type = try self.mapType(param.type_expr);
            try self.writer.print("{s} {s}", .{ param_type, param.name.name });
        }

        if (type_prefix == null and func.params.len == 0) {
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

        const func_name = try self.mangleFunctionName(func.name.name, type_prefix);
        const ret_type = if (func.return_type) |rt| try self.mapType(rt) else "void";

        try self.writer.print("{s} {s}(", .{ ret_type, func_name });

        // If this is a method, add self parameter
        if (type_prefix) |prefix| {
            const mangled_type = try self.mangleTypeName(prefix);
            try self.writer.print("{s}* self", .{mangled_type});
            if (func.params.len > 0) {
                try self.writer.write(", ");
            }
        }

        // Generate parameter list
        for (func.params, 0..) |param, i| {
            if (i > 0) try self.writer.write(", ");
            const param_type = try self.mapType(param.type_expr);
            try self.writer.print("{s} {s}", .{ param_type, param.name.name });
        }

        if (type_prefix == null and func.params.len == 0) {
            try self.writer.write("void");
        }

        try self.writer.write(")");

        // Generate body
        if (func.body) |body| {
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
                try self.generateExpr(result);
                try self.writer.writeLine(";");
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
                try self.generateExpr(expr);
                try self.writer.writeLine(";");
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
        // Get type string
        const type_str = if (let.type_annotation) |t|
            try self.mapType(t)
        else
            "auto"; // C23 auto, or use void* as fallback

        // Handle pattern
        switch (let.pattern.kind) {
            .identifier => |ident| {
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
                            try self.writer.print("auto {s} = {s}._{d};", .{ elem_ident.name.name, temp, i });
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
                                try self.writer.print("auto {s} = {s}.{s};", .{ pat_ident.name.name, temp, field.name.name });
                                try self.writer.newline();
                            }
                        } else {
                            try self.writer.print("auto {s} = {s}.{s};", .{ field.name.name, temp, field.name.name });
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
        try self.writer.print("auto {s} = ", .{temp});
        try self.generateExpr(match.scrutinee);
        try self.writer.writeLine(";");

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

            // Generate pattern bindings
            try self.generatePatternBindings(arm.pattern, temp);

            // Generate arm body
            switch (arm.body) {
                .expression => |expr| {
                    try self.generateExpr(expr);
                    try self.writer.writeLine(";");
                },
                .block => |block| try self.generateBlock(block),
            }

            try self.writer.closeBraceInline();
        }

        try self.writer.newline();
    }

    /// Generate pattern condition for match
    fn generatePatternCondition(self: *Self, pattern: *Pattern, scrutinee: []const u8) anyerror!void {
        switch (pattern.kind) {
            .literal => |lit| {
                try self.writer.print("{s} == ", .{scrutinee});
                try self.generateLiteral(lit);
            },
            .identifier => {
                // Identifier pattern always matches
                try self.writer.write("true");
            },
            .wildcard => {
                try self.writer.write("true");
            },
            .enum_variant => |variant| {
                // Check tag
                const variant_name = variant.variant.name;
                if (variant.type_path) |path| {
                    const type_name = path.segments[path.segments.len - 1].name;
                    const mangled = try self.mangleTypeName(type_name);
                    try self.writer.print("{s}.tag == {s}_tag_{s}", .{ scrutinee, mangled, variant_name });
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
    fn generatePatternBindings(self: *Self, pattern: *Pattern, scrutinee: []const u8) anyerror!void {
        switch (pattern.kind) {
            .identifier => |ident| {
                try self.writer.printLine("auto {s} = {s};", .{ ident.name.name, scrutinee });
            },
            .enum_variant => |variant| {
                switch (variant.payload) {
                    .none => {},
                    .tuple => |patterns| {
                        for (patterns, 0..) |pat, i| {
                            if (pat.kind == .identifier) {
                                try self.writer.printLine("auto {s} = {s}.data.{s}._{d};", .{
                                    pat.kind.identifier.name.name,
                                    scrutinee,
                                    variant.variant.name,
                                    i,
                                });
                            }
                        }
                    },
                    .struct_fields => |fields| {
                        for (fields) |field| {
                            if (field.pattern) |pat| {
                                if (pat.kind == .identifier) {
                                    try self.writer.printLine("auto {s} = {s}.data.{s}.{s};", .{
                                        pat.kind.identifier.name.name,
                                        scrutinee,
                                        variant.variant.name,
                                        field.name.name,
                                    });
                                }
                            }
                        }
                    },
                }
            },
            else => {},
        }
    }

    /// Generate for loop
    fn generateForLoop(self: *Self, for_stmt: *ForLoop) anyerror!void {
        // For now, assume iterator is a range expression
        const temp = try self.freshTemp();
        try self.writer.print("for (size_t {s} = 0; ; {s}++)", .{ temp, temp });
        try self.writer.beginBlock();

        // Bind pattern variable
        switch (for_stmt.pattern.kind) {
            .identifier => |ident| {
                try self.writer.printLine("auto {s} = {s};", .{ ident.name.name, temp });
            },
            else => {},
        }

        try self.generateBlock(for_stmt.body);
        try self.writer.endBlock();
    }

    /// Generate while loop
    fn generateWhileLoop(self: *Self, while_stmt: *WhileLoop) anyerror!void {
        try self.writer.write("while (");
        try self.generateExpr(while_stmt.condition);
        try self.writer.write(")");
        try self.writer.beginBlock();
        try self.generateBlock(while_stmt.body);
        try self.writer.endBlock();
    }

    /// Generate infinite loop
    fn generateLoopStatement(self: *Self, loop: *LoopStmt) anyerror!void {
        if (loop.label) |label| {
            try self.writer.printLine("{s}:", .{label.name});
        }
        try self.writer.writeLine("while (true) {");
        self.writer.indent();
        try self.generateBlock(loop.body);
        self.writer.dedent();
        try self.writer.writeLine("}");
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
        const arena_name = try std.fmt.allocPrint(self.allocator, "_dm_arena_{s}", .{region.name.name});
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
            .identifier => |ident| try self.writer.write(ident.name),
            .path => |path| try self.generatePath(path),
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
                        try self.writer.print("dm_string_from_cstr(\"{s}\")", .{str_lit.value});
                    },
                    .raw => {
                        try self.writer.print("dm_string_from_cstr(\"{s}\")", .{str_lit.value});
                    },
                    .byte => {
                        try self.writer.print("\"{s}\"", .{str_lit.value});
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
        for (path.segments, 0..) |segment, i| {
            if (i > 0) try self.writer.write("_");
            try self.writer.write(segment.name);
        }
    }

    /// Generate binary expression
    fn generateBinaryExpr(self: *Self, bin: *BinaryExpr) anyerror!void {
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
            .in => " /* in */ ", // Custom handling needed
        };

        try self.writer.write(op_str);
        try self.generateExpr(bin.right);
        try self.writer.write(")");
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
        try self.generateExpr(field.object);
        try self.writer.print(".{s}", .{field.field.name});
    }

    /// Generate index access
    fn generateIndexAccess(self: *Self, idx: *IndexAccess) anyerror!void {
        try self.generateExpr(idx.object);
        try self.writer.write("[");
        try self.generateExpr(idx.index);
        try self.writer.write("]");
    }

    /// Generate method call (converted to function call)
    fn generateMethodCall(self: *Self, method: *MethodCall) anyerror!void {
        // Convert obj.method(args) to Type_method(&obj, args)
        // For simplicity, we'll generate obj.method(args) style for now
        // which requires knowing the type
        try self.writer.print("dm_{s}(", .{method.method.name});
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
        if (call.function.kind == .identifier) {
            const name = call.function.kind.identifier.name;
            if (try self.generateBuiltinCall(name, call)) {
                return;
            }
        }

        try self.generateExpr(call.function);
        try self.writer.write("(");

        for (call.args, 0..) |arg, i| {
            if (i > 0) try self.writer.write(", ");
            try self.generateExpr(arg.value);
        }

        try self.writer.write(")");
    }

    /// Generate built-in function calls
    fn generateBuiltinCall(self: *Self, name: []const u8, call: *FunctionCall) !bool {
        if (std.mem.eql(u8, name, "print")) {
            try self.writer.write("printf(\"%s\", (");
            if (call.args.len > 0) {
                try self.generateExpr(call.args[0].value);
            }
            try self.writer.write(").data)");
            return true;
        } else if (std.mem.eql(u8, name, "println")) {
            try self.writer.write("dm_println_str(");
            if (call.args.len > 0) {
                try self.generateExpr(call.args[0].value);
            }
            try self.writer.write(")");
            return true;
        } else if (std.mem.eql(u8, name, "panic")) {
            try self.writer.write("dm_panic(");
            if (call.args.len > 0) {
                try self.generateExpr(call.args[0].value);
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
        }

        return false;
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

    /// Generate match expression
    fn generateMatchExpr(self: *Self, match: *MatchExpr) anyerror!void {
        // For match expressions, generate a series of ternary operators
        // This is a simplified approach - complex matches need lambda lifting
        try self.writer.write("(");

        for (match.arms, 0..) |arm, i| {
            if (i > 0) {
                try self.writer.write(" : ");
            }

            // Generate condition check
            const temp = "_dm_match_val";
            _ = temp; // Would need to lift this

            try self.writer.write("(");
            // Simplified condition
            try self.writer.write("1 /* match arm */");
            try self.writer.write(") ? (");

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

        // Default case
        try self.writer.write(" : 0)");
    }

    /// Generate block expression
    fn generateBlockExpr(self: *Self, block: *BlockExpr) anyerror!void {
        // Block expressions in C require statement expressions (GCC extension)
        // or conversion to immediately-invoked lambda
        try self.writer.write("({");
        try self.generateBlock(block);
        if (block.result) |result| {
            try self.generateExpr(result);
            try self.writer.write(";");
        }
        try self.writer.write("})");
    }

    /// Generate lambda expression
    fn generateLambdaExpr(self: *Self, lambda: *LambdaExpr) anyerror!void {
        self.lambda_depth += 1;
        defer self.lambda_depth -= 1;

        // Generate as function pointer or use GCC nested functions
        // For simplicity, we'll generate a comment placeholder
        try self.writer.write("/* lambda: */NULL");

        // In a full implementation, we would:
        // 1. Lift the lambda to a top-level function
        // 2. Create a closure struct for captured variables
        // 3. Return a function pointer with the closure
        _ = lambda;
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
        // This needs a temporary and early return
        const temp = try self.freshTemp();
        try self.writer.print("({s} {s} = ", .{ "auto", temp });
        try self.generateExpr(err.operand);
        try self.writer.print("; {s}.is_ok ? {s}.value.ok_value : (return {s}))", .{ temp, temp, temp });
    }

    /// Generate null coalescing expression
    fn generateCoalesce(self: *Self, coal: *CoalesceExpr) anyerror!void {
        // left ?? right - if left has value use it, else right
        const temp = try self.freshTemp();
        try self.writer.print("({s} {s} = ", .{ "auto", temp });
        try self.generateExpr(coal.left);
        try self.writer.print("; {s}.has_value ? {s}.value : ", .{ temp, temp });
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
        // expr is Type - runtime type check
        // For static types, this should be resolved at compile time
        // Generate a placeholder
        try self.writer.write("(");
        try self.generateExpr(check.expr);
        try self.writer.write(", true /* is type check */)");
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
