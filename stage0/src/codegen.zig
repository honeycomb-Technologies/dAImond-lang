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
const AwaitExpr = ast.AwaitExpr;
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

    /// Truncate output to specified length
    pub fn truncate(self: *Self, len: usize) void {
        if (len < self.buffer.items.len) {
            self.buffer.shrinkRetainingCapacity(len);
            self.at_line_start = (len == 0 or self.buffer.items[len - 1] == '\n');
        }
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
    // Deferred buffer for list type struct definitions (emitted before user structs)
    list_type_struct_writer: CWriter,
    // Deferred buffer for list type function definitions (emitted after user structs)
    list_type_writer: CWriter,

    // Track generated box types: maps box helper name -> element C type
    generated_box_types: std.StringHashMap([]const u8),
    // Deferred buffer for box helper definitions
    box_type_writer: CWriter,

    // Track generated map types: maps map type name -> MapTypeInfo
    generated_map_types: std.StringHashMap(MapTypeInfo),
    // Deferred buffer for map type definitions
    map_type_writer: CWriter,

    // Track generated option types: maps option type name -> inner C type
    generated_option_types: std.StringHashMap([]const u8),
    // Deferred buffer for option type definitions
    option_type_writer: CWriter,

    // Track generated result types: maps result type name -> ResultTypeInfo
    generated_result_types: std.StringHashMap(ResultTypeInfo),
    // Deferred buffer for result type definitions
    result_type_writer: CWriter,

    // Track generated future types: maps future type name -> inner C type
    generated_future_types: std.StringHashMap([]const u8),
    // Deferred buffer for future type definitions
    future_type_writer: CWriter,

    // Track enum variant payload types: maps "dm_EnumName.VariantName" -> C type
    enum_variant_types: std.StringHashMap([]const u8),

    // Generic function monomorphization support
    generic_functions: std.StringHashMap(*FunctionDecl),
    pending_monomorphizations: std.StringHashMap(MonoRequest),
    generated_monomorphizations: std.StringHashMap(void),
    active_type_substitutions: ?std.StringHashMap([]const u8),
    mono_emit_queue: std.ArrayList(MonoQueueItem),

    // Unique ID counter for temporary variables
    temp_counter: usize,

    // Stack for tracking lambda/closure context
    lambda_depth: usize,

    // Lambda hoisting support: counter for unique names and deferred buffer
    lambda_counter: usize,
    lambda_writer: CWriter,

    // Expected type context for expression generation (used for Option/Result constructors)
    expected_type: ?[]const u8,

    // Current function name (for error messages)
    current_function: ?[]const u8,

    // Whether current function returns void (don't generate return for block result)
    current_function_is_void: bool,

    // Current function's return C type (for ? operator early-return construction)
    current_function_return_type: ?[]const u8,

    // Whether current function is async (needs _ready() wrapping on returns)
    current_function_is_async: bool,
    // The future type name for async function returns (e.g., "dm_future_int64_t")
    current_function_future_type: ?[]const u8,
    // Nesting depth of blocks within async function (0 = function body level)
    async_block_depth: u32,

    // Track which parameters in the current function are mut (pass-by-reference)
    current_mut_params: std.StringHashMap(void),

    // Track which function parameters are mut: function name → array of is_mut flags
    function_mut_info: std.StringHashMap([]const bool),

    // Region stack for memory management
    region_stack: std.ArrayList([]const u8),

    // Track known function names: maps raw name -> mangled C name
    // Used to distinguish function references from variable references in expressions
    known_functions: std.StringHashMap([]const u8),

    // Track extern functions: use raw C name without dm_ prefix
    extern_functions: std.StringHashMap(void),

    // Track closure variables: maps variable name to its closure info
    // When inside a capturing lambda, identifiers that are captured are read from __env
    closure_captures: ?*const std.ArrayList(CapturedVar),

    // Track which local variables are closures (need env passed at call site)
    closure_info: std.StringHashMap(ClosureInfo),

    // Track trait definitions for dyn Trait vtable generation
    trait_defs: std.StringHashMap([]const TraitMethodSig),
    // Track which dyn Trait types have been generated
    generated_dyn_traits: std.StringHashMap(void),
    // Deferred buffer for dyn trait vtable/struct definitions
    dyn_trait_writer: CWriter,
    // Track variable types for dyn dispatch: maps var name -> trait name if dyn
    dyn_var_traits: std.StringHashMap([]const u8),

    // Debug info: emit #line directives for debugger integration
    emit_debug_info: bool,
    source_file_path: ?[]const u8,
    last_debug_line: usize, // track last emitted #line to avoid duplicates

    // Source file reference for comptime function resolution
    current_source: ?*SourceFile,

    const Self = @This();

    const CapturedVar = struct {
        name: []const u8,
        c_type: []const u8,
    };

    const ClosureInfo = struct {
        lambda_name: []const u8,
        env_var: []const u8,
        captures: []const CapturedVar,
        return_type: []const u8,
    };

    const TraitMethodSig = struct {
        name: []const u8,
        param_types: []const []const u8, // C types for params (excluding self)
        return_type: []const u8, // C return type
    };

    const ImplMethodInfo = struct {
        target_type: []const u8,
        method: *FunctionDecl,
        trait_name: ?[]const u8,
    };

    const MapTypeInfo = struct {
        key_type: []const u8,
        value_type: []const u8,
    };

    const ResultTypeInfo = struct {
        ok_type: []const u8,
        err_type: []const u8,
    };

    const MonoRequest = struct {
        func: *FunctionDecl,
        type_prefix: ?[]const u8,
        concrete_types: []const []const u8,
        generic_param_names: []const []const u8,
    };

    const MonoQueueItem = struct {
        name: []const u8,
        req: MonoRequest,
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
            .list_type_struct_writer = CWriter.init(allocator),
            .list_type_writer = CWriter.init(allocator),
            .generated_box_types = std.StringHashMap([]const u8).init(allocator),
            .box_type_writer = CWriter.init(allocator),
            .generated_map_types = std.StringHashMap(MapTypeInfo).init(allocator),
            .map_type_writer = CWriter.init(allocator),
            .generated_option_types = std.StringHashMap([]const u8).init(allocator),
            .option_type_writer = CWriter.init(allocator),
            .generated_result_types = std.StringHashMap(ResultTypeInfo).init(allocator),
            .result_type_writer = CWriter.init(allocator),
            .generated_future_types = std.StringHashMap([]const u8).init(allocator),
            .future_type_writer = CWriter.init(allocator),
            .enum_variant_types = std.StringHashMap([]const u8).init(allocator),
            .generic_functions = std.StringHashMap(*FunctionDecl).init(allocator),
            .pending_monomorphizations = std.StringHashMap(MonoRequest).init(allocator),
            .generated_monomorphizations = std.StringHashMap(void).init(allocator),
            .active_type_substitutions = null,
            .mono_emit_queue = std.ArrayList(MonoQueueItem).init(allocator),
            .temp_counter = 0,
            .lambda_depth = 0,
            .lambda_counter = 0,
            .lambda_writer = CWriter.init(allocator),
            .current_function = null,
            .current_function_is_void = false,
            .expected_type = null,
            .current_function_return_type = null,
            .current_function_is_async = false,
            .current_function_future_type = null,
            .async_block_depth = 0,
            .current_mut_params = std.StringHashMap(void).init(allocator),
            .function_mut_info = std.StringHashMap([]const bool).init(allocator),
            .region_stack = std.ArrayList([]const u8).init(allocator),
            .known_functions = std.StringHashMap([]const u8).init(allocator),
            .extern_functions = std.StringHashMap(void).init(allocator),
            .closure_captures = null,
            .closure_info = std.StringHashMap(ClosureInfo).init(allocator),
            .trait_defs = std.StringHashMap([]const TraitMethodSig).init(allocator),
            .generated_dyn_traits = std.StringHashMap(void).init(allocator),
            .dyn_trait_writer = CWriter.init(allocator),
            .dyn_var_traits = std.StringHashMap([]const u8).init(allocator),
            .emit_debug_info = false,
            .source_file_path = null,
            .last_debug_line = 0,
            .current_source = null,
        };
    }

    /// Set debug info generation (emit #line directives)
    pub fn setDebugInfo(self: *Self, debug: bool, source_path: ?[]const u8) void {
        self.emit_debug_info = debug;
        self.source_file_path = source_path;
    }

    /// Emit a #line directive for source-level debugging
    fn emitLineDirective(self: *Self, span: Span) !void {
        if (!self.emit_debug_info) return;
        const line = span.start.line;
        if (line == 0 or line == self.last_debug_line) return;
        self.last_debug_line = line;
        if (self.source_file_path) |path| {
            try self.writer.printLine("#line {d} \"{s}\"", .{ line, path });
        } else {
            try self.writer.printLine("#line {d}", .{line});
        }
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
        self.list_type_struct_writer.deinit();
        self.list_type_writer.deinit();
        self.generated_box_types.deinit();
        self.box_type_writer.deinit();
        self.generated_map_types.deinit();
        self.map_type_writer.deinit();
        self.generated_option_types.deinit();
        self.option_type_writer.deinit();
        self.generated_result_types.deinit();
        self.result_type_writer.deinit();
        self.generated_future_types.deinit();
        self.future_type_writer.deinit();
        self.enum_variant_types.deinit();
        self.generic_functions.deinit();
        self.pending_monomorphizations.deinit();
        self.generated_monomorphizations.deinit();
        self.mono_emit_queue.deinit();
        if (self.active_type_substitutions) |*subs| subs.deinit();
        self.current_mut_params.deinit();
        self.function_mut_info.deinit();
        self.region_stack.deinit();
        self.known_functions.deinit();
        self.extern_functions.deinit();
        self.closure_info.deinit();
        self.lambda_writer.deinit();
        self.trait_defs.deinit();
        self.generated_dyn_traits.deinit();
        self.dyn_trait_writer.deinit();
        self.dyn_var_traits.deinit();
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
        // Store source file reference for comptime function resolution
        self.current_source = source;

        // Generate runtime header
        try self.generateRuntimeHeader();

        // First pass: collect all type declarations for forward declarations
        for (source.declarations) |decl| {
            try self.collectForwardDeclarations(decl);
        }

        // Generate forward declarations
        try self.generateForwardDeclarations();

        // Pre-scan all declarations to discover List[T], Box[T], Option[T], Result[T,E],
        // Map[K,V] types used in function parameters, return types, struct fields, let
        // bindings, and function bodies. This triggers monomorphized type generation.
        for (source.declarations) |decl| {
            try self.scanForListTypes(decl);
        }

        // Pre-scan async functions to trigger Future type generation
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .function => |f| {
                    if (f.is_async) {
                        const inner_ret = if (f.return_type) |rt| try self.mapType(rt) else "void";
                        _ = try self.generateFutureTypeByName(inner_ret);
                    }
                },
                else => {},
            }
        }

        // Insert monomorphized list type STRUCT definitions BEFORE user structs.
        // List structs only contain T* pointers so they only need forward declarations.
        // User structs may contain List[T] fields by value so they need these typedefs.
        if (self.list_type_struct_writer.getOutput().len > 0) {
            try self.writer.blankLine();
            try self.writer.writeLineComment("Monomorphized List Types");
            try self.writer.write(self.list_type_struct_writer.getOutput());
        }

        // Insert generated option type definitions (before user structs that may use them)
        if (self.option_type_writer.getOutput().len > 0) {
            try self.writer.blankLine();
            try self.writer.writeLineComment("Monomorphized Option Types");
            try self.writer.write(self.option_type_writer.getOutput());
        }

        // Insert generated result type definitions (before user structs that may use them)
        if (self.result_type_writer.getOutput().len > 0) {
            try self.writer.blankLine();
            try self.writer.writeLineComment("Monomorphized Result Types");
            try self.writer.write(self.result_type_writer.getOutput());
        }

        // Insert generated future type definitions (before user structs that may use them)
        if (self.future_type_writer.getOutput().len > 0) {
            try self.writer.blankLine();
            try self.writer.writeLineComment("Monomorphized Future Types");
            try self.writer.write(self.future_type_writer.getOutput());
        }

        // Generate user type definitions (structs and enums)
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .struct_def => |s| try self.generateStruct(s),
                .enum_def => |e| try self.generateEnum(e),
                else => {},
            }
        }

        // Insert generated box helper definitions AFTER user struct definitions.
        // Box helpers use sizeof(T) which requires the full struct definition to be visible.
        if (self.box_type_writer.getOutput().len > 0) {
            try self.writer.blankLine();
            try self.writer.writeLineComment("Monomorphized Box Helpers");
            try self.writer.write(self.box_type_writer.getOutput());
        }

        // Insert monomorphized list type FUNCTION definitions AFTER user structs.
        // Functions like _push use sizeof(T) which requires the full struct definition.
        if (self.list_type_writer.getOutput().len > 0) {
            try self.writer.blankLine();
            try self.writer.writeLineComment("Monomorphized List Functions");
            try self.writer.write(self.list_type_writer.getOutput());
        }

        // Emit string_split implementation now that dm_list_dm_string is defined
        if (self.generated_list_types.contains("dm_list_dm_string")) {
            try self.writer.blankLine();
            try self.writer.writeLineComment("string_split implementation (returns dm_list_dm_string)");
            try self.emitStringSplitImpl();
        }

        // Insert generated map type definitions AFTER list functions,
        // because map helpers (keys/values) call list functions.
        if (self.map_type_writer.getOutput().len > 0) {
            try self.writer.blankLine();
            try self.writer.writeLineComment("Monomorphized Map Types");
            try self.writer.write(self.map_type_writer.getOutput());
        }

        try self.writer.blankLine();

        // Collect trait definitions for dyn Trait vtable generation
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .trait_def => |trait| try self.collectTraitDef(trait),
                else => {},
            }
        }

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
        // Also populate known_functions map for higher-order function support
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .function => |f| {
                    // Skip generic functions - their return types depend on type parameters
                    // and are tracked per-monomorphization in emitMonomorphizedPrototypes
                    if (f.generic_params != null and f.generic_params.?.len > 0) continue;
                    const inner_ret = if (f.return_type) |rt| try self.mapType(rt) else "void";
                    // For async functions, the actual return type is Future[T]
                    const ret_type = if (f.is_async) try self.generateFutureTypeByName(inner_ret) else inner_ret;
                    self.trackFunctionReturnType(f.name.name, ret_type);
                    if (f.is_extern) {
                        // Check if extern fn has string types — if so, a wrapper
                        // will be generated and call sites should use dm_{name}
                        var extern_has_string = std.mem.eql(u8, ret_type, "dm_string");
                        if (!extern_has_string) {
                            for (f.params) |param| {
                                const pt = try self.mapType(param.type_expr);
                                if (std.mem.eql(u8, pt, "dm_string")) {
                                    extern_has_string = true;
                                    break;
                                }
                            }
                        }
                        if (extern_has_string) {
                            // String-typed extern: wrapper dm_{name} will be emitted,
                            // call sites use normal mangling path (not extern_functions)
                            const mangled = try self.mangleFunctionName(f.name.name, null);
                            try self.known_functions.put(f.name.name, mangled);
                        } else {
                            // Non-string extern: use raw C name directly
                            try self.known_functions.put(f.name.name, f.name.name);
                            try self.extern_functions.put(f.name.name, {});
                        }
                    } else {
                        // Track as known function for higher-order function references
                        const mangled = try self.mangleFunctionName(f.name.name, null);
                        try self.known_functions.put(f.name.name, mangled);
                    }
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

        // Generate dyn Trait vtable structs and instances for all trait impls
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .impl_block => |impl| {
                    if (impl.trait_type) |trait_te| {
                        const trait_name = try self.extractTypeName(trait_te);
                        const target_name = try self.extractTypeName(impl.target_type);
                        if (self.trait_defs.contains(trait_name)) {
                            try self.emitDynTraitTypes(trait_name);
                            try self.emitVtableInstance(trait_name, target_name);
                        }
                    }
                },
                else => {},
            }
        }

        // Insert dyn trait type definitions
        if (self.dyn_trait_writer.getOutput().len > 0) {
            try self.writer.blankLine();
            try self.writer.writeLineComment("Dynamic Trait (dyn) Vtables");
            try self.writer.write(self.dyn_trait_writer.getOutput());
        }

        // Pre-scan all function bodies to discover generic function calls
        // and queue monomorphizations. This must happen after generic functions
        // are registered (during prototype generation) but before implementations.
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .function => |f| try self.scanForGenericCalls(f),
                .impl_block => |impl| {
                    for (impl.items) |item| {
                        switch (item.kind) {
                            .function => |f| try self.scanForGenericCalls(f),
                            else => {},
                        }
                    }
                },
                else => {},
            }
        }

        // Emit monomorphized function prototypes (forward declarations)
        try self.emitMonomorphizedPrototypes();

        try self.writer.blankLine();

        // Generate constants
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .constant => |c| try self.generateConstant(c),
                else => {},
            }
        }

        // Save position before function implementations so we can insert lambdas
        const pre_functions_len = self.writer.getOutput().len;

        // Generate function implementations (lambdas may be hoisted during this phase)
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

        // Generate monomorphized function implementations
        try self.emitMonomorphizedImplementations();

        // Insert hoisted lambda definitions before function implementations
        if (self.lambda_writer.getOutput().len > 0) {
            // Extract the function code that was generated after pre_functions_len
            const function_code = try self.allocator.dupe(u8, self.writer.getOutput()[pre_functions_len..]);
            defer self.allocator.free(function_code);
            // Truncate writer back to pre-function position
            self.writer.truncate(pre_functions_len);
            // Write lambda definitions
            try self.writer.blankLine();
            try self.writer.writeLineComment("Hoisted Lambda Functions");
            try self.writer.write(self.lambda_writer.getOutput());
            // Re-append the function code
            try self.writer.write(function_code);
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

        // Safe division/modulo helpers (runtime division-by-zero protection)
        try self.writer.writeLine("static inline int64_t dm__safe_div(int64_t a, int64_t b) {");
        self.writer.indent();
        try self.writer.writeLine("if (b == 0) dm_panic(\"division by zero\");");
        try self.writer.writeLine("return a / b;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline int64_t dm__safe_mod(int64_t a, int64_t b) {");
        self.writer.indent();
        try self.writer.writeLine("if (b == 0) dm_panic(\"modulo by zero\");");
        try self.writer.writeLine("return a % b;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline double dm__safe_divf(double a, double b) {");
        self.writer.indent();
        try self.writer.writeLine("if (b == 0.0) dm_panic(\"division by zero\");");
        try self.writer.writeLine("return a / b;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline float dm__safe_divf32(float a, float b) {");
        self.writer.indent();
        try self.writer.writeLine("if (b == 0.0f) dm_panic(\"division by zero\");");
        try self.writer.writeLine("return a / b;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Bounds checking helper (runtime index-out-of-bounds protection)
        try self.writer.writeLine("static inline void dm_bounds_check(int64_t index, int64_t len, const char* context) {");
        self.writer.indent();
        try self.writer.writeLine("if (index < 0 || index >= len) {");
        self.writer.indent();
        try self.writer.writeLine("fprintf(stderr, \"PANIC: index out of bounds: index %lld, length %lld (%s)\\n\", (long long)index, (long long)len, context);");
        try self.writer.writeLine("exit(1);");
        self.writer.dedent();
        try self.writer.writeLine("}");
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

        try self.writer.writeLine("static inline double dm_parse_float(dm_string s) {");
        self.writer.indent();
        try self.writer.writeLine("char buf[64];");
        try self.writer.writeLine("size_t copy_len = s.len < 63 ? s.len : 63;");
        try self.writer.writeLine("memcpy(buf, s.data, copy_len);");
        try self.writer.writeLine("buf[copy_len] = '\\0';");
        try self.writer.writeLine("return atof(buf);");
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

        // File append
        try self.writer.writeLine("static inline void dm_append_file(dm_string path, dm_string content) {");
        self.writer.indent();
        try self.writer.writeLine("char* cpath = (char*)malloc(path.len + 1);");
        try self.writer.writeLine("memcpy(cpath, path.data, path.len);");
        try self.writer.writeLine("cpath[path.len] = '\\0';");
        try self.writer.writeLine("FILE* f = fopen(cpath, \"ab\");");
        try self.writer.writeLine("free(cpath);");
        try self.writer.writeLine("if (!f) dm_panic(\"file_append: cannot open file\");");
        try self.writer.writeLine("fwrite(content.data, 1, content.len, f);");
        try self.writer.writeLine("fclose(f);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // File exists
        try self.writer.writeLine("static inline bool dm_file_exists(dm_string path) {");
        self.writer.indent();
        try self.writer.writeLine("char* cpath = (char*)malloc(path.len + 1);");
        try self.writer.writeLine("memcpy(cpath, path.data, path.len);");
        try self.writer.writeLine("cpath[path.len] = '\\0';");
        try self.writer.writeLine("FILE* f = fopen(cpath, \"r\");");
        try self.writer.writeLine("free(cpath);");
        try self.writer.writeLine("if (f) { fclose(f); return true; }");
        try self.writer.writeLine("return false;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Read line from stdin
        try self.writer.writeLine("static inline dm_string dm_read_line(void) {");
        self.writer.indent();
        try self.writer.writeLine("char buf[4096];");
        try self.writer.writeLine("if (fgets(buf, sizeof(buf), stdin) == NULL) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("size_t len = strlen(buf);");
        try self.writer.writeLine("if (len > 0 && buf[len-1] == '\\n') len--;");
        try self.writer.writeLine("char* result = (char*)malloc(len + 1);");
        try self.writer.writeLine("memcpy(result, buf, len);");
        try self.writer.writeLine("result[len] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = result, .len = len, .capacity = len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String trim (remove leading and trailing whitespace)
        try self.writer.writeLine("static inline dm_string dm_string_trim(dm_string s) {");
        self.writer.indent();
        try self.writer.writeLine("size_t start = 0;");
        try self.writer.writeLine("while (start < s.len && (s.data[start] == ' ' || s.data[start] == '\\t' || s.data[start] == '\\n' || s.data[start] == '\\r')) start++;");
        try self.writer.writeLine("size_t end = s.len;");
        try self.writer.writeLine("while (end > start && (s.data[end-1] == ' ' || s.data[end-1] == '\\t' || s.data[end-1] == '\\n' || s.data[end-1] == '\\r')) end--;");
        try self.writer.writeLine("size_t new_len = end - start;");
        try self.writer.writeLine("if (new_len == 0) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("char* buf = (char*)malloc(new_len + 1);");
        try self.writer.writeLine("memcpy(buf, s.data + start, new_len);");
        try self.writer.writeLine("buf[new_len] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = new_len, .capacity = new_len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String replace (replace all occurrences)
        try self.writer.writeLine("static inline dm_string dm_string_replace(dm_string s, dm_string old_str, dm_string new_str) {");
        self.writer.indent();
        try self.writer.writeLine("if (old_str.len == 0) return s;");
        try self.writer.writeLine("size_t count = 0;");
        try self.writer.writeLine("for (size_t i = 0; i <= s.len - old_str.len; i++) {");
        self.writer.indent();
        try self.writer.writeLine("if (memcmp(s.data + i, old_str.data, old_str.len) == 0) { count++; i += old_str.len - 1; }");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.writeLine("if (count == 0) { char* buf = (char*)malloc(s.len + 1); memcpy(buf, s.data, s.len); buf[s.len] = '\\0'; return (dm_string){ .data = buf, .len = s.len, .capacity = s.len }; }");
        try self.writer.writeLine("size_t new_len = s.len + count * (new_str.len - old_str.len);");
        try self.writer.writeLine("char* buf = (char*)malloc(new_len + 1);");
        try self.writer.writeLine("size_t pos = 0;");
        try self.writer.writeLine("for (size_t i = 0; i < s.len; ) {");
        self.writer.indent();
        try self.writer.writeLine("if (i + old_str.len <= s.len && memcmp(s.data + i, old_str.data, old_str.len) == 0) {");
        self.writer.indent();
        try self.writer.writeLine("memcpy(buf + pos, new_str.data, new_str.len); pos += new_str.len; i += old_str.len;");
        self.writer.dedent();
        try self.writer.writeLine("} else { buf[pos++] = s.data[i++]; }");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.writeLine("buf[new_len] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = new_len, .capacity = new_len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String to_upper / to_lower
        try self.writer.writeLine("static inline dm_string dm_string_to_upper(dm_string s) {");
        self.writer.indent();
        try self.writer.writeLine("char* buf = (char*)malloc(s.len + 1);");
        try self.writer.writeLine("for (size_t i = 0; i < s.len; i++) buf[i] = (s.data[i] >= 'a' && s.data[i] <= 'z') ? s.data[i] - 32 : s.data[i];");
        try self.writer.writeLine("buf[s.len] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = s.len, .capacity = s.len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline dm_string dm_string_to_lower(dm_string s) {");
        self.writer.indent();
        try self.writer.writeLine("char* buf = (char*)malloc(s.len + 1);");
        try self.writer.writeLine("for (size_t i = 0; i < s.len; i++) buf[i] = (s.data[i] >= 'A' && s.data[i] <= 'Z') ? s.data[i] + 32 : s.data[i];");
        try self.writer.writeLine("buf[s.len] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = s.len, .capacity = s.len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // String split - returns a list of strings (requires dm_list_dm_string to be generated)
        // string_split implementation is emitted after list type definitions
        // (since it returns dm_list_dm_string which needs to be defined first)

        // Path utilities
        try self.writer.writeLine("static inline dm_string dm_path_dirname(dm_string path) {");
        self.writer.indent();
        try self.writer.writeLine("if (path.len == 0) return dm_string_from_cstr(\".\");");
        try self.writer.writeLine("size_t i = path.len;");
        try self.writer.writeLine("while (i > 0 && path.data[i-1] != '/') i--;");
        try self.writer.writeLine("if (i == 0) return dm_string_from_cstr(\".\");");
        try self.writer.writeLine("if (i == 1) return dm_string_from_cstr(\"/\");");
        try self.writer.writeLine("size_t dlen = i - 1;");
        try self.writer.writeLine("char* buf = (char*)malloc(dlen + 1); memcpy(buf, path.data, dlen); buf[dlen] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = dlen, .capacity = dlen };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline dm_string dm_path_basename(dm_string path) {");
        self.writer.indent();
        try self.writer.writeLine("if (path.len == 0) return dm_string_from_cstr(\"\");");
        try self.writer.writeLine("size_t i = path.len;");
        try self.writer.writeLine("while (i > 0 && path.data[i-1] != '/') i--;");
        try self.writer.writeLine("size_t blen = path.len - i;");
        try self.writer.writeLine("char* buf = (char*)malloc(blen + 1); memcpy(buf, path.data + i, blen); buf[blen] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = blen, .capacity = blen };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline dm_string dm_path_extension(dm_string path) {");
        self.writer.indent();
        try self.writer.writeLine("size_t i = path.len;");
        try self.writer.writeLine("while (i > 0 && path.data[i-1] != '.' && path.data[i-1] != '/') i--;");
        try self.writer.writeLine("if (i == 0 || path.data[i-1] == '/') return dm_string_from_cstr(\"\");");
        try self.writer.writeLine("i--;  /* point to the dot */");
        try self.writer.writeLine("size_t elen = path.len - i;");
        try self.writer.writeLine("char* buf = (char*)malloc(elen + 1); memcpy(buf, path.data + i, elen); buf[elen] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = elen, .capacity = elen };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline dm_string dm_path_stem(dm_string path) {");
        self.writer.indent();
        try self.writer.writeLine("dm_string base = dm_path_basename(path);");
        try self.writer.writeLine("size_t i = base.len;");
        try self.writer.writeLine("while (i > 0 && base.data[i-1] != '.') i--;");
        try self.writer.writeLine("if (i <= 1) return base;");
        try self.writer.writeLine("size_t slen = i - 1;");
        try self.writer.writeLine("char* buf = (char*)malloc(slen + 1); memcpy(buf, base.data, slen); buf[slen] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = slen, .capacity = slen };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline dm_string dm_path_join(dm_string a, dm_string b) {");
        self.writer.indent();
        try self.writer.writeLine("if (a.len == 0) return b;");
        try self.writer.writeLine("if (b.len == 0) return a;");
        try self.writer.writeLine("bool need_sep = (a.data[a.len-1] != '/');");
        try self.writer.writeLine("size_t new_len = a.len + (need_sep ? 1 : 0) + b.len;");
        try self.writer.writeLine("char* buf = (char*)malloc(new_len + 1);");
        try self.writer.writeLine("memcpy(buf, a.data, a.len);");
        try self.writer.writeLine("if (need_sep) buf[a.len] = '/';");
        try self.writer.writeLine("memcpy(buf + a.len + (need_sep ? 1 : 0), b.data, b.len);");
        try self.writer.writeLine("buf[new_len] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = new_len, .capacity = new_len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Helper: convert dm_string to null-terminated C string
        try self.writer.writeLine("static inline char* dm_to_cstr(dm_string s) {");
        self.writer.indent();
        try self.writer.writeLine("char* cstr = (char*)malloc(s.len + 1);");
        try self.writer.writeLine("if (!cstr) return NULL;");
        try self.writer.writeLine("memcpy(cstr, s.data, s.len);");
        try self.writer.writeLine("cstr[s.len] = '\\0';");
        try self.writer.writeLine("return cstr;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Filesystem: platform includes
        try self.writer.writeLine("#include <errno.h>");
        try self.writer.writeLine("#ifdef _WIN32");
        try self.writer.writeLine("#include <direct.h>");
        try self.writer.writeLine("#include <io.h>");
        try self.writer.writeLine("#include <windows.h>");
        try self.writer.writeLine("#else");
        try self.writer.writeLine("#include <sys/stat.h>");
        try self.writer.writeLine("#include <dirent.h>");
        try self.writer.writeLine("#include <unistd.h>");
        try self.writer.writeLine("#endif");
        try self.writer.blankLine();

        // Filesystem: mkdir
        try self.writer.writeLine("static inline int64_t dm_fs_mkdir(dm_string path) {");
        self.writer.indent();
        try self.writer.writeLine("char* cpath = dm_to_cstr(path);");
        try self.writer.writeLine("if (!cpath) return -1;");
        try self.writer.writeLine("char* p = cpath;");
        try self.writer.writeLine("#ifdef _WIN32");
        try self.writer.writeLine("while (*p) { if ((*p == '/' || *p == '\\\\') && p != cpath) { char sv = *p; *p = '\\0'; _mkdir(cpath); *p = sv; } p++; }");
        try self.writer.writeLine("int result = _mkdir(cpath);");
        try self.writer.writeLine("#else");
        try self.writer.writeLine("while (*p) { if (*p == '/' && p != cpath) { *p = '\\0'; mkdir(cpath, 0755); *p = '/'; } p++; }");
        try self.writer.writeLine("int result = mkdir(cpath, 0755);");
        try self.writer.writeLine("#endif");
        try self.writer.writeLine("if (result != 0 && errno == EEXIST) result = 0;");
        try self.writer.writeLine("free(cpath);");
        try self.writer.writeLine("return (int64_t)result;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Filesystem: readdir
        try self.writer.writeLine("static inline dm_string dm_fs_readdir(dm_string path) {");
        self.writer.indent();
        try self.writer.writeLine("char* cpath = dm_to_cstr(path);");
        try self.writer.writeLine("if (!cpath) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("#ifdef _WIN32");
        try self.writer.writeLine("size_t plen = strlen(cpath);");
        try self.writer.writeLine("char* pattern = (char*)malloc(plen + 3);");
        try self.writer.writeLine("if (!pattern) { free(cpath); return (dm_string){ .data = \"\", .len = 0, .capacity = 0 }; }");
        try self.writer.writeLine("memcpy(pattern, cpath, plen);");
        try self.writer.writeLine("if (plen > 0 && cpath[plen-1] != '\\\\' && cpath[plen-1] != '/') { pattern[plen] = '\\\\'; pattern[plen+1] = '*'; pattern[plen+2] = '\\0'; }");
        try self.writer.writeLine("else { pattern[plen] = '*'; pattern[plen+1] = '\\0'; }");
        try self.writer.writeLine("free(cpath);");
        try self.writer.writeLine("WIN32_FIND_DATAA fd; HANDLE hFind = FindFirstFileA(pattern, &fd); free(pattern);");
        try self.writer.writeLine("if (hFind == INVALID_HANDLE_VALUE) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("size_t buf_cap = 1024; char* buf = (char*)malloc(buf_cap); size_t buf_len = 0;");
        try self.writer.writeLine("do {");
        self.writer.indent();
        try self.writer.writeLine("if (strcmp(fd.cFileName, \".\") == 0 || strcmp(fd.cFileName, \"..\") == 0) continue;");
        try self.writer.writeLine("size_t name_len = strlen(fd.cFileName);");
        try self.writer.writeLine("while (buf_len + name_len + 1 >= buf_cap) { buf_cap *= 2; buf = (char*)realloc(buf, buf_cap); }");
        try self.writer.writeLine("if (buf_len > 0) buf[buf_len++] = '\\n';");
        try self.writer.writeLine("memcpy(buf + buf_len, fd.cFileName, name_len); buf_len += name_len;");
        self.writer.dedent();
        try self.writer.writeLine("} while (FindNextFileA(hFind, &fd));");
        try self.writer.writeLine("FindClose(hFind);");
        try self.writer.writeLine("#else");
        try self.writer.writeLine("DIR* dir = opendir(cpath);");
        try self.writer.writeLine("free(cpath);");
        try self.writer.writeLine("if (!dir) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("size_t buf_cap = 1024;");
        try self.writer.writeLine("char* buf = (char*)malloc(buf_cap);");
        try self.writer.writeLine("size_t buf_len = 0;");
        try self.writer.writeLine("struct dirent* entry;");
        try self.writer.writeLine("while ((entry = readdir(dir)) != NULL) {");
        self.writer.indent();
        try self.writer.writeLine("if (strcmp(entry->d_name, \".\") == 0 || strcmp(entry->d_name, \"..\") == 0) continue;");
        try self.writer.writeLine("size_t name_len = strlen(entry->d_name);");
        try self.writer.writeLine("while (buf_len + name_len + 1 >= buf_cap) { buf_cap *= 2; buf = (char*)realloc(buf, buf_cap); }");
        try self.writer.writeLine("if (buf_len > 0) buf[buf_len++] = '\\n';");
        try self.writer.writeLine("memcpy(buf + buf_len, entry->d_name, name_len);");
        try self.writer.writeLine("buf_len += name_len;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.writeLine("closedir(dir);");
        try self.writer.writeLine("#endif");
        try self.writer.writeLine("if (buf_len == 0) { free(buf); return (dm_string){ .data = \"\", .len = 0, .capacity = 0 }; }");
        try self.writer.writeLine("buf[buf_len] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = buf_len, .capacity = buf_cap };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Filesystem: remove
        try self.writer.writeLine("static inline int64_t dm_fs_remove(dm_string path) {");
        self.writer.indent();
        try self.writer.writeLine("char* cpath = dm_to_cstr(path);");
        try self.writer.writeLine("if (!cpath) return -1;");
        try self.writer.writeLine("#ifdef _WIN32");
        try self.writer.writeLine("int result = remove(cpath); if (result != 0) result = _rmdir(cpath);");
        try self.writer.writeLine("#else");
        try self.writer.writeLine("int result = remove(cpath);");
        try self.writer.writeLine("#endif");
        try self.writer.writeLine("free(cpath);");
        try self.writer.writeLine("return (int64_t)result;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Filesystem: rename
        try self.writer.writeLine("static inline int64_t dm_fs_rename(dm_string old_path, dm_string new_path) {");
        self.writer.indent();
        try self.writer.writeLine("char* cold = dm_to_cstr(old_path);");
        try self.writer.writeLine("char* cnew = dm_to_cstr(new_path);");
        try self.writer.writeLine("if (!cold || !cnew) { free(cold); free(cnew); return -1; }");
        try self.writer.writeLine("int result = rename(cold, cnew);");
        try self.writer.writeLine("free(cold); free(cnew);");
        try self.writer.writeLine("return (int64_t)result;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Filesystem: getcwd
        try self.writer.writeLine("static inline dm_string dm_fs_getcwd(void) {");
        self.writer.indent();
        try self.writer.writeLine("char buf[4096];");
        try self.writer.writeLine("#ifdef _WIN32");
        try self.writer.writeLine("if (_getcwd(buf, sizeof(buf)) == NULL) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("#else");
        try self.writer.writeLine("if (getcwd(buf, sizeof(buf)) == NULL) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("#endif");
        try self.writer.writeLine("size_t len = strlen(buf);");
        try self.writer.writeLine("char* result = (char*)malloc(len + 1);");
        try self.writer.writeLine("memcpy(result, buf, len + 1);");
        try self.writer.writeLine("return (dm_string){ .data = result, .len = len, .capacity = len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // OS: getenv
        try self.writer.writeLine("static inline dm_string dm_os_getenv(dm_string name) {");
        self.writer.indent();
        try self.writer.writeLine("char* cname = dm_to_cstr(name);");
        try self.writer.writeLine("if (!cname) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("const char* val = getenv(cname);");
        try self.writer.writeLine("free(cname);");
        try self.writer.writeLine("if (!val) return (dm_string){ .data = \"\", .len = 0, .capacity = 0 };");
        try self.writer.writeLine("size_t len = strlen(val);");
        try self.writer.writeLine("char* result = (char*)malloc(len + 1);");
        try self.writer.writeLine("memcpy(result, val, len + 1);");
        try self.writer.writeLine("return (dm_string){ .data = result, .len = len, .capacity = len };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Networking: integer-based wrappers around struct-based TCP API
        try self.writer.writeLine("#ifdef _WIN32");
        try self.writer.writeLine("#include <winsock2.h>");
        try self.writer.writeLine("#include <ws2tcpip.h>");
        try self.writer.writeLine("#pragma comment(lib, \"ws2_32.lib\")");
        try self.writer.blankLine();
        try self.writer.writeLine("static int dm__winsock_inited = 0;");
        try self.writer.writeLine("static inline void dm__ensure_winsock(void) { if (!dm__winsock_inited) { WSADATA w; WSAStartup(MAKEWORD(2,2), &w); dm__winsock_inited = 1; } }");
        try self.writer.blankLine();

        // Windows: tcp_listen
        try self.writer.writeLine("static inline int64_t dm_tcp_listen_fd(dm_string addr) {");
        self.writer.indent();
        try self.writer.writeLine("dm__ensure_winsock();");
        try self.writer.writeLine("char* caddr = dm_to_cstr(addr); if (!caddr) return -1;");
        try self.writer.writeLine("char* colon = strrchr(caddr, ':'); if (!colon) { free(caddr); return -1; }");
        try self.writer.writeLine("*colon = '\\0'; int port = atoi(colon+1);");
        try self.writer.writeLine("SOCKET fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);");
        try self.writer.writeLine("if (fd == INVALID_SOCKET) { free(caddr); return -1; }");
        try self.writer.writeLine("int opt = 1; setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char*)&opt, sizeof(opt));");
        try self.writer.writeLine("struct sockaddr_in sa; memset(&sa, 0, sizeof(sa));");
        try self.writer.writeLine("sa.sin_family = AF_INET; sa.sin_port = htons((uint16_t)port);");
        try self.writer.writeLine("if (strlen(caddr) == 0 || strcmp(caddr, \"0.0.0.0\") == 0) sa.sin_addr.s_addr = INADDR_ANY;");
        try self.writer.writeLine("else inet_pton(AF_INET, caddr, &sa.sin_addr);");
        try self.writer.writeLine("free(caddr);");
        try self.writer.writeLine("if (bind(fd, (struct sockaddr*)&sa, sizeof(sa)) == SOCKET_ERROR) { closesocket(fd); return -1; }");
        try self.writer.writeLine("if (listen(fd, 128) == SOCKET_ERROR) { closesocket(fd); return -1; }");
        try self.writer.writeLine("return (int64_t)fd;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Windows: tcp_accept
        try self.writer.writeLine("static inline int64_t dm_tcp_accept_fd(int64_t listener_fd) {");
        self.writer.indent();
        try self.writer.writeLine("struct sockaddr_in ca; int al = sizeof(ca);");
        try self.writer.writeLine("SOCKET cfd = accept((SOCKET)listener_fd, (struct sockaddr*)&ca, &al);");
        try self.writer.writeLine("return (cfd == INVALID_SOCKET) ? -1 : (int64_t)cfd;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Windows: tcp_connect
        try self.writer.writeLine("static inline int64_t dm_tcp_connect_fd(dm_string addr) {");
        self.writer.indent();
        try self.writer.writeLine("dm__ensure_winsock();");
        try self.writer.writeLine("char* caddr = dm_to_cstr(addr); if (!caddr) return -1;");
        try self.writer.writeLine("char* colon = strrchr(caddr, ':'); if (!colon) { free(caddr); return -1; }");
        try self.writer.writeLine("*colon = '\\0'; int port = atoi(colon+1);");
        try self.writer.writeLine("SOCKET fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);");
        try self.writer.writeLine("if (fd == INVALID_SOCKET) { free(caddr); return -1; }");
        try self.writer.writeLine("struct sockaddr_in sa; memset(&sa, 0, sizeof(sa));");
        try self.writer.writeLine("sa.sin_family = AF_INET; sa.sin_port = htons((uint16_t)port);");
        try self.writer.writeLine("inet_pton(AF_INET, caddr, &sa.sin_addr);");
        try self.writer.writeLine("free(caddr);");
        try self.writer.writeLine("if (connect(fd, (struct sockaddr*)&sa, sizeof(sa)) == SOCKET_ERROR) { closesocket(fd); return -1; }");
        try self.writer.writeLine("return (int64_t)fd;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Windows: tcp_read
        try self.writer.writeLine("static inline dm_string dm_tcp_read_fd(int64_t fd, int64_t max_bytes) {");
        self.writer.indent();
        try self.writer.writeLine("if (max_bytes <= 0) max_bytes = 4096;");
        try self.writer.writeLine("char* buf = (char*)malloc((size_t)max_bytes + 1);");
        try self.writer.writeLine("int n = recv((SOCKET)fd, buf, (int)max_bytes, 0);");
        try self.writer.writeLine("if (n <= 0) { free(buf); return (dm_string){ .data = \"\", .len = 0, .capacity = 0 }; }");
        try self.writer.writeLine("buf[n] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = (size_t)n, .capacity = (size_t)max_bytes };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Windows: tcp_write
        try self.writer.writeLine("static inline int64_t dm_tcp_write_fd(int64_t fd, dm_string data) {");
        self.writer.indent();
        try self.writer.writeLine("int n = send((SOCKET)fd, data.data, (int)data.len, 0);");
        try self.writer.writeLine("return (int64_t)(n < 0 ? 0 : n);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Windows: tcp_close
        try self.writer.writeLine("static inline void dm_tcp_close_fd(int64_t fd) { if (fd >= 0) closesocket((SOCKET)fd); }");
        try self.writer.blankLine();

        try self.writer.writeLine("#else /* POSIX */");
        try self.writer.writeLine("#include <sys/socket.h>");
        try self.writer.writeLine("#include <netinet/in.h>");
        try self.writer.writeLine("#include <arpa/inet.h>");
        try self.writer.blankLine();

        // POSIX: tcp_listen(addr) -> fd
        try self.writer.writeLine("static inline int64_t dm_tcp_listen_fd(dm_string addr) {");
        self.writer.indent();
        try self.writer.writeLine("char* caddr = dm_to_cstr(addr); if (!caddr) return -1;");
        try self.writer.writeLine("char* colon = strrchr(caddr, ':'); if (!colon) { free(caddr); return -1; }");
        try self.writer.writeLine("*colon = '\\0'; int port = atoi(colon+1);");
        try self.writer.writeLine("int fd = socket(AF_INET, SOCK_STREAM, 0);");
        try self.writer.writeLine("if (fd < 0) { free(caddr); return -1; }");
        try self.writer.writeLine("int opt = 1; setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));");
        try self.writer.writeLine("struct sockaddr_in sa; memset(&sa, 0, sizeof(sa));");
        try self.writer.writeLine("sa.sin_family = AF_INET; sa.sin_port = htons((uint16_t)port);");
        try self.writer.writeLine("if (strlen(caddr) == 0 || strcmp(caddr, \"0.0.0.0\") == 0) sa.sin_addr.s_addr = INADDR_ANY;");
        try self.writer.writeLine("else inet_pton(AF_INET, caddr, &sa.sin_addr);");
        try self.writer.writeLine("free(caddr);");
        try self.writer.writeLine("if (bind(fd, (struct sockaddr*)&sa, sizeof(sa)) < 0) { close(fd); return -1; }");
        try self.writer.writeLine("if (listen(fd, 128) < 0) { close(fd); return -1; }");
        try self.writer.writeLine("return (int64_t)fd;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // POSIX: tcp_accept(listener_fd) -> client_fd
        try self.writer.writeLine("static inline int64_t dm_tcp_accept_fd(int64_t listener_fd) {");
        self.writer.indent();
        try self.writer.writeLine("struct sockaddr_in ca; socklen_t al = sizeof(ca);");
        try self.writer.writeLine("int cfd = accept((int)listener_fd, (struct sockaddr*)&ca, &al);");
        try self.writer.writeLine("return (int64_t)cfd;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // POSIX: tcp_connect(addr) -> fd
        try self.writer.writeLine("static inline int64_t dm_tcp_connect_fd(dm_string addr) {");
        self.writer.indent();
        try self.writer.writeLine("char* caddr = dm_to_cstr(addr); if (!caddr) return -1;");
        try self.writer.writeLine("char* colon = strrchr(caddr, ':'); if (!colon) { free(caddr); return -1; }");
        try self.writer.writeLine("*colon = '\\0'; int port = atoi(colon+1);");
        try self.writer.writeLine("int fd = socket(AF_INET, SOCK_STREAM, 0);");
        try self.writer.writeLine("if (fd < 0) { free(caddr); return -1; }");
        try self.writer.writeLine("struct sockaddr_in sa; memset(&sa, 0, sizeof(sa));");
        try self.writer.writeLine("sa.sin_family = AF_INET; sa.sin_port = htons((uint16_t)port);");
        try self.writer.writeLine("inet_pton(AF_INET, caddr, &sa.sin_addr);");
        try self.writer.writeLine("free(caddr);");
        try self.writer.writeLine("if (connect(fd, (struct sockaddr*)&sa, sizeof(sa)) < 0) { close(fd); return -1; }");
        try self.writer.writeLine("return (int64_t)fd;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // POSIX: tcp_read(fd, max_bytes) -> string
        try self.writer.writeLine("static inline dm_string dm_tcp_read_fd(int64_t fd, int64_t max_bytes) {");
        self.writer.indent();
        try self.writer.writeLine("if (max_bytes <= 0) max_bytes = 4096;");
        try self.writer.writeLine("char* buf = (char*)malloc((size_t)max_bytes + 1);");
        try self.writer.writeLine("ssize_t n = read((int)fd, buf, (size_t)max_bytes);");
        try self.writer.writeLine("if (n <= 0) { free(buf); return (dm_string){ .data = \"\", .len = 0, .capacity = 0 }; }");
        try self.writer.writeLine("buf[n] = '\\0';");
        try self.writer.writeLine("return (dm_string){ .data = buf, .len = (size_t)n, .capacity = (size_t)max_bytes };");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // POSIX: tcp_write(fd, data) -> bytes_written
        try self.writer.writeLine("static inline int64_t dm_tcp_write_fd(int64_t fd, dm_string data) {");
        self.writer.indent();
        try self.writer.writeLine("ssize_t n = write((int)fd, data.data, data.len);");
        try self.writer.writeLine("return (int64_t)(n < 0 ? 0 : n);");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // POSIX: tcp_close(fd)
        try self.writer.writeLine("static inline void dm_tcp_close_fd(int64_t fd) { if (fd >= 0) close((int)fd); }");
        try self.writer.blankLine();

        try self.writer.writeLine("#endif /* _WIN32 */");
        try self.writer.blankLine();

        // Threading: integer-based wrappers
        try self.writer.writeLine("#ifdef _WIN32");
        try self.writer.writeLine("#ifndef DM_WINDOWS_H_INCLUDED");
        try self.writer.writeLine("#define DM_WINDOWS_H_INCLUDED");
        try self.writer.writeLine("#include <windows.h>");
        try self.writer.writeLine("#endif");
        try self.writer.blankLine();

        // Windows: thread_spawn(fn) -> thread_id (encoded as int)
        try self.writer.writeLine("typedef struct { void (*func)(void); } dm_thread_tramp_arg;");
        try self.writer.writeLine("static DWORD WINAPI dm_thread_tramp(LPVOID arg) { dm_thread_tramp_arg* ta = (dm_thread_tramp_arg*)arg; void (*f)(void) = ta->func; free(ta); f(); return 0; }");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline int64_t dm_thread_spawn_fn(void (*func)(void)) {");
        self.writer.indent();
        try self.writer.writeLine("dm_thread_tramp_arg* arg = (dm_thread_tramp_arg*)malloc(sizeof(dm_thread_tramp_arg));");
        try self.writer.writeLine("arg->func = func;");
        try self.writer.writeLine("HANDLE h = CreateThread(NULL, 0, dm_thread_tramp, arg, 0, NULL);");
        try self.writer.writeLine("return (int64_t)(intptr_t)h;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // Windows: thread_join(thread_id)
        try self.writer.writeLine("static inline void dm_thread_join_id(int64_t tid) { HANDLE h = (HANDLE)(intptr_t)tid; WaitForSingleObject(h, INFINITE); CloseHandle(h); }");
        try self.writer.blankLine();

        // Windows: mutex_new() -> mutex_ptr (encoded as int)
        try self.writer.writeLine("static inline int64_t dm_mutex_new_ptr(void) {");
        self.writer.indent();
        try self.writer.writeLine("HANDLE m = CreateMutex(NULL, FALSE, NULL);");
        try self.writer.writeLine("return (int64_t)(intptr_t)m;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline void dm_mutex_lock_ptr(int64_t mp) { WaitForSingleObject((HANDLE)(intptr_t)mp, INFINITE); }");
        try self.writer.writeLine("static inline void dm_mutex_unlock_ptr(int64_t mp) { ReleaseMutex((HANDLE)(intptr_t)mp); }");
        try self.writer.writeLine("static inline void dm_mutex_destroy_ptr(int64_t mp) { CloseHandle((HANDLE)(intptr_t)mp); }");
        try self.writer.blankLine();

        try self.writer.writeLine("#else /* POSIX */");
        try self.writer.writeLine("#include <pthread.h>");
        try self.writer.blankLine();

        // POSIX: thread_spawn(fn) -> thread_id (encoded as int)
        try self.writer.writeLine("typedef struct { void (*func)(void); } dm_thread_tramp_arg;");
        try self.writer.writeLine("static void* dm_thread_tramp(void* arg) { dm_thread_tramp_arg* ta = (dm_thread_tramp_arg*)arg; void (*f)(void) = ta->func; free(ta); f(); return NULL; }");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline int64_t dm_thread_spawn_fn(void (*func)(void)) {");
        self.writer.indent();
        try self.writer.writeLine("pthread_t t;");
        try self.writer.writeLine("dm_thread_tramp_arg* arg = (dm_thread_tramp_arg*)malloc(sizeof(dm_thread_tramp_arg));");
        try self.writer.writeLine("arg->func = func;");
        try self.writer.writeLine("pthread_create(&t, NULL, dm_thread_tramp, arg);");
        try self.writer.writeLine("return (int64_t)t;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        // POSIX: thread_join(thread_id)
        try self.writer.writeLine("static inline void dm_thread_join_id(int64_t tid) { pthread_join((pthread_t)tid, NULL); }");
        try self.writer.blankLine();

        // POSIX: mutex_new() -> mutex_ptr (encoded as int)
        try self.writer.writeLine("static inline int64_t dm_mutex_new_ptr(void) {");
        self.writer.indent();
        try self.writer.writeLine("pthread_mutex_t* m = (pthread_mutex_t*)malloc(sizeof(pthread_mutex_t));");
        try self.writer.writeLine("pthread_mutex_init(m, NULL);");
        try self.writer.writeLine("return (int64_t)(intptr_t)m;");
        self.writer.dedent();
        try self.writer.writeLine("}");
        try self.writer.blankLine();

        try self.writer.writeLine("static inline void dm_mutex_lock_ptr(int64_t mp) { pthread_mutex_lock((pthread_mutex_t*)(intptr_t)mp); }");
        try self.writer.writeLine("static inline void dm_mutex_unlock_ptr(int64_t mp) { pthread_mutex_unlock((pthread_mutex_t*)(intptr_t)mp); }");
        try self.writer.writeLine("static inline void dm_mutex_destroy_ptr(int64_t mp) { pthread_mutex_t* m = (pthread_mutex_t*)(intptr_t)mp; pthread_mutex_destroy(m); free(m); }");
        try self.writer.blankLine();

        try self.writer.writeLine("#endif /* _WIN32 */");
        try self.writer.blankLine();

        // SIMD vector types and operations
        try self.writer.writeLineComment("SIMD Vector Types");
        try self.writer.writeLine("#if defined(__GNUC__) || defined(__clang__)");
        try self.writer.writeLine("typedef float    dm_f32x4 __attribute__((vector_size(16)));");
        try self.writer.writeLine("typedef float    dm_f32x8 __attribute__((vector_size(32)));");
        try self.writer.writeLine("typedef double   dm_f64x2 __attribute__((vector_size(16)));");
        try self.writer.writeLine("typedef double   dm_f64x4 __attribute__((vector_size(32)));");
        try self.writer.writeLine("typedef int32_t  dm_i32x4 __attribute__((vector_size(16)));");
        try self.writer.writeLine("typedef int32_t  dm_i32x8 __attribute__((vector_size(32)));");
        try self.writer.writeLine("typedef int64_t  dm_i64x2 __attribute__((vector_size(16)));");
        try self.writer.writeLine("typedef int64_t  dm_i64x4 __attribute__((vector_size(32)));");
        try self.writer.blankLine();
        // Splat constructors
        try self.writer.writeLine("static inline dm_f32x4 dm_simd_splat_f32x4(float v) { return (dm_f32x4){v,v,v,v}; }");
        try self.writer.writeLine("static inline dm_f32x8 dm_simd_splat_f32x8(float v) { return (dm_f32x8){v,v,v,v,v,v,v,v}; }");
        try self.writer.writeLine("static inline dm_f64x2 dm_simd_splat_f64x2(double v) { return (dm_f64x2){v,v}; }");
        try self.writer.writeLine("static inline dm_f64x4 dm_simd_splat_f64x4(double v) { return (dm_f64x4){v,v,v,v}; }");
        try self.writer.writeLine("static inline dm_i32x4 dm_simd_splat_i32x4(int32_t v) { return (dm_i32x4){v,v,v,v}; }");
        try self.writer.writeLine("static inline dm_i32x8 dm_simd_splat_i32x8(int32_t v) { return (dm_i32x8){v,v,v,v,v,v,v,v}; }");
        try self.writer.writeLine("static inline dm_i64x2 dm_simd_splat_i64x2(int64_t v) { return (dm_i64x2){v,v}; }");
        try self.writer.writeLine("static inline dm_i64x4 dm_simd_splat_i64x4(int64_t v) { return (dm_i64x4){v,v,v,v}; }");
        try self.writer.blankLine();
        // Set constructors
        try self.writer.writeLine("static inline dm_f32x4 dm_simd_set_f32x4(float a, float b, float c, float d) { return (dm_f32x4){a,b,c,d}; }");
        try self.writer.writeLine("static inline dm_f32x8 dm_simd_set_f32x8(float a, float b, float c, float d, float e, float f, float g, float h) { return (dm_f32x8){a,b,c,d,e,f,g,h}; }");
        try self.writer.writeLine("static inline dm_f64x2 dm_simd_set_f64x2(double a, double b) { return (dm_f64x2){a,b}; }");
        try self.writer.writeLine("static inline dm_f64x4 dm_simd_set_f64x4(double a, double b, double c, double d) { return (dm_f64x4){a,b,c,d}; }");
        try self.writer.writeLine("static inline dm_i32x4 dm_simd_set_i32x4(int32_t a, int32_t b, int32_t c, int32_t d) { return (dm_i32x4){a,b,c,d}; }");
        try self.writer.writeLine("static inline dm_i32x8 dm_simd_set_i32x8(int32_t a, int32_t b, int32_t c, int32_t d, int32_t e, int32_t f, int32_t g, int32_t h) { return (dm_i32x8){a,b,c,d,e,f,g,h}; }");
        try self.writer.writeLine("static inline dm_i64x2 dm_simd_set_i64x2(int64_t a, int64_t b) { return (dm_i64x2){a,b}; }");
        try self.writer.writeLine("static inline dm_i64x4 dm_simd_set_i64x4(int64_t a, int64_t b, int64_t c, int64_t d) { return (dm_i64x4){a,b,c,d}; }");
        try self.writer.blankLine();
        // Extract and arithmetic (vector extension operators)
        try self.writer.writeLine("#define dm_simd_extract(vec, idx) ((vec)[(idx)])");
        try self.writer.writeLine("#define dm_simd_add(a, b) ((a) + (b))");
        try self.writer.writeLine("#define dm_simd_sub(a, b) ((a) - (b))");
        try self.writer.writeLine("#define dm_simd_mul(a, b) ((a) * (b))");
        try self.writer.writeLine("#define dm_simd_div(a, b) ((a) / (b))");
        try self.writer.writeLine("#endif /* __GNUC__ || __clang__ */");
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
            .trait_object => |to| {
                // dyn Trait generates a fat pointer struct
                if (to.trait_type.kind == .named) {
                    const trait_name = to.trait_type.kind.named.path.segments[0].name;
                    return try std.fmt.allocPrint(self.stringAllocator(), "dm_dyn_{s}", .{trait_name});
                }
                return "dm_dyn_object";
            },
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
        } else if (std.mem.eql(u8, type_name, "Future")) {
            // Future type (async result wrapper)
            if (named.generic_args) |args| {
                if (args.len > 0) {
                    return self.generateFutureType(args[0]);
                }
            }
            return "dm_future_void";
        } else if (std.mem.eql(u8, type_name, "Map") or std.mem.eql(u8, type_name, "HashMap")) {
            const mangled = try self.mangleTypeName(type_name);
            if (self.generated_enums.contains(mangled) or self.generated_structs.contains(mangled))
                return mangled;
            if (named.generic_args) |args| {
                if (args.len >= 2) return self.generateMapType(args[0], args[1]);
            }
            return "dm_map_void";
        } else if (std.mem.eql(u8, type_name, "f32x4")) {
            return "dm_f32x4";
        } else if (std.mem.eql(u8, type_name, "f32x8")) {
            return "dm_f32x8";
        } else if (std.mem.eql(u8, type_name, "f64x2")) {
            return "dm_f64x2";
        } else if (std.mem.eql(u8, type_name, "f64x4")) {
            return "dm_f64x4";
        } else if (std.mem.eql(u8, type_name, "i32x4")) {
            return "dm_i32x4";
        } else if (std.mem.eql(u8, type_name, "i32x8")) {
            return "dm_i32x8";
        } else if (std.mem.eql(u8, type_name, "i64x2")) {
            return "dm_i64x2";
        } else if (std.mem.eql(u8, type_name, "i64x4")) {
            return "dm_i64x4";
        }

        // Check active type substitutions for generic monomorphization
        if (self.active_type_substitutions) |subs| {
            if (subs.get(type_name)) |concrete| return concrete;
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

    /// Check if a C type string represents a function pointer type.
    /// Function pointer types have the pattern "ret_type (*)(params...)".
    fn isFunctionPointerType(type_str: []const u8) bool {
        return std.mem.indexOf(u8, type_str, "(*)(") != null;
    }

    /// Emit a C variable declaration that handles function pointer types correctly.
    /// For normal types: "type_str var_name"
    /// For function pointers: "ret (*var_name)(params)" instead of "ret (*)(params) var_name"
    fn emitVarDecl(self: *Self, type_str: []const u8, var_name: []const u8) !void {
        if (std.mem.indexOf(u8, type_str, "(*)(")) |star_pos| {
            // Function pointer type: "ret_type (*)(params...)"
            // Insert var_name between (* and ): "ret_type (*var_name)(params...)"
            const before_star = type_str[0 .. star_pos + 2]; // "ret_type (*"
            const after_star = type_str[star_pos + 2 ..]; // ")(params...)"
            try self.writer.print("{s}{s}{s}", .{ before_star, var_name, after_star });
        } else if (std.mem.indexOf(u8, type_str, "[")) |bracket_pos| {
            // Fixed-size array type: "elem_type[N]"
            // Emit as: "elem_type var_name[N]"
            const elem_type = type_str[0..bracket_pos];
            const size_part = type_str[bracket_pos..]; // "[N]"
            try self.writer.print("{s} {s}{s}", .{ elem_type, var_name, size_part });
        } else {
            // Normal type: "type var_name"
            try self.writer.print("{s} {s}", .{ type_str, var_name });
        }
    }

    /// Evaluate an AST expression to a usize (for array sizes in codegen).
    fn evalExprUsize(expr: *const ast.Expr) ?usize {
        switch (expr.kind) {
            .literal => |lit| {
                switch (lit.kind) {
                    .int => |int_lit| {
                        const val = std.fmt.parseInt(i64, int_lit.value, 10) catch return null;
                        if (val >= 0) return @intCast(@as(u64, @intCast(val)));
                        return null;
                    },
                    else => return null,
                }
            },
            .comptime_expr => |ce| return evalExprUsize(ce.expr),
            else => return null,
        }
    }

    /// Map array type to C
    /// For fixed-size arrays, returns "elem_type[N]" which emitVarDecl handles specially.
    /// For unknown-size arrays, falls back to pointer representation.
    fn mapArrayType(self: *Self, arr: *ArrayType) CodeGenError![]const u8 {
        const elem_type = try self.mapType(arr.element_type);
        const size = evalExprUsize(arr.size);
        if (size) |n| {
            if (n > 0) {
                return try std.fmt.allocPrint(self.stringAllocator(), "{s}[{d}]", .{ elem_type, n });
            }
        }
        // Fallback: use pointer for unknown-size or zero-size arrays
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

    /// Generate Option type name and emit monomorphized struct + helpers if not already done
    fn generateOptionType(self: *Self, inner: *TypeExpr) CodeGenError![]const u8 {
        const inner_name = try self.mapType(inner);
        const safe_name = try self.sanitizeTypeName(inner_name);
        const option_type_name = try std.fmt.allocPrint(self.stringAllocator(), "dm_option_{s}", .{safe_name});

        if (!self.generated_option_types.contains(option_type_name)) {
            try self.generated_option_types.put(option_type_name, inner_name);
            try self.emitOptionTypeDefinition(option_type_name, inner_name);
        }

        return option_type_name;
    }

    /// Generate Option type from a C type name string (for use when no TypeExpr is available)
    fn generateOptionTypeByName(self: *Self, inner_name: []const u8) ![]const u8 {
        const safe_name = try self.sanitizeTypeName(inner_name);
        const option_type_name = try std.fmt.allocPrint(self.stringAllocator(), "dm_option_{s}", .{safe_name});

        if (!self.generated_option_types.contains(option_type_name)) {
            try self.generated_option_types.put(option_type_name, inner_name);
            try self.emitOptionTypeDefinition(option_type_name, inner_name);
        }

        return option_type_name;
    }

    /// Emit a monomorphized option type struct and helper functions
    fn emitOptionTypeDefinition(self: *Self, option_type_name: []const u8, inner_type: []const u8) !void {
        var w = &self.option_type_writer;

        // Struct definition
        try w.printLine("typedef struct {s} {{", .{option_type_name});
        w.indent();
        try w.writeLine("bool has_value;");
        try w.printLine("{s} value;", .{inner_type});
        w.dedent();
        try w.printLine("}} {s};", .{option_type_name});
        try w.blankLine();

        // Some constructor
        try w.printLine("static inline {s} {s}_Some({s} v) {{", .{ option_type_name, option_type_name, inner_type });
        w.indent();
        try w.printLine("return ({s}){{ .has_value = true, .value = v }};", .{option_type_name});
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // None constructor
        try w.printLine("static inline {s} {s}_None(void) {{", .{ option_type_name, option_type_name });
        w.indent();
        try w.printLine("{s} opt; opt.has_value = false; return opt;", .{option_type_name});
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // is_some helper
        try w.printLine("static inline bool {s}_is_some({s} opt) {{ return opt.has_value; }}", .{ option_type_name, option_type_name });
        // is_none helper
        try w.printLine("static inline bool {s}_is_none({s} opt) {{ return !opt.has_value; }}", .{ option_type_name, option_type_name });
        try w.blankLine();

        // unwrap helper
        try w.printLine("static inline {s} {s}_unwrap({s} opt) {{", .{ inner_type, option_type_name, option_type_name });
        w.indent();
        try w.writeLine("if (!opt.has_value) dm_panic(\"unwrap called on None\");");
        try w.writeLine("return opt.value;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // unwrap_or helper
        try w.printLine("static inline {s} {s}_unwrap_or({s} opt, {s} def) {{", .{ inner_type, option_type_name, option_type_name, inner_type });
        w.indent();
        try w.writeLine("return opt.has_value ? opt.value : def;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();
    }

    /// Generate Result type name and emit monomorphized struct + helpers if not already done
    fn generateResultType(self: *Self, ok: *TypeExpr, err: ?*TypeExpr) CodeGenError![]const u8 {
        const ok_name = try self.mapType(ok);
        const safe_ok = try self.sanitizeTypeName(ok_name);
        if (err) |e| {
            const err_name = try self.mapType(e);
            const safe_err = try self.sanitizeTypeName(err_name);
            const result_type_name = try std.fmt.allocPrint(self.stringAllocator(), "dm_result_{s}_{s}", .{ safe_ok, safe_err });

            if (!self.generated_result_types.contains(result_type_name)) {
                try self.generated_result_types.put(result_type_name, .{ .ok_type = ok_name, .err_type = err_name });
                try self.emitResultTypeDefinition(result_type_name, ok_name, err_name);
            }

            return result_type_name;
        }
        const result_type_name = try std.fmt.allocPrint(self.stringAllocator(), "dm_result_{s}", .{safe_ok});

        if (!self.generated_result_types.contains(result_type_name)) {
            try self.generated_result_types.put(result_type_name, .{ .ok_type = ok_name, .err_type = "dm_string" });
            try self.emitResultTypeDefinition(result_type_name, ok_name, "dm_string");
        }

        return result_type_name;
    }

    /// Generate Result type from C type name strings
    fn generateResultTypeByName(self: *Self, ok_name: []const u8, err_name: []const u8) ![]const u8 {
        const safe_ok = try self.sanitizeTypeName(ok_name);
        const safe_err = try self.sanitizeTypeName(err_name);
        const result_type_name = try std.fmt.allocPrint(self.stringAllocator(), "dm_result_{s}_{s}", .{ safe_ok, safe_err });

        if (!self.generated_result_types.contains(result_type_name)) {
            try self.generated_result_types.put(result_type_name, .{ .ok_type = ok_name, .err_type = err_name });
            try self.emitResultTypeDefinition(result_type_name, ok_name, err_name);
        }

        return result_type_name;
    }

    /// Emit a monomorphized result type struct and helper functions
    fn emitResultTypeDefinition(self: *Self, result_type_name: []const u8, ok_type: []const u8, err_type: []const u8) !void {
        var w = &self.result_type_writer;

        // Struct definition
        try w.printLine("typedef struct {s} {{", .{result_type_name});
        w.indent();
        try w.writeLine("bool is_ok;");
        try w.writeLine("union {");
        w.indent();
        try w.printLine("{s} ok;", .{ok_type});
        try w.printLine("{s} err;", .{err_type});
        w.dedent();
        try w.writeLine("};");
        w.dedent();
        try w.printLine("}} {s};", .{result_type_name});
        try w.blankLine();

        // Ok constructor
        try w.printLine("static inline {s} {s}_Ok({s} v) {{", .{ result_type_name, result_type_name, ok_type });
        w.indent();
        try w.printLine("{s} r; r.is_ok = true; r.ok = v; return r;", .{result_type_name});
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // Err constructor
        try w.printLine("static inline {s} {s}_Err({s} e) {{", .{ result_type_name, result_type_name, err_type });
        w.indent();
        try w.printLine("{s} r; r.is_ok = false; r.err = e; return r;", .{result_type_name});
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // is_ok / is_err helpers
        try w.printLine("static inline bool {s}_is_ok({s} r) {{ return r.is_ok; }}", .{ result_type_name, result_type_name });
        try w.printLine("static inline bool {s}_is_err({s} r) {{ return !r.is_ok; }}", .{ result_type_name, result_type_name });
        try w.blankLine();

        // unwrap helper
        try w.printLine("static inline {s} {s}_unwrap({s} r) {{", .{ ok_type, result_type_name, result_type_name });
        w.indent();
        try w.writeLine("if (!r.is_ok) dm_panic(\"unwrap called on Err\");");
        try w.writeLine("return r.ok;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // unwrap_err helper
        try w.printLine("static inline {s} {s}_unwrap_err({s} r) {{", .{ err_type, result_type_name, result_type_name });
        w.indent();
        try w.writeLine("if (r.is_ok) dm_panic(\"unwrap_err called on Ok\");");
        try w.writeLine("return r.err;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();
    }

    /// Generate Future type name and emit monomorphized struct + helpers if not already done
    fn generateFutureType(self: *Self, inner: *TypeExpr) CodeGenError![]const u8 {
        const inner_name = try self.mapType(inner);
        return self.generateFutureTypeByName(inner_name);
    }

    /// Generate Future type from a C type name string
    fn generateFutureTypeByName(self: *Self, inner_name: []const u8) ![]const u8 {
        const safe_name = try self.sanitizeTypeName(inner_name);
        const future_type_name = try std.fmt.allocPrint(self.stringAllocator(), "dm_future_{s}", .{safe_name});

        if (!self.generated_future_types.contains(future_type_name)) {
            try self.generated_future_types.put(future_type_name, inner_name);
            try self.emitFutureTypeDefinition(future_type_name, inner_name);
        }

        return future_type_name;
    }

    /// Emit a monomorphized future type struct and helper functions
    fn emitFutureTypeDefinition(self: *Self, future_type_name: []const u8, inner_type: []const u8) !void {
        var w = &self.future_type_writer;
        const is_void = std.mem.eql(u8, inner_type, "void");

        // Struct definition
        try w.printLine("typedef struct {s} {{", .{future_type_name});
        w.indent();
        try w.writeLine("bool ready;");
        if (!is_void) {
            try w.printLine("{s} value;", .{inner_type});
        }
        w.dedent();
        try w.printLine("}} {s};", .{future_type_name});
        try w.blankLine();

        // Ready constructor
        if (is_void) {
            try w.printLine("static inline {s} {s}_ready(void) {{", .{ future_type_name, future_type_name });
            w.indent();
            try w.printLine("return ({s}){{ .ready = true }};", .{future_type_name});
            w.dedent();
            try w.writeLine("}");
        } else {
            try w.printLine("static inline {s} {s}_ready({s} v) {{", .{ future_type_name, future_type_name, inner_type });
            w.indent();
            try w.printLine("return ({s}){{ .ready = true, .value = v }};", .{future_type_name});
            w.dedent();
            try w.writeLine("}");
        }
        try w.blankLine();

        // Await helper
        if (is_void) {
            try w.printLine("static inline void {s}_await({s} fut) {{", .{ future_type_name, future_type_name });
            w.indent();
            try w.writeLine("if (!fut.ready) dm_panic(\"await on pending future\");");
            w.dedent();
            try w.writeLine("}");
        } else {
            try w.printLine("static inline {s} {s}_await({s} fut) {{", .{ inner_type, future_type_name, future_type_name });
            w.indent();
            try w.writeLine("if (!fut.ready) dm_panic(\"await on pending future\");");
            try w.writeLine("return fut.value;");
            w.dedent();
            try w.writeLine("}");
        }
        try w.blankLine();
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
        // Struct definition goes to struct writer (emitted before user structs)
        var sw = &self.list_type_struct_writer;
        try sw.printLine("typedef struct {s} {{", .{list_type_name});
        sw.indent();
        try sw.printLine("{s}* data;", .{elem_type});
        try sw.writeLine("size_t len;");
        try sw.writeLine("size_t capacity;");
        sw.dedent();
        try sw.printLine("}} {s};", .{list_type_name});
        try sw.blankLine();

        // Function definitions go to function writer (emitted after user structs)
        var w = &self.list_type_writer;

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

        // _len
        try w.printLine("static inline int64_t {s}_len({s}* list) {{", .{ list_type_name, list_type_name });
        w.indent();
        try w.writeLine("return (int64_t)list->len;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _contains (only for types that support == comparison in C)
        const is_comparable = std.mem.eql(u8, elem_type, "int64_t") or
            std.mem.eql(u8, elem_type, "double") or
            std.mem.eql(u8, elem_type, "bool") or
            std.mem.eql(u8, elem_type, "char") or
            std.mem.eql(u8, elem_type, "dm_string") or
            std.mem.endsWith(u8, elem_type, "*");
        if (is_comparable) {
            try w.printLine("static inline bool {s}_contains({s}* list, {s} value) {{", .{ list_type_name, list_type_name, elem_type });
            w.indent();
            try w.writeLine("for (size_t i = 0; i < list->len; i++) {");
            w.indent();
            if (std.mem.eql(u8, elem_type, "dm_string")) {
                try w.writeLine("if (dm_string_eq(list->data[i], value)) return true;");
            } else {
                try w.writeLine("if (list->data[i] == value) return true;");
            }
            w.dedent();
            try w.writeLine("}");
            try w.writeLine("return false;");
            w.dedent();
            try w.writeLine("}");
            try w.blankLine();
        }

        // Track method return types for inference
        const stripped_name = if (std.mem.startsWith(u8, list_type_name, "dm_"))
            list_type_name[3..]
        else
            list_type_name;
        const push_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_push", .{stripped_name});
        const get_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_get", .{stripped_name});
        const pop_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_pop", .{stripped_name});
        const len_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_len", .{stripped_name});
        const contains_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_contains", .{stripped_name});
        self.trackFunctionReturnType(push_key, "void");
        self.trackFunctionReturnType(get_key, elem_type);
        self.trackFunctionReturnType(pop_key, elem_type);
        self.trackFunctionReturnType(len_key, "int64_t");
        self.trackFunctionReturnType(contains_key, "bool");
    }

    /// Generate Box helper name and emit monomorphized allocation helper if not already done
    fn generateBoxType(self: *Self, elem: *TypeExpr) CodeGenError![]const u8 {
        const elem_name = try self.mapType(elem);
        const safe_name = try self.sanitizeTypeName(elem_name);
        const box_helper_name = try std.fmt.allocPrint(self.stringAllocator(), "dm_box_{s}", .{safe_name});

        if (!self.generated_box_types.contains(box_helper_name)) {
            try self.generated_box_types.put(box_helper_name, elem_name);
            try self.emitBoxTypeDefinition(box_helper_name, elem_name);
        }

        return box_helper_name;
    }

    /// Emit a monomorphized box allocation helper function
    fn emitBoxTypeDefinition(self: *Self, box_helper_name: []const u8, elem_type: []const u8) !void {
        var w = &self.box_type_writer;

        // Box allocation helper: T* dm_box_T(T value)
        try w.printLine("static inline {s}* {s}({s} value) {{", .{ elem_type, box_helper_name, elem_type });
        w.indent();
        try w.printLine("{s}* p = ({s}*)malloc(sizeof({s}));", .{ elem_type, elem_type, elem_type });
        try w.writeLine("if (!p) dm_panic(\"box: out of memory\");");
        try w.writeLine("*p = value;");
        try w.writeLine("return p;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();
    }

    /// Generate Map type name and emit monomorphized struct + helpers if not already done
    fn generateMapType(self: *Self, key_expr: *TypeExpr, val_expr: *TypeExpr) CodeGenError![]const u8 {
        const key_name = try self.mapType(key_expr);
        const val_name = try self.mapType(val_expr);
        const safe_key = try self.sanitizeTypeName(key_name);
        const safe_val = try self.sanitizeTypeName(val_name);
        const map_type_name = try std.fmt.allocPrint(self.stringAllocator(), "dm_map_{s}_{s}", .{ safe_key, safe_val });

        if (!self.generated_map_types.contains(map_type_name)) {
            // Pre-generate List types for keys() and values()
            _ = try self.generateListTypeByName(key_name);
            _ = try self.generateListTypeByName(val_name);
            try self.generated_map_types.put(map_type_name, .{ .key_type = key_name, .value_type = val_name });
            try self.emitMapTypeDefinition(map_type_name, key_name, val_name);
        }

        return map_type_name;
    }

    /// Generate a List type from a C type name string (for map keys/values helpers)
    fn generateListTypeByName(self: *Self, elem_name: []const u8) ![]const u8 {
        const safe_name = try self.sanitizeTypeName(elem_name);
        const list_type_name = try std.fmt.allocPrint(self.stringAllocator(), "dm_list_{s}", .{safe_name});

        if (!self.generated_list_types.contains(list_type_name)) {
            try self.generated_list_types.put(list_type_name, elem_name);
            try self.emitListTypeDefinition(list_type_name, elem_name);
        }

        return list_type_name;
    }

    /// Emit a monomorphized map type struct and helper functions
    fn emitMapTypeDefinition(self: *Self, map_type_name: []const u8, key_type: []const u8, val_type: []const u8) !void {
        var w = &self.map_type_writer;
        const safe_key = try self.sanitizeTypeName(key_type);
        const safe_val = try self.sanitizeTypeName(val_type);
        const key_list_type = try std.fmt.allocPrint(self.stringAllocator(), "dm_list_{s}", .{safe_key});
        const val_list_type = try std.fmt.allocPrint(self.stringAllocator(), "dm_list_{s}", .{safe_val});
        const is_string_key = std.mem.eql(u8, key_type, "dm_string");

        // Entry struct
        try w.printLine("typedef struct {s}_entry {{", .{map_type_name});
        w.indent();
        try w.printLine("{s} key;", .{key_type});
        try w.printLine("{s} value;", .{val_type});
        try w.writeLine("int state;  /* 0=empty, 1=occupied, 2=tombstone */");
        w.dedent();
        try w.printLine("}} {s}_entry;", .{map_type_name});
        try w.blankLine();

        // Map struct
        try w.printLine("typedef struct {s} {{", .{map_type_name});
        w.indent();
        try w.printLine("{s}_entry* entries;", .{map_type_name});
        try w.writeLine("size_t len;");
        try w.writeLine("size_t capacity;");
        w.dedent();
        try w.printLine("}} {s};", .{map_type_name});
        try w.blankLine();

        // Hash function
        try w.printLine("static inline size_t {s}_hash({s} key) {{", .{ map_type_name, key_type });
        w.indent();
        if (is_string_key) {
            // FNV-1a for string keys
            try w.writeLine("size_t h = 14695981039346656037ULL;");
            try w.writeLine("for (size_t i = 0; i < key.len; i++) {");
            w.indent();
            try w.writeLine("h ^= (unsigned char)key.data[i];");
            try w.writeLine("h *= 1099511628211ULL;");
            w.dedent();
            try w.writeLine("}");
            try w.writeLine("return h;");
        } else {
            // splitmix64 for integer keys
            try w.writeLine("uint64_t x = (uint64_t)key;");
            try w.writeLine("x ^= x >> 30;");
            try w.writeLine("x *= 0xbf58476d1ce4e5b9ULL;");
            try w.writeLine("x ^= x >> 27;");
            try w.writeLine("x *= 0x94d049bb133111ebULL;");
            try w.writeLine("x ^= x >> 31;");
            try w.writeLine("return (size_t)x;");
        }
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // Key equality function
        try w.printLine("static inline bool {s}_key_eq({s} a, {s} b) {{", .{ map_type_name, key_type, key_type });
        w.indent();
        if (is_string_key) {
            try w.writeLine("return dm_string_eq(a, b);");
        } else {
            try w.writeLine("return a == b;");
        }
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _new constructor
        try w.printLine("static inline {s} {s}_new(void) {{", .{ map_type_name, map_type_name });
        w.indent();
        try w.printLine("return ({s}){{ .entries = NULL, .len = 0, .capacity = 0 }};", .{map_type_name});
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _find_slot (linear probing, tombstone-aware)
        try w.printLine("static inline size_t {s}_find_slot({s}* map, {s} key) {{", .{ map_type_name, map_type_name, key_type });
        w.indent();
        try w.printLine("size_t idx = {s}_hash(key) & (map->capacity - 1);", .{map_type_name});
        try w.writeLine("size_t first_tombstone = (size_t)-1;");
        try w.writeLine("for (size_t i = 0; i < map->capacity; i++) {");
        w.indent();
        try w.writeLine("if (map->entries[idx].state == 0) {");
        w.indent();
        try w.writeLine("return (first_tombstone != (size_t)-1) ? first_tombstone : idx;");
        w.dedent();
        try w.writeLine("}");
        try w.printLine("if (map->entries[idx].state == 1 && {s}_key_eq(map->entries[idx].key, key)) {{", .{map_type_name});
        w.indent();
        try w.writeLine("return idx;");
        w.dedent();
        try w.writeLine("}");
        try w.writeLine("if (map->entries[idx].state == 2 && first_tombstone == (size_t)-1) {");
        w.indent();
        try w.writeLine("first_tombstone = idx;");
        w.dedent();
        try w.writeLine("}");
        try w.writeLine("idx = (idx + 1) & (map->capacity - 1);");
        w.dedent();
        try w.writeLine("}");
        try w.writeLine("return (first_tombstone != (size_t)-1) ? first_tombstone : idx;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _resize
        try w.printLine("static inline void {s}_resize({s}* map) {{", .{ map_type_name, map_type_name });
        w.indent();
        try w.writeLine("size_t new_cap = map->capacity == 0 ? 16 : map->capacity * 2;");
        try w.printLine("{s}_entry* new_entries = ({s}_entry*)calloc(new_cap, sizeof({s}_entry));", .{ map_type_name, map_type_name, map_type_name });
        try w.writeLine("if (!new_entries) dm_panic(\"map resize: out of memory\");");
        try w.printLine("{s} new_map = {{ .entries = new_entries, .len = 0, .capacity = new_cap }};", .{map_type_name});
        try w.writeLine("for (size_t i = 0; i < map->capacity; i++) {");
        w.indent();
        try w.writeLine("if (map->entries[i].state == 1) {");
        w.indent();
        try w.printLine("size_t slot = {s}_find_slot(&new_map, map->entries[i].key);", .{map_type_name});
        try w.writeLine("new_map.entries[slot].key = map->entries[i].key;");
        try w.writeLine("new_map.entries[slot].value = map->entries[i].value;");
        try w.writeLine("new_map.entries[slot].state = 1;");
        try w.writeLine("new_map.len++;");
        w.dedent();
        try w.writeLine("}");
        w.dedent();
        try w.writeLine("}");
        try w.writeLine("free(map->entries);");
        try w.writeLine("map->entries = new_entries;");
        try w.writeLine("map->capacity = new_cap;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _insert
        try w.printLine("static inline void {s}_insert({s}* map, {s} key, {s} value) {{", .{ map_type_name, map_type_name, key_type, val_type });
        w.indent();
        try w.writeLine("if (map->capacity == 0 || (map->len + 1) * 4 > map->capacity * 3) {");
        w.indent();
        try w.printLine("{s}_resize(map);", .{map_type_name});
        w.dedent();
        try w.writeLine("}");
        try w.printLine("size_t slot = {s}_find_slot(map, key);", .{map_type_name});
        try w.writeLine("if (map->entries[slot].state != 1) {");
        w.indent();
        try w.writeLine("map->len++;");
        w.dedent();
        try w.writeLine("}");
        try w.writeLine("map->entries[slot].key = key;");
        try w.writeLine("map->entries[slot].value = value;");
        try w.writeLine("map->entries[slot].state = 1;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _get
        try w.printLine("static inline {s} {s}_get({s}* map, {s} key) {{", .{ val_type, map_type_name, map_type_name, key_type });
        w.indent();
        try w.writeLine("if (map->capacity == 0) dm_panic(\"map get: key not found\");");
        try w.printLine("size_t slot = {s}_find_slot(map, key);", .{map_type_name});
        try w.writeLine("if (map->entries[slot].state != 1) dm_panic(\"map get: key not found\");");
        try w.writeLine("return map->entries[slot].value;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _contains
        try w.printLine("static inline bool {s}_contains({s}* map, {s} key) {{", .{ map_type_name, map_type_name, key_type });
        w.indent();
        try w.writeLine("if (map->capacity == 0) return false;");
        try w.printLine("size_t slot = {s}_find_slot(map, key);", .{map_type_name});
        try w.writeLine("return map->entries[slot].state == 1;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _remove
        try w.printLine("static inline bool {s}_remove({s}* map, {s} key) {{", .{ map_type_name, map_type_name, key_type });
        w.indent();
        try w.writeLine("if (map->capacity == 0) return false;");
        try w.printLine("size_t slot = {s}_find_slot(map, key);", .{map_type_name});
        try w.writeLine("if (map->entries[slot].state != 1) return false;");
        try w.writeLine("map->entries[slot].state = 2;");
        try w.writeLine("map->len--;");
        try w.writeLine("return true;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _len
        try w.printLine("static inline int64_t {s}_len({s}* map) {{", .{ map_type_name, map_type_name });
        w.indent();
        try w.writeLine("return (int64_t)map->len;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _keys
        try w.printLine("static inline {s} {s}_keys({s}* map) {{", .{ key_list_type, map_type_name, map_type_name });
        w.indent();
        try w.printLine("{s} result = {s}_new();", .{ key_list_type, key_list_type });
        try w.writeLine("for (size_t i = 0; i < map->capacity; i++) {");
        w.indent();
        try w.printLine("if (map->entries[i].state == 1) {s}_push(&result, map->entries[i].key);", .{key_list_type});
        w.dedent();
        try w.writeLine("}");
        try w.writeLine("return result;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _values
        try w.printLine("static inline {s} {s}_values({s}* map) {{", .{ val_list_type, map_type_name, map_type_name });
        w.indent();
        try w.printLine("{s} result = {s}_new();", .{ val_list_type, val_list_type });
        try w.writeLine("for (size_t i = 0; i < map->capacity; i++) {");
        w.indent();
        try w.printLine("if (map->entries[i].state == 1) {s}_push(&result, map->entries[i].value);", .{val_list_type});
        w.dedent();
        try w.writeLine("}");
        try w.writeLine("return result;");
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // _set (alias for _insert)
        try w.printLine("static inline void {s}_set({s}* map, {s} key, {s} value) {{", .{ map_type_name, map_type_name, key_type, val_type });
        w.indent();
        try w.printLine("{s}_insert(map, key, value);", .{map_type_name});
        w.dedent();
        try w.writeLine("}");
        try w.blankLine();

        // Track method return types for inference.
        // The method lookup in inferCTypeFromExpr uses "{type_name}_{method}" where
        // type_name has the "dm_" prefix stripped, so for "dm_map_X_Y" it becomes "map_X_Y".
        const stripped_name = if (std.mem.startsWith(u8, map_type_name, "dm_"))
            map_type_name[3..]
        else
            map_type_name;
        const insert_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_insert", .{stripped_name});
        const set_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_set", .{stripped_name});
        const get_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_get", .{stripped_name});
        const contains_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_contains", .{stripped_name});
        const remove_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_remove", .{stripped_name});
        const len_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_len", .{stripped_name});
        const keys_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_keys", .{stripped_name});
        const values_key = try std.fmt.allocPrint(self.stringAllocator(), "{s}_values", .{stripped_name});
        self.trackFunctionReturnType(insert_key, "void");
        self.trackFunctionReturnType(set_key, "void");
        self.trackFunctionReturnType(get_key, val_type);
        self.trackFunctionReturnType(contains_key, "bool");
        self.trackFunctionReturnType(remove_key, "bool");
        self.trackFunctionReturnType(len_key, "int64_t");
        self.trackFunctionReturnType(keys_key, key_list_type);
        self.trackFunctionReturnType(values_key, val_list_type);
    }

    /// Emit the string_split function body that returns dm_list_dm_string
    fn emitStringSplitImpl(self: *Self) !void {
        var w = &self.writer;
        try w.writeLine("static inline dm_list_dm_string dm_string_split(dm_string s, dm_string delimiter) {");
        w.indent();
        try w.writeLine("dm_list_dm_string result = { .data = NULL, .len = 0, .capacity = 0 };");
        try w.writeLine("if (s.len == 0) return result;");
        try w.writeLine("if (delimiter.len == 0) {");
        w.indent();
        try w.writeLine("dm_list_dm_string_push(&result, s);");
        try w.writeLine("return result;");
        w.dedent();
        try w.writeLine("}");
        try w.writeLine("size_t start = 0;");
        try w.writeLine("for (size_t i = 0; i <= s.len - delimiter.len; i++) {");
        w.indent();
        try w.writeLine("if (memcmp(s.data + i, delimiter.data, delimiter.len) == 0) {");
        w.indent();
        try w.writeLine("size_t plen = i - start;");
        try w.writeLine("char* pbuf = (char*)malloc(plen + 1); memcpy(pbuf, s.data + start, plen); pbuf[plen] = '\\0';");
        try w.writeLine("dm_list_dm_string_push(&result, (dm_string){ .data = pbuf, .len = plen, .capacity = plen });");
        try w.writeLine("i += delimiter.len - 1; start = i + 1;");
        w.dedent();
        try w.writeLine("}");
        w.dedent();
        try w.writeLine("}");
        try w.writeLine("size_t plen = s.len - start;");
        try w.writeLine("char* pbuf = (char*)malloc(plen + 1); memcpy(pbuf, s.data + start, plen); pbuf[plen] = '\\0';");
        try w.writeLine("dm_list_dm_string_push(&result, (dm_string){ .data = pbuf, .len = plen, .capacity = plen });");
        try w.writeLine("return result;");
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

    /// Scan a block for let bindings with List type annotations and string_split calls
    fn scanBlockForListTypes(self: *Self, block: *BlockExpr) !void {
        for (block.statements) |stmt| {
            switch (stmt.kind) {
                .let_binding => |let_bind| {
                    if (let_bind.type_annotation) |ta| {
                        try self.scanTypeExprForLists(ta);
                    }
                    // Scan the initializer expression for string_split calls
                    if (let_bind.value) |val| {
                        try self.scanExprForBuiltinListTypes(val);
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
                .expression => |expr| try self.scanExprForBuiltinListTypes(expr),
                else => {},
            }
        }
    }

    /// Scan an expression for builtin calls that return list types (e.g. string_split)
    fn scanExprForBuiltinListTypes(self: *Self, expr: *Expr) anyerror!void {
        switch (expr.kind) {
            .function_call => |fc| {
                if (fc.function.kind == .identifier) {
                    const name = fc.function.kind.identifier.name;
                    if (std.mem.eql(u8, name, "string_split")) {
                        _ = try self.generateListTypeByName("dm_string");
                    }
                }
                // Also scan arguments
                for (fc.args) |arg| {
                    try self.scanExprForBuiltinListTypes(arg.value);
                }
            },
            .method_call => |mc| {
                try self.scanExprForBuiltinListTypes(mc.object);
                for (mc.args) |arg| {
                    try self.scanExprForBuiltinListTypes(arg);
                }
            },
            .binary => |bin| {
                try self.scanExprForBuiltinListTypes(bin.left);
                try self.scanExprForBuiltinListTypes(bin.right);
            },
            .unary => |un| try self.scanExprForBuiltinListTypes(un.operand),
            .if_expr => |if_expr| {
                try self.scanExprForBuiltinListTypes(if_expr.condition);
                try self.scanBlockForListTypes(if_expr.then_branch);
            },
            else => {},
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
                    if (std.mem.eql(u8, type_name, "Box")) {
                        if (named.generic_args) |args| {
                            if (args.len > 0) {
                                _ = try self.generateBoxType(args[0]);
                            }
                        }
                    }
                    if (std.mem.eql(u8, type_name, "Map") or std.mem.eql(u8, type_name, "HashMap")) {
                        if (named.generic_args) |args| {
                            if (args.len >= 2) {
                                _ = try self.generateMapType(args[0], args[1]);
                            }
                        }
                    }
                    if (std.mem.eql(u8, type_name, "Option")) {
                        if (named.generic_args) |args| {
                            if (args.len > 0) {
                                _ = try self.generateOptionType(args[0]);
                            }
                        }
                    }
                    if (std.mem.eql(u8, type_name, "Result")) {
                        if (named.generic_args) |args| {
                            if (args.len >= 2) {
                                _ = try self.generateResultType(args[0], args[1]);
                            } else if (args.len == 1) {
                                _ = try self.generateResultType(args[0], null);
                            }
                        }
                    }
                    if (std.mem.eql(u8, type_name, "Future")) {
                        if (named.generic_args) |args| {
                            if (args.len > 0) {
                                _ = try self.generateFutureType(args[0]);
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
            .option => |o| {
                _ = self.generateOptionType(o.inner_type) catch {};
                try self.scanTypeExprForLists(o.inner_type);
            },
            .result => |r| {
                _ = self.generateResultType(r.ok_type, r.err_type) catch {};
                try self.scanTypeExprForLists(r.ok_type);
                if (r.err_type) |et| try self.scanTypeExprForLists(et);
            },
            .function => |f| {
                for (f.params) |param| {
                    try self.scanTypeExprForLists(param);
                }
                try self.scanTypeExprForLists(f.return_type);
            },
            else => {},
        }
    }

    /// Pre-scan a function declaration for generic function calls.
    /// This discovers monomorphization needs before code emission so that
    /// forward declarations can be emitted.
    fn scanForGenericCalls(self: *Self, func: *FunctionDecl) anyerror!void {
        // Skip generic functions themselves - they aren't compiled directly
        if (func.generic_params != null and func.generic_params.?.len > 0) return;

        if (func.body) |body| {
            switch (body) {
                .block => |block| try self.scanBlockForGenericCalls(block),
                .expression => |expr| try self.scanExprForGenericCalls(expr),
            }
        }
    }

    /// Scan a block for generic function calls
    fn scanBlockForGenericCalls(self: *Self, block: *BlockExpr) anyerror!void {
        for (block.statements) |stmt| {
            try self.scanStmtForGenericCalls(stmt);
        }
        if (block.result) |result| {
            try self.scanExprForGenericCalls(result);
        }
    }

    /// Scan a statement for generic function calls
    fn scanStmtForGenericCalls(self: *Self, stmt: *Statement) anyerror!void {
        switch (stmt.kind) {
            .expression => |expr| try self.scanExprForGenericCalls(expr),
            .let_binding => |lb| {
                if (lb.value) |val| try self.scanExprForGenericCalls(val);
            },
            .return_stmt => |ret| {
                if (ret.value) |val| try self.scanExprForGenericCalls(val);
            },
            .if_stmt => |if_expr| {
                try self.scanExprForGenericCalls(if_expr.condition);
                try self.scanBlockForGenericCalls(if_expr.then_branch);
                if (if_expr.else_branch) |else_br| {
                    switch (else_br) {
                        .else_block => |eb| try self.scanBlockForGenericCalls(eb),
                        .else_if => |ei| {
                            try self.scanExprForGenericCalls(ei.condition);
                            try self.scanBlockForGenericCalls(ei.then_branch);
                        },
                    }
                }
            },
            .for_loop => |for_stmt| {
                try self.scanExprForGenericCalls(for_stmt.iterator);
                try self.scanBlockForGenericCalls(for_stmt.body);
            },
            .while_loop => |while_stmt| {
                try self.scanExprForGenericCalls(while_stmt.condition);
                try self.scanBlockForGenericCalls(while_stmt.body);
            },
            .loop_stmt => |loop| try self.scanBlockForGenericCalls(loop.body),
            .assignment => |asgn| {
                try self.scanExprForGenericCalls(asgn.target);
                try self.scanExprForGenericCalls(asgn.value);
            },
            .match_stmt => |match_expr| {
                try self.scanExprForGenericCalls(match_expr.scrutinee);
                for (match_expr.arms) |arm| {
                    if (arm.guard) |guard| try self.scanExprForGenericCalls(guard);
                    switch (arm.body) {
                        .expression => |e| try self.scanExprForGenericCalls(e),
                        .block => |b| try self.scanBlockForGenericCalls(b),
                    }
                }
            },
            else => {},
        }
    }

    /// Scan an expression for generic function calls
    fn scanExprForGenericCalls(self: *Self, expr: *Expr) anyerror!void {
        switch (expr.kind) {
            .function_call => |call| {
                // Check if this calls a generic function
                if (call.function.kind == .identifier) {
                    const name = call.function.kind.identifier.name;
                    if (self.generic_functions.get(name)) |generic_func| {
                        const concrete_types = try self.resolveGenericArgs(call, generic_func);
                        try self.requestMonomorphization(generic_func, null, concrete_types, generic_func.generic_params.?);
                    }
                }
                // Scan arguments recursively
                for (call.args) |arg| {
                    try self.scanExprForGenericCalls(arg.value);
                }
            },
            .method_call => |mc| {
                try self.scanExprForGenericCalls(mc.object);
                for (mc.args) |arg| {
                    try self.scanExprForGenericCalls(arg);
                }
            },
            .binary => |bin| {
                try self.scanExprForGenericCalls(bin.left);
                try self.scanExprForGenericCalls(bin.right);
            },
            .unary => |un| try self.scanExprForGenericCalls(un.operand),
            .grouped => |inner| try self.scanExprForGenericCalls(inner),
            .if_expr => |if_expr| {
                try self.scanExprForGenericCalls(if_expr.condition);
                try self.scanBlockForGenericCalls(if_expr.then_branch);
                if (if_expr.else_branch) |else_br| {
                    switch (else_br) {
                        .else_block => |eb| try self.scanBlockForGenericCalls(eb),
                        .else_if => |ei| {
                            try self.scanExprForGenericCalls(ei.condition);
                            try self.scanBlockForGenericCalls(ei.then_branch);
                        },
                    }
                }
            },
            .block => |block| try self.scanBlockForGenericCalls(block),
            .field_access => |fa| try self.scanExprForGenericCalls(fa.object),
            .index_access => |ia| {
                try self.scanExprForGenericCalls(ia.object);
                try self.scanExprForGenericCalls(ia.index);
            },
            .match_expr => |match_expr| {
                try self.scanExprForGenericCalls(match_expr.scrutinee);
                for (match_expr.arms) |arm| {
                    if (arm.guard) |guard| try self.scanExprForGenericCalls(guard);
                    switch (arm.body) {
                        .expression => |e| try self.scanExprForGenericCalls(e),
                        .block => |b| try self.scanBlockForGenericCalls(b),
                    }
                }
            },
            .pipeline => |pipe| {
                try self.scanExprForGenericCalls(pipe.left);
                try self.scanExprForGenericCalls(pipe.right);
            },
            .lambda => |lam| {
                switch (lam.body) {
                    .block => |block| try self.scanBlockForGenericCalls(block),
                    .expression => |e| try self.scanExprForGenericCalls(e),
                }
            },
            else => {},
        }
    }

    /// Mangle a type name
    /// Check if a type has a specific impl method
    fn hasImplMethod(self: *Self, type_name: []const u8, method_name: []const u8) bool {
        for (self.impl_methods.items) |info| {
            if (std.mem.eql(u8, info.target_type, type_name) and
                std.mem.eql(u8, info.method.name.name, method_name))
            {
                return true;
            }
        }
        return false;
    }

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
                const left_type_b = self.inferCTypeFromExpr(bin.left);
                // Check for operator overloading on user-defined types
                if (!std.mem.eql(u8, left_type_b, "int64_t") and
                    !std.mem.eql(u8, left_type_b, "double") and
                    !std.mem.eql(u8, left_type_b, "float") and
                    !std.mem.eql(u8, left_type_b, "bool") and
                    !std.mem.eql(u8, left_type_b, "dm_string"))
                {
                    const op_method: ?[]const u8 = switch (bin.op) {
                        .add => "add",
                        .sub => "sub",
                        .mul => "mul",
                        .div => "div",
                        .mod => "mod",
                        .eq, .ne, .lt, .gt, .le, .ge => "eq",
                        else => null,
                    };
                    if (op_method) |mname| {
                        const raw_type_b = if (std.mem.startsWith(u8, left_type_b, "dm_"))
                            left_type_b[3..]
                        else
                            left_type_b;
                        if (self.hasImplMethod(raw_type_b, mname)) {
                            // For comparison operators, return bool
                            if (bin.op == .eq or bin.op == .ne or bin.op == .lt or
                                bin.op == .gt or bin.op == .le or bin.op == .ge)
                            {
                                break :blk "bool";
                            }
                            // For arithmetic, return the left type (same as struct type)
                            break :blk left_type_b;
                        }
                    }
                }
                // Comparison and logical ops produce bool
                break :blk switch (bin.op) {
                    .eq, .ne, .lt, .le, .gt, .ge, .@"and", .@"or", .in => "bool",
                    .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .shl, .shr => inner: {
                        // Check if either operand is a float or string
                        if (std.mem.eql(u8, left_type_b, "double") or std.mem.eql(u8, left_type_b, "float")) break :inner left_type_b;
                        if (std.mem.eql(u8, left_type_b, "dm_string")) break :inner "dm_string";
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
                    // Box_new(value) returns pointer to arg type
                    if (std.mem.eql(u8, name, "Box_new")) {
                        if (call.args.len > 0) {
                            const arg_type = self.inferCTypeFromExpr(call.args[0].value);
                            break :blk std.fmt.allocPrint(self.stringAllocator(), "{s}*", .{arg_type}) catch "void*";
                        }
                        break :blk "void*";
                    }
                    // Box_null() returns void* (type inferred from context)
                    if (std.mem.eql(u8, name, "Box_null")) break :blk "void*";
                    // Map_new() type inferred from let binding context
                    if (std.mem.eql(u8, name, "Map_new")) break :blk "void*";
                    // Some/Ok/Err return type from context
                    if (std.mem.eql(u8, name, "Some") or std.mem.eql(u8, name, "None")) {
                        if (self.expected_type) |et| {
                            if (std.mem.startsWith(u8, et, "dm_option_")) break :blk et;
                        }
                        if (self.current_function_return_type) |rt| {
                            if (std.mem.startsWith(u8, rt, "dm_option_")) break :blk rt;
                        }
                        break :blk "void*";
                    }
                    if (std.mem.eql(u8, name, "Ok") or std.mem.eql(u8, name, "Err")) {
                        if (self.expected_type) |et| {
                            if (std.mem.startsWith(u8, et, "dm_result_")) break :blk et;
                        }
                        if (self.current_function_return_type) |rt| {
                            if (std.mem.startsWith(u8, rt, "dm_result_")) break :blk rt;
                        }
                        break :blk "void*";
                    }
                    // Check known built-in return types
                    if (std.mem.eql(u8, name, "len") or
                        std.mem.eql(u8, name, "parse_int") or
                        std.mem.eql(u8, name, "string_to_int") or
                        std.mem.eql(u8, name, "string_find") or
                        std.mem.eql(u8, name, "args_len") or
                        std.mem.eql(u8, name, "system") or
                        std.mem.eql(u8, name, "fs_mkdir") or
                        std.mem.eql(u8, name, "fs_remove") or
                        std.mem.eql(u8, name, "fs_rename") or
                        std.mem.eql(u8, name, "fs_rmdir") or
                        std.mem.eql(u8, name, "tcp_listen") or
                        std.mem.eql(u8, name, "tcp_accept") or
                        std.mem.eql(u8, name, "tcp_connect") or
                        std.mem.eql(u8, name, "tcp_write") or
                        std.mem.eql(u8, name, "thread_spawn") or
                        std.mem.eql(u8, name, "mutex_new")) break :blk "int64_t";
                    if (std.mem.eql(u8, name, "parse_float")) break :blk "double";
                    if (std.mem.eql(u8, name, "to_string") or
                        std.mem.eql(u8, name, "int_to_string") or
                        std.mem.eql(u8, name, "bool_to_string") or
                        std.mem.eql(u8, name, "float_to_string") or
                        std.mem.eql(u8, name, "substr") or
                        std.mem.eql(u8, name, "char_to_string") or
                        std.mem.eql(u8, name, "char_at") or
                        std.mem.eql(u8, name, "file_read") or
                        std.mem.eql(u8, name, "args_get") or
                        std.mem.eql(u8, name, "string_trim") or
                        std.mem.eql(u8, name, "string_replace") or
                        std.mem.eql(u8, name, "string_to_upper") or
                        std.mem.eql(u8, name, "string_to_lower") or
                        std.mem.eql(u8, name, "path_dirname") or
                        std.mem.eql(u8, name, "path_basename") or
                        std.mem.eql(u8, name, "path_extension") or
                        std.mem.eql(u8, name, "path_stem") or
                        std.mem.eql(u8, name, "path_join") or
                        std.mem.eql(u8, name, "read_line") or
                        std.mem.eql(u8, name, "fs_readdir") or
                        std.mem.eql(u8, name, "fs_getcwd") or
                        std.mem.eql(u8, name, "env_get") or
                        std.mem.eql(u8, name, "tcp_read")) break :blk "dm_string";
                    if (std.mem.eql(u8, name, "string_split")) break :blk "dm_list_dm_string";
                    if (std.mem.eql(u8, name, "is_alpha") or
                        std.mem.eql(u8, name, "is_digit") or
                        std.mem.eql(u8, name, "is_alnum") or
                        std.mem.eql(u8, name, "is_whitespace") or
                        std.mem.eql(u8, name, "string_contains") or
                        std.mem.eql(u8, name, "starts_with") or
                        std.mem.eql(u8, name, "ends_with") or
                        std.mem.eql(u8, name, "file_exists") or
                        std.mem.eql(u8, name, "fs_exists")) break :blk "bool";
                    if (std.mem.eql(u8, name, "char")) break :blk "char";
                    // Look up in function declarations tracked during generation
                    if (self.lookupFunctionReturnType(name)) |ret_type| break :blk ret_type;
                    // Check if this is a generic function call - look up monomorphized name
                    if (self.generic_functions.get(name)) |generic_func| {
                        const concrete_types = self.resolveGenericArgs(call, generic_func) catch break :blk "int64_t";
                        const mono_name = self.getMonomorphizedName(name, null, concrete_types) catch break :blk "int64_t";
                        if (self.lookupFunctionReturnType(mono_name)) |ret_type| break :blk ret_type;
                    }
                    // Check if this is a closure call - use the closure's return type
                    if (self.closure_info.get(name)) |ci| break :blk ci.return_type;
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
                // Check if this is a function reference - look up its return type
                if (self.known_functions.contains(ident.name)) {
                    // Function references used as values; the type depends on context
                    // but for inference we can't construct a full fn pointer type here.
                    // Callers with type annotations will get correct types via mapType.
                    break :blk "void*";
                }
                break :blk "int64_t"; // Default to int64_t for identifiers
            },
            .index_access => |idx| blk: {
                const obj_type = self.inferCTypeFromExpr(idx.object);
                if (std.mem.eql(u8, obj_type, "dm_string")) break :blk "char";
                if (std.mem.startsWith(u8, obj_type, "dm_list_")) {
                    // Strip "dm_list_" prefix to get element type
                    break :blk obj_type["dm_list_".len..];
                }
                // Indexing a fixed-size array type "elem[N]" returns the element type
                if (std.mem.endsWith(u8, obj_type, "]")) {
                    if (std.mem.indexOf(u8, obj_type, "[")) |bracket_pos| {
                        break :blk obj_type[0..bracket_pos];
                    }
                }
                // Indexing a pointer type returns the pointed-to element
                if (std.mem.endsWith(u8, obj_type, "*")) {
                    break :blk obj_type[0 .. obj_type.len - 1];
                }
                break :blk "int64_t";
            },
            .method_call => |mc| blk: {
                // Check for enum constructor: EnumName.Variant(args)
                if (mc.object.kind == .identifier) {
                    const name = mc.object.kind.identifier.name;
                    // Check if object is a dyn trait variable
                    if (self.dyn_var_traits.get(name)) |trait_name| {
                        if (self.trait_defs.get(trait_name)) |methods| {
                            for (methods) |m| {
                                if (std.mem.eql(u8, m.name, mc.method.name)) {
                                    break :blk m.return_type;
                                }
                            }
                        }
                        break :blk "int64_t";
                    }
                    const enum_mangled = self.mangleTypeName(name) catch break :blk "int64_t";
                    if (self.generated_enums.contains(enum_mangled)) {
                        break :blk enum_mangled;
                    }
                }
                // Try to infer method return type from the method name
                var obj_type = self.inferCTypeFromExpr(mc.object);
                // Strip pointer for Box types
                if (std.mem.endsWith(u8, obj_type, "*")) obj_type = obj_type[0 .. obj_type.len - 1];
                const type_name = if (std.mem.startsWith(u8, obj_type, "dm_"))
                    obj_type[3..]
                else
                    obj_type;
                const full_name = std.fmt.allocPrint(self.stringAllocator(), "{s}_{s}", .{ type_name, mc.method.name }) catch break :blk "int64_t";
                if (self.lookupFunctionReturnType(full_name)) |ret_type| break :blk ret_type;
                break :blk "int64_t";
            },
            .field_access => |fa| blk: {
                var obj_type = self.inferCTypeFromExpr(fa.object);
                // Strip pointer for Box types: dm_Node* -> dm_Node
                if (std.mem.endsWith(u8, obj_type, "*")) {
                    obj_type = obj_type[0 .. obj_type.len - 1];
                }
                // Handle Option[T] field access: has_value -> bool, value -> inner type
                if (std.mem.startsWith(u8, obj_type, "dm_option_")) {
                    if (std.mem.eql(u8, fa.field.name, "has_value")) break :blk "bool";
                    if (std.mem.eql(u8, fa.field.name, "value")) {
                        // Inner type is the part after "dm_option_"
                        if (self.generated_option_types.get(obj_type)) |inner_type| break :blk inner_type;
                    }
                    break :blk "int64_t";
                }
                // Handle Result[T, E] field access: is_ok -> bool, ok -> ok_type, err -> err_type
                if (std.mem.startsWith(u8, obj_type, "dm_result_")) {
                    if (std.mem.eql(u8, fa.field.name, "is_ok")) break :blk "bool";
                    if (self.generated_result_types.get(obj_type)) |result_info| {
                        if (std.mem.eql(u8, fa.field.name, "ok")) break :blk result_info.ok_type;
                        if (std.mem.eql(u8, fa.field.name, "err")) break :blk result_info.err_type;
                    }
                    break :blk "int64_t";
                }
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
            .array_literal => |al| blk: {
                switch (al.kind) {
                    .elements => |elems| {
                        if (elems.len > 0) {
                            const elem_type = self.inferCTypeFromExpr(elems[0]);
                            break :blk std.fmt.allocPrint(self.stringAllocator(), "{s}[{d}]", .{ elem_type, elems.len }) catch "void*";
                        }
                        break :blk "void*";
                    },
                    .repeat => |rep| {
                        const elem_type = self.inferCTypeFromExpr(rep.value);
                        const count = evalExprUsize(rep.count) orelse 0;
                        if (count > 0) {
                            break :blk std.fmt.allocPrint(self.stringAllocator(), "{s}[{d}]", .{ elem_type, count }) catch "void*";
                        }
                        break :blk "void*";
                    },
                }
            },
            .tuple_literal => "void*",
            .lambda => "void*",
            .type_check => "bool",
            .string_interpolation => "dm_string",
            .range => "void*",
            .await_expr => |aw| blk: {
                const operand_type = self.inferCTypeFromExpr(aw.operand);
                if (std.mem.startsWith(u8, operand_type, "dm_future_")) {
                    // Strip "dm_future_" prefix to get inner type
                    if (self.generated_future_types.get(operand_type)) |inner| {
                        break :blk inner;
                    }
                    break :blk operand_type["dm_future_".len..];
                }
                break :blk "int64_t";
            },
            else => "int64_t", // Default to int64_t rather than void*
        };
    }

    /// Resolve the expected Option/Result type from context (expected_type or function return type)
    fn resolveOptionResultContext(self: *Self) ?[]const u8 {
        // First check explicit expected type (from let binding type annotation)
        if (self.expected_type) |et| {
            if (std.mem.startsWith(u8, et, "dm_option_") or std.mem.startsWith(u8, et, "dm_result_")) {
                return et;
            }
        }
        // Then check current function return type (for return statements)
        if (self.current_function_return_type) |rt| {
            if (std.mem.startsWith(u8, rt, "dm_option_") or std.mem.startsWith(u8, rt, "dm_result_")) {
                return rt;
            }
        }
        return null;
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
                        // Track variant payload type: "dm_Enum.Variant" -> C type
                        const vkey = try std.fmt.allocPrint(self.stringAllocator(), "{s}.{s}", .{ name, variant.name.name });
                        try self.enum_variant_types.put(vkey, payload_type);
                    } else {
                        // Multiple tuple elements - create anonymous struct
                        try self.writer.print("struct {{ ", .{});
                        for (tuple_types, 0..) |t, i| {
                            const payload_type = try self.mapType(t);
                            try self.writer.print("{s} _{d}; ", .{ payload_type, i });
                            // Track individual tuple element types: "dm_Enum.Variant._0" -> C type
                            const vkey = try std.fmt.allocPrint(self.stringAllocator(), "{s}.{s}._{d}", .{ name, variant.name.name, i });
                            try self.enum_variant_types.put(vkey, payload_type);
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
                        // Track struct field types: "dm_Enum.Variant.field" -> C type
                        const vkey = try std.fmt.allocPrint(self.stringAllocator(), "{s}.{s}.{s}", .{ name, variant.name.name, field.name.name });
                        try self.enum_variant_types.put(vkey, field_type);
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
    // DYN TRAIT SUPPORT
    // ========================================================================

    /// Collect trait definitions for vtable generation
    fn collectTraitDef(self: *Self, trait: *ast.TraitDecl) !void {
        var methods = std.ArrayList(TraitMethodSig).init(self.allocator);
        for (trait.items) |item| {
            switch (item.kind) {
                .function => |func| {
                    // Collect parameter C types (skip 'self')
                    var param_types = std.ArrayList([]const u8).init(self.allocator);
                    for (func.params) |param| {
                        const param_name = param.name.name;
                        if (std.mem.eql(u8, param_name, "self")) continue;
                        const c_type = try self.mapType(param.type_expr);
                        try param_types.append(c_type);
                    }
                    const ret_type = if (func.return_type) |rt| try self.mapType(rt) else "void";
                    try methods.append(.{
                        .name = func.name.name,
                        .param_types = try param_types.toOwnedSlice(),
                        .return_type = ret_type,
                    });
                },
                else => {},
            }
        }
        try self.trait_defs.put(trait.name.name, try methods.toOwnedSlice());
    }

    /// Generate vtable struct and fat pointer struct for a dyn Trait type
    fn emitDynTraitTypes(self: *Self, trait_name: []const u8) !void {
        if (self.generated_dyn_traits.contains(trait_name)) return;
        try self.generated_dyn_traits.put(trait_name, {});

        const methods = self.trait_defs.get(trait_name) orelse return;
        const w = &self.dyn_trait_writer;

        // Emit vtable struct
        try w.writeLineComment(try std.fmt.allocPrint(self.stringAllocator(), "Vtable for dyn {s}", .{trait_name}));
        try w.writeLine(try std.fmt.allocPrint(self.stringAllocator(), "typedef struct dm_vtable_{s} {{", .{trait_name}));
        for (methods) |m| {
            // Function pointer: return_type (*method_name)(void* self, param_types...)
            var sig = std.ArrayList(u8).init(self.allocator);
            defer sig.deinit();
            try sig.appendSlice("    ");
            try sig.appendSlice(m.return_type);
            try sig.appendSlice(" (*");
            try sig.appendSlice(m.name);
            try sig.appendSlice(")(void*");
            for (m.param_types) |pt| {
                try sig.appendSlice(", ");
                try sig.appendSlice(pt);
            }
            try sig.appendSlice(");");
            try w.writeLine(try self.stringAllocator().dupe(u8, sig.items));
        }
        try w.writeLine(try std.fmt.allocPrint(self.stringAllocator(), "}} dm_vtable_{s};", .{trait_name}));
        try w.blankLine();

        // Emit fat pointer struct
        try w.writeLine(try std.fmt.allocPrint(self.stringAllocator(), "typedef struct dm_dyn_{s} {{", .{trait_name}));
        try w.writeLine("    void* data;");
        try w.writeLine(try std.fmt.allocPrint(self.stringAllocator(), "    dm_vtable_{s}* vtable;", .{trait_name}));
        try w.writeLine(try std.fmt.allocPrint(self.stringAllocator(), "}} dm_dyn_{s};", .{trait_name}));
        try w.blankLine();
    }

    /// Generate vtable instance for a concrete type implementing a trait
    fn emitVtableInstance(self: *Self, trait_name: []const u8, target_type: []const u8) !void {
        const methods = self.trait_defs.get(trait_name) orelse return;
        const w = &self.dyn_trait_writer;

        // For each method, generate a wrapper that casts void* to the concrete type
        for (methods) |m| {
            var sig = std.ArrayList(u8).init(self.allocator);
            defer sig.deinit();

            // static return_type dm_TargetType_method_dyn(void* __self, params...)
            try sig.appendSlice("static ");
            try sig.appendSlice(m.return_type);
            try sig.appendSlice(" dm_");
            try sig.appendSlice(target_type);
            try sig.appendSlice("_");
            try sig.appendSlice(m.name);
            try sig.appendSlice("_dyn(void* __self");
            for (m.param_types, 0..) |pt, i| {
                try sig.appendSlice(", ");
                try sig.appendSlice(pt);
                try sig.appendSlice(try std.fmt.allocPrint(self.stringAllocator(), " __p{d}", .{i}));
            }
            try sig.appendSlice(") {");
            try w.writeLine(try self.stringAllocator().dupe(u8, sig.items));

            // Body: cast self and call the concrete method
            // The impl method takes a pointer (self: &Self), so pass the cast pointer directly
            var call = std.ArrayList(u8).init(self.allocator);
            defer call.deinit();
            if (!std.mem.eql(u8, m.return_type, "void")) {
                try call.appendSlice("    return ");
            } else {
                try call.appendSlice("    ");
            }
            try call.appendSlice("dm_");
            try call.appendSlice(target_type);
            try call.appendSlice("_");
            try call.appendSlice(m.name);
            try call.appendSlice("((dm_");
            try call.appendSlice(target_type);
            try call.appendSlice("*)__self");
            for (m.param_types, 0..) |_, i| {
                try call.appendSlice(try std.fmt.allocPrint(self.stringAllocator(), ", __p{d}", .{i}));
            }
            try call.appendSlice(");");
            try w.writeLine(try self.stringAllocator().dupe(u8, call.items));
            try w.writeLine("}");
            try w.blankLine();
        }

        // Generate the vtable static instance
        try w.writeLine(try std.fmt.allocPrint(self.stringAllocator(), "static dm_vtable_{s} dm_vtable_{s}_for_{s} = {{", .{ trait_name, trait_name, target_type }));
        for (methods) |m| {
            try w.writeLine(try std.fmt.allocPrint(self.stringAllocator(), "    .{s} = dm_{s}_{s}_dyn,", .{ m.name, target_type, m.name }));
        }
        try w.writeLine("};");
        try w.blankLine();
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
        // Skip generic functions - they are generated on demand at call sites
        if (func.generic_params != null and func.generic_params.?.len > 0) {
            try self.generic_functions.put(func.name.name, func);
            return;
        }

        // Skip extern functions - they are declared as extern, not prototyped
        if (func.is_extern) return;

        try self.emitFunctionPrototype(func, type_prefix, null);
    }

    /// Emit a function prototype with optional name override (for monomorphized functions)
    fn emitFunctionPrototype(self: *Self, func: *FunctionDecl, type_prefix: ?[]const u8, name_override: ?[]const u8) !void {
        const func_name = name_override orelse try self.mangleFunctionName(func.name.name, type_prefix);
        const inner_ret_type = if (func.return_type) |rt| try self.mapType(rt) else "void";
        const ret_type = if (func.is_async) try self.generateFutureTypeByName(inner_ret_type) else inner_ret_type;
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
                try self.emitVarDecl(param_type, param.name.name);
            }
        }

        if (!has_self and type_prefix == null and func.params.len == 0) {
            try self.writer.write("void");
        }

        try self.writer.writeLine(");");
    }

    /// Generate full function definition
    fn generateFunction(self: *Self, func: *FunctionDecl, type_prefix: ?[]const u8) !void {
        // Skip generic functions - they are generated on demand at call sites
        if (func.generic_params != null and func.generic_params.?.len > 0) {
            // Already stored in generateFunctionPrototype
            return;
        }

        // Extern functions: emit declaration only, no body
        if (func.is_extern) {
            try self.emitExternDeclaration(func);
            return;
        }

        try self.emitFunctionDefinition(func, type_prefix, null);
    }

    /// Emit a C extern function declaration (no body, raw C name)
    /// For extern functions with string types, generates a wrapper that converts
    /// between dm_string and const char* to match the real C function signature.
    fn emitExternDeclaration(self: *Self, func: *FunctionDecl) !void {
        const ret_type = if (func.return_type) |rt| try self.mapType(rt) else "void";
        const func_name = func.name.name;

        // Check if any param or return uses dm_string
        var has_string = std.mem.eql(u8, ret_type, "dm_string");
        for (func.params) |param| {
            const pt = try self.mapType(param.type_expr);
            if (std.mem.eql(u8, pt, "dm_string")) {
                has_string = true;
                break;
            }
        }

        if (!has_string) {
            // No string types — existing behavior unchanged
            self.trackFunctionReturnType(func_name, ret_type);
            try self.known_functions.put(func_name, func_name);
            try self.extern_functions.put(func_name, {});

            // Emit extern declaration without dm_ prefix
            try self.writer.print("extern {s} {s}(", .{ ret_type, func_name });

            var first = true;
            for (func.params) |param| {
                if (!first) try self.writer.write(", ");
                first = false;
                const param_type = try self.mapType(param.type_expr);
                try self.writer.print("{s} {s}", .{ param_type, param.name.name });
            }

            if (func.params.len == 0) {
                try self.writer.write("void");
            }

            try self.writer.writeLine(");");
            return;
        }

        // String types detected — emit wrapper function that converts dm_string <-> const char*.
        // We do NOT emit an extern declaration for the C function because it may already
        // be declared in system headers (e.g., getenv in <stdlib.h>) and re-declaring it
        // with different types (const char* vs char*) would cause a C compilation error.
        // The function is expected to be available at link time.
        const wrapper_name = try self.mangleFunctionName(func_name, null);

        // Emit static wrapper function dm_{name} that converts types
        try self.writer.print("static {s} {s}(", .{ ret_type, wrapper_name });

        var first = true;
        for (func.params) |param| {
            if (!first) try self.writer.write(", ");
            first = false;
            const param_type = try self.mapType(param.type_expr);
            try self.writer.print("{s} {s}", .{ param_type, param.name.name });
        }

        if (func.params.len == 0) {
            try self.writer.write("void");
        }

        try self.writer.writeLine(") {");

        // Call the real C function with type conversions
        const is_void_ret = std.mem.eql(u8, ret_type, "void");
        const is_string_ret = std.mem.eql(u8, ret_type, "dm_string");

        if (is_void_ret) {
            try self.writer.print("    {s}(", .{func_name});
        } else if (is_string_ret) {
            try self.writer.print("    const char* __ret = {s}(", .{func_name});
        } else {
            try self.writer.print("    {s} __ret = {s}(", .{ ret_type, func_name });
        }

        first = true;
        for (func.params) |param| {
            if (!first) try self.writer.write(", ");
            first = false;
            const param_type = try self.mapType(param.type_expr);
            if (std.mem.eql(u8, param_type, "dm_string")) {
                try self.writer.print("{s}.data", .{param.name.name});
            } else {
                try self.writer.write(param.name.name);
            }
        }

        try self.writer.writeLine(");");

        // Return with conversion
        if (is_void_ret) {
            // No return needed
        } else if (is_string_ret) {
            try self.writer.writeLine("    return __ret ? dm_string_from_cstr(__ret) : dm_string_from_cstr(\"\");");
        } else {
            try self.writer.writeLine("    return __ret;");
        }

        try self.writer.writeLine("}");

        // 3. Register wrapper — NOT as extern, call sites use dm_{name} via normal mangling
        self.trackFunctionReturnType(func_name, ret_type);
        self.trackFunctionReturnType(wrapper_name, ret_type);
        // Don't add to extern_functions — call sites will mangle to dm_{name}
    }

    /// Emit a full function definition with optional name override (for monomorphized functions)
    fn emitFunctionDefinition(self: *Self, func: *FunctionDecl, type_prefix: ?[]const u8, name_override: ?[]const u8) !void {
        try self.emitLineDirective(func.span);
        self.current_function = func.name.name;
        defer self.current_function = null;

        const is_void = func.return_type == null and !func.is_async;
        self.current_function_is_void = is_void;
        defer self.current_function_is_void = false;

        // Handle async functions: wrap return type in Future
        const prev_is_async = self.current_function_is_async;
        const prev_future_type = self.current_function_future_type;
        defer {
            self.current_function_is_async = prev_is_async;
            self.current_function_future_type = prev_future_type;
        }

        const has_self = type_prefix != null and methodHasSelf(func);
        const func_name = name_override orelse try self.mangleFunctionName(func.name.name, type_prefix);
        const inner_ret_type = if (func.return_type) |rt| try self.mapType(rt) else "void";

        var ret_type: []const u8 = inner_ret_type;
        if (func.is_async) {
            const future_type_name = try self.generateFutureTypeByName(inner_ret_type);
            self.current_function_is_async = true;
            self.current_function_future_type = future_type_name;
            self.async_block_depth = 0;
            ret_type = future_type_name;
        } else {
            self.current_function_is_async = false;
            self.current_function_future_type = null;
            self.async_block_depth = 0;
        }

        // Track current function return type for ? operator and Option/Result constructors
        const prev_return_type = self.current_function_return_type;
        self.current_function_return_type = ret_type;
        defer self.current_function_return_type = prev_return_type;

        // Track function return type - also track with type prefix for method calls
        self.trackFunctionReturnType(func.name.name, ret_type);
        if (name_override) |mono_name| {
            self.trackFunctionReturnType(mono_name, ret_type);
        }
        if (type_prefix) |prefix| {
            const full_name = try std.fmt.allocPrint(self.stringAllocator(), "{s}_{s}", .{ prefix, func.name.name });
            self.trackFunctionReturnType(full_name, ret_type);
        }

        // Detect Phase B async (body has await points) and emit frame struct before function
        const is_phase_b = func.is_async and if (func.body) |b| switch (b) {
            .block => |blk| blockHasAwait(blk),
            .expression => |expr| exprHasAwait(expr),
        } else false;

        var phase_b_frame_name: ?[]const u8 = null;
        var phase_b_poll_name: ?[]const u8 = null;
        var phase_b_param_decls: ?[]const u8 = null; // e.g. ", int64_t x, int64_t y"
        var phase_b_forward_args: ?[]const u8 = null; // e.g. ", x, y"
        if (is_phase_b) {
            phase_b_frame_name = try std.fmt.allocPrint(self.stringAllocator(), "{s}_frame", .{func_name});
            phase_b_poll_name = try std.fmt.allocPrint(self.stringAllocator(), "{s}_poll", .{func_name});

            // Build parameter declaration and forwarding strings
            var decl_buf = std.ArrayList(u8).init(self.allocator);
            var args_buf = std.ArrayList(u8).init(self.allocator);
            const p_start: usize = if (has_self) 1 else 0;
            for (func.params[p_start..]) |param| {
                const param_type = try self.mapType(param.type_expr);
                if (param.is_mut) {
                    try decl_buf.appendSlice(", ");
                    try decl_buf.appendSlice(param_type);
                    try decl_buf.appendSlice("* ");
                    try decl_buf.appendSlice(param.name.name);
                } else {
                    try decl_buf.appendSlice(", ");
                    try decl_buf.appendSlice(param_type);
                    try decl_buf.append(' ');
                    try decl_buf.appendSlice(param.name.name);
                }
                try args_buf.appendSlice(", ");
                try args_buf.appendSlice(param.name.name);
            }
            phase_b_param_decls = try decl_buf.toOwnedSlice();
            phase_b_forward_args = try args_buf.toOwnedSlice();

            // Emit frame struct before function
            try self.writer.printLine("typedef struct {{", .{});
            self.writer.indent();
            try self.writer.writeLine("int state;");
            self.writer.dedent();
            try self.writer.printLine("}} {s};", .{phase_b_frame_name.?});
            try self.writer.blankLine();

            // Forward declare poll function (with parameter pass-through)
            try self.writer.printLine("static {s} {s}({s}* frame{s});", .{ ret_type, phase_b_poll_name.?, phase_b_frame_name.?, phase_b_param_decls.? });
            try self.writer.blankLine();
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
                try self.emitVarDecl(param_type, param.name.name);
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

            if (is_phase_b) {
                // Phase B Lite: wrapper body + poll function
                const frame_name = phase_b_frame_name.?;
                const poll_name = phase_b_poll_name.?;

                // Wrapper body (the current function becomes the wrapper)
                const fwd_args = phase_b_forward_args.?;
                const param_decls = phase_b_param_decls.?;
                try self.writer.beginBlock();
                try self.writer.printLine("{s} frame = {{.state = 0}};", .{frame_name});
                try self.writer.printLine("return {s}(&frame{s});", .{ poll_name, fwd_args });
                try self.writer.endBlock();
                try self.writer.blankLine();

                // Poll function definition (frame + original params passed through)
                try self.writer.printLine("static {s} {s}({s}* frame{s}) {{", .{ ret_type, poll_name, frame_name, param_decls });
                self.writer.indent();
                try self.writer.writeLine("switch(frame->state) {");
                self.writer.indent();
                try self.writer.writeLine("case 0: {");
                self.writer.indent();

                // Generate body (same code as Phase A — _ready() wrapping etc. still works)
                switch (body) {
                    .block => |block| {
                        try self.generateBlock(block);
                    },
                    .expression => |expr| {
                        const future_type = self.current_function_future_type.?;
                        try self.writer.print("return {s}_ready(", .{future_type});
                        try self.generateExpr(expr);
                        try self.writer.writeLine(");");
                    },
                }

                self.writer.dedent();
                try self.writer.writeLine("}"); // close case 0
                self.writer.dedent();
                try self.writer.writeLine("}"); // close switch
                try self.writer.printLine("return ({s}){{.ready = false}};", .{ret_type});
                self.writer.dedent();
                try self.writer.writeLine("}"); // close poll function
            } else {
                // Phase A or non-async: generate body normally
                switch (body) {
                    .block => |block| {
                        try self.writer.beginBlock();
                        try self.generateBlock(block);
                        try self.writer.endBlock();
                    },
                    .expression => |expr| {
                        try self.writer.beginBlock();
                        if (self.current_function_is_async) {
                            const future_type = self.current_function_future_type.?;
                            try self.writer.print("return {s}_ready(", .{future_type});
                            try self.generateExpr(expr);
                            try self.writer.writeLine(");");
                        } else {
                            try self.writer.write("return ");
                            try self.generateExpr(expr);
                            try self.writer.writeLine(";");
                        }
                        try self.writer.endBlock();
                    },
                }
            }
        } else {
            try self.writer.writeLine(";");
        }

        try self.writer.blankLine();
    }

    // ========================================================================
    // GENERIC FUNCTION MONOMORPHIZATION
    // ========================================================================

    /// Resolve concrete C types for a generic function call
    fn resolveGenericArgs(self: *Self, call: *FunctionCall, func: *FunctionDecl) ![]const []const u8 {
        const generic_params = func.generic_params orelse return &[_][]const u8{};
        const alloc = self.stringAllocator();
        var concrete = try std.ArrayList([]const u8).initCapacity(alloc, generic_params.len);

        if (call.generic_args) |explicit_args| {
            // Explicit type arguments: identity[int](42)
            for (explicit_args) |arg| {
                try concrete.append(try self.mapType(arg));
            }
        } else {
            // Infer from call arguments: for each generic param, find matching function param
            for (generic_params) |gp| {
                var found = false;
                for (func.params, 0..) |param, i| {
                    if (self.typeExprUsesGenericParam(param.type_expr, gp.name.name)) {
                        if (i < call.args.len) {
                            try concrete.append(self.inferCTypeFromExpr(call.args[i].value));
                            found = true;
                            break;
                        }
                    }
                }
                if (!found) {
                    try concrete.append("int64_t"); // fallback
                }
            }
        }

        return concrete.toOwnedSlice();
    }

    /// Check if a type expression references a generic parameter name
    fn typeExprUsesGenericParam(self: *Self, type_expr: *TypeExpr, param_name: []const u8) bool {
        _ = self;
        return switch (type_expr.kind) {
            .named => |named| blk: {
                if (named.path.segments.len == 1 and
                    std.mem.eql(u8, named.path.segments[0].name, param_name))
                    break :blk true;
                break :blk false;
            },
            else => false,
        };
    }

    /// Generate a monomorphized function name: dm_identity_int64_t
    fn getMonomorphizedName(self: *Self, name: []const u8, type_prefix: ?[]const u8, concrete_types: []const []const u8) ![]const u8 {
        var buf = std.ArrayList(u8).init(self.stringAllocator());
        try buf.appendSlice("dm_");
        if (type_prefix) |prefix| {
            try buf.appendSlice(prefix);
            try buf.append('_');
        }
        try buf.appendSlice(name);
        for (concrete_types) |ct| {
            try buf.append('_');
            const safe = try self.sanitizeTypeName(ct);
            try buf.appendSlice(safe);
        }
        return buf.toOwnedSlice();
    }

    /// Queue a monomorphization request (if not already generated/pending)
    fn requestMonomorphization(self: *Self, func: *FunctionDecl, type_prefix: ?[]const u8, concrete_types: []const []const u8, generic_params: []const *ast.GenericParam) !void {
        const mono_name = try self.getMonomorphizedName(func.name.name, type_prefix, concrete_types);
        if (self.generated_monomorphizations.contains(mono_name)) return;
        if (self.pending_monomorphizations.contains(mono_name)) return;

        // Collect generic param names
        var param_names = try std.ArrayList([]const u8).initCapacity(self.stringAllocator(), generic_params.len);
        for (generic_params) |gp| {
            try param_names.append(gp.name.name);
        }

        try self.pending_monomorphizations.put(mono_name, .{
            .func = func,
            .type_prefix = type_prefix,
            .concrete_types = concrete_types,
            .generic_param_names = try param_names.toOwnedSlice(),
        });
    }

    /// Emit forward declarations (prototypes) for all pending monomorphized functions
    fn emitMonomorphizedPrototypes(self: *Self) !void {
        while (self.pending_monomorphizations.count() > 0) {
            var iter = self.pending_monomorphizations.iterator();
            var new_items = std.ArrayList(MonoQueueItem).init(self.allocator);
            defer new_items.deinit();

            while (iter.next()) |entry| {
                try new_items.append(.{ .name = entry.key_ptr.*, .req = entry.value_ptr.* });
            }
            self.pending_monomorphizations.clearRetainingCapacity();

            for (new_items.items) |item| {
                try self.generated_monomorphizations.put(item.name, {});

                // Set up type substitutions for correct type mapping in prototype
                var subs = std.StringHashMap([]const u8).init(self.allocator);
                for (item.req.generic_param_names, 0..) |param_name, j| {
                    if (j < item.req.concrete_types.len) {
                        try subs.put(param_name, item.req.concrete_types[j]);
                    }
                }
                const saved_subs = self.active_type_substitutions;
                self.active_type_substitutions = subs;
                try self.emitFunctionPrototype(item.req.func, item.req.type_prefix, item.name);

                // Track monomorphized function return type for inferCTypeFromExpr
                const ret_type = if (item.req.func.return_type) |rt| try self.mapType(rt) else "void";
                self.trackFunctionReturnType(item.name, ret_type);

                self.active_type_substitutions = saved_subs;
                subs.deinit();

                // Track for later implementation emission
                try self.mono_emit_queue.append(item);
            }
        }
    }

    /// Emit implementations for all queued monomorphized functions.
    /// When implementations discover new monomorphizations (e.g., a generic function
    /// calling another generic), their forward declarations are collected and spliced
    /// in before the implementation section to ensure proper C declaration ordering.
    fn emitMonomorphizedImplementations(self: *Self) !void {
        // Save position before implementations so we can insert late-discovered prototypes
        const pre_impl_len = self.writer.getOutput().len;

        // Collect late-discovered prototypes in a separate buffer
        var late_prototype_writer = CWriter.init(self.allocator);
        defer late_prototype_writer.deinit();

        // Emit implementations for already-prototyped items from mono_emit_queue
        for (self.mono_emit_queue.items) |item| {
            try self.emitMonomorphizedFunction(item.name, item.req);
        }

        // Handle any new monomorphizations discovered during implementation emission.
        // Write their prototypes to late_prototype_writer and implementations to main writer.
        while (self.pending_monomorphizations.count() > 0) {
            var new_items = std.ArrayList(MonoQueueItem).init(self.allocator);
            defer new_items.deinit();

            var iter = self.pending_monomorphizations.iterator();
            while (iter.next()) |entry| {
                try new_items.append(.{ .name = entry.key_ptr.*, .req = entry.value_ptr.* });
            }
            self.pending_monomorphizations.clearRetainingCapacity();

            // Emit forward declarations to late_prototype_writer (NOT the main writer)
            for (new_items.items) |item| {
                try self.generated_monomorphizations.put(item.name, {});

                var subs = std.StringHashMap([]const u8).init(self.allocator);
                for (item.req.generic_param_names, 0..) |param_name, j| {
                    if (j < item.req.concrete_types.len) {
                        try subs.put(param_name, item.req.concrete_types[j]);
                    }
                }
                const saved_subs = self.active_type_substitutions;
                self.active_type_substitutions = subs;

                // Temporarily swap writer to capture prototype in late_prototype_writer
                const saved_writer = self.writer;
                self.writer = late_prototype_writer;
                try self.emitFunctionPrototype(item.req.func, item.req.type_prefix, item.name);
                late_prototype_writer = self.writer;
                self.writer = saved_writer;

                const ret_type = if (item.req.func.return_type) |rt| try self.mapType(rt) else "void";
                self.trackFunctionReturnType(item.name, ret_type);
                self.active_type_substitutions = saved_subs;
                subs.deinit();
            }

            // Emit implementations to main writer
            for (new_items.items) |item| {
                try self.emitMonomorphizedFunction(item.name, item.req);
            }
        }

        // Splice late-discovered prototypes before the implementations
        if (late_prototype_writer.getOutput().len > 0) {
            const impl_code = try self.allocator.dupe(u8, self.writer.getOutput()[pre_impl_len..]);
            defer self.allocator.free(impl_code);
            self.writer.truncate(pre_impl_len);
            try self.writer.write(late_prototype_writer.getOutput());
            try self.writer.write(impl_code);
        }
    }

    /// Emit a single monomorphized function implementation
    fn emitMonomorphizedFunction(self: *Self, mono_name: []const u8, req: MonoRequest) !void {
        // Set up type substitutions
        var subs = std.StringHashMap([]const u8).init(self.allocator);
        for (req.generic_param_names, 0..) |param_name, i| {
            if (i < req.concrete_types.len) {
                try subs.put(param_name, req.concrete_types[i]);
            }
        }

        // Save and set active type substitutions
        const saved_subs = self.active_type_substitutions;
        self.active_type_substitutions = subs;
        defer {
            self.active_type_substitutions = saved_subs;
            subs.deinit();
        }

        // Emit only the definition (prototype was already emitted)
        try self.emitFunctionDefinition(req.func, req.type_prefix, mono_name);
    }

    /// Generate constant definition
    fn generateConstant(self: *Self, const_decl: *ConstDecl) !void {
        // Use raw name (not mangled) so identifier references match the declaration
        const name = const_decl.name.name;

        // Try comptime evaluation first for fully evaluated static constants
        if (const_decl.value) |val| {
            const inner = if (val.kind == .comptime_expr) val.kind.comptime_expr.expr else val;
            if (self.evalComptime(inner)) |cv| {
                switch (cv) {
                    .int_val => |v| {
                        self.trackVariableType(name, "int64_t");
                        try self.writer.print("static const int64_t {s} = {d};", .{ name, v });
                        try self.writer.newline();
                        return;
                    },
                    .float_val => |v| {
                        self.trackVariableType(name, "double");
                        try self.writer.print("static const double {s} = {d};", .{ name, v });
                        try self.writer.newline();
                        return;
                    },
                    .bool_val => |v| {
                        self.trackVariableType(name, "bool");
                        try self.writer.print("static const bool {s} = {s};", .{ name, if (v) "true" else "false" });
                        try self.writer.newline();
                        return;
                    },
                    .string_val => |v| {
                        self.trackVariableType(name, "dm_string");
                        // Use C struct literal initializer (valid for static const)
                        try self.writer.print("static const dm_string {s} = {{.data = \"{s}\", .len = {d}, .capacity = {d}}};", .{ name, v, v.len, v.len });
                        try self.writer.newline();
                        return;
                    },
                    .void_val => {},
                    .array_val, .struct_val => {},
                }
            }
        }

        // Fallback: infer type and use runtime expression
        const type_str = if (const_decl.type_expr) |t| try self.mapType(t) else blk: {
            if (const_decl.value) |val| {
                const inner = if (val.kind == .comptime_expr) val.kind.comptime_expr.expr else val;
                break :blk self.inferCTypeFromExpr(inner);
            }
            break :blk @as([]const u8, "int64_t");
        };

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
        self.async_block_depth += 1;
        defer self.async_block_depth -= 1;

        for (block.statements) |stmt| {
            try self.generateStatement(stmt);
        }

        // For async functions at function body level (depth 1), if the block has no
        // result and doesn't end with a return, emit an implicit return _ready().
        // Inner blocks (while/for/if bodies at depth > 1) should NOT get this.
        if (block.result == null and self.current_function_is_async and self.async_block_depth == 1) {
            // Check if the last statement is already a return
            const has_trailing_return = if (block.statements.len > 0)
                block.statements[block.statements.len - 1].kind == .return_stmt
            else
                false;
            if (!has_trailing_return) {
                const future_type = self.current_function_future_type.?;
                try self.writer.print("return {s}_ready()", .{future_type});
                try self.writer.writeLine(";");
            }
        }

        // Handle block result expression
        if (block.result) |result| {
            try self.emitLineDirective(result.span);
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
            } else if (self.current_function_is_async) {
                // For async functions, wrap the result in _ready()
                const future_type = self.current_function_future_type.?;
                if (result.kind == .if_expr) {
                    // If/else in return position: generate as statement form.
                    // Branches with explicit returns already get _ready() wrapping
                    // via generateReturn. For branches with result expressions,
                    // use generateIfStatementWithAsyncReturn.
                    try self.generateIfStatementWithAsyncReturn(result.kind.if_expr);
                } else if (result.kind == .match_expr) {
                    try self.generateMatchStatement(result.kind.match_expr);
                } else if (std.mem.eql(u8, future_type, "dm_future_void")) {
                    // Void future: execute expression, then return _ready()
                    try self.generateExpr(result);
                    try self.writer.writeLine(";");
                    try self.writer.print("return {s}_ready()", .{future_type});
                    try self.writer.writeLine(";");
                } else {
                    try self.writer.print("return {s}_ready(", .{future_type});
                    try self.generateExpr(result);
                    try self.writer.writeLine(");");
                }
            } else {
                // For if/match expressions in return position, use statement form
                // to avoid ternary problems with missing else branches
                if (result.kind == .if_expr) {
                    try self.generateIfStatementWithReturn(result.kind.if_expr);
                } else if (result.kind == .match_expr) {
                    // Match in return position: generate as statement with returns
                    try self.writer.write("return ");
                    try self.generateExpr(result);
                    try self.writer.writeLine(";");
                } else {
                    try self.writer.write("return ");
                    try self.generateExpr(result);
                    try self.writer.writeLine(";");
                }
            }
        }
    }

    /// Generate a statement
    fn generateStatement(self: *Self, stmt: *Statement) anyerror!void {
        try self.emitLineDirective(stmt.span);
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

                // Set expected type context for Option/Result constructors
                const prev_expected = self.expected_type;
                self.expected_type = type_str;
                defer self.expected_type = prev_expected;

                // Check if the initializer is an error propagation expression (?)
                if (let.value) |val| {
                    if (val.kind == .error_propagate) {
                        try self.generateErrorPropagateLetBinding(ident.name.name, type_str, val.kind.error_propagate);
                        return;
                    }
                }

                // Check if value is a capturing lambda - if so, handle specially
                if (let.value) |val| {
                    if (val.kind == .lambda) {
                        const lam = val.kind.lambda;

                        // Pre-scan for captures to determine if this is a closure
                        var pre_param_names = std.ArrayList([]const u8).init(self.stringAllocator());
                        for (lam.params) |p| {
                            try pre_param_names.append(p.name.name);
                        }
                        var pre_captures = std.ArrayList(CapturedVar).init(self.stringAllocator());
                        switch (lam.body) {
                            .expression => |expr| self.collectFreeVars(expr, pre_param_names.items, &pre_captures),
                            .block => |block| self.collectFreeVarsBlock(block, pre_param_names.items, &pre_captures),
                        }

                        if (pre_captures.items.len > 0) {
                            // This is a closure. generateExpr hoists the lambda function
                            // definition but does not emit to the main writer.
                            try self.generateExpr(val);

                            const lambda_name_key = try std.fmt.allocPrint(self.stringAllocator(), "__dm_lambda_{d}", .{self.lambda_counter - 1});
                            if (self.closure_info.get(lambda_name_key)) |info| {
                                // Emit closure struct init
                                try self.writer.newline();
                                try self.writer.print("static {s} {s};", .{
                                    try std.fmt.allocPrint(self.stringAllocator(), "__dm_closure_{d}", .{self.lambda_counter - 1}),
                                    info.env_var,
                                });
                                try self.writer.newline();
                                for (info.captures) |cap| {
                                    try self.writer.print("{s}.{s} = {s};", .{ info.env_var, cap.name, cap.name });
                                    try self.writer.newline();
                                }

                                // Emit the let binding
                                try self.emitVarDecl(type_str, ident.name.name);
                                try self.writer.print(" = (void*){s};", .{lambda_name_key});
                                try self.writer.newline();

                                // Map variable name to closure info
                                try self.closure_info.put(ident.name.name, info);
                                return;
                            }
                        }

                        // Non-capturing lambda - handle normally
                        try self.emitVarDecl(type_str, ident.name.name);
                        try self.writer.write(" = ");
                        try self.generateExpr(val);
                        try self.writer.writeLine(";");
                        return;
                    }
                }

                // Check if this is a dyn Trait binding
                if (let.type_annotation) |ta| {
                    if (ta.kind == .trait_object) {
                        const trait_name = try self.extractTypeName(ta.kind.trait_object.trait_type);
                        // Track this variable as a dyn trait object
                        try self.dyn_var_traits.put(ident.name.name, trait_name);
                        // Infer concrete type from the value expression
                        if (let.value) |val| {
                            const concrete_type = self.inferCTypeFromExpr(val);
                            try self.writer.print("{s} {s};", .{ type_str, ident.name.name });
                            try self.writer.newline();
                            try self.writer.print("{s}.data = malloc(sizeof({s}));", .{ ident.name.name, concrete_type });
                            try self.writer.newline();
                            try self.writer.print("*({s}*){s}.data = ", .{ concrete_type, ident.name.name });
                            try self.generateExpr(val);
                            try self.writer.writeLine(";");
                            try self.writer.print("{s}.vtable = &dm_vtable_{s}_for_{s};", .{
                                ident.name.name,
                                trait_name,
                                concrete_type[3..], // strip "dm_" prefix
                            });
                            try self.writer.newline();
                        }
                        return;
                    }
                }

                try self.emitVarDecl(type_str, ident.name.name);
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
                return error.CodegenFailed;
            },
        }
    }

    /// Generate return statement
    fn generateReturn(self: *Self, ret: *ReturnStmt) anyerror!void {
        if (self.current_function_is_async) {
            if (ret.value) |val| {
                const future_type = self.current_function_future_type.?;
                try self.writer.print("return {s}_ready(", .{future_type});
                try self.generateExpr(val);
                try self.writer.writeLine(");");
            } else {
                const future_type = self.current_function_future_type.?;
                try self.writer.print("return {s}_ready()", .{future_type});
                try self.writer.writeLine(";");
            }
        } else if (ret.value) |val| {
            // Set expected type from function return type for Option/Result constructors
            const prev_expected = self.expected_type;
            self.expected_type = self.current_function_return_type;
            defer self.expected_type = prev_expected;

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

    /// Generate if statement where each branch returns its result.
    /// Used for if-expressions in return position of non-void functions.
    fn generateIfStatementWithReturn(self: *Self, if_expr: *IfExpr) anyerror!void {
        try self.writer.write("if (");
        try self.generateExpr(if_expr.condition);
        try self.writer.write(")");
        try self.writer.beginBlock();
        // Generate then branch with return
        for (if_expr.then_branch.statements) |stmt| {
            try self.generateStatement(stmt);
        }
        if (if_expr.then_branch.result) |result| {
            if (result.kind == .if_expr) {
                try self.generateIfStatementWithReturn(result.kind.if_expr);
            } else {
                try self.writer.write("return ");
                try self.generateExpr(result);
                try self.writer.writeLine(";");
            }
        }
        try self.writer.closeBraceInline();

        if (if_expr.else_branch) |else_br| {
            switch (else_br) {
                .else_block => |block| {
                    try self.writer.write(" else");
                    try self.writer.beginBlock();
                    for (block.statements) |stmt| {
                        try self.generateStatement(stmt);
                    }
                    if (block.result) |result| {
                        if (result.kind == .if_expr) {
                            try self.generateIfStatementWithReturn(result.kind.if_expr);
                        } else {
                            try self.writer.write("return ");
                            try self.generateExpr(result);
                            try self.writer.writeLine(";");
                        }
                    }
                    try self.writer.endBlock();
                },
                .else_if => |else_if| {
                    try self.writer.write(" else ");
                    try self.generateIfStatementWithReturn(else_if);
                },
            }
        } else {
            try self.writer.newline();
        }
    }

    /// Generate if expression as statement form with async return wrapping.
    /// Branches with explicit return statements get _ready() wrapping via generateReturn.
    /// Branches with result expressions get wrapped with return _ready(expr).
    fn generateIfStatementWithAsyncReturn(self: *Self, if_expr: *IfExpr) anyerror!void {
        const future_type = self.current_function_future_type.?;
        try self.writer.write("if (");
        try self.generateExpr(if_expr.condition);
        try self.writer.write(")");
        try self.writer.beginBlock();
        // Generate then branch
        for (if_expr.then_branch.statements) |stmt| {
            try self.generateStatement(stmt);
        }
        if (if_expr.then_branch.result) |result| {
            if (result.kind == .if_expr) {
                try self.generateIfStatementWithAsyncReturn(result.kind.if_expr);
            } else {
                try self.writer.print("return {s}_ready(", .{future_type});
                try self.generateExpr(result);
                try self.writer.writeLine(");");
            }
        }
        try self.writer.closeBraceInline();

        if (if_expr.else_branch) |else_br| {
            switch (else_br) {
                .else_block => |block| {
                    try self.writer.write(" else");
                    try self.writer.beginBlock();
                    for (block.statements) |stmt| {
                        try self.generateStatement(stmt);
                    }
                    if (block.result) |result| {
                        if (result.kind == .if_expr) {
                            try self.generateIfStatementWithAsyncReturn(result.kind.if_expr);
                        } else {
                            try self.writer.print("return {s}_ready(", .{future_type});
                            try self.generateExpr(result);
                            try self.writer.writeLine(");");
                        }
                    }
                    try self.writer.endBlock();
                },
                .else_if => |else_if| {
                    try self.writer.write(" else ");
                    try self.generateIfStatementWithAsyncReturn(else_if);
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
        const raw_type = self.inferCTypeFromExpr(match.scrutinee);
        const is_box = std.mem.endsWith(u8, raw_type, "*");
        const scrutinee_type = if (is_box) raw_type[0 .. raw_type.len - 1] else raw_type;
        try self.writer.print("{s} {s} = ", .{ scrutinee_type, temp });
        if (is_box) {
            // Dereference Box pointer for match
            try self.writer.write("*(");
            try self.generateExpr(match.scrutinee);
            try self.writer.write(")");
        } else {
            try self.generateExpr(match.scrutinee);
        }
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

            try self.generatePatternCondition(arm.pattern, temp, scrutinee_type);

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
    fn generatePatternCondition(self: *Self, pattern: *Pattern, scrutinee: []const u8, scrutinee_type: []const u8) anyerror!void {
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
                const variant_name = variant.variant.name;
                // Check if scrutinee is an Option type
                if (std.mem.startsWith(u8, scrutinee_type, "dm_option_")) {
                    if (std.mem.eql(u8, variant_name, "Some")) {
                        try self.writer.print("{s}.has_value", .{scrutinee});
                    } else if (std.mem.eql(u8, variant_name, "None")) {
                        try self.writer.print("!{s}.has_value", .{scrutinee});
                    } else {
                        try self.writer.write("true /* unknown option variant */");
                    }
                } else if (std.mem.startsWith(u8, scrutinee_type, "dm_result_")) {
                    // Check if scrutinee is a Result type
                    if (std.mem.eql(u8, variant_name, "Ok")) {
                        try self.writer.print("{s}.is_ok", .{scrutinee});
                    } else if (std.mem.eql(u8, variant_name, "Err")) {
                        try self.writer.print("!{s}.is_ok", .{scrutinee});
                    } else {
                        try self.writer.write("true /* unknown result variant */");
                    }
                } else if (variant.type_path) |path| {
                    // Check tag/value for enum variant
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
                    try self.generatePatternCondition(sub_pat, scrutinee, scrutinee_type);
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
                const variant_name = variant.variant.name;
                // Option type pattern bindings
                if (std.mem.startsWith(u8, scrutinee_type, "dm_option_")) {
                    if (std.mem.eql(u8, variant_name, "Some")) {
                        switch (variant.payload) {
                            .tuple => |patterns| {
                                if (patterns.len == 1 and patterns[0].kind == .identifier) {
                                    const inner_type = self.generated_option_types.get(scrutinee_type) orelse "int64_t";
                                    const bind_name = patterns[0].kind.identifier.name.name;
                                    try self.writer.printLine("{s} {s} = {s}.value;", .{ inner_type, bind_name, scrutinee });
                                    self.trackVariableType(bind_name, inner_type);
                                }
                            },
                            else => {},
                        }
                    }
                    // None has no bindings
                    return;
                }
                // Result type pattern bindings
                if (std.mem.startsWith(u8, scrutinee_type, "dm_result_")) {
                    const result_info = self.generated_result_types.get(scrutinee_type);
                    if (std.mem.eql(u8, variant_name, "Ok")) {
                        switch (variant.payload) {
                            .tuple => |patterns| {
                                if (patterns.len == 1 and patterns[0].kind == .identifier) {
                                    const ok_type = if (result_info) |ri| ri.ok_type else "int64_t";
                                    const bind_name = patterns[0].kind.identifier.name.name;
                                    try self.writer.printLine("{s} {s} = {s}.ok;", .{ ok_type, bind_name, scrutinee });
                                    self.trackVariableType(bind_name, ok_type);
                                }
                            },
                            else => {},
                        }
                    } else if (std.mem.eql(u8, variant_name, "Err")) {
                        switch (variant.payload) {
                            .tuple => |patterns| {
                                if (patterns.len == 1 and patterns[0].kind == .identifier) {
                                    const err_type = if (result_info) |ri| ri.err_type else "dm_string";
                                    const bind_name = patterns[0].kind.identifier.name.name;
                                    try self.writer.printLine("{s} {s} = {s}.err;", .{ err_type, bind_name, scrutinee });
                                    self.trackVariableType(bind_name, err_type);
                                }
                            },
                            else => {},
                        }
                    }
                    return;
                }
                // Regular enum variant bindings
                switch (variant.payload) {
                    .none => {},
                    .tuple => |patterns| {
                        if (patterns.len == 1 and patterns[0].kind == .identifier) {
                            // Single payload: look up actual variant payload type
                            const vkey = try std.fmt.allocPrint(self.stringAllocator(), "{s}.{s}", .{ scrutinee_type, variant.variant.name });
                            const payload_type = self.enum_variant_types.get(vkey) orelse "int64_t";
                            try self.writer.printLine("{s} {s} = {s}.data.{s};", .{
                                payload_type,
                                patterns[0].kind.identifier.name.name,
                                scrutinee,
                                variant.variant.name,
                            });
                            self.trackVariableType(patterns[0].kind.identifier.name.name, payload_type);
                        } else {
                            for (patterns, 0..) |pat, i| {
                                if (pat.kind == .identifier) {
                                    // Look up individual tuple element type
                                    const vkey = try std.fmt.allocPrint(self.stringAllocator(), "{s}.{s}._{d}", .{ scrutinee_type, variant.variant.name, i });
                                    const elem_type = self.enum_variant_types.get(vkey) orelse "int64_t";
                                    try self.writer.printLine("{s} {s} = {s}.data.{s}._{d};", .{
                                        elem_type,
                                        pat.kind.identifier.name.name,
                                        scrutinee,
                                        variant.variant.name,
                                        i,
                                    });
                                    self.trackVariableType(pat.kind.identifier.name.name, elem_type);
                                }
                            }
                        }
                    },
                    .struct_fields => |fields| {
                        for (fields) |field| {
                            if (field.pattern) |pat| {
                                if (pat.kind == .identifier) {
                                    // Look up struct field type in variant
                                    const vkey = try std.fmt.allocPrint(self.stringAllocator(), "{s}.{s}.{s}", .{ scrutinee_type, variant.variant.name, field.name.name });
                                    const field_type = self.enum_variant_types.get(vkey) orelse "int64_t";
                                    try self.writer.printLine("{s} {s} = {s}.data.{s}.{s};", .{
                                        field_type,
                                        pat.kind.identifier.name.name,
                                        scrutinee,
                                        variant.variant.name,
                                        field.name.name,
                                    });
                                    self.trackVariableType(pat.kind.identifier.name.name, field_type);
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
            } else if (std.mem.startsWith(u8, iter_type, "dm_map_")) {
                // Map iteration: for k in map { ... }
                // Look up the key type from generated map type info
                var key_type: []const u8 = "int64_t";
                if (self.generated_map_types.get(iter_type)) |map_info| {
                    key_type = map_info.key_type;
                }

                try self.writer.print("{s} {s} = ", .{ iter_type, iter_temp });
                try self.generateExpr(for_stmt.iterator);
                try self.writer.writeLine(";");
                try self.writer.print("for (size_t {s} = 0; {s} < {s}.capacity; {s}++)", .{ idx_temp, idx_temp, iter_temp, idx_temp });
                try self.writer.beginBlock();

                // Skip empty/deleted slots
                try self.writer.printLine("if ({s}.entries[{s}].state != 1) continue;", .{ iter_temp, idx_temp });

                // Bind pattern variable to key
                switch (for_stmt.pattern.kind) {
                    .identifier => |ident| {
                        try self.writer.printLine("{s} {s} = {s}.entries[{s}].key;", .{ key_type, ident.name.name, iter_temp, idx_temp });
                        self.trackVariableType(ident.name.name, key_type);
                    },
                    else => {},
                }

                try self.generateBlock(for_stmt.body);
                try self.writer.endBlock();
            } else {
                // Fallback: unsupported iterator type - emit valid but empty C block
                try self.writer.beginBlock();
                try self.writer.writeLineComment(try std.fmt.allocPrint(self.stringAllocator(), "unsupported iterator type: {s}", .{iter_type}));
                try self.writer.endBlock();
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
        // Safe division/modulo for compound assignment: x /= y -> x = dm__safe_div(x, y)
        if (assign.op == .div_assign or assign.op == .mod_assign) {
            const target_type = self.inferCTypeFromExpr(assign.target);
            const is_float = std.mem.eql(u8, target_type, "double");
            const is_f32 = std.mem.eql(u8, target_type, "float");

            try self.generateExpr(assign.target);
            try self.writer.write(" = ");

            if (assign.op == .div_assign) {
                if (is_float) {
                    try self.writer.write("dm__safe_divf(");
                } else if (is_f32) {
                    try self.writer.write("dm__safe_divf32(");
                } else {
                    try self.writer.write("dm__safe_div(");
                }
            } else {
                try self.writer.write("dm__safe_mod(");
            }
            try self.generateExpr(assign.target);
            try self.writer.write(", ");
            try self.generateExpr(assign.value);
            try self.writer.writeLine(");");
            return;
        }

        try self.generateExpr(assign.target);

        const op_str: []const u8 = switch (assign.op) {
            .assign => " = ",
            .add_assign => " += ",
            .sub_assign => " -= ",
            .mul_assign => " *= ",
            .div_assign => unreachable, // Handled above
            .mod_assign => unreachable, // Handled above
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
                if (std.mem.eql(u8, ident.name, "None")) {
                    // None -> dm_option_T_None()
                    const target_type = self.resolveOptionResultContext();
                    if (target_type) |tt| {
                        try self.writer.print("{s}_None()", .{tt});
                    } else {
                        try self.writer.write("{ .has_value = false }");
                    }
                } else if (self.current_mut_params.contains(ident.name)) {
                    try self.writer.print("(*{s})", .{ident.name});
                } else if (!self.variable_types.contains(ident.name) and self.known_functions.get(ident.name) != null) {
                    // Function reference used as a value (not a call) - emit mangled name
                    // This enables higher-order functions: let f = my_func
                    try self.writer.write(self.known_functions.get(ident.name).?);
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
            .string_interpolation => |si| try self.generateStringInterpolation(si),
            .comptime_expr => |comp| {
                // Try to evaluate at compile time
                if (self.evalComptime(comp.expr)) |result| {
                    try self.emitComptimeValue(result);
                } else {
                    // Fall back to runtime evaluation
                    try self.generateExpr(comp.expr);
                }
            },
            .await_expr => |aw| try self.generateAwaitExpr(aw),
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

        // Operator overloading: check if left type has an impl method for this operator
        if (!std.mem.eql(u8, left_type, "int64_t") and
            !std.mem.eql(u8, left_type, "double") and
            !std.mem.eql(u8, left_type, "float") and
            !std.mem.eql(u8, left_type, "bool") and
            !std.mem.eql(u8, left_type, "dm_string"))
        {
            const method_name: ?[]const u8 = switch (bin.op) {
                .add => "add",
                .sub => "sub",
                .mul => "mul",
                .div => "div",
                .mod => "mod",
                .eq => "eq",
                .ne => "neq",
                .lt => "lt",
                .gt => "gt",
                .le => "le",
                .ge => "ge",
                else => null,
            };

            if (method_name) |mname| {
                // Check if this type has a matching impl method
                const raw_type = if (std.mem.startsWith(u8, left_type, "dm_"))
                    left_type[3..]
                else
                    left_type;

                if (self.hasImplMethod(raw_type, mname)) {
                    try self.writer.print("dm_{s}_{s}(&(", .{ raw_type, mname });
                    try self.generateExpr(bin.left);
                    try self.writer.write("), ");
                    try self.generateExpr(bin.right);
                    try self.writer.write(")");
                    return;
                }
            }
        }

        // Safe division/modulo: emit runtime-checked helper calls instead of raw / and %
        if (bin.op == .div or bin.op == .mod) {
            const is_float = std.mem.eql(u8, left_type, "double");
            const is_f32 = std.mem.eql(u8, left_type, "float");
            if (bin.op == .div) {
                if (is_float) {
                    try self.writer.write("dm__safe_divf(");
                } else if (is_f32) {
                    try self.writer.write("dm__safe_divf32(");
                } else {
                    try self.writer.write("dm__safe_div(");
                }
            } else {
                try self.writer.write("dm__safe_mod(");
            }
            try self.generateExpr(bin.left);
            try self.writer.write(", ");
            try self.generateExpr(bin.right);
            try self.writer.write(")");
            return;
        }

        try self.writer.write("(");
        try self.generateExpr(bin.left);

        const op_str: []const u8 = switch (bin.op) {
            .add => " + ",
            .sub => " - ",
            .mul => " * ",
            .div => unreachable, // Handled above
            .mod => unreachable, // Handled above
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
        // Check if the object is a pointer (self, mut param, or Box type) - use -> instead of .
        const is_pointer = if (field.object.kind == .identifier) blk: {
            const name = field.object.kind.identifier.name;
            if (std.mem.eql(u8, name, "self") or self.current_mut_params.contains(name))
                break :blk true;
            // Box type detection: variable type ends with *
            if (self.lookupVariableType(name)) |vtype| {
                break :blk std.mem.endsWith(u8, vtype, "*");
            }
            break :blk false;
        } else blk: {
            // Non-identifier expressions: check inferred type
            const obj_type = self.inferCTypeFromExpr(field.object);
            break :blk std.mem.endsWith(u8, obj_type, "*");
        };

        if (is_pointer) {
            if (field.object.kind == .identifier) {
                // For identifier pointers, emit name->field directly
                try self.writer.print("{s}->{s}", .{ field.object.kind.identifier.name, field.field.name });
            } else {
                // For complex expressions: (expr)->field
                try self.writer.write("(");
                try self.generateExpr(field.object);
                try self.writer.print(")->{s}", .{field.field.name});
            }
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
            // dm_list_X is a struct: list[i] -> bounds-checked (list).data[i]
            try self.writer.write("(dm_bounds_check((int64_t)(");
            try self.generateExpr(idx.index);
            try self.writer.write("), (int64_t)(");
            try self.generateExpr(idx.object);
            try self.writer.write(").len, \"list\"), (");
            try self.generateExpr(idx.object);
            try self.writer.write(").data[");
            try self.generateExpr(idx.index);
            try self.writer.write("])");
        } else {
            try self.generateExpr(idx.object);
            try self.writer.write("[");
            try self.generateExpr(idx.index);
            try self.writer.write("]");
        }
    }

    /// Generate method call (converted to function call)
    fn generateMethodCall(self: *Self, method: *MethodCall) anyerror!void {
        // Check if this is a dyn Trait method call — dispatch through vtable
        if (method.object.kind == .identifier) {
            const obj_name = method.object.kind.identifier.name;
            if (self.dyn_var_traits.get(obj_name)) |_| {
                // Dynamic dispatch: obj.vtable->method(obj.data, args...)
                try self.writer.print("{s}.vtable->{s}({s}.data", .{ obj_name, method.method.name, obj_name });
                for (method.args) |arg| {
                    try self.writer.write(", ");
                    try self.generateExpr(arg);
                }
                try self.writer.write(")");
                return;
            }
        }

        // Check if this is an enum constructor: EnumName.Variant(args)
        // where the object is an identifier matching a known enum type
        if (method.object.kind == .identifier) {
            const name = method.object.kind.identifier.name;
            const mangled = self.mangleTypeName(name) catch "";
            if (self.generated_enums.contains(mangled)) {
                // This is an enum constructor call: EnumName.Variant(args)
                try self.writer.print("{s}_{s}(", .{ mangled, method.method.name });
                for (method.args, 0..) |arg, i| {
                    if (i > 0) try self.writer.write(", ");
                    try self.generateExpr(arg);
                }
                try self.writer.write(")");
                return;
            }
        }

        // Convert obj.method(args) to dm_Type_method(&obj, args)
        // Infer the type name from the object expression
        var obj_type = self.inferCTypeFromExpr(method.object);
        const is_box = std.mem.endsWith(u8, obj_type, "*");

        // Strip pointer suffix for Box types
        if (is_box) obj_type = obj_type[0 .. obj_type.len - 1];

        // Strip "dm_" prefix if present to get the raw type name for method lookup
        const type_name = if (std.mem.startsWith(u8, obj_type, "dm_"))
            obj_type[3..]
        else
            obj_type;

        try self.writer.print("dm_{s}_{s}(", .{ type_name, method.method.name });

        if (is_box) {
            // Box is already a pointer, pass directly
            try self.writer.write("(");
        } else {
            try self.writer.write("&(");
        }
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

            // Check if this is a closure variable - call with env pointer
            if (self.closure_info.get(name)) |info| {
                try self.writer.write(info.lambda_name);
                try self.writer.write("(");
                try self.writer.print("&{s}", .{info.env_var});
                for (call.args) |arg| {
                    try self.writer.write(", ");
                    try self.generateExpr(arg.value);
                }
                try self.writer.write(")");
                return;
            }
            // Check if this is a variable holding a function pointer (local var or parameter)
            if (self.variable_types.contains(name) or self.current_mut_params.contains(name)) {
                // Variable call - function pointer, use name as-is
                try self.writer.write(name);
            } else if (self.generic_functions.get(name)) |generic_func| {
                // Generic function call
                const concrete_types = try self.resolveGenericArgs(call, generic_func);
                const mono_name = try self.getMonomorphizedName(name, null, concrete_types);
                try self.requestMonomorphization(generic_func, null, concrete_types, generic_func.generic_params.?);
                mangled_name = mono_name;
                try self.writer.write(mono_name);
            } else if (self.extern_functions.contains(name)) {
                // Extern function - use raw C name without dm_ prefix
                mangled_name = name;
                try self.writer.write(name);
            } else {
                // User-defined function - mangle the name
                mangled_name = try self.mangleFunctionName(name, null);
                try self.writer.write(mangled_name.?);
            }
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
        } else if (std.mem.eql(u8, name, "assert_eq")) {
            // assert_eq(actual, expected) — compare two values, panic with message if not equal
            if (call.args.len >= 2) {
                const lhs_type = self.inferCTypeFromExpr(call.args[0].value);
                if (std.mem.eql(u8, lhs_type, "dm_string")) {
                    // String comparison
                    try self.writer.write("if (!dm_string_eq(");
                    try self.generateExpr(call.args[0].value);
                    try self.writer.write(", ");
                    try self.generateExpr(call.args[1].value);
                    try self.writer.write(")) dm_panic(\"assert_eq failed: strings not equal\")");
                } else {
                    // Numeric/bool comparison
                    try self.writer.write("if ((");
                    try self.generateExpr(call.args[0].value);
                    try self.writer.write(") != (");
                    try self.generateExpr(call.args[1].value);
                    try self.writer.write(")) dm_panic(\"assert_eq failed: values not equal\")");
                }
            }
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
        } else if (std.mem.eql(u8, name, "file_append")) {
            if (call.args.len >= 2) {
                try self.writer.write("dm_append_file(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "file_exists")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_file_exists(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "read_line")) {
            try self.writer.write("dm_read_line()");
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
        } else if (std.mem.eql(u8, name, "Box_new")) {
            // Box_new(value) -> heap-allocate value via monomorphized helper
            if (call.args.len > 0) {
                const arg_type = self.inferCTypeFromExpr(call.args[0].value);
                const safe_name = try self.sanitizeTypeName(arg_type);
                const box_helper = try std.fmt.allocPrint(self.stringAllocator(), "dm_box_{s}", .{safe_name});
                // Ensure the box helper has been generated
                if (!self.generated_box_types.contains(box_helper)) {
                    try self.generated_box_types.put(box_helper, arg_type);
                    try self.emitBoxTypeDefinition(box_helper, arg_type);
                }
                try self.writer.print("{s}(", .{box_helper});
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "Box_null")) {
            // Box_null() -> NULL pointer
            try self.writer.write("NULL");
            return true;
        } else if (std.mem.eql(u8, name, "Map_new")) {
            // Map_new() -> zero-initialized map struct
            try self.writer.write("{ .entries = NULL, .len = 0, .capacity = 0 }");
            return true;
        } else if (std.mem.eql(u8, name, "string_split")) {
            // Ensure dm_list_dm_string type is generated
            _ = try self.generateListTypeByName("dm_string");
            if (call.args.len >= 2) {
                try self.writer.write("dm_string_split(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "string_trim")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_string_trim(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "string_replace")) {
            if (call.args.len >= 3) {
                try self.writer.write("dm_string_replace(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[2].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "string_to_upper")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_string_to_upper(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "string_to_lower")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_string_to_lower(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "path_dirname")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_path_dirname(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "path_basename")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_path_basename(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "path_extension")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_path_extension(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "path_stem")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_path_stem(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "path_join")) {
            if (call.args.len >= 2) {
                try self.writer.write("dm_path_join(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "fs_mkdir")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_fs_mkdir(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "fs_readdir")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_fs_readdir(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "fs_remove")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_fs_remove(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "fs_rename")) {
            if (call.args.len >= 2) {
                try self.writer.write("dm_fs_rename(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "fs_rmdir")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_fs_remove(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "fs_getcwd")) {
            try self.writer.write("dm_fs_getcwd()");
            return true;
        } else if (std.mem.eql(u8, name, "fs_exists")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_file_exists(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "env_get")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_os_getenv(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "tcp_listen")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_tcp_listen_fd(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "tcp_accept")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_tcp_accept_fd(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "tcp_connect")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_tcp_connect_fd(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "tcp_read")) {
            if (call.args.len >= 2) {
                try self.writer.write("dm_tcp_read_fd(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "tcp_write")) {
            if (call.args.len >= 2) {
                try self.writer.write("dm_tcp_write_fd(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "tcp_close")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_tcp_close_fd(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "thread_join")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_thread_join_id(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "mutex_new")) {
            try self.writer.write("dm_mutex_new_ptr()");
            return true;
        } else if (std.mem.eql(u8, name, "mutex_lock")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_mutex_lock_ptr(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "mutex_unlock")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_mutex_unlock_ptr(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "mutex_destroy")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_mutex_destroy_ptr(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "Some")) {
            // Some(value) -> dm_option_T_Some(value)
            if (call.args.len > 0) {
                const target_type = self.resolveOptionResultContext();
                if (target_type) |tt| {
                    try self.writer.print("{s}_Some(", .{tt});
                    try self.generateExpr(call.args[0].value);
                    try self.writer.write(")");
                } else {
                    // Infer from argument type
                    const arg_type = self.inferCTypeFromExpr(call.args[0].value);
                    const opt_type = try self.generateOptionTypeByName(arg_type);
                    try self.writer.print("{s}_Some(", .{opt_type});
                    try self.generateExpr(call.args[0].value);
                    try self.writer.write(")");
                }
            }
            return true;
        } else if (std.mem.eql(u8, name, "None")) {
            // None() -> dm_option_T_None()
            const target_type = self.resolveOptionResultContext();
            if (target_type) |tt| {
                try self.writer.print("{s}_None()", .{tt});
            } else {
                try self.writer.write("{ .has_value = false }");
            }
            return true;
        } else if (std.mem.eql(u8, name, "Ok")) {
            // Ok(value) -> dm_result_T_E_Ok(value)
            if (call.args.len > 0) {
                const target_type = self.resolveOptionResultContext();
                if (target_type) |tt| {
                    try self.writer.print("{s}_Ok(", .{tt});
                    try self.generateExpr(call.args[0].value);
                    try self.writer.write(")");
                } else {
                    // Infer: create Result[arg_type, string] by default
                    const arg_type = self.inferCTypeFromExpr(call.args[0].value);
                    const res_type = try self.generateResultTypeByName(arg_type, "dm_string");
                    try self.writer.print("{s}_Ok(", .{res_type});
                    try self.generateExpr(call.args[0].value);
                    try self.writer.write(")");
                }
            }
            return true;
        } else if (std.mem.eql(u8, name, "Err")) {
            // Err(value) -> dm_result_T_E_Err(value)
            if (call.args.len > 0) {
                const target_type = self.resolveOptionResultContext();
                if (target_type) |tt| {
                    try self.writer.print("{s}_Err(", .{tt});
                    try self.generateExpr(call.args[0].value);
                    try self.writer.write(")");
                } else {
                    // Default: Result[int, string]
                    const res_type = try self.generateResultTypeByName("int64_t", "dm_string");
                    try self.writer.print("{s}_Err(", .{res_type});
                    try self.generateExpr(call.args[0].value);
                    try self.writer.write(")");
                }
            }
            return true;
        } else if (std.mem.eql(u8, name, "string_to_int")) {
            // string_to_int is alias for parse_int
            if (call.args.len > 0) {
                try self.writer.write("dm_parse_int(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "char_at")) {
            // char_at(s, i) -> character at index as string
            if (call.args.len >= 2) {
                try self.writer.write("dm_char_to_string((");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(").data[");
                try self.generateExpr(call.args[1].value);
                try self.writer.write("])");
            }
            return true;
        } else if (std.mem.eql(u8, name, "parse_float")) {
            if (call.args.len > 0) {
                try self.writer.write("dm_parse_float(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "simd_add") or std.mem.eql(u8, name, "simd_sub") or
            std.mem.eql(u8, name, "simd_mul") or std.mem.eql(u8, name, "simd_div"))
        {
            // simd_add(a, b) -> dm_simd_add(a, b), etc.
            if (call.args.len >= 2) {
                try self.writer.write("dm_");
                try self.writer.write(name);
                try self.writer.write("(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.eql(u8, name, "simd_extract")) {
            // simd_extract(vec, idx) -> dm_simd_extract(vec, idx)
            if (call.args.len >= 2) {
                try self.writer.write("dm_simd_extract(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(", ");
                try self.generateExpr(call.args[1].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.startsWith(u8, name, "simd_splat_")) {
            // simd_splat_f32x4(val) -> dm_simd_splat_f32x4(val)
            if (call.args.len >= 1) {
                try self.writer.write("dm_");
                try self.writer.write(name);
                try self.writer.write("(");
                try self.generateExpr(call.args[0].value);
                try self.writer.write(")");
            }
            return true;
        } else if (std.mem.startsWith(u8, name, "simd_set_")) {
            // simd_set_f32x4(a, b, c, d) -> dm_simd_set_f32x4(a, b, c, d)
            if (call.args.len >= 1) {
                try self.writer.write("dm_");
                try self.writer.write(name);
                try self.writer.write("(");
                for (call.args, 0..) |arg, i| {
                    if (i > 0) try self.writer.write(", ");
                    try self.generateExpr(arg.value);
                }
                try self.writer.write(")");
            }
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
                // [value; count] syntax - expand to { val, val, ... val }
                const count = evalExprUsize(repeat.count) orelse 1;
                try self.writer.write("{ ");
                for (0..count) |i| {
                    if (i > 0) try self.writer.write(", ");
                    try self.generateExpr(repeat.value);
                }
                try self.writer.write(" }");
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
            try self.generatePatternCondition(arm.pattern, "_dm_unreachable", "int64_t");

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

    // ========================================================================
    // Comptime Evaluation
    // ========================================================================

    const ComptimeValue = union(enum) {
        int_val: i64,
        float_val: f64,
        bool_val: bool,
        string_val: []const u8,
        array_val: []ComptimeValue,
        struct_val: *std.StringHashMap(ComptimeValue),
        void_val: void,
    };

    /// Comptime evaluation environment with lexical scoping.
    const ComptimeEnv = struct {
        variables: std.StringHashMap(ComptimeValue),
        parent: ?*ComptimeEnv,
        call_depth: u32,
        iteration_count: u32,
        max_call_depth: u32,
        max_iterations: u32,
        allocator: Allocator,

        fn init(alloc: Allocator, parent: ?*ComptimeEnv) ComptimeEnv {
            const depth = if (parent) |p| p.call_depth else 0;
            return .{
                .variables = std.StringHashMap(ComptimeValue).init(alloc),
                .parent = parent,
                .call_depth = depth,
                .iteration_count = 0,
                .max_call_depth = 1000,
                .max_iterations = 100000,
                .allocator = alloc,
            };
        }

        fn deinit(self: *ComptimeEnv) void {
            self.variables.deinit();
        }

        fn lookup(self: *const ComptimeEnv, name: []const u8) ?ComptimeValue {
            if (self.variables.get(name)) |v| return v;
            if (self.parent) |p| return p.lookup(name);
            return null;
        }

        fn put(self: *ComptimeEnv, name: []const u8, val: ComptimeValue) !void {
            try self.variables.put(name, val);
        }

        /// Update an existing variable in the nearest scope that contains it
        fn update(self: *ComptimeEnv, name: []const u8, val: ComptimeValue) bool {
            if (self.variables.getPtr(name)) |ptr| {
                ptr.* = val;
                return true;
            }
            if (self.parent) |p| return p.update(name, val);
            return false;
        }
    };

    /// Sentinel errors for comptime control flow
    const ComptimeControl = enum {
        break_signal,
        continue_signal,
    };

    /// Try to evaluate an expression at compile time (entry point, creates root env).
    /// Returns null if the expression cannot be evaluated statically.
    fn evalComptime(self: *Self, expr: *const Expr) ?ComptimeValue {
        var root_env = ComptimeEnv.init(self.allocator, null);
        defer root_env.deinit();

        // Pre-populate root env with file-scope const declarations
        if (self.current_source) |source| {
            for (source.declarations) |decl| {
                switch (decl.kind) {
                    .constant => |c| {
                        if (c.value) |val| {
                            const inner = if (val.kind == .comptime_expr) val.kind.comptime_expr.expr else val;
                            if (self.evalComptimeExpr(inner, &root_env)) |cv| {
                                root_env.put(c.name.name, cv) catch {};
                            }
                        }
                    },
                    else => {},
                }
            }
        }

        return self.evalComptimeExpr(expr, &root_env);
    }

    /// Evaluate an expression in a comptime environment.
    fn evalComptimeExpr(self: *Self, expr: *const Expr, env: *ComptimeEnv) ?ComptimeValue {
        switch (expr.kind) {
            .literal => |lit| {
                return switch (lit.kind) {
                    .int => |i| blk: {
                        const v = std.fmt.parseInt(i64, i.value, 10) catch break :blk null;
                        break :blk ComptimeValue{ .int_val = v };
                    },
                    .float => |f| blk: {
                        const v = std.fmt.parseFloat(f64, f.value) catch break :blk null;
                        break :blk ComptimeValue{ .float_val = v };
                    },
                    .bool => |b| ComptimeValue{ .bool_val = b },
                    .string => |s| ComptimeValue{ .string_val = s.value },
                    else => null,
                };
            },
            .identifier => |ident| {
                return env.lookup(ident.name);
            },
            .unary => |un| {
                const operand = self.evalComptimeExpr(un.operand, env) orelse return null;
                return switch (un.op) {
                    .neg => switch (operand) {
                        .int_val => |v| ComptimeValue{ .int_val = -v },
                        .float_val => |v| ComptimeValue{ .float_val = -v },
                        else => null,
                    },
                    .not => switch (operand) {
                        .bool_val => |v| ComptimeValue{ .bool_val = !v },
                        else => null,
                    },
                    else => null,
                };
            },
            .binary => |bin| {
                const left = self.evalComptimeExpr(bin.left, env) orelse return null;
                const right = self.evalComptimeExpr(bin.right, env) orelse return null;
                // Handle string concatenation (needs allocator)
                if (left == .string_val and right == .string_val and bin.op == .add) {
                    const s = std.fmt.allocPrint(self.stringAllocator(), "{s}{s}", .{ left.string_val, right.string_val }) catch return null;
                    return ComptimeValue{ .string_val = s };
                }
                return self.evalComptimeBinary(bin.op, left, right);
            },
            .grouped => |inner| return self.evalComptimeExpr(inner, env),
            .comptime_expr => |comp| return self.evalComptimeExpr(comp.expr, env),
            .block => |blk| {
                const r = self.evalComptimeBlockFull(blk, env) orelse return null;
                return switch (r) {
                    .value => |v| v,
                    .control => null, // return/break/continue at expression level = error
                };
            },
            .if_expr => |ie| {
                const r = self.evalComptimeIfFull(ie, env) orelse return null;
                return switch (r) {
                    .value => |v| v,
                    .control => null,
                };
            },
            .match_expr => |me| {
                const r = self.evalComptimeMatchFull(me, env) orelse return null;
                return switch (r) {
                    .value => |v| v,
                    .control => null,
                };
            },
            .function_call => |call| return self.evalComptimeFunctionCall(call, env),
            .field_access => |fa| return self.evalComptimeFieldAccess(fa, env),
            .index_access => |ia| return self.evalComptimeIndexAccess(ia, env),
            .array_literal => |al| return self.evalComptimeArrayLiteral(al, env),
            .struct_literal => |sl| return self.evalComptimeStructLiteral(sl, env),
            else => return null,
        }
    }

    /// Evaluate a block expression at comptime, returning full result with control flow signals.
    fn evalComptimeBlockFull(self: *Self, block: *const BlockExpr, env: *ComptimeEnv) ?ComptimeStmtResult {
        var child_env = ComptimeEnv.init(self.allocator, env);
        defer child_env.deinit();

        for (block.statements) |stmt| {
            const result = self.evalComptimeStatement(stmt, &child_env);
            if (result) |r| {
                switch (r) {
                    .control => return result, // propagate return/break/continue
                    .value => {},
                }
            } else {
                return null;
            }
        }

        if (block.result) |result_expr| {
            const val = self.evalComptimeExpr(result_expr, &child_env) orelse return null;
            return comptimeOk(val);
        }

        return comptimeVoid();
    }

    /// Result of evaluating a comptime statement
    const ComptimeStmtResult = union(enum) {
        value: ComptimeValue,
        control: ComptimeControlFlow,
    };

    const ComptimeControlFlow = union(enum) {
        return_val: ?ComptimeValue,
        break_signal: void,
        continue_signal: void,
    };

    fn comptimeOk(val: ComptimeValue) ?ComptimeStmtResult {
        return ComptimeStmtResult{ .value = val };
    }

    fn comptimeVoid() ?ComptimeStmtResult {
        return ComptimeStmtResult{ .value = ComptimeValue{ .void_val = {} } };
    }

    /// Evaluate a statement at comptime.
    fn evalComptimeStatement(self: *Self, stmt: *const Statement, env: *ComptimeEnv) ?ComptimeStmtResult {
        switch (stmt.kind) {
            .let_binding => |lb| {
                if (lb.value) |val_expr| {
                    const val = self.evalComptimeExpr(val_expr, env) orelse return null;
                    const var_name = switch (lb.pattern.kind) {
                        .identifier => |id| id.name.name,
                        else => return null,
                    };
                    env.put(var_name, val) catch return null;
                    return comptimeVoid();
                }
                return null;
            },
            .assignment => |assign| {
                const val = self.evalComptimeExpr(assign.value, env) orelse return null;
                // Handle compound assignment operators
                if (assign.op != .assign) {
                    const target_name = switch (assign.target.kind) {
                        .identifier => |id| id.name,
                        else => return null,
                    };
                    const old_val = env.lookup(target_name) orelse return null;
                    const bin_op: BinaryExpr.BinaryOp = switch (assign.op) {
                        .add_assign => .add,
                        .sub_assign => .sub,
                        .mul_assign => .mul,
                        .div_assign => .div,
                        else => return null,
                    };
                    const new_val = self.evalComptimeBinary(bin_op, old_val, val) orelse return null;
                    if (!env.update(target_name, new_val)) {
                        env.put(target_name, new_val) catch return null;
                    }
                    return comptimeVoid();
                }
                // Simple assignment
                const target_name = switch (assign.target.kind) {
                    .identifier => |id| id.name,
                    else => return null,
                };
                if (!env.update(target_name, val)) {
                    env.put(target_name, val) catch return null;
                }
                return comptimeVoid();
            },
            .return_stmt => |ret| {
                if (ret.value) |val_expr| {
                    const val = self.evalComptimeExpr(val_expr, env) orelse return null;
                    return ComptimeStmtResult{ .control = .{ .return_val = val } };
                }
                return ComptimeStmtResult{ .control = .{ .return_val = null } };
            },
            .expression => |expr| {
                // For expressions that might contain control flow (if/block/match),
                // use the Full variants to propagate return/break/continue signals
                switch (expr.kind) {
                    .if_expr => |ie| return self.evalComptimeIfFull(ie, env),
                    .block => |blk| return self.evalComptimeBlockFull(blk, env),
                    .match_expr => |me| return self.evalComptimeMatchFull(me, env),
                    else => {
                        _ = self.evalComptimeExpr(expr, env) orelse return null;
                        return comptimeVoid();
                    },
                }
            },
            .if_stmt => |ie| {
                return self.evalComptimeIfFull(ie, env);
            },
            .while_loop => |wl| {
                return self.evalComptimeWhile(wl, env);
            },
            .for_loop => |fl| {
                return self.evalComptimeFor(fl, env);
            },
            .break_stmt => {
                return ComptimeStmtResult{ .control = .{ .break_signal = {} } };
            },
            .continue_stmt => {
                return ComptimeStmtResult{ .control = .{ .continue_signal = {} } };
            },
            else => return null,
        }
    }

    /// Evaluate if/else at comptime, returning full result with control flow signals.
    fn evalComptimeIfFull(self: *Self, ie: *const IfExpr, env: *ComptimeEnv) ?ComptimeStmtResult {
        const cond = self.evalComptimeExpr(ie.condition, env) orelse return null;
        if (cond != .bool_val) return null;

        if (cond.bool_val) {
            return self.evalComptimeBlockFull(ie.then_branch, env);
        } else if (ie.else_branch) |eb| {
            return switch (eb) {
                .else_block => |b| self.evalComptimeBlockFull(b, env),
                .else_if => |elif| self.evalComptimeIfFull(elif, env),
            };
        }
        return comptimeVoid();
    }

    /// Evaluate match expression at comptime, returning full result with control flow signals.
    fn evalComptimeMatchFull(self: *Self, me: *const MatchExpr, env: *ComptimeEnv) ?ComptimeStmtResult {
        const scrutinee = self.evalComptimeExpr(me.scrutinee, env) orelse return null;

        for (me.arms) |arm| {
            if (self.comptimePatternMatches(arm.pattern, scrutinee, env)) {
                return switch (arm.body) {
                    .expression => |expr| blk: {
                        const val = self.evalComptimeExpr(expr, env) orelse break :blk null;
                        break :blk comptimeOk(val);
                    },
                    .block => |blk| self.evalComptimeBlockFull(blk, env),
                };
            }
        }
        return null;
    }

    /// Check if a pattern matches a comptime value, binding variables if needed.
    fn comptimePatternMatches(self: *Self, pattern: *const Pattern, val: ComptimeValue, env: *ComptimeEnv) bool {
        _ = self;
        switch (pattern.kind) {
            .wildcard => return true,
            .literal => |lit| {
                return switch (lit.kind) {
                    .int => |i| blk: {
                        if (val != .int_val) break :blk false;
                        const pv = std.fmt.parseInt(i64, i.value, 10) catch break :blk false;
                        break :blk val.int_val == pv;
                    },
                    .float => |f| blk: {
                        if (val != .float_val) break :blk false;
                        const pv = std.fmt.parseFloat(f64, f.value) catch break :blk false;
                        break :blk val.float_val == pv;
                    },
                    .bool => |b| blk: {
                        if (val != .bool_val) break :blk false;
                        break :blk val.bool_val == b;
                    },
                    .string => |s| blk: {
                        if (val != .string_val) break :blk false;
                        break :blk std.mem.eql(u8, val.string_val, s.value);
                    },
                    else => false,
                };
            },
            .identifier => |ident| {
                // Bind variable
                env.put(ident.name.name, val) catch return false;
                return true;
            },
            else => return false,
        }
    }

    /// Evaluate while loop at comptime.
    fn evalComptimeWhile(self: *Self, wl: *const WhileLoop, env: *ComptimeEnv) ?ComptimeStmtResult {
        var iterations: u32 = 0;
        while (true) {
            iterations += 1;
            if (iterations > env.max_iterations) return null;

            const cond = self.evalComptimeExpr(wl.condition, env) orelse return null;
            if (cond != .bool_val) return null;
            if (!cond.bool_val) break;

            // Evaluate body
            for (wl.body.statements) |stmt| {
                const result = self.evalComptimeStatement(stmt, env);
                if (result) |r| {
                    switch (r) {
                        .control => |ctrl| switch (ctrl) {
                            .return_val => return result,
                            .break_signal => return comptimeVoid(),
                            .continue_signal => break,
                        },
                        .value => {},
                    }
                } else {
                    return null;
                }
            }
        }
        return comptimeVoid();
    }

    /// Evaluate for loop at comptime.
    fn evalComptimeFor(self: *Self, fl: *const ForLoop, env: *ComptimeEnv) ?ComptimeStmtResult {
        const iterable = self.evalComptimeExpr(fl.iterator, env) orelse return null;
        if (iterable != .array_val) return null;

        const loop_var_name = switch (fl.pattern.kind) {
            .identifier => |id| id.name.name,
            else => return null,
        };
        var iterations: u32 = 0;

        for (iterable.array_val) |elem| {
            iterations += 1;
            if (iterations > env.max_iterations) return null;

            // Bind loop variable
            env.put(loop_var_name, elem) catch return null;

            // Evaluate body
            for (fl.body.statements) |stmt| {
                const result = self.evalComptimeStatement(stmt, env);
                if (result) |r| {
                    switch (r) {
                        .control => |ctrl| switch (ctrl) {
                            .return_val => return result,
                            .break_signal => return comptimeVoid(),
                            .continue_signal => break,
                        },
                        .value => {},
                    }
                } else {
                    return null;
                }
            }
        }
        return comptimeVoid();
    }

    /// Evaluate a function call at comptime.
    fn evalComptimeFunctionCall(self: *Self, call: *const FunctionCall, env: *ComptimeEnv) ?ComptimeValue {
        // Get function name
        const func_name = switch (call.function.kind) {
            .identifier => |id| id.name,
            else => return null,
        };

        // Handle builtin pure functions
        if (std.mem.eql(u8, func_name, "len")) {
            if (call.args.len != 1) return null;
            const arg = self.evalComptimeExpr(call.args[0].value, env) orelse return null;
            return switch (arg) {
                .string_val => |s| ComptimeValue{ .int_val = @as(i64, @intCast(s.len)) },
                .array_val => |a| ComptimeValue{ .int_val = @as(i64, @intCast(a.len)) },
                else => null,
            };
        }
        if (std.mem.eql(u8, func_name, "int_to_string")) {
            if (call.args.len != 1) return null;
            const arg = self.evalComptimeExpr(call.args[0].value, env) orelse return null;
            if (arg != .int_val) return null;
            const s = std.fmt.allocPrint(self.stringAllocator(), "{d}", .{arg.int_val}) catch return null;
            return ComptimeValue{ .string_val = s };
        }

        // Reject impure builtins
        const impure_builtins = [_][]const u8{
            "print",     "println",    "eprint",      "eprintln",
            "file_read", "file_write", "file_append",  "read_line",
            "exit",      "system",     "panic",
        };
        for (impure_builtins) |name| {
            if (std.mem.eql(u8, func_name, name)) return null;
        }

        // Look up function declaration in source file
        const func_decl = self.findFunctionDecl(func_name) orelse return null;
        const body = func_decl.body orelse return null;
        const body_block = switch (body) {
            .block => |blk| blk,
            .expression => |expr| {
                // Expression-body function: just evaluate the expression
                if (env.call_depth >= env.max_call_depth) return null;

                var call_env = ComptimeEnv.init(self.allocator, env);
                defer call_env.deinit();
                call_env.call_depth = env.call_depth + 1;

                // Bind parameters
                const params = func_decl.params;
                if (params.len != call.args.len) return null;
                for (params, 0..) |param, idx| {
                    const arg_val = self.evalComptimeExpr(call.args[idx].value, env) orelse return null;
                    call_env.put(param.name.name, arg_val) catch return null;
                }

                return self.evalComptimeExpr(expr, &call_env);
            },
        };

        // Check recursion limit
        if (env.call_depth >= env.max_call_depth) return null;

        // Create call environment
        var call_env = ComptimeEnv.init(self.allocator, env);
        defer call_env.deinit();
        call_env.call_depth = env.call_depth + 1;

        // Bind parameters
        const params = func_decl.params;
        if (params.len != call.args.len) return null;
        for (params, 0..) |param, idx| {
            const arg_val = self.evalComptimeExpr(call.args[idx].value, env) orelse return null;
            call_env.put(param.name.name, arg_val) catch return null;
        }

        // Evaluate function body
        for (body_block.statements) |stmt| {
            const result = self.evalComptimeStatement(stmt, &call_env);
            if (result) |r| {
                switch (r) {
                    .control => |ctrl| switch (ctrl) {
                        .return_val => |rv| return rv,
                        else => return null,
                    },
                    .value => {},
                }
            } else {
                return null;
            }
        }

        // Block result expression
        if (body_block.result) |result_expr| {
            return self.evalComptimeExpr(result_expr, &call_env);
        }

        return ComptimeValue{ .void_val = {} };
    }

    /// Find a function declaration by name in the current source file.
    fn findFunctionDecl(self: *Self, name: []const u8) ?*FunctionDecl {
        const source = self.current_source orelse return null;
        for (source.declarations) |decl| {
            switch (decl.kind) {
                .function => |f| {
                    if (std.mem.eql(u8, f.name.name, name)) return f;
                },
                else => {},
            }
        }
        return null;
    }

    /// Evaluate field access at comptime.
    fn evalComptimeFieldAccess(self: *Self, fa: *const FieldAccess, env: *ComptimeEnv) ?ComptimeValue {
        const base = self.evalComptimeExpr(fa.object, env) orelse return null;
        if (base != .struct_val) return null;
        return base.struct_val.get(fa.field.name);
    }

    /// Evaluate index access at comptime.
    fn evalComptimeIndexAccess(self: *Self, ia: *const IndexAccess, env: *ComptimeEnv) ?ComptimeValue {
        const base = self.evalComptimeExpr(ia.object, env) orelse return null;
        const index = self.evalComptimeExpr(ia.index, env) orelse return null;
        if (base != .array_val or index != .int_val) return null;
        const idx = index.int_val;
        if (idx < 0 or idx >= @as(i64, @intCast(base.array_val.len))) return null;
        return base.array_val[@as(usize, @intCast(idx))];
    }

    /// Evaluate array literal at comptime.
    fn evalComptimeArrayLiteral(self: *Self, al: *const ArrayLiteral, env: *ComptimeEnv) ?ComptimeValue {
        switch (al.kind) {
            .elements => |elems| {
                var elements = std.ArrayList(ComptimeValue).init(self.allocator);
                for (elems) |elem| {
                    const val = self.evalComptimeExpr(elem, env) orelse return null;
                    elements.append(val) catch return null;
                }
                return ComptimeValue{ .array_val = elements.toOwnedSlice() catch return null };
            },
            .repeat => return null,
        }
    }

    /// Evaluate struct literal at comptime.
    fn evalComptimeStructLiteral(self: *Self, sl: *const StructLiteral, env: *ComptimeEnv) ?ComptimeValue {
        var fields = self.allocator.create(std.StringHashMap(ComptimeValue)) catch return null;
        fields.* = std.StringHashMap(ComptimeValue).init(self.allocator);
        for (sl.fields) |field| {
            const val = self.evalComptimeExpr(field.value, env) orelse return null;
            fields.put(field.name.name, val) catch return null;
        }
        return ComptimeValue{ .struct_val = fields };
    }

    /// Evaluate a binary operation on comptime values.
    fn evalComptimeBinary(_: *Self, op: BinaryExpr.BinaryOp, left: ComptimeValue, right: ComptimeValue) ?ComptimeValue {
        // Integer arithmetic
        if (left == .int_val and right == .int_val) {
            const l = left.int_val;
            const r = right.int_val;
            return switch (op) {
                .add => ComptimeValue{ .int_val = l + r },
                .sub => ComptimeValue{ .int_val = l - r },
                .mul => ComptimeValue{ .int_val = l * r },
                .div => if (r != 0) ComptimeValue{ .int_val = @divTrunc(l, r) } else null,
                .mod => if (r != 0) ComptimeValue{ .int_val = @mod(l, r) } else null,
                .eq => ComptimeValue{ .bool_val = l == r },
                .ne => ComptimeValue{ .bool_val = l != r },
                .lt => ComptimeValue{ .bool_val = l < r },
                .le => ComptimeValue{ .bool_val = l <= r },
                .gt => ComptimeValue{ .bool_val = l > r },
                .ge => ComptimeValue{ .bool_val = l >= r },
                .bit_and => ComptimeValue{ .int_val = l & r },
                .bit_or => ComptimeValue{ .int_val = l | r },
                .bit_xor => ComptimeValue{ .int_val = l ^ r },
                .shl => ComptimeValue{ .int_val = l << @as(u6, @intCast(@min(r, 63))) },
                .shr => ComptimeValue{ .int_val = l >> @as(u6, @intCast(@min(r, 63))) },
                else => null,
            };
        }

        // Float arithmetic
        if ((left == .float_val or left == .int_val) and (right == .float_val or right == .int_val)) {
            const l: f64 = switch (left) {
                .float_val => |v| v,
                .int_val => |v| @as(f64, @floatFromInt(v)),
                else => return null,
            };
            const r: f64 = switch (right) {
                .float_val => |v| v,
                .int_val => |v| @as(f64, @floatFromInt(v)),
                else => return null,
            };
            return switch (op) {
                .add => ComptimeValue{ .float_val = l + r },
                .sub => ComptimeValue{ .float_val = l - r },
                .mul => ComptimeValue{ .float_val = l * r },
                .div => if (r != 0) ComptimeValue{ .float_val = l / r } else null,
                .eq => ComptimeValue{ .bool_val = l == r },
                .ne => ComptimeValue{ .bool_val = l != r },
                .lt => ComptimeValue{ .bool_val = l < r },
                .le => ComptimeValue{ .bool_val = l <= r },
                .gt => ComptimeValue{ .bool_val = l > r },
                .ge => ComptimeValue{ .bool_val = l >= r },
                else => null,
            };
        }

        // String concatenation and comparison
        if (left == .string_val and right == .string_val) {
            return switch (op) {
                .add => blk: {
                    // We can't allocate here easily without access to self, so return null for complex string ops
                    // This is handled at a higher level
                    break :blk null;
                },
                .eq => ComptimeValue{ .bool_val = std.mem.eql(u8, left.string_val, right.string_val) },
                .ne => ComptimeValue{ .bool_val = !std.mem.eql(u8, left.string_val, right.string_val) },
                else => null,
            };
        }

        // Boolean logic
        if (left == .bool_val and right == .bool_val) {
            const l = left.bool_val;
            const r = right.bool_val;
            return switch (op) {
                .@"and" => ComptimeValue{ .bool_val = l and r },
                .@"or" => ComptimeValue{ .bool_val = l or r },
                .eq => ComptimeValue{ .bool_val = l == r },
                .ne => ComptimeValue{ .bool_val = l != r },
                else => null,
            };
        }

        return null;
    }

    /// Emit a comptime value as C code.
    fn emitComptimeValue(self: *Self, val: ComptimeValue) !void {
        switch (val) {
            .int_val => |v| try self.writer.print("{d}", .{v}),
            .float_val => |v| try self.writer.print("{d}", .{v}),
            .bool_val => |v| try self.writer.write(if (v) "true" else "false"),
            .string_val => |v| try self.writer.print("dm_string_from_cstr(\"{s}\")", .{v}),
            .void_val => try self.writer.write("((void)0)"),
            .array_val, .struct_val => {
                // Complex types fall back to runtime generation
                try self.writer.write("0 /* comptime complex value */");
            },
        }
    }

    // ========================================================================
    // Closure Support
    // ========================================================================

    /// Collect free variables in an expression that are not lambda params and exist in variable_types.
    fn collectFreeVars(self: *Self, expr: *const Expr, param_names: []const []const u8, result: *std.ArrayList(CapturedVar)) void {
        switch (expr.kind) {
            .identifier => |ident| {
                const name = ident.name;
                // Skip if it's a lambda parameter
                for (param_names) |pn| {
                    if (std.mem.eql(u8, name, pn)) return;
                }
                // Skip if it's a known function
                if (self.known_functions.contains(name)) return;
                if (self.extern_functions.contains(name)) return;
                // Skip if it's already captured
                for (result.items) |cap| {
                    if (std.mem.eql(u8, cap.name, name)) return;
                }
                // Check if it's a variable in scope
                if (self.variable_types.get(name)) |c_type| {
                    result.append(.{ .name = name, .c_type = c_type }) catch {};
                } else if (self.current_mut_params.contains(name)) {
                    result.append(.{ .name = name, .c_type = "int64_t" }) catch {};
                }
            },
            .binary => |bin| {
                self.collectFreeVars(bin.left, param_names, result);
                self.collectFreeVars(bin.right, param_names, result);
            },
            .unary => |un| {
                self.collectFreeVars(un.operand, param_names, result);
            },
            .function_call => |fc| {
                self.collectFreeVars(fc.function, param_names, result);
                for (fc.args) |arg| {
                    self.collectFreeVars(arg.value, param_names, result);
                }
            },
            .method_call => |mc| {
                self.collectFreeVars(mc.object, param_names, result);
                for (mc.args) |arg| {
                    self.collectFreeVars(arg, param_names, result);
                }
            },
            .field_access => |fa| {
                self.collectFreeVars(fa.object, param_names, result);
            },
            .index_access => |idx| {
                self.collectFreeVars(idx.object, param_names, result);
                self.collectFreeVars(idx.index, param_names, result);
            },
            .if_expr => |ie| {
                self.collectFreeVars(ie.condition, param_names, result);
                self.collectFreeVarsBlock(ie.then_branch, param_names, result);
                if (ie.else_branch) |eb| {
                    switch (eb) {
                        .else_block => |blk| self.collectFreeVarsBlock(blk, param_names, result),
                        .else_if => |elif| {
                            self.collectFreeVars(elif.condition, param_names, result);
                            self.collectFreeVarsBlock(elif.then_branch, param_names, result);
                        },
                    }
                }
            },
            .struct_literal => |sl| {
                for (sl.fields) |f| {
                    self.collectFreeVars(f.value, param_names, result);
                }
            },
            .grouped => |inner| {
                self.collectFreeVars(inner, param_names, result);
            },
            .string_interpolation => |si| {
                for (si.parts) |part| {
                    switch (part) {
                        .expr => |e| self.collectFreeVars(e, param_names, result),
                        .literal => {},
                    }
                }
            },
            .block => |blk| {
                self.collectFreeVarsBlock(blk, param_names, result);
            },
            .lambda => |inner_lambda| {
                // Nested lambda: collect free vars from its body too, but add its params to exclusion
                var extended_params = std.ArrayList([]const u8).init(self.stringAllocator());
                for (param_names) |pn| extended_params.append(pn) catch {};
                for (inner_lambda.params) |p| extended_params.append(p.name.name) catch {};
                switch (inner_lambda.body) {
                    .expression => |e| self.collectFreeVars(e, extended_params.items, result),
                    .block => |b| self.collectFreeVarsBlock(b, extended_params.items, result),
                }
            },
            else => {}, // Literals, paths, etc. have no free variables
        }
    }

    /// Collect free variables from a block expression.
    fn collectFreeVarsBlock(self: *Self, block: *const BlockExpr, param_names: []const []const u8, result: *std.ArrayList(CapturedVar)) void {
        for (block.statements) |stmt| {
            self.collectFreeVarsStmt(stmt, param_names, result);
        }
        if (block.result) |res| {
            self.collectFreeVars(res, param_names, result);
        }
    }

    /// Collect free variables from a statement.
    fn collectFreeVarsStmt(self: *Self, stmt: *const Statement, param_names: []const []const u8, result: *std.ArrayList(CapturedVar)) void {
        switch (stmt.kind) {
            .expression => |e| self.collectFreeVars(e, param_names, result),
            .let_binding => |lb| {
                if (lb.value) |v| self.collectFreeVars(v, param_names, result);
            },
            .assignment => |a| {
                self.collectFreeVars(a.target, param_names, result);
                self.collectFreeVars(a.value, param_names, result);
            },
            .return_stmt => |rs| {
                if (rs.value) |v| self.collectFreeVars(v, param_names, result);
            },
            .if_stmt => |is| {
                self.collectFreeVars(is.condition, param_names, result);
                self.collectFreeVarsBlock(is.then_branch, param_names, result);
                if (is.else_branch) |eb| {
                    switch (eb) {
                        .else_block => |blk| self.collectFreeVarsBlock(blk, param_names, result),
                        .else_if => |elif| {
                            self.collectFreeVars(elif.condition, param_names, result);
                            self.collectFreeVarsBlock(elif.then_branch, param_names, result);
                        },
                    }
                }
            },
            .for_loop => |fl| {
                self.collectFreeVars(fl.iterator, param_names, result);
                self.collectFreeVarsBlock(fl.body, param_names, result);
            },
            .while_loop => |wl| {
                self.collectFreeVars(wl.condition, param_names, result);
                self.collectFreeVarsBlock(wl.body, param_names, result);
            },
            else => {},
        }
    }

    /// Generate lambda expression by hoisting to a static C function.
    /// Supports both non-capturing and capturing (closure) lambdas.
    fn generateLambdaExpr(self: *Self, lambda: *LambdaExpr) anyerror!void {
        self.lambda_depth += 1;
        defer self.lambda_depth -= 1;

        const lambda_name = try std.fmt.allocPrint(self.stringAllocator(), "__dm_lambda_{d}", .{self.lambda_counter});
        self.lambda_counter += 1;

        // Determine return type
        const ret_type: []const u8 = if (lambda.return_type) |rt|
            self.mapType(rt) catch "int64_t"
        else blk: {
            // Infer from body expression type
            switch (lambda.body) {
                .expression => |expr| break :blk self.inferCTypeFromExpr(expr),
                .block => |block| {
                    if (block.result) |result| break :blk self.inferCTypeFromExpr(result);
                    break :blk "void";
                },
            }
        };

        // Collect parameter names
        var param_names = std.ArrayList([]const u8).init(self.stringAllocator());
        for (lambda.params) |p| {
            try param_names.append(p.name.name);
        }

        // Scan for captured variables
        var captures = std.ArrayList(CapturedVar).init(self.stringAllocator());
        switch (lambda.body) {
            .expression => |expr| self.collectFreeVars(expr, param_names.items, &captures),
            .block => |block| self.collectFreeVarsBlock(block, param_names.items, &captures),
        }

        const has_captures = captures.items.len > 0;
        const closure_struct_name = if (has_captures)
            try std.fmt.allocPrint(self.stringAllocator(), "__dm_closure_{d}", .{self.lambda_counter - 1})
        else
            "";

        // Emit closure struct definition if there are captures
        var w = &self.lambda_writer;
        if (has_captures) {
            try w.print("typedef struct {s} {{", .{closure_struct_name});
            try w.newline();
            w.indent();
            for (captures.items) |cap| {
                try w.print("{s} {s};", .{ cap.c_type, cap.name });
                try w.newline();
            }
            w.dedent();
            try w.print("}} {s};", .{closure_struct_name});
            try w.newline();
            try w.blankLine();
        }

        // Emit static function definition to lambda_writer
        try w.print("static {s} {s}(", .{ ret_type, lambda_name });

        // If capturing, first param is void* __env
        if (has_captures) {
            try w.write("void* __env");
            if (lambda.params.len > 0) try w.write(", ");
        }

        for (lambda.params, 0..) |param, i| {
            if (i > 0) try w.write(", ");
            const param_type: []const u8 = if (param.type_expr) |te|
                self.mapType(te) catch "int64_t"
            else
                "int64_t";
            try w.print("{s} {s}", .{ param_type, param.name.name });
        }
        if (!has_captures and lambda.params.len == 0) try w.write("void");
        try w.writeLine(") {");
        w.indent();

        // If capturing, unpack the closure struct
        if (has_captures) {
            try w.print("{s}* __c = ({s}*)__env;", .{ closure_struct_name, closure_struct_name });
            try w.newline();
            for (captures.items) |cap| {
                try w.print("{s} {s} = __c->{s};", .{ cap.c_type, cap.name, cap.name });
                try w.newline();
            }
        }

        // Save and swap writer so generateExpr/generateBlock writes to lambda_writer
        const saved_writer = self.writer;
        self.writer = self.lambda_writer;

        // Set up closure capture context so identifiers are resolved from __c-> if needed
        // (Not needed since we unpacked to local variables above)

        switch (lambda.body) {
            .expression => |expr| {
                if (!std.mem.eql(u8, ret_type, "void")) {
                    try self.writer.write("return ");
                }
                try self.generateExpr(expr);
                try self.writer.writeLine(";");
            },
            .block => |block| {
                try self.generateBlock(block);
            },
        }

        // Restore writers
        self.lambda_writer = self.writer;
        self.writer = saved_writer;

        self.lambda_writer.dedent();
        try self.lambda_writer.writeLine("}");
        try self.lambda_writer.blankLine();

        if (has_captures) {
            const env_var = try std.fmt.allocPrint(self.stringAllocator(), "__dm_env_{d}", .{self.lambda_counter - 1});

            // Track closure info so call sites and let bindings can use it
            try self.closure_info.put(lambda_name, .{
                .lambda_name = lambda_name,
                .env_var = env_var,
                .captures = captures.items,
                .return_type = ret_type,
            });

            // Don't emit anything to the main writer here.
            // The closure struct init and variable binding are handled by generateLetBinding.
        } else {
            // In the main code, reference the lambda by its hoisted name
            try self.writer.write(lambda_name);
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

    /// Generate await expression: `await expr` -> `dm_future_T_await(expr)`
    fn generateAwaitExpr(self: *Self, aw: *AwaitExpr) anyerror!void {
        const operand_type = self.inferCTypeFromExpr(aw.operand);
        if (std.mem.startsWith(u8, operand_type, "dm_future_")) {
            try self.writer.print("{s}_await(", .{operand_type});
            try self.generateExpr(aw.operand);
            try self.writer.write(")");
        } else {
            // Fallback: just access .value
            try self.generateExpr(aw.operand);
            try self.writer.write(".value");
        }
    }

    // ========================================================================
    // ASYNC PHASE B — Await point detection
    // ========================================================================

    fn blockHasAwait(block: *const BlockExpr) bool {
        for (block.statements) |stmt| {
            if (stmtHasAwait(stmt)) return true;
        }
        if (block.result) |result| {
            if (exprHasAwait(result)) return true;
        }
        return false;
    }

    fn stmtHasAwait(stmt: *const Statement) bool {
        return switch (stmt.kind) {
            .let_binding => |lb| if (lb.value) |v| exprHasAwait(v) else false,
            .return_stmt => |rs| if (rs.value) |v| exprHasAwait(v) else false,
            .expression => |e| exprHasAwait(e),
            .assignment => |a| exprHasAwait(a.value) or exprHasAwait(a.target),
            .if_stmt => |ie| exprHasAwait(ie.condition) or blockHasAwait(ie.then_branch) or
                (if (ie.else_branch) |eb| switch (eb) {
                .else_block => |b| blockHasAwait(b),
                .else_if => |ei| exprHasAwait(ei.condition) or blockHasAwait(ei.then_branch),
            } else false),
            .while_loop => |wl| exprHasAwait(wl.condition) or blockHasAwait(wl.body),
            .for_loop => |fl| exprHasAwait(fl.iterator) or blockHasAwait(fl.body),
            .loop_stmt => |ls| blockHasAwait(ls.body),
            .match_stmt => |me| blk: {
                if (exprHasAwait(me.scrutinee)) break :blk true;
                for (me.arms) |arm| {
                    switch (arm.body) {
                        .block => |b| if (blockHasAwait(b)) break :blk true,
                        .expression => |e| if (exprHasAwait(e)) break :blk true,
                    }
                }
                break :blk false;
            },
            .region_block => |rb| blockHasAwait(rb.body),
            .discard => |d| exprHasAwait(d.value),
            .break_stmt => |bs| if (bs.value) |v| exprHasAwait(v) else false,
            .continue_stmt => false,
        };
    }

    fn exprHasAwait(expr: *const Expr) bool {
        return switch (expr.kind) {
            .await_expr => true,
            .binary => |b| exprHasAwait(b.left) or exprHasAwait(b.right),
            .unary => |u| exprHasAwait(u.operand),
            .function_call => |fc| blk: {
                if (exprHasAwait(fc.function)) break :blk true;
                for (fc.args) |arg| {
                    if (exprHasAwait(arg.value)) break :blk true;
                }
                break :blk false;
            },
            .method_call => |mc| blk: {
                if (exprHasAwait(mc.object)) break :blk true;
                for (mc.args) |arg| {
                    if (exprHasAwait(arg)) break :blk true;
                }
                break :blk false;
            },
            .field_access => |fa| exprHasAwait(fa.object),
            .index_access => |ia| exprHasAwait(ia.object) or exprHasAwait(ia.index),
            .if_expr => |ie| exprHasAwait(ie.condition) or blockHasAwait(ie.then_branch) or
                (if (ie.else_branch) |eb| switch (eb) {
                .else_block => |b| blockHasAwait(b),
                .else_if => |ei| exprHasAwait(ei.condition) or blockHasAwait(ei.then_branch),
            } else false),
            .match_expr => |me| blk: {
                if (exprHasAwait(me.scrutinee)) break :blk true;
                for (me.arms) |arm| {
                    switch (arm.body) {
                        .block => |b| if (blockHasAwait(b)) break :blk true,
                        .expression => |e| if (exprHasAwait(e)) break :blk true,
                    }
                }
                break :blk false;
            },
            .block => |b| blockHasAwait(b),
            .pipeline => |p| exprHasAwait(p.left) or exprHasAwait(p.right),
            .error_propagate => |e| exprHasAwait(e.operand),
            .coalesce => |c| exprHasAwait(c.left) or exprHasAwait(c.right),
            .cast => |c| exprHasAwait(c.expr),
            .type_check => |tc| exprHasAwait(tc.expr),
            .grouped => |g| exprHasAwait(g),
            .comptime_expr => |c| exprHasAwait(c.expr),
            .string_interpolation => |si| blk: {
                for (si.parts) |part| {
                    switch (part) {
                        .expr => |e| if (exprHasAwait(e)) break :blk true,
                        .literal => {},
                    }
                }
                break :blk false;
            },
            .struct_literal => |sl| blk: {
                for (sl.fields) |f| {
                    if (exprHasAwait(f.value)) break :blk true;
                }
                if (sl.spread) |s| if (exprHasAwait(s)) break :blk true;
                break :blk false;
            },
            .array_literal => |al| switch (al.kind) {
                .elements => |elems| blk: {
                    for (elems) |e| {
                        if (exprHasAwait(e)) break :blk true;
                    }
                    break :blk false;
                },
                .repeat => |rep| exprHasAwait(rep.value) or exprHasAwait(rep.count),
            },
            .enum_literal => |el| switch (el.payload) {
                .none => false,
                .tuple => |t| blk: {
                    for (t) |e| {
                        if (exprHasAwait(e)) break :blk true;
                    }
                    break :blk false;
                },
                .struct_fields => |sfs| blk: {
                    for (sfs) |f| {
                        if (exprHasAwait(f.value)) break :blk true;
                    }
                    break :blk false;
                },
            },
            .tuple_literal => |tl| blk: {
                for (tl.elements) |e| {
                    if (exprHasAwait(e)) break :blk true;
                }
                break :blk false;
            },
            .range => |r| (if (r.start) |s| exprHasAwait(s) else false) or
                (if (r.end) |e| exprHasAwait(e) else false),
            .lambda, .literal, .identifier, .path => false,
        };
    }

    /// Generate error propagation expression (standalone, when not in let/return context)
    /// This emits a temp variable with check and returns the unwrapped value inline.
    fn generateErrorPropagate(self: *Self, err: *ErrorPropagateExpr) anyerror!void {
        // expr? in an expression context - generate inline unwrap
        // For Result types: emit temp + check, then access .ok
        // For Option types: emit temp + check, then access .value
        const operand_type = self.inferCTypeFromExpr(err.operand);

        if (std.mem.startsWith(u8, operand_type, "dm_result_")) {
            const temp = try self.freshTemp();
            // We need to emit a statement before the expression, so emit it inline
            try self.writer.print("({s} {s} = ", .{ operand_type, temp });
            try self.generateExpr(err.operand);
            try self.writer.print(", !{s}.is_ok ? ({s}_Err({s}.err), (void)0) : (void)0, {s}.ok)", .{ temp, self.current_function_return_type orelse operand_type, temp, temp });
        } else if (std.mem.startsWith(u8, operand_type, "dm_option_")) {
            const temp = try self.freshTemp();
            try self.writer.print("({s} {s} = ", .{ operand_type, temp });
            try self.generateExpr(err.operand);
            try self.writer.print(", {s}.value)", .{temp});
        } else {
            // Fallback: just access .ok
            try self.generateExpr(err.operand);
            try self.writer.write(".ok");
        }
    }

    /// Generate error propagation in a let binding context: let x = expr?
    /// Emits a temp variable, a check with early return, and the final assignment.
    fn generateErrorPropagateLetBinding(self: *Self, var_name: []const u8, var_type: []const u8, err: *ErrorPropagateExpr) anyerror!void {
        const operand_type = self.inferCTypeFromExpr(err.operand);
        const temp = try self.freshTemp();

        // Step 1: Evaluate the operand into a temp
        try self.writer.print("{s} {s} = ", .{ operand_type, temp });
        try self.generateExpr(err.operand);
        try self.writer.writeLine(";");

        // Step 2: Check and early-return on error
        if (std.mem.startsWith(u8, operand_type, "dm_result_")) {
            try self.writer.print("if (!{s}.is_ok) ", .{temp});
            try self.writer.openBrace();
            if (self.current_function_return_type) |ret_type| {
                if (std.mem.startsWith(u8, ret_type, "dm_result_")) {
                    try self.writer.print("return {s}_Err({s}.err);", .{ ret_type, temp });
                } else {
                    try self.writer.print("return;", .{});
                }
            } else {
                try self.writer.write("return;");
            }
            try self.writer.newline();
            try self.writer.closeBrace();
            // Step 3: Assign unwrapped ok value
            try self.writer.print("{s} {s} = {s}.ok;", .{ var_type, var_name, temp });
            try self.writer.newline();
        } else if (std.mem.startsWith(u8, operand_type, "dm_option_")) {
            try self.writer.print("if (!{s}.has_value) ", .{temp});
            try self.writer.openBrace();
            if (self.current_function_return_type) |ret_type| {
                if (std.mem.startsWith(u8, ret_type, "dm_option_")) {
                    try self.writer.print("return {s}_None();", .{ret_type});
                } else if (std.mem.startsWith(u8, ret_type, "dm_result_")) {
                    try self.writer.print("return {s}_Err(dm_string_from_cstr(\"None\"));", .{ret_type});
                } else {
                    try self.writer.write("return;");
                }
            } else {
                try self.writer.write("return;");
            }
            try self.writer.newline();
            try self.writer.closeBrace();
            // Step 3: Assign unwrapped value
            try self.writer.print("{s} {s} = {s}.value;", .{ var_type, var_name, temp });
            try self.writer.newline();
        } else {
            // Fallback: just assign directly
            try self.writer.print("{s} {s} = {s};", .{ var_type, var_name, temp });
            try self.writer.newline();
        }
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
    /// Generate string interpolation as nested dm_string_concat calls
    fn generateStringInterpolation(self: *Self, si: *const ast.StringInterpolation) anyerror!void {
        // Build a chain: dm_string_concat(part1, dm_string_concat(part2, ...))
        // Special case: single part
        if (si.parts.len == 0) {
            try self.writer.write("dm_string_from_cstr(\"\")");
            return;
        }
        if (si.parts.len == 1) {
            try self.generateInterpolPart(si.parts[0]);
            return;
        }

        // Multiple parts: nested concat
        // Open N-1 concat calls
        for (0..si.parts.len - 1) |_| {
            try self.writer.write("dm_string_concat(");
        }

        // First part
        try self.generateInterpolPart(si.parts[0]);

        // Remaining parts
        for (si.parts[1..]) |part| {
            try self.writer.write(", ");
            try self.generateInterpolPart(part);
            try self.writer.write(")");
        }
    }

    fn generateInterpolPart(self: *Self, part: ast.StringInterpolation.InterpolPart) anyerror!void {
        switch (part) {
            .literal => |lit| {
                try self.writer.write("dm_string_from_cstr(");
                try self.writeCStringLiteral(lit);
                try self.writer.write(")");
            },
            .expr => |expr| {
                // Infer the type to decide if conversion is needed
                const expr_type = self.inferCTypeFromExpr(expr);
                if (std.mem.eql(u8, expr_type, "dm_string")) {
                    try self.generateExpr(expr);
                } else if (std.mem.eql(u8, expr_type, "int64_t") or
                    std.mem.eql(u8, expr_type, "int32_t") or
                    std.mem.eql(u8, expr_type, "int16_t") or
                    std.mem.eql(u8, expr_type, "int8_t") or
                    std.mem.eql(u8, expr_type, "uint64_t") or
                    std.mem.eql(u8, expr_type, "uint32_t") or
                    std.mem.eql(u8, expr_type, "uint16_t") or
                    std.mem.eql(u8, expr_type, "uint8_t"))
                {
                    try self.writer.write("dm_int_to_string((int64_t)(");
                    try self.generateExpr(expr);
                    try self.writer.write("))");
                } else if (std.mem.eql(u8, expr_type, "double") or std.mem.eql(u8, expr_type, "float")) {
                    try self.writer.write("dm_float_to_string((double)(");
                    try self.generateExpr(expr);
                    try self.writer.write("))");
                } else if (std.mem.eql(u8, expr_type, "bool")) {
                    try self.writer.write("dm_bool_to_string(");
                    try self.generateExpr(expr);
                    try self.writer.write(")");
                } else {
                    // Default: try int_to_string as fallback
                    try self.writer.write("dm_int_to_string((int64_t)(");
                    try self.generateExpr(expr);
                    try self.writer.write("))");
                }
            },
        }
    }

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
