//! dAImond Stage 3 Compiler - LLVM Backend
//!
//! Compiles dAImond source to native binaries via LLVM.
//! Reuses the Stage 0 frontend (lexer, parser, checker).

const std = @import("std");
const ir = @import("ir.zig");
const ir_gen = @import("ir_gen.zig");
const llvm_gen = @import("llvm_gen.zig");
const llvm = @import("llvm_bindings.zig");
const frontend = @import("frontend");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("dAImond Stage 3 Compiler (LLVM Backend)\n\n", .{});
        std.debug.print("Usage: daimond-llvm <file.dm> [-o output] [-O0|-O1|-O2|-O3] [--emit-ir] [--emit-llvm]\n", .{});
        return;
    }

    const input_file = args[1];
    var output_file: []const u8 = "a.out";
    var opt_level: llvm.OptLevel = .O0;
    var emit_ir = false;
    var emit_llvm = false;
    var verbose = false;

    // Parse flags
    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "-o") and i + 1 < args.len) {
            output_file = args[i + 1];
            i += 1;
        } else if (std.mem.eql(u8, args[i], "-O0")) {
            opt_level = .O0;
        } else if (std.mem.eql(u8, args[i], "-O1")) {
            opt_level = .O1;
        } else if (std.mem.eql(u8, args[i], "-O2")) {
            opt_level = .O2;
        } else if (std.mem.eql(u8, args[i], "-O3")) {
            opt_level = .O3;
        } else if (std.mem.eql(u8, args[i], "--emit-ir")) {
            emit_ir = true;
        } else if (std.mem.eql(u8, args[i], "--emit-llvm")) {
            emit_llvm = true;
        } else if (std.mem.eql(u8, args[i], "-v") or std.mem.eql(u8, args[i], "--verbose")) {
            verbose = true;
        }
    }

    std.debug.print("dAImond Stage 3 Compiler (LLVM Backend)\n", .{});
    std.debug.print("Input: {s}\n", .{input_file});
    std.debug.print("Output: {s}\n", .{output_file});

    // Initialize LLVM
    llvm.initializeAllTargets();

    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, input_file, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {}\n", .{ input_file, err });
        std.process.exit(1);
    };
    defer allocator.free(source);

    // Phase 1: Lex
    if (verbose) std.debug.print("  [1/6] Lexing...\n", .{});
    var lexer = frontend.Lexer.init(source, allocator);
    defer lexer.deinit();
    const tokens = try lexer.scanAll();
    defer allocator.free(tokens);

    if (lexer.hasErrors()) {
        std.debug.print("Lexer errors found\n", .{});
        std.process.exit(1);
    }

    // Phase 2: Parse
    if (verbose) std.debug.print("  [2/6] Parsing...\n", .{});
    var parser = frontend.Parser.init(tokens, allocator);
    defer parser.deinit();
    const ast_result = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        std.process.exit(1);
    };

    if (parser.hasErrors()) {
        std.debug.print("Parse errors found\n", .{});
        std.process.exit(1);
    }

    // Phase 2.5: Resolve imports
    const final_ast = if (ast_result.imports.len > 0) blk: {
        if (verbose) std.debug.print("  [2.5/6] Resolving imports...\n", .{});
        const input_dir = std.fs.path.dirname(input_file) orelse ".";
        break :blk resolveImports(allocator, ast_result, input_dir, verbose) catch |err| {
            std.debug.print("Import resolution error: {}\n", .{err});
            std.process.exit(1);
        };
    } else ast_result;

    // Phase 3: Type check
    if (verbose) std.debug.print("  [3/6] Type checking...\n", .{});
    var type_checker = frontend.TypeChecker.init(allocator) catch |err| {
        std.debug.print("Error initializing type checker: {}\n", .{err});
        std.process.exit(1);
    };
    defer type_checker.deinit();

    type_checker.setSource(source);
    type_checker.setSourceFile(input_file);

    type_checker.checkSourceFile(final_ast) catch |err| {
        std.debug.print("Type check error: {}\n", .{err});
        std.process.exit(1);
    };

    if (type_checker.hasErrors()) {
        std.debug.print("Type errors found\n", .{});
        std.process.exit(1);
    }

    // Phase 4: IR Generation
    if (verbose) std.debug.print("  [4/6] Generating IR...\n", .{});
    var ir_generator = ir_gen.IRGenerator.init(allocator);
    defer ir_generator.deinit();

    ir_generator.generate(final_ast) catch |err| {
        std.debug.print("IR generation error: {}\n", .{err});
        std.process.exit(1);
    };

    // Print dAImond IR if requested
    if (emit_ir) {
        var ir_buf = std.ArrayList(u8).init(allocator);
        defer ir_buf.deinit();
        ir.printModule(ir_generator.getModule(), ir_buf.writer()) catch {};
        std.debug.print("\nGenerated dAImond IR:\n{s}\n", .{ir_buf.items});
    }

    // Phase 5: LLVM IR Generation
    if (verbose) std.debug.print("  [5/6] Generating LLVM IR...\n", .{});
    var gen = llvm_gen.LLVMGenerator.init(allocator, "daimond_module");
    defer gen.deinit();

    // Set target
    const triple = llvm.getDefaultTriple();
    gen.module.setTarget(triple);

    gen.generate(ir_generator.getModule()) catch |err| {
        std.debug.print("LLVM generation error: {}\n", .{err});
        std.process.exit(1);
    };

    gen.module.verify() catch |err| {
        std.debug.print("LLVM verification error: {}\n", .{err});
        std.process.exit(1);
    };

    // Phase 5b: Run optimization passes if requested
    if (opt_level != .O0) {
        if (verbose) std.debug.print("  [5b/6] Running optimization passes ({s})...\n", .{@tagName(opt_level)});

        const tm_for_passes = llvm.TargetMachine.create(llvm.getDefaultTriple(), opt_level) catch |err| {
            std.debug.print("Error creating target machine for passes: {}\n", .{err});
            std.process.exit(1);
        };
        defer tm_for_passes.dispose();

        const pass_pipeline: [*:0]const u8 = switch (opt_level) {
            .O0 => unreachable,
            .O1 => "default<O1>",
            .O2 => "default<O2>",
            .O3 => "default<O3>",
        };

        gen.module.runPasses(pass_pipeline, tm_for_passes) catch |err| {
            std.debug.print("Optimization pass error: {}\n", .{err});
            // Non-fatal: continue without optimizations
        };
    }

    // Print LLVM IR if requested (after optimization)
    if (emit_llvm) {
        const ir_str = gen.module.printToString();
        std.debug.print("\nGenerated LLVM IR:\n{s}\n", .{std.mem.span(ir_str)});
    }

    // Phase 6: Emit and link
    if (verbose) std.debug.print("  [6/6] Emitting binary...\n", .{});

    const tm = llvm.TargetMachine.create(triple, opt_level) catch |err| {
        std.debug.print("Error creating target machine: {}\n", .{err});
        std.process.exit(1);
    };
    defer tm.dispose();

    // Generate unique temp file names based on output path to avoid races
    const obj_path = try std.fmt.allocPrintZ(allocator, "{s}.o", .{output_file});
    defer allocator.free(obj_path);
    const runtime_obj_path = try std.fmt.allocPrintZ(allocator, "{s}_runtime.o", .{output_file});
    defer allocator.free(runtime_obj_path);
    const wrappers_obj_path = try std.fmt.allocPrintZ(allocator, "{s}_wrappers.o", .{output_file});
    defer allocator.free(wrappers_obj_path);

    tm.emitToFile(gen.module, obj_path, .object) catch |err| {
        std.debug.print("Error emitting object file: {}\n", .{err});
        std.process.exit(1);
    };

    // Find project root for runtime source files
    const project_root = findProjectRoot(allocator) catch |err| {
        std.debug.print("Error finding project root: {}\n", .{err});
        std.process.exit(1);
    };

    // Compile runtime and ABI wrappers
    const runtime_obj = compileRuntime(allocator, project_root, runtime_obj_path) catch |err| {
        std.debug.print("Error compiling runtime: {}\n", .{err});
        std.process.exit(1);
    };
    const wrappers_obj = compileWrappers(allocator, project_root, wrappers_obj_path) catch |err| {
        std.debug.print("Error compiling ABI wrappers: {}\n", .{err});
        std.process.exit(1);
    };

    // Link with runtime, wrappers, and system libraries
    var link_argv = std.ArrayList([]const u8).init(allocator);
    defer link_argv.deinit();
    try link_argv.append("cc");
    try link_argv.append(obj_path);
    if (runtime_obj) |rt| try link_argv.append(rt);
    if (wrappers_obj) |wr| try link_argv.append(wr);
    try link_argv.append("-o");
    try link_argv.append(output_file);
    try link_argv.append("-lm");
    const link_args = link_argv.items;

    const link_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = link_args,
    }) catch |err| {
        std.debug.print("Linker error: {}\n", .{err});
        std.process.exit(1);
    };

    if (link_result.term.Exited != 0) {
        std.debug.print("Linker failed:\n{s}\n", .{link_result.stderr});
        std.process.exit(1);
    }

    // Clean up temp object files
    std.fs.cwd().deleteFile(obj_path) catch {};
    std.fs.cwd().deleteFile(runtime_obj_path) catch {};
    std.fs.cwd().deleteFile(wrappers_obj_path) catch {};

    std.debug.print("Successfully compiled: {s}\n", .{output_file});
}

/// Find the project root by locating the directory containing stage0/ and stage3/
/// relative to the executable's location
fn findProjectRoot(allocator: std.mem.Allocator) ![]const u8 {
    // Get the directory containing the executable
    const exe_path = try std.fs.selfExeDirPathAlloc(allocator);
    defer allocator.free(exe_path);

    // Walk up from exe dir (stage3/zig-out/bin/) to find project root
    var dir = exe_path;
    var attempts: u32 = 0;
    while (attempts < 10) : (attempts += 1) {
        // Check if stage0/runtime/daimond_runtime.c exists relative to this dir
        const check_path = try std.fmt.allocPrint(allocator, "{s}/stage0/runtime/daimond_runtime.c", .{dir});
        defer allocator.free(check_path);
        std.fs.cwd().access(check_path, .{}) catch {
            // Go up one directory
            const parent = std.fs.path.dirname(dir) orelse break;
            dir = try allocator.dupe(u8, parent);
            continue;
        };
        return allocator.dupe(u8, dir);
    }

    // Fallback: try CWD-relative paths
    // Check ../stage0/ (if running from stage3/)
    std.fs.cwd().access("../stage0/runtime/daimond_runtime.c", .{}) catch {
        // Check stage0/ (if running from project root)
        std.fs.cwd().access("stage0/runtime/daimond_runtime.c", .{}) catch {
            return error.ProjectRootNotFound;
        };
        return allocator.dupe(u8, ".");
    };
    return allocator.dupe(u8, "..");
}

/// Compile the LLVM ABI wrapper functions to an object file
fn compileWrappers(allocator: std.mem.Allocator, project_root: []const u8, wrappers_o: []const u8) !?[]const u8 {
    const wrappers_c = try std.fmt.allocPrint(allocator, "{s}/stage3/runtime/llvm_wrappers.c", .{project_root});
    defer allocator.free(wrappers_c);
    const runtime_inc = try std.fmt.allocPrint(allocator, "{s}/stage0/runtime", .{project_root});
    defer allocator.free(runtime_inc);

    std.fs.cwd().access(wrappers_c, .{}) catch return null;

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{
            "cc", "-c", "-O2",
            "-I", runtime_inc,
            wrappers_c,
            "-o", wrappers_o,
        },
    });

    if (result.term.Exited != 0) {
        std.debug.print("Wrappers compilation failed:\n{s}\n", .{result.stderr});
        return null;
    }

    return wrappers_o;
}

/// Compile the dAImond C runtime to an object file
fn compileRuntime(allocator: std.mem.Allocator, project_root: []const u8, runtime_o: []const u8) !?[]const u8 {
    const runtime_c = try std.fmt.allocPrint(allocator, "{s}/stage0/runtime/daimond_runtime.c", .{project_root});
    defer allocator.free(runtime_c);
    const runtime_inc = try std.fmt.allocPrint(allocator, "{s}/stage0/runtime", .{project_root});
    defer allocator.free(runtime_inc);

    std.fs.cwd().access(runtime_c, .{}) catch return null;

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{
            "cc", "-c", "-O2",
            "-I", runtime_inc,
            runtime_c,
            "-o", runtime_o,
        },
    });

    if (result.term.Exited != 0) {
        std.debug.print("Runtime compilation failed:\n{s}\n", .{result.stderr});
        return null;
    }

    return runtime_o;
}

// ========================================================================
// Import Resolution
// ========================================================================

const ast = frontend.ast;

/// Resolve import declarations by loading, lexing, and parsing imported files,
/// then merging their declarations into a single SourceFile.
fn resolveImports(
    allocator: std.mem.Allocator,
    source_file: *ast.SourceFile,
    input_dir: []const u8,
    verbose: bool,
) !*ast.SourceFile {
    var all_decls = std.ArrayList(*ast.Declaration).init(allocator);
    var loaded_paths = std.StringHashMap(void).init(allocator);
    defer loaded_paths.deinit();

    // Recursively process imports
    try processFileImports(allocator, source_file, input_dir, &all_decls, &loaded_paths, verbose);

    // Add entry file's own declarations after imported ones
    for (source_file.declarations) |decl| {
        try all_decls.append(decl);
    }

    // Create merged source file
    const merged = try allocator.create(ast.SourceFile);
    merged.* = .{
        .module_decl = source_file.module_decl,
        .imports = &[_]*ast.ImportDecl{},
        .declarations = try all_decls.toOwnedSlice(),
        .span = source_file.span,
    };
    return merged;
}

/// Process imports from a single source file, recursively loading transitive imports.
fn processFileImports(
    allocator: std.mem.Allocator,
    source_file: *ast.SourceFile,
    file_dir: []const u8,
    all_decls: *std.ArrayList(*ast.Declaration),
    loaded_paths: *std.StringHashMap(void),
    verbose: bool,
) !void {
    for (source_file.imports) |import_decl| {
        const resolved_path = try resolveImportPath(allocator, import_decl, file_dir);

        // Skip if already loaded (handles diamond imports)
        if (loaded_paths.contains(resolved_path)) {
            allocator.free(resolved_path);
            continue;
        }
        try loaded_paths.put(resolved_path, {});

        if (verbose) std.debug.print("    Loading import: {s}\n", .{resolved_path});

        // Read the file
        const imp_source = std.fs.cwd().readFileAlloc(allocator, resolved_path, 10 * 1024 * 1024) catch |err| {
            std.debug.print("Error reading imported file '{s}': {}\n", .{ resolved_path, err });
            return error.ImportFileNotFound;
        };

        // Lex
        var lexer = frontend.Lexer.init(imp_source, allocator);
        defer lexer.deinit();
        const imp_tokens = try lexer.scanAll();
        defer allocator.free(imp_tokens);

        if (lexer.hasErrors()) {
            std.debug.print("Lexer errors in imported file '{s}'\n", .{resolved_path});
            return error.ImportLexError;
        }

        // Parse
        var imp_parser = frontend.Parser.init(imp_tokens, allocator);
        defer imp_parser.deinit();
        const imp_ast = imp_parser.parse() catch |err| {
            std.debug.print("Parse error in imported file '{s}': {}\n", .{ resolved_path, err });
            return error.ImportParseError;
        };

        if (imp_parser.hasErrors()) {
            std.debug.print("Parse errors in imported file '{s}'\n", .{resolved_path});
            return error.ImportParseError;
        }

        // Recursively process imports from the imported file
        const import_dir = std.fs.path.dirname(resolved_path) orelse ".";
        try processFileImports(allocator, imp_ast, import_dir, all_decls, loaded_paths, verbose);

        // Add declarations from imported file, filtered by selective imports
        if (import_decl.items) |items| {
            // Selective import: only add named declarations
            for (imp_ast.declarations) |decl| {
                const decl_name = getDeclName(decl) orelse continue;
                for (items) |item| {
                    if (std.mem.eql(u8, decl_name, item.name.name)) {
                        try all_decls.append(decl);
                        break;
                    }
                }
            }
        } else {
            // Import all declarations
            for (imp_ast.declarations) |decl| {
                try all_decls.append(decl);
            }
        }
    }
}

/// Resolve an import path to a filesystem path.
/// `import foo` -> `<dir>/foo.dm`
/// `import foo::bar` -> `<dir>/foo/bar.dm`
fn resolveImportPath(allocator: std.mem.Allocator, import_decl: *const ast.ImportDecl, importing_dir: []const u8) ![]const u8 {
    const segments = import_decl.path.segments;
    if (segments.len == 0) return error.EmptyImportPath;

    // Build path from segments
    var path_parts = std.ArrayList([]const u8).init(allocator);
    defer path_parts.deinit();

    try path_parts.append(importing_dir);
    for (segments) |seg| {
        try path_parts.append(seg.name);
    }

    const joined = try std.fs.path.join(allocator, path_parts.items);
    defer allocator.free(joined);

    return std.fmt.allocPrint(allocator, "{s}.dm", .{joined});
}

/// Get the name of a declaration for selective import filtering.
fn getDeclName(decl: *ast.Declaration) ?[]const u8 {
    return switch (decl.kind) {
        .function => |f| f.name.name,
        .struct_def => |s| s.name.name,
        .enum_def => |e| e.name.name,
        .trait_def => |t| t.name.name,
        .constant => |c| c.name.name,
        .impl_block => null,
    };
}
