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
        return;
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
        return;
    }

    // Phase 2: Parse
    if (verbose) std.debug.print("  [2/6] Parsing...\n", .{});
    var parser = frontend.Parser.init(tokens, allocator);
    defer parser.deinit();
    const ast_result = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return;
    };

    if (parser.hasErrors()) {
        std.debug.print("Parse errors found\n", .{});
        return;
    }

    // Phase 3: Type check
    if (verbose) std.debug.print("  [3/6] Type checking...\n", .{});
    var type_checker = frontend.TypeChecker.init(allocator) catch |err| {
        std.debug.print("Error initializing type checker: {}\n", .{err});
        return;
    };
    defer type_checker.deinit();

    type_checker.setSource(source);
    type_checker.setSourceFile(input_file);

    type_checker.checkSourceFile(ast_result) catch |err| {
        std.debug.print("Type check error: {}\n", .{err});
        return;
    };

    if (type_checker.hasErrors()) {
        std.debug.print("Type errors found\n", .{});
        return;
    }

    // Phase 4: IR Generation
    if (verbose) std.debug.print("  [4/6] Generating IR...\n", .{});
    var ir_generator = ir_gen.IRGenerator.init(allocator);
    defer ir_generator.deinit();

    ir_generator.generate(ast_result) catch |err| {
        std.debug.print("IR generation error: {}\n", .{err});
        return;
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
        return;
    };

    gen.module.verify() catch |err| {
        std.debug.print("LLVM verification error: {}\n", .{err});
        return;
    };

    // Print LLVM IR if requested
    if (emit_llvm) {
        const ir_str = gen.module.printToString();
        std.debug.print("\nGenerated LLVM IR:\n{s}\n", .{std.mem.span(ir_str)});
    }

    // Phase 6: Emit and link
    if (verbose) std.debug.print("  [6/6] Emitting binary...\n", .{});

    const tm = llvm.TargetMachine.create(triple, opt_level) catch |err| {
        std.debug.print("Error creating target machine: {}\n", .{err});
        return;
    };
    defer tm.dispose();

    tm.emitToFile(gen.module, "/tmp/daimond_out.o", .object) catch |err| {
        std.debug.print("Error emitting object file: {}\n", .{err});
        return;
    };

    // Compile runtime and ABI wrappers
    const runtime_obj = compileRuntime(allocator) catch |err| {
        std.debug.print("Error compiling runtime: {}\n", .{err});
        return;
    };
    const wrappers_obj = compileWrappers(allocator) catch |err| {
        std.debug.print("Error compiling ABI wrappers: {}\n", .{err});
        return;
    };

    // Link with runtime, wrappers, and system libraries
    var link_argv = std.ArrayList([]const u8).init(allocator);
    defer link_argv.deinit();
    try link_argv.append("cc");
    try link_argv.append("/tmp/daimond_out.o");
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
        return;
    };

    if (link_result.term.Exited != 0) {
        std.debug.print("Linker failed:\n{s}\n", .{link_result.stderr});
        return;
    }

    std.debug.print("Successfully compiled: {s}\n", .{output_file});
}

/// Compile the LLVM ABI wrapper functions to an object file
fn compileWrappers(allocator: std.mem.Allocator) !?[]const u8 {
    const wrappers_c = "../stage3/runtime/llvm_wrappers.c";
    const wrappers_o = "/tmp/daimond_wrappers.o";

    std.fs.cwd().access(wrappers_c, .{}) catch return null;

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{
            "cc", "-c", "-O2",
            "-I", "../stage0/runtime",
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
fn compileRuntime(allocator: std.mem.Allocator) !?[]const u8 {
    const runtime_c = "../stage0/runtime/daimond_runtime.c";
    const runtime_h = "../stage0/runtime/daimond_runtime.h";
    const runtime_o = "/tmp/daimond_runtime.o";

    // Check if runtime source exists
    std.fs.cwd().access(runtime_c, .{}) catch return null;
    _ = std.fs.cwd().statFile(runtime_h) catch return null;

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{
            "cc", "-c", "-O2",
            "-I", "../stage0/runtime",
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
