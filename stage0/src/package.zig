//! dAImond Package Manager
//! Provides TOML manifest parsing, dependency resolution, and package management commands.
//! Manifest format: daimond.toml

const std = @import("std");
const Allocator = std.mem.Allocator;

// ============================================================================
// Package Manifest
// ============================================================================

pub const Manifest = struct {
    name: []const u8 = "unnamed",
    version: []const u8 = "0.1.0",
    description: []const u8 = "",
    author: []const u8 = "",
    license: []const u8 = "",
    dependencies: std.ArrayList(Dependency),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Manifest {
        return .{
            .dependencies = std.ArrayList(Dependency).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Manifest) void {
        self.dependencies.deinit();
    }

    /// Serialize manifest to TOML format
    pub fn toToml(self: *const Manifest, allocator: Allocator) ![]const u8 {
        var result = std.ArrayList(u8).init(allocator);
        errdefer result.deinit();

        try result.appendSlice("[package]\n");
        try result.appendSlice("name = \"");
        try result.appendSlice(self.name);
        try result.appendSlice("\"\n");
        try result.appendSlice("version = \"");
        try result.appendSlice(self.version);
        try result.appendSlice("\"\n");
        if (self.description.len > 0) {
            try result.appendSlice("description = \"");
            try result.appendSlice(self.description);
            try result.appendSlice("\"\n");
        }
        if (self.author.len > 0) {
            try result.appendSlice("author = \"");
            try result.appendSlice(self.author);
            try result.appendSlice("\"\n");
        }
        if (self.license.len > 0) {
            try result.appendSlice("license = \"");
            try result.appendSlice(self.license);
            try result.appendSlice("\"\n");
        }

        if (self.dependencies.items.len > 0) {
            try result.appendSlice("\n[dependencies]\n");
            for (self.dependencies.items) |dep| {
                try result.appendSlice(dep.name);
                try result.appendSlice(" = ");
                switch (dep.source) {
                    .version => |v| {
                        try result.appendSlice("\"");
                        try result.appendSlice(v);
                        try result.appendSlice("\"");
                    },
                    .path => |p| {
                        try result.appendSlice("{ path = \"");
                        try result.appendSlice(p);
                        try result.appendSlice("\" }");
                    },
                    .git => |g| {
                        try result.appendSlice("{ git = \"");
                        try result.appendSlice(g.url);
                        try result.appendSlice("\"");
                        if (g.branch.len > 0) {
                            try result.appendSlice(", branch = \"");
                            try result.appendSlice(g.branch);
                            try result.appendSlice("\"");
                        }
                        try result.appendSlice(" }");
                    },
                }
                try result.appendSlice("\n");
            }
        }

        return try result.toOwnedSlice();
    }
};

pub const Dependency = struct {
    name: []const u8,
    source: DepSource,
};

pub const DepSource = union(enum) {
    version: []const u8,
    path: []const u8,
    git: GitSource,
};

pub const GitSource = struct {
    url: []const u8,
    branch: []const u8 = "",
};

// ============================================================================
// Simple TOML Parser (subset: strings, tables, inline tables)
// ============================================================================

pub fn parseManifest(source: []const u8, allocator: Allocator) !Manifest {
    var manifest = Manifest.init(allocator);
    errdefer manifest.deinit();

    var current_section: enum { none, package, dependencies } = .none;
    var lines = std.mem.splitScalar(u8, source, '\n');

    while (lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");

        // Skip empty lines and comments
        if (line.len == 0) continue;
        if (line[0] == '#') continue;

        // Section headers
        if (line[0] == '[') {
            if (std.mem.eql(u8, line, "[package]")) {
                current_section = .package;
            } else if (std.mem.eql(u8, line, "[dependencies]")) {
                current_section = .dependencies;
            } else {
                current_section = .none;
            }
            continue;
        }

        // Key = value pairs
        if (std.mem.indexOf(u8, line, "=")) |eq_pos| {
            const key = std.mem.trim(u8, line[0..eq_pos], " \t");
            const raw_val = std.mem.trim(u8, line[eq_pos + 1 ..], " \t");

            switch (current_section) {
                .package => {
                    const val = stripQuotes(raw_val);
                    if (std.mem.eql(u8, key, "name")) {
                        manifest.name = val;
                    } else if (std.mem.eql(u8, key, "version")) {
                        manifest.version = val;
                    } else if (std.mem.eql(u8, key, "description")) {
                        manifest.description = val;
                    } else if (std.mem.eql(u8, key, "author")) {
                        manifest.author = val;
                    } else if (std.mem.eql(u8, key, "license")) {
                        manifest.license = val;
                    }
                },
                .dependencies => {
                    // Parse dependency value: either "version" or { path = "..." } or { git = "..." }
                    if (raw_val.len > 0 and raw_val[0] == '"') {
                        // Simple version string
                        try manifest.dependencies.append(.{
                            .name = key,
                            .source = .{ .version = stripQuotes(raw_val) },
                        });
                    } else if (raw_val.len > 0 and raw_val[0] == '{') {
                        // Inline table
                        const dep = parseInlineDep(key, raw_val);
                        try manifest.dependencies.append(dep);
                    }
                },
                .none => {},
            }
        }
    }

    return manifest;
}

fn stripQuotes(s: []const u8) []const u8 {
    if (s.len >= 2 and s[0] == '"' and s[s.len - 1] == '"') {
        return s[1 .. s.len - 1];
    }
    return s;
}

fn parseInlineDep(name: []const u8, inline_table: []const u8) Dependency {
    // Parse { path = "..." } or { git = "...", branch = "..." }
    if (findInlineValue(inline_table, "path")) |path| {
        return .{ .name = name, .source = .{ .path = path } };
    }
    if (findInlineValue(inline_table, "git")) |url| {
        const branch = findInlineValue(inline_table, "branch") orelse "";
        return .{ .name = name, .source = .{ .git = .{ .url = url, .branch = branch } } };
    }
    // Fallback
    return .{ .name = name, .source = .{ .version = "0.0.0" } };
}

fn findInlineValue(table: []const u8, key: []const u8) ?[]const u8 {
    // Search for key = "value" within { ... }
    var i: usize = 0;
    while (i + key.len < table.len) : (i += 1) {
        if (std.mem.startsWith(u8, table[i..], key)) {
            // Check it's a word boundary (preceded by space, comma, or {)
            if (i > 0 and table[i - 1] != ' ' and table[i - 1] != ',' and table[i - 1] != '{') {
                continue;
            }
            var j = i + key.len;
            // Skip whitespace and =
            while (j < table.len and (table[j] == ' ' or table[j] == '=')) : (j += 1) {}
            if (j < table.len and table[j] == '"') {
                j += 1;
                const start = j;
                while (j < table.len and table[j] != '"') : (j += 1) {}
                if (j < table.len) {
                    return table[start..j];
                }
            }
        }
    }
    return null;
}

// ============================================================================
// Package Commands
// ============================================================================

pub const PkgCommand = enum {
    init_pkg,
    add,
    list,
    help,
};

pub fn parsePkgCommand(args: []const []const u8) struct { cmd: PkgCommand, args: []const []const u8 } {
    if (args.len == 0) return .{ .cmd = .help, .args = &.{} };

    if (std.mem.eql(u8, args[0], "init")) {
        return .{ .cmd = .init_pkg, .args = if (args.len > 1) args[1..] else &.{} };
    } else if (std.mem.eql(u8, args[0], "add")) {
        return .{ .cmd = .add, .args = if (args.len > 1) args[1..] else &.{} };
    } else if (std.mem.eql(u8, args[0], "list")) {
        return .{ .cmd = .list, .args = &.{} };
    }

    return .{ .cmd = .help, .args = &.{} };
}

pub fn executePkgInit(allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    // Check if daimond.toml already exists
    if (std.fs.cwd().access("daimond.toml", .{})) |_| {
        try stdout.print("daimond.toml already exists.\n", .{});
        return;
    } else |_| {}

    // Detect project name from current directory
    var name_buf: [256]u8 = undefined;
    const cwd = std.fs.cwd().realpath(".", &name_buf) catch "unnamed";
    const project_name = std.fs.path.basename(cwd);

    var manifest = Manifest.init(allocator);
    defer manifest.deinit();
    manifest.name = project_name;
    manifest.version = "0.1.0";

    const toml = try manifest.toToml(allocator);
    defer allocator.free(toml);

    const file = try std.fs.cwd().createFile("daimond.toml", .{});
    defer file.close();
    try file.writeAll(toml);

    try stdout.print("Created daimond.toml for '{s}'\n", .{project_name});
}

pub fn executePkgAdd(dep_name: []const u8, extra_args: []const []const u8, allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    // Read existing manifest or create new one
    const source = std.fs.cwd().readFileAlloc(allocator, "daimond.toml", 1024 * 1024) catch {
        try stdout.print("No daimond.toml found. Run 'daimond pkg init' first.\n", .{});
        return;
    };
    defer allocator.free(source);

    var manifest = try parseManifest(source, allocator);
    defer manifest.deinit();

    // Check if dependency already exists
    for (manifest.dependencies.items) |dep| {
        if (std.mem.eql(u8, dep.name, dep_name)) {
            try stdout.print("Dependency '{s}' already exists.\n", .{dep_name});
            return;
        }
    }

    // Determine dependency source
    var dep: Dependency = undefined;
    if (extra_args.len >= 2 and std.mem.eql(u8, extra_args[0], "--path")) {
        dep = .{ .name = dep_name, .source = .{ .path = extra_args[1] } };
    } else if (extra_args.len >= 2 and std.mem.eql(u8, extra_args[0], "--git")) {
        var branch: []const u8 = "";
        if (extra_args.len >= 4 and std.mem.eql(u8, extra_args[2], "--branch")) {
            branch = extra_args[3];
        }
        dep = .{ .name = dep_name, .source = .{ .git = .{ .url = extra_args[1], .branch = branch } } };
    } else if (extra_args.len >= 1) {
        dep = .{ .name = dep_name, .source = .{ .version = extra_args[0] } };
    } else {
        dep = .{ .name = dep_name, .source = .{ .version = "*" } };
    }

    try manifest.dependencies.append(dep);

    // Write updated manifest
    const toml = try manifest.toToml(allocator);
    defer allocator.free(toml);

    const file = try std.fs.cwd().createFile("daimond.toml", .{});
    defer file.close();
    try file.writeAll(toml);

    try stdout.print("Added dependency '{s}'\n", .{dep_name});
}

pub fn executePkgList(allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    const source = std.fs.cwd().readFileAlloc(allocator, "daimond.toml", 1024 * 1024) catch {
        try stdout.print("No daimond.toml found. Run 'daimond pkg init' first.\n", .{});
        return;
    };
    defer allocator.free(source);

    var manifest = try parseManifest(source, allocator);
    defer manifest.deinit();

    try stdout.print("{s} v{s}\n", .{ manifest.name, manifest.version });
    if (manifest.description.len > 0) {
        try stdout.print("{s}\n", .{manifest.description});
    }
    try stdout.print("\n", .{});

    if (manifest.dependencies.items.len == 0) {
        try stdout.print("No dependencies.\n", .{});
        return;
    }

    try stdout.print("Dependencies:\n", .{});
    for (manifest.dependencies.items) |dep| {
        try stdout.print("  {s} = ", .{dep.name});
        switch (dep.source) {
            .version => |v| try stdout.print("{s}\n", .{v}),
            .path => |p| try stdout.print("(path: {s})\n", .{p}),
            .git => |g| {
                try stdout.print("(git: {s}", .{g.url});
                if (g.branch.len > 0) {
                    try stdout.print(", branch: {s}", .{g.branch});
                }
                try stdout.print(")\n", .{});
            },
        }
    }
}

pub fn printPkgHelp() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print(
        \\Usage: daimond pkg <command>
        \\
        \\Commands:
        \\  init          Create a new daimond.toml manifest
        \\  add <name>    Add a dependency
        \\  list          List dependencies
        \\
        \\Add options:
        \\  daimond pkg add <name> <version>        Add with version
        \\  daimond pkg add <name> --path <path>     Add local dependency
        \\  daimond pkg add <name> --git <url>       Add git dependency
        \\
    , .{});
}

// ============================================================================
// Tests
// ============================================================================

test "parse empty manifest" {
    const source = "[package]\nname = \"test\"\nversion = \"1.0.0\"\n";
    var manifest = try parseManifest(source, std.testing.allocator);
    defer manifest.deinit();

    try std.testing.expectEqualStrings("test", manifest.name);
    try std.testing.expectEqualStrings("1.0.0", manifest.version);
    try std.testing.expectEqual(@as(usize, 0), manifest.dependencies.items.len);
}

test "parse manifest with version dependency" {
    const source = "[package]\nname = \"myapp\"\nversion = \"0.1.0\"\n\n[dependencies]\nmath = \"1.2.3\"\n";
    var manifest = try parseManifest(source, std.testing.allocator);
    defer manifest.deinit();

    try std.testing.expectEqualStrings("myapp", manifest.name);
    try std.testing.expectEqual(@as(usize, 1), manifest.dependencies.items.len);
    try std.testing.expectEqualStrings("math", manifest.dependencies.items[0].name);

    switch (manifest.dependencies.items[0].source) {
        .version => |v| try std.testing.expectEqualStrings("1.2.3", v),
        else => return error.TestUnexpectedResult,
    }
}

test "parse manifest with path dependency" {
    const source = "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\nutils = { path = \"../utils\" }\n";
    var manifest = try parseManifest(source, std.testing.allocator);
    defer manifest.deinit();

    try std.testing.expectEqual(@as(usize, 1), manifest.dependencies.items.len);

    switch (manifest.dependencies.items[0].source) {
        .path => |p| try std.testing.expectEqualStrings("../utils", p),
        else => return error.TestUnexpectedResult,
    }
}

test "parse manifest with git dependency" {
    const source = "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\nlib = { git = \"https://github.com/user/lib\", branch = \"main\" }\n";
    var manifest = try parseManifest(source, std.testing.allocator);
    defer manifest.deinit();

    try std.testing.expectEqual(@as(usize, 1), manifest.dependencies.items.len);

    switch (manifest.dependencies.items[0].source) {
        .git => |g| {
            try std.testing.expectEqualStrings("https://github.com/user/lib", g.url);
            try std.testing.expectEqualStrings("main", g.branch);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "manifest round-trip" {
    var manifest = Manifest.init(std.testing.allocator);
    defer manifest.deinit();

    manifest.name = "roundtrip";
    manifest.version = "2.0.0";
    manifest.description = "A test";

    try manifest.dependencies.append(.{ .name = "dep1", .source = .{ .version = "1.0.0" } });
    try manifest.dependencies.append(.{ .name = "dep2", .source = .{ .path = "../dep2" } });

    const toml = try manifest.toToml(std.testing.allocator);
    defer std.testing.allocator.free(toml);

    var parsed = try parseManifest(toml, std.testing.allocator);
    defer parsed.deinit();

    try std.testing.expectEqualStrings("roundtrip", parsed.name);
    try std.testing.expectEqualStrings("2.0.0", parsed.version);
    try std.testing.expectEqual(@as(usize, 2), parsed.dependencies.items.len);
}

test "strip quotes" {
    try std.testing.expectEqualStrings("hello", stripQuotes("\"hello\""));
    try std.testing.expectEqualStrings("bare", stripQuotes("bare"));
    try std.testing.expectEqualStrings("", stripQuotes("\"\""));
}
