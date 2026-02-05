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
// Semantic Versioning
// ============================================================================

pub const SemVer = struct {
    major: u32,
    minor: u32,
    patch: u32,

    /// Parse a semantic version string (major.minor.patch).
    /// Accepts formats: "1.2.3", "1.2", "1" (missing parts default to 0).
    pub fn parse(input: []const u8) ?SemVer {
        if (input.len == 0) return null;

        var parts: [3]u32 = .{ 0, 0, 0 };
        var part_idx: usize = 0;
        var i: usize = 0;

        while (i < input.len and part_idx < 3) {
            // Parse a number
            const start = i;
            while (i < input.len and input[i] >= '0' and input[i] <= '9') : (i += 1) {}
            if (i == start) return null; // Expected a digit
            parts[part_idx] = std.fmt.parseInt(u32, input[start..i], 10) catch return null;
            part_idx += 1;

            // Expect a dot separator or end of string
            if (i < input.len) {
                if (input[i] == '.') {
                    i += 1;
                } else {
                    return null; // Unexpected character
                }
            }
        }

        // If there are remaining characters after 3 parts, it's invalid
        if (i < input.len) return null;

        if (part_idx == 0) return null;

        return SemVer{
            .major = parts[0],
            .minor = parts[1],
            .patch = parts[2],
        };
    }

    /// Compare two versions: returns .lt, .eq, or .gt
    pub fn order(self: SemVer, other: SemVer) std.math.Order {
        if (self.major != other.major) {
            return if (self.major < other.major) .lt else .gt;
        }
        if (self.minor != other.minor) {
            return if (self.minor < other.minor) .lt else .gt;
        }
        if (self.patch != other.patch) {
            return if (self.patch < other.patch) .lt else .gt;
        }
        return .eq;
    }

    /// Check if this version satisfies a given constraint.
    pub fn satisfies(self: SemVer, constraint: VersionConstraint) bool {
        return constraint.matches(self);
    }

    /// Format as "major.minor.patch"
    pub fn format(self: SemVer, allocator: Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{d}.{d}.{d}", .{ self.major, self.minor, self.patch });
    }

    pub fn eql(self: SemVer, other: SemVer) bool {
        return self.major == other.major and self.minor == other.minor and self.patch == other.patch;
    }
};

/// Constraint operator for version matching.
pub const ConstraintOp = enum {
    /// Exact match: =1.2.3
    eq,
    /// Greater than: >1.2.3
    gt,
    /// Greater or equal: >=1.2.3
    gte,
    /// Less than: <1.2.3
    lt,
    /// Less or equal: <=1.2.3
    lte,
    /// Compatible (same major, >= given): ^1.2.3
    caret,
    /// Tilde (same major.minor, >= given): ~1.2.3
    tilde,
    /// Any version: *
    any,
};

pub const VersionConstraint = struct {
    op: ConstraintOp,
    version: SemVer,

    /// Parse a version constraint string.
    /// Supported formats: "^1.2.3", "~1.2.3", ">=1.2.3", "<=1.2.3", ">1.2.3", "<1.2.3", "=1.2.3", "1.2.3", "*"
    pub fn parse(input: []const u8) ?VersionConstraint {
        const trimmed = std.mem.trim(u8, input, " \t");
        if (trimmed.len == 0) return null;

        // Wildcard
        if (std.mem.eql(u8, trimmed, "*")) {
            return VersionConstraint{
                .op = .any,
                .version = SemVer{ .major = 0, .minor = 0, .patch = 0 },
            };
        }

        // Caret
        if (trimmed[0] == '^') {
            const ver = SemVer.parse(trimmed[1..]) orelse return null;
            return VersionConstraint{ .op = .caret, .version = ver };
        }

        // Tilde
        if (trimmed[0] == '~') {
            const ver = SemVer.parse(trimmed[1..]) orelse return null;
            return VersionConstraint{ .op = .tilde, .version = ver };
        }

        // >= or <=
        if (trimmed.len >= 2) {
            if (trimmed[0] == '>' and trimmed[1] == '=') {
                const ver = SemVer.parse(std.mem.trim(u8, trimmed[2..], " ")) orelse return null;
                return VersionConstraint{ .op = .gte, .version = ver };
            }
            if (trimmed[0] == '<' and trimmed[1] == '=') {
                const ver = SemVer.parse(std.mem.trim(u8, trimmed[2..], " ")) orelse return null;
                return VersionConstraint{ .op = .lte, .version = ver };
            }
        }

        // > or <
        if (trimmed[0] == '>') {
            const ver = SemVer.parse(std.mem.trim(u8, trimmed[1..], " ")) orelse return null;
            return VersionConstraint{ .op = .gt, .version = ver };
        }
        if (trimmed[0] == '<') {
            const ver = SemVer.parse(std.mem.trim(u8, trimmed[1..], " ")) orelse return null;
            return VersionConstraint{ .op = .lt, .version = ver };
        }

        // Explicit =
        if (trimmed[0] == '=') {
            const ver = SemVer.parse(std.mem.trim(u8, trimmed[1..], " ")) orelse return null;
            return VersionConstraint{ .op = .eq, .version = ver };
        }

        // Bare version (treated as exact match)
        const ver = SemVer.parse(trimmed) orelse return null;
        return VersionConstraint{ .op = .eq, .version = ver };
    }

    /// Check if a given version matches this constraint.
    pub fn matches(self: VersionConstraint, version: SemVer) bool {
        switch (self.op) {
            .any => return true,
            .eq => return version.order(self.version) == .eq,
            .gt => return version.order(self.version) == .gt,
            .gte => {
                const ord = version.order(self.version);
                return ord == .gt or ord == .eq;
            },
            .lt => return version.order(self.version) == .lt,
            .lte => {
                const ord = version.order(self.version);
                return ord == .lt or ord == .eq;
            },
            .caret => {
                // ^major.minor.patch: >=major.minor.patch and <(major+1).0.0
                // Same major version required, must be >= specified
                if (version.major != self.version.major) return false;
                const ord = version.order(self.version);
                return ord == .gt or ord == .eq;
            },
            .tilde => {
                // ~major.minor.patch: >=major.minor.patch and <major.(minor+1).0
                // Same major.minor required, must be >= specified
                if (version.major != self.version.major) return false;
                if (version.minor != self.version.minor) return false;
                const ord = version.order(self.version);
                return ord == .gt or ord == .eq;
            },
        }
    }
};

// ============================================================================
// Dependency Resolution
// ============================================================================

/// A resolved dependency with its source fully qualified.
pub const ResolvedDep = struct {
    name: []const u8,
    version: []const u8,
    source: ResolvedSource,
};

pub const ResolvedSource = union(enum) {
    path: []const u8,
    git: ResolvedGitSource,
    version: []const u8,
};

pub const ResolvedGitSource = struct {
    url: []const u8,
    branch: []const u8,
};

/// Resolve all dependencies from a manifest.
/// For path deps: resolve relative paths to absolute using project_dir as base.
/// For git deps: keep the URL as-is.
/// For version deps: record the constraint string (no registry lookup).
pub fn resolve(manifest: *const Manifest, project_dir: []const u8, allocator: Allocator) !std.ArrayList(ResolvedDep) {
    var resolved = std.ArrayList(ResolvedDep).init(allocator);
    errdefer resolved.deinit();

    for (manifest.dependencies.items) |dep| {
        switch (dep.source) {
            .path => |p| {
                // Resolve relative path to absolute
                const abs_path = try resolveRelativePath(project_dir, p, allocator);
                try resolved.append(.{
                    .name = dep.name,
                    .version = "0.0.0",
                    .source = .{ .path = abs_path },
                });
            },
            .git => |g| {
                try resolved.append(.{
                    .name = dep.name,
                    .version = "0.0.0",
                    .source = .{ .git = .{ .url = g.url, .branch = g.branch } },
                });
            },
            .version => |v| {
                try resolved.append(.{
                    .name = dep.name,
                    .version = v,
                    .source = .{ .version = v },
                });
            },
        }
    }

    return resolved;
}

/// Resolve a potentially relative path against a base directory.
fn resolveRelativePath(base_dir: []const u8, rel_path: []const u8, allocator: Allocator) ![]const u8 {
    // If already absolute, return as-is (allocate a copy)
    if (rel_path.len > 0 and rel_path[0] == '/') {
        return try allocator.dupe(u8, rel_path);
    }

    // Build base_dir + "/" + rel_path
    var components = std.ArrayList([]const u8).init(allocator);
    defer components.deinit();

    // Split base dir into components
    var base_iter = std.mem.splitScalar(u8, base_dir, '/');
    while (base_iter.next()) |part| {
        if (part.len == 0) continue;
        try components.append(part);
    }

    // Process rel_path components
    var rel_iter = std.mem.splitScalar(u8, rel_path, '/');
    while (rel_iter.next()) |part| {
        if (part.len == 0 or std.mem.eql(u8, part, ".")) continue;
        if (std.mem.eql(u8, part, "..")) {
            if (components.items.len > 0) {
                _ = components.pop();
            }
        } else {
            try components.append(part);
        }
    }

    // Rebuild absolute path
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    for (components.items) |comp| {
        try result.append('/');
        try result.appendSlice(comp);
    }

    if (result.items.len == 0) {
        try result.append('/');
    }

    return try result.toOwnedSlice();
}

// ============================================================================
// Lock File
// ============================================================================

/// Generate a lock file from resolved dependencies.
/// Format:
/// ```
/// # daimond.lock - Auto-generated, do not edit
/// [[package]]
/// name = "dep_name"
/// version = "1.2.3"
/// source = "path:/absolute/path"
/// ```
pub fn generateLockFile(resolved: *const std.ArrayList(ResolvedDep), allocator: Allocator) ![]const u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    try result.appendSlice("# daimond.lock - Auto-generated, do not edit\n");

    for (resolved.items) |dep| {
        try result.appendSlice("\n[[package]]\n");
        try result.appendSlice("name = \"");
        try result.appendSlice(dep.name);
        try result.appendSlice("\"\n");
        try result.appendSlice("version = \"");
        try result.appendSlice(dep.version);
        try result.appendSlice("\"\n");
        try result.appendSlice("source = \"");
        switch (dep.source) {
            .path => |p| {
                try result.appendSlice("path:");
                try result.appendSlice(p);
            },
            .git => |g| {
                try result.appendSlice("git:");
                try result.appendSlice(g.url);
                if (g.branch.len > 0) {
                    try result.append('#');
                    try result.appendSlice(g.branch);
                }
            },
            .version => |v| {
                try result.appendSlice("registry:");
                try result.appendSlice(v);
            },
        }
        try result.appendSlice("\"\n");
    }

    return try result.toOwnedSlice();
}

/// A parsed lock file entry.
pub const LockEntry = struct {
    name: []const u8,
    version: []const u8,
    source: []const u8,
};

/// Parse a lock file back into a list of lock entries.
pub fn parseLockFile(source: []const u8, allocator: Allocator) !std.ArrayList(LockEntry) {
    var entries = std.ArrayList(LockEntry).init(allocator);
    errdefer entries.deinit();

    var current_name: ?[]const u8 = null;
    var current_version: ?[]const u8 = null;
    var current_source: ?[]const u8 = null;

    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");

        // Skip empty lines and comments
        if (line.len == 0) continue;
        if (line[0] == '#') continue;

        // Section header [[package]]
        if (std.mem.eql(u8, line, "[[package]]")) {
            // Flush previous entry if complete
            if (current_name != null and current_version != null and current_source != null) {
                try entries.append(.{
                    .name = current_name.?,
                    .version = current_version.?,
                    .source = current_source.?,
                });
            }
            current_name = null;
            current_version = null;
            current_source = null;
            continue;
        }

        // Key = value pairs
        if (std.mem.indexOf(u8, line, "=")) |eq_pos| {
            const key = std.mem.trim(u8, line[0..eq_pos], " \t");
            const raw_val = std.mem.trim(u8, line[eq_pos + 1 ..], " \t");
            const val = stripQuotes(raw_val);

            if (std.mem.eql(u8, key, "name")) {
                current_name = val;
            } else if (std.mem.eql(u8, key, "version")) {
                current_version = val;
            } else if (std.mem.eql(u8, key, "source")) {
                current_source = val;
            }
        }
    }

    // Flush final entry
    if (current_name != null and current_version != null and current_source != null) {
        try entries.append(.{
            .name = current_name.?,
            .version = current_version.?,
            .source = current_source.?,
        });
    }

    return entries;
}

// ============================================================================
// Package Download
// ============================================================================

/// Download/install a single resolved dependency into the deps/ directory.
/// - Git deps: `git clone --depth 1` into deps/<name>/
/// - Path deps: create a symlink from deps/<name>/ to the path
/// - Version deps: print a warning (no registry yet)
fn downloadDep(dep: ResolvedDep, deps_dir: std.fs.Dir, allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    switch (dep.source) {
        .git => |g| {
            // Remove existing directory if present
            deps_dir.deleteTree(dep.name) catch {};

            try stdout.print("  Fetching {s} from {s}...\n", .{ dep.name, g.url });

            // Build git clone arguments
            var argv = std.ArrayList([]const u8).init(allocator);
            defer argv.deinit();

            try argv.append("git");
            try argv.append("clone");
            try argv.append("--depth");
            try argv.append("1");
            if (g.branch.len > 0) {
                try argv.append("--branch");
                try argv.append(g.branch);
            }
            try argv.append(g.url);
            try argv.append(dep.name);

            var child = std.process.Child.init(argv.items, allocator);
            child.cwd_dir = deps_dir;
            child.stdout_behavior = .Ignore;
            child.stderr_behavior = .Pipe;

            try child.spawn();
            const result = try child.wait();

            switch (result) {
                .Exited => |code| {
                    if (code == 0) {
                        try stdout.print("  Installed {s} (git)\n", .{dep.name});
                    } else {
                        try stdout.print("  Error: git clone failed for {s} (exit code {d})\n", .{ dep.name, code });
                    }
                },
                else => {
                    try stdout.print("  Error: git clone failed for {s}\n", .{dep.name});
                },
            }
        },
        .path => |p| {
            // Remove existing entry if present
            deps_dir.deleteTree(dep.name) catch {};

            try stdout.print("  Linking {s} -> {s}\n", .{ dep.name, p });

            // Create symlink
            deps_dir.symLink(p, dep.name, .{ .is_directory = true }) catch |err| {
                try stdout.print("  Error: could not create symlink for {s}: {}\n", .{ dep.name, err });
                return;
            };

            try stdout.print("  Installed {s} (path)\n", .{dep.name});
        },
        .version => |v| {
            try stdout.print("  Warning: registry not yet available. Skipping {s} ({s})\n", .{ dep.name, v });
        },
    }
}

/// Install all resolved dependencies.
fn installDeps(resolved: *const std.ArrayList(ResolvedDep), allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    if (resolved.items.len == 0) {
        try stdout.print("No dependencies to install.\n", .{});
        return;
    }

    // Create deps/ directory if it doesn't exist
    std.fs.cwd().makeDir("deps") catch |err| {
        switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        }
    };

    var deps_dir = try std.fs.cwd().openDir("deps", .{});
    defer deps_dir.close();

    try stdout.print("Installing {d} dependencies...\n", .{resolved.items.len});

    for (resolved.items) |dep| {
        try downloadDep(dep, deps_dir, allocator);
    }

    try stdout.print("Done.\n", .{});
}

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
    install,
    update,
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
    } else if (std.mem.eql(u8, args[0], "install")) {
        return .{ .cmd = .install, .args = &.{} };
    } else if (std.mem.eql(u8, args[0], "update")) {
        return .{ .cmd = .update, .args = &.{} };
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

/// Execute `daimond pkg install`: read manifest, resolve deps, download, generate lock file.
pub fn executePkgInstall(allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    // Read manifest
    const source = std.fs.cwd().readFileAlloc(allocator, "daimond.toml", 1024 * 1024) catch {
        try stdout.print("No daimond.toml found. Run 'daimond pkg init' first.\n", .{});
        return;
    };
    defer allocator.free(source);

    var manifest = try parseManifest(source, allocator);
    defer manifest.deinit();

    // Check for existing lock file
    const has_lock = blk: {
        std.fs.cwd().access("daimond.lock", .{}) catch break :blk false;
        break :blk true;
    };

    if (has_lock) {
        try stdout.print("Using existing daimond.lock\n", .{});
    }

    // Resolve project dir
    var dir_buf: [4096]u8 = undefined;
    const project_dir = std.fs.cwd().realpath(".", &dir_buf) catch ".";

    // Resolve dependencies
    var resolved = try resolve(&manifest, project_dir, allocator);
    defer {
        // Free allocated path strings
        for (resolved.items) |dep| {
            switch (dep.source) {
                .path => |p| allocator.free(p),
                else => {},
            }
        }
        resolved.deinit();
    }

    // Download/install
    try installDeps(&resolved, allocator);

    // Generate lock file
    const lock_content = try generateLockFile(&resolved, allocator);
    defer allocator.free(lock_content);

    const lock_file = try std.fs.cwd().createFile("daimond.lock", .{});
    defer lock_file.close();
    try lock_file.writeAll(lock_content);

    try stdout.print("Generated daimond.lock\n", .{});
}

/// Execute `daimond pkg update`: re-resolve (ignore existing lock), download, regenerate lock file.
pub fn executePkgUpdate(allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    // Read manifest
    const source = std.fs.cwd().readFileAlloc(allocator, "daimond.toml", 1024 * 1024) catch {
        try stdout.print("No daimond.toml found. Run 'daimond pkg init' first.\n", .{});
        return;
    };
    defer allocator.free(source);

    var manifest = try parseManifest(source, allocator);
    defer manifest.deinit();

    try stdout.print("Updating dependencies (ignoring existing lock file)...\n", .{});

    // Remove existing lock file if present
    std.fs.cwd().deleteFile("daimond.lock") catch {};

    // Resolve project dir
    var dir_buf: [4096]u8 = undefined;
    const project_dir = std.fs.cwd().realpath(".", &dir_buf) catch ".";

    // Resolve dependencies fresh
    var resolved = try resolve(&manifest, project_dir, allocator);
    defer {
        for (resolved.items) |dep| {
            switch (dep.source) {
                .path => |p| allocator.free(p),
                else => {},
            }
        }
        resolved.deinit();
    }

    // Download/install
    try installDeps(&resolved, allocator);

    // Generate new lock file
    const lock_content = try generateLockFile(&resolved, allocator);
    defer allocator.free(lock_content);

    const lock_file = try std.fs.cwd().createFile("daimond.lock", .{});
    defer lock_file.close();
    try lock_file.writeAll(lock_content);

    try stdout.print("Generated daimond.lock\n", .{});
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
        \\  install       Download and install dependencies
        \\  update        Re-resolve and update all dependencies
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

// ============================================================================
// SemVer Tests
// ============================================================================

test "semver parse full version" {
    const v = SemVer.parse("1.2.3").?;
    try std.testing.expectEqual(@as(u32, 1), v.major);
    try std.testing.expectEqual(@as(u32, 2), v.minor);
    try std.testing.expectEqual(@as(u32, 3), v.patch);
}

test "semver parse two parts" {
    const v = SemVer.parse("2.5").?;
    try std.testing.expectEqual(@as(u32, 2), v.major);
    try std.testing.expectEqual(@as(u32, 5), v.minor);
    try std.testing.expectEqual(@as(u32, 0), v.patch);
}

test "semver parse single part" {
    const v = SemVer.parse("7").?;
    try std.testing.expectEqual(@as(u32, 7), v.major);
    try std.testing.expectEqual(@as(u32, 0), v.minor);
    try std.testing.expectEqual(@as(u32, 0), v.patch);
}

test "semver parse zero version" {
    const v = SemVer.parse("0.0.0").?;
    try std.testing.expectEqual(@as(u32, 0), v.major);
    try std.testing.expectEqual(@as(u32, 0), v.minor);
    try std.testing.expectEqual(@as(u32, 0), v.patch);
}

test "semver parse invalid" {
    try std.testing.expect(SemVer.parse("") == null);
    try std.testing.expect(SemVer.parse("abc") == null);
    try std.testing.expect(SemVer.parse("1.2.3.4") == null);
    try std.testing.expect(SemVer.parse("1.a.3") == null);
    try std.testing.expect(SemVer.parse(".1.2") == null);
}

test "semver ordering" {
    const v100 = SemVer.parse("1.0.0").?;
    const v110 = SemVer.parse("1.1.0").?;
    const v111 = SemVer.parse("1.1.1").?;
    const v200 = SemVer.parse("2.0.0").?;

    try std.testing.expectEqual(std.math.Order.eq, v100.order(v100));
    try std.testing.expectEqual(std.math.Order.lt, v100.order(v110));
    try std.testing.expectEqual(std.math.Order.gt, v200.order(v111));
    try std.testing.expectEqual(std.math.Order.lt, v110.order(v111));
}

test "semver equality" {
    const v1 = SemVer.parse("1.2.3").?;
    const v2 = SemVer.parse("1.2.3").?;
    const v3 = SemVer.parse("1.2.4").?;

    try std.testing.expect(v1.eql(v2));
    try std.testing.expect(!v1.eql(v3));
}

test "semver format" {
    const v = SemVer{ .major = 1, .minor = 2, .patch = 3 };
    const formatted = try v.format(std.testing.allocator);
    defer std.testing.allocator.free(formatted);
    try std.testing.expectEqualStrings("1.2.3", formatted);
}

// ============================================================================
// Version Constraint Tests
// ============================================================================

test "constraint parse wildcard" {
    const c = VersionConstraint.parse("*").?;
    try std.testing.expectEqual(ConstraintOp.any, c.op);
}

test "constraint parse caret" {
    const c = VersionConstraint.parse("^1.2.3").?;
    try std.testing.expectEqual(ConstraintOp.caret, c.op);
    try std.testing.expectEqual(@as(u32, 1), c.version.major);
    try std.testing.expectEqual(@as(u32, 2), c.version.minor);
    try std.testing.expectEqual(@as(u32, 3), c.version.patch);
}

test "constraint parse tilde" {
    const c = VersionConstraint.parse("~1.2.0").?;
    try std.testing.expectEqual(ConstraintOp.tilde, c.op);
    try std.testing.expectEqual(@as(u32, 1), c.version.major);
    try std.testing.expectEqual(@as(u32, 2), c.version.minor);
}

test "constraint parse gte" {
    const c = VersionConstraint.parse(">=2.0.0").?;
    try std.testing.expectEqual(ConstraintOp.gte, c.op);
    try std.testing.expectEqual(@as(u32, 2), c.version.major);
}

test "constraint parse lte" {
    const c = VersionConstraint.parse("<=3.1.0").?;
    try std.testing.expectEqual(ConstraintOp.lte, c.op);
    try std.testing.expectEqual(@as(u32, 3), c.version.major);
    try std.testing.expectEqual(@as(u32, 1), c.version.minor);
}

test "constraint parse gt" {
    const c = VersionConstraint.parse(">1.0.0").?;
    try std.testing.expectEqual(ConstraintOp.gt, c.op);
}

test "constraint parse lt" {
    const c = VersionConstraint.parse("<2.0.0").?;
    try std.testing.expectEqual(ConstraintOp.lt, c.op);
}

test "constraint parse eq explicit" {
    const c = VersionConstraint.parse("=1.5.0").?;
    try std.testing.expectEqual(ConstraintOp.eq, c.op);
    try std.testing.expectEqual(@as(u32, 1), c.version.major);
    try std.testing.expectEqual(@as(u32, 5), c.version.minor);
}

test "constraint parse bare version as eq" {
    const c = VersionConstraint.parse("1.2.3").?;
    try std.testing.expectEqual(ConstraintOp.eq, c.op);
    try std.testing.expect(c.version.eql(SemVer.parse("1.2.3").?));
}

test "constraint parse invalid" {
    try std.testing.expect(VersionConstraint.parse("") == null);
    try std.testing.expect(VersionConstraint.parse("^abc") == null);
    try std.testing.expect(VersionConstraint.parse(">>1.0.0") == null);
}

test "constraint matches wildcard" {
    const c = VersionConstraint.parse("*").?;
    try std.testing.expect(c.matches(SemVer.parse("0.0.0").?));
    try std.testing.expect(c.matches(SemVer.parse("99.99.99").?));
}

test "constraint matches eq" {
    const c = VersionConstraint.parse("=1.2.3").?;
    try std.testing.expect(c.matches(SemVer.parse("1.2.3").?));
    try std.testing.expect(!c.matches(SemVer.parse("1.2.4").?));
    try std.testing.expect(!c.matches(SemVer.parse("1.2.2").?));
}

test "constraint matches gt" {
    const c = VersionConstraint.parse(">1.0.0").?;
    try std.testing.expect(c.matches(SemVer.parse("1.0.1").?));
    try std.testing.expect(c.matches(SemVer.parse("2.0.0").?));
    try std.testing.expect(!c.matches(SemVer.parse("1.0.0").?));
    try std.testing.expect(!c.matches(SemVer.parse("0.9.9").?));
}

test "constraint matches gte" {
    const c = VersionConstraint.parse(">=1.0.0").?;
    try std.testing.expect(c.matches(SemVer.parse("1.0.0").?));
    try std.testing.expect(c.matches(SemVer.parse("1.0.1").?));
    try std.testing.expect(!c.matches(SemVer.parse("0.9.9").?));
}

test "constraint matches lt" {
    const c = VersionConstraint.parse("<2.0.0").?;
    try std.testing.expect(c.matches(SemVer.parse("1.9.9").?));
    try std.testing.expect(!c.matches(SemVer.parse("2.0.0").?));
    try std.testing.expect(!c.matches(SemVer.parse("2.0.1").?));
}

test "constraint matches lte" {
    const c = VersionConstraint.parse("<=2.0.0").?;
    try std.testing.expect(c.matches(SemVer.parse("2.0.0").?));
    try std.testing.expect(c.matches(SemVer.parse("1.9.9").?));
    try std.testing.expect(!c.matches(SemVer.parse("2.0.1").?));
}

test "constraint matches caret" {
    const c = VersionConstraint.parse("^1.2.3").?;
    // Same major, >= specified
    try std.testing.expect(c.matches(SemVer.parse("1.2.3").?));
    try std.testing.expect(c.matches(SemVer.parse("1.2.4").?));
    try std.testing.expect(c.matches(SemVer.parse("1.9.0").?));
    // Different major
    try std.testing.expect(!c.matches(SemVer.parse("2.0.0").?));
    try std.testing.expect(!c.matches(SemVer.parse("0.9.9").?));
    // Same major, < specified
    try std.testing.expect(!c.matches(SemVer.parse("1.2.2").?));
}

test "constraint matches tilde" {
    const c = VersionConstraint.parse("~1.2.0").?;
    // Same major.minor, >= specified
    try std.testing.expect(c.matches(SemVer.parse("1.2.0").?));
    try std.testing.expect(c.matches(SemVer.parse("1.2.5").?));
    // Different minor
    try std.testing.expect(!c.matches(SemVer.parse("1.3.0").?));
    // Different major
    try std.testing.expect(!c.matches(SemVer.parse("2.2.0").?));
}

test "semver satisfies constraint" {
    const v = SemVer.parse("1.5.0").?;
    const c = VersionConstraint.parse("^1.0.0").?;
    try std.testing.expect(v.satisfies(c));
}

// ============================================================================
// Dependency Resolution Tests
// ============================================================================

test "resolve path dependency" {
    var manifest = Manifest.init(std.testing.allocator);
    defer manifest.deinit();
    manifest.name = "test";

    try manifest.dependencies.append(.{ .name = "utils", .source = .{ .path = "../utils" } });

    var resolved = try resolve(&manifest, "/home/user/project", std.testing.allocator);
    defer {
        for (resolved.items) |dep| {
            switch (dep.source) {
                .path => |p| std.testing.allocator.free(p),
                else => {},
            }
        }
        resolved.deinit();
    }

    try std.testing.expectEqual(@as(usize, 1), resolved.items.len);
    try std.testing.expectEqualStrings("utils", resolved.items[0].name);
    switch (resolved.items[0].source) {
        .path => |p| try std.testing.expectEqualStrings("/home/user/utils", p),
        else => return error.TestUnexpectedResult,
    }
}

test "resolve git dependency" {
    var manifest = Manifest.init(std.testing.allocator);
    defer manifest.deinit();
    manifest.name = "test";

    try manifest.dependencies.append(.{
        .name = "lib",
        .source = .{ .git = .{ .url = "https://github.com/user/lib", .branch = "main" } },
    });

    var resolved = try resolve(&manifest, "/home/user/project", std.testing.allocator);
    defer {
        for (resolved.items) |dep| {
            switch (dep.source) {
                .path => |p| std.testing.allocator.free(p),
                else => {},
            }
        }
        resolved.deinit();
    }

    try std.testing.expectEqual(@as(usize, 1), resolved.items.len);
    switch (resolved.items[0].source) {
        .git => |g| {
            try std.testing.expectEqualStrings("https://github.com/user/lib", g.url);
            try std.testing.expectEqualStrings("main", g.branch);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "resolve version dependency" {
    var manifest = Manifest.init(std.testing.allocator);
    defer manifest.deinit();
    manifest.name = "test";

    try manifest.dependencies.append(.{ .name = "math", .source = .{ .version = "^1.2.0" } });

    var resolved = try resolve(&manifest, "/home/user/project", std.testing.allocator);
    defer {
        for (resolved.items) |dep| {
            switch (dep.source) {
                .path => |p| std.testing.allocator.free(p),
                else => {},
            }
        }
        resolved.deinit();
    }

    try std.testing.expectEqual(@as(usize, 1), resolved.items.len);
    try std.testing.expectEqualStrings("^1.2.0", resolved.items[0].version);
    switch (resolved.items[0].source) {
        .version => |v| try std.testing.expectEqualStrings("^1.2.0", v),
        else => return error.TestUnexpectedResult,
    }
}

test "resolve multiple dependencies" {
    var manifest = Manifest.init(std.testing.allocator);
    defer manifest.deinit();
    manifest.name = "test";

    try manifest.dependencies.append(.{ .name = "utils", .source = .{ .path = "./lib/utils" } });
    try manifest.dependencies.append(.{ .name = "net", .source = .{ .git = .{ .url = "https://example.com/net.git", .branch = "" } } });
    try manifest.dependencies.append(.{ .name = "math", .source = .{ .version = "2.0.0" } });

    var resolved = try resolve(&manifest, "/projects/myapp", std.testing.allocator);
    defer {
        for (resolved.items) |dep| {
            switch (dep.source) {
                .path => |p| std.testing.allocator.free(p),
                else => {},
            }
        }
        resolved.deinit();
    }

    try std.testing.expectEqual(@as(usize, 3), resolved.items.len);
    try std.testing.expectEqualStrings("utils", resolved.items[0].name);
    try std.testing.expectEqualStrings("net", resolved.items[1].name);
    try std.testing.expectEqualStrings("math", resolved.items[2].name);
}

// ============================================================================
// Lock File Tests
// ============================================================================

test "generate lock file" {
    var resolved = std.ArrayList(ResolvedDep).init(std.testing.allocator);
    defer resolved.deinit();

    try resolved.append(.{
        .name = "utils",
        .version = "0.0.0",
        .source = .{ .path = "/home/user/utils" },
    });
    try resolved.append(.{
        .name = "lib",
        .version = "0.0.0",
        .source = .{ .git = .{ .url = "https://github.com/user/lib", .branch = "main" } },
    });
    try resolved.append(.{
        .name = "math",
        .version = "1.2.3",
        .source = .{ .version = "1.2.3" },
    });

    const lock = try generateLockFile(&resolved, std.testing.allocator);
    defer std.testing.allocator.free(lock);

    // Verify it contains expected content
    try std.testing.expect(std.mem.indexOf(u8, lock, "# daimond.lock") != null);
    try std.testing.expect(std.mem.indexOf(u8, lock, "[[package]]") != null);
    try std.testing.expect(std.mem.indexOf(u8, lock, "name = \"utils\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, lock, "source = \"path:/home/user/utils\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, lock, "name = \"lib\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, lock, "source = \"git:https://github.com/user/lib#main\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, lock, "name = \"math\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, lock, "source = \"registry:1.2.3\"") != null);
}

test "parse lock file" {
    const lock_content =
        \\# daimond.lock - Auto-generated, do not edit
        \\
        \\[[package]]
        \\name = "utils"
        \\version = "0.0.0"
        \\source = "path:/home/user/utils"
        \\
        \\[[package]]
        \\name = "lib"
        \\version = "0.0.0"
        \\source = "git:https://github.com/user/lib#main"
        \\
        \\[[package]]
        \\name = "math"
        \\version = "1.2.3"
        \\source = "registry:1.2.3"
    ;

    var entries = try parseLockFile(lock_content, std.testing.allocator);
    defer entries.deinit();

    try std.testing.expectEqual(@as(usize, 3), entries.items.len);

    try std.testing.expectEqualStrings("utils", entries.items[0].name);
    try std.testing.expectEqualStrings("0.0.0", entries.items[0].version);
    try std.testing.expectEqualStrings("path:/home/user/utils", entries.items[0].source);

    try std.testing.expectEqualStrings("lib", entries.items[1].name);
    try std.testing.expectEqualStrings("0.0.0", entries.items[1].version);
    try std.testing.expectEqualStrings("git:https://github.com/user/lib#main", entries.items[1].source);

    try std.testing.expectEqualStrings("math", entries.items[2].name);
    try std.testing.expectEqualStrings("1.2.3", entries.items[2].version);
    try std.testing.expectEqualStrings("registry:1.2.3", entries.items[2].source);
}

test "lock file round-trip" {
    var resolved = std.ArrayList(ResolvedDep).init(std.testing.allocator);
    defer resolved.deinit();

    try resolved.append(.{
        .name = "dep_a",
        .version = "1.0.0",
        .source = .{ .path = "/abs/path/a" },
    });
    try resolved.append(.{
        .name = "dep_b",
        .version = "0.0.0",
        .source = .{ .git = .{ .url = "https://example.com/b.git", .branch = "dev" } },
    });

    // Generate
    const lock = try generateLockFile(&resolved, std.testing.allocator);
    defer std.testing.allocator.free(lock);

    // Parse back
    var entries = try parseLockFile(lock, std.testing.allocator);
    defer entries.deinit();

    try std.testing.expectEqual(@as(usize, 2), entries.items.len);

    try std.testing.expectEqualStrings("dep_a", entries.items[0].name);
    try std.testing.expectEqualStrings("1.0.0", entries.items[0].version);
    try std.testing.expectEqualStrings("path:/abs/path/a", entries.items[0].source);

    try std.testing.expectEqualStrings("dep_b", entries.items[1].name);
    try std.testing.expectEqualStrings("0.0.0", entries.items[1].version);
    try std.testing.expectEqualStrings("git:https://example.com/b.git#dev", entries.items[1].source);
}

test "parse empty lock file" {
    const lock_content = "# daimond.lock - Auto-generated, do not edit\n";
    var entries = try parseLockFile(lock_content, std.testing.allocator);
    defer entries.deinit();
    try std.testing.expectEqual(@as(usize, 0), entries.items.len);
}

test "parse pkg command install" {
    const args = [_][]const u8{"install"};
    const parsed = parsePkgCommand(&args);
    try std.testing.expectEqual(PkgCommand.install, parsed.cmd);
}

test "parse pkg command update" {
    const args = [_][]const u8{"update"};
    const parsed = parsePkgCommand(&args);
    try std.testing.expectEqual(PkgCommand.update, parsed.cmd);
}

test "resolve relative path normalization" {
    // Test ".." navigation
    const result = try resolveRelativePath("/home/user/project", "../sibling", std.testing.allocator);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("/home/user/sibling", result);
}

test "resolve relative path current dir" {
    const result = try resolveRelativePath("/home/user/project", "./lib", std.testing.allocator);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("/home/user/project/lib", result);
}

test "resolve absolute path passthrough" {
    const result = try resolveRelativePath("/home/user/project", "/opt/libs/mylib", std.testing.allocator);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("/opt/libs/mylib", result);
}

test "git source without branch in lock file" {
    var resolved = std.ArrayList(ResolvedDep).init(std.testing.allocator);
    defer resolved.deinit();

    try resolved.append(.{
        .name = "lib",
        .version = "0.0.0",
        .source = .{ .git = .{ .url = "https://example.com/lib.git", .branch = "" } },
    });

    const lock = try generateLockFile(&resolved, std.testing.allocator);
    defer std.testing.allocator.free(lock);

    // Should not have a # when branch is empty
    try std.testing.expect(std.mem.indexOf(u8, lock, "source = \"git:https://example.com/lib.git\"") != null);
}
