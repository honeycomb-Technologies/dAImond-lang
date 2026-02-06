//! dAImond Compilation Cache
//!
//! Caches generated C code indexed by source file content hash (SHA-256).
//! When source hasn't changed, the cached C code is reused, skipping
//! lexing, parsing, type checking, and code generation.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Sha256 = std.crypto.hash.sha2.Sha256;

/// Default cache directory relative to the project root.
pub const default_cache_dir = ".daimond-cache";

/// Compilation cache for reusing generated C code.
pub const Cache = struct {
    cache_dir: []const u8,
    allocator: Allocator,

    const Self = @This();

    /// Initialize the cache with a cache directory path.
    pub fn init(allocator: Allocator, cache_dir: ?[]const u8) Self {
        return .{
            .cache_dir = cache_dir orelse default_cache_dir,
            .allocator = allocator,
        };
    }

    /// Compute SHA-256 hash of source content and return hex string.
    pub fn hashSource(self: *Self, source: []const u8) ![Sha256.digest_length * 2]u8 {
        _ = self;
        var hasher = Sha256.init(.{});
        hasher.update(source);
        const digest = hasher.finalResult();
        return std.fmt.bytesToHex(digest, .lower);
    }

    /// Look up cached C code by source hash. Returns null on cache miss.
    pub fn lookup(self: *Self, hash_hex: []const u8) ?[]const u8 {
        const path = std.fmt.allocPrint(self.allocator, "{s}/{s}.c", .{ self.cache_dir, hash_hex }) catch return null;
        defer self.allocator.free(path);

        const content = std.fs.cwd().readFileAlloc(self.allocator, path, 10 * 1024 * 1024) catch return null;
        return content;
    }

    /// Store generated C code in cache indexed by source hash.
    pub fn store(self: *Self, hash_hex: []const u8, c_code: []const u8) !void {
        // Ensure cache directory exists
        std.fs.cwd().makePath(self.cache_dir) catch |err| {
            if (err != error.PathAlreadyExists) return err;
        };

        const path = try std.fmt.allocPrint(self.allocator, "{s}/{s}.c", .{ self.cache_dir, hash_hex });
        defer self.allocator.free(path);

        try std.fs.cwd().writeFile(.{
            .sub_path = path,
            .data = c_code,
        });
    }

    /// Remove the entire cache directory.
    pub fn clean(self: *Self) !void {
        std.fs.cwd().deleteTree(self.cache_dir) catch |err| {
            if (err != error.FileNotFound) return err;
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "hash source produces consistent hex string" {
    var cache = Cache.init(std.testing.allocator, null);
    const hash1 = try cache.hashSource("hello world");
    const hash2 = try cache.hashSource("hello world");
    try std.testing.expectEqualSlices(u8, &hash1, &hash2);
}

test "hash source produces different hashes for different input" {
    var cache = Cache.init(std.testing.allocator, null);
    const hash1 = try cache.hashSource("hello");
    const hash2 = try cache.hashSource("world");
    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
}

test "hash source length is 64 hex chars" {
    var cache = Cache.init(std.testing.allocator, null);
    const hash = try cache.hashSource("test");
    try std.testing.expectEqual(@as(usize, 64), hash.len);
}

test "lookup returns null on cache miss" {
    var cache = Cache.init(std.testing.allocator, "/tmp/daimond-cache-test-nonexistent");
    const result = cache.lookup("deadbeef");
    try std.testing.expect(result == null);
}

test "store and lookup round-trip" {
    const allocator = std.testing.allocator;
    var cache = Cache.init(allocator, "/tmp/daimond-cache-test");
    defer cache.clean() catch {};

    const test_hash = "0000000000000000000000000000000000000000000000000000000000000000";
    const test_code = "int main() { return 0; }";

    try cache.store(test_hash, test_code);

    const result = cache.lookup(test_hash);
    try std.testing.expect(result != null);
    try std.testing.expectEqualSlices(u8, test_code, result.?);
    allocator.free(result.?);
}

test "clean removes cache directory" {
    var cache = Cache.init(std.testing.allocator, "/tmp/daimond-cache-test-clean");

    // Create cache dir and a file
    try cache.store("aaaa", "test");

    // Clean
    try cache.clean();

    // Verify directory is gone - openDir should fail
    const dir_result = std.fs.cwd().openDir("/tmp/daimond-cache-test-clean", .{});
    if (dir_result) |*dir| {
        var d = dir.*;
        d.close();
        try std.testing.expect(false); // Should not reach here
    } else |_| {
        // Expected: directory not found
    }
}
