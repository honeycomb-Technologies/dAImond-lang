const std = @import("std");

// Struct definitions

const Point = struct { x: i64, y: i64 };
const Vec3f = struct { x: f64, y: f64, z: f64 };
const Color = struct { r: i64, g: i64, b: i64, a: i64 };
const Rect = struct { x: i64, y: i64, width: i64, height: i64 };
const Circle = struct { cx: f64, cy: f64, radius: f64 };
const Matrix2 = struct { a: f64, b: f64, c: f64, d: f64 };
const Stats = struct { count: i64, total: f64, min_val: f64, max_val: f64 };

// Arithmetic functions

fn add_vals(a: i64, b: i64) i64 { return a + b; }
fn sub_vals(a: i64, b: i64) i64 { return a - b; }
fn mul_vals(a: i64, b: i64) i64 { return a * b; }
fn div_safe(a: i64, b: i64) i64 { return if (b == 0) 0 else @divTrunc(a, b); }
fn modulo(a: i64, b: i64) i64 { return if (b == 0) 0 else @mod(a, b); }
fn negate_val(a: i64) i64 { return -a; }
fn abs_val(a: i64) i64 { return if (a < 0) -a else a; }
fn max_of(a: i64, b: i64) i64 { return if (a > b) a else b; }
fn min_of(a: i64, b: i64) i64 { return if (a < b) a else b; }
fn clamp_val(val: i64, lo: i64, hi: i64) i64 {
    if (val < lo) return lo;
    if (val > hi) return hi;
    return val;
}

// Float arithmetic

fn fadd(a: f64, b: f64) f64 { return a + b; }
fn fsub(a: f64, b: f64) f64 { return a - b; }
fn fmul(a: f64, b: f64) f64 { return a * b; }
fn fdiv(a: f64, b: f64) f64 { return if (b == 0.0) 0.0 else a / b; }
fn fabs_val(a: f64) f64 { return if (a < 0.0) -a else a; }
fn fmax_of(a: f64, b: f64) f64 { return if (a > b) a else b; }
fn fmin_of(a: f64, b: f64) f64 { return if (a < b) a else b; }
fn lerp(a: f64, b: f64, t: f64) f64 { return a + (b - a) * t; }

// Recursive functions

fn fibonacci(n: i64) i64 {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

fn factorial(n: i64) i64 {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

fn power_val(base: i64, exp_val: i64) i64 {
    if (exp_val == 0) return 1;
    return base * power_val(base, exp_val - 1);
}

fn gcd(a: i64, b: i64) i64 {
    if (b == 0) return a;
    return gcd(b, @mod(a, b));
}

fn sum_range(lo: i64, hi: i64) i64 {
    if (lo > hi) return 0;
    return lo + sum_range(lo + 1, hi);
}

// Loop-based functions

fn sum_to(n: i64) i64 {
    var total: i64 = 0;
    var i: i64 = 0;
    while (i <= n) : (i += 1) { total += i; }
    return total;
}

fn product_to(n: i64) i64 {
    var result: i64 = 1;
    var i: i64 = 1;
    while (i <= n) : (i += 1) { result *= i; }
    return result;
}

fn count_divisors(n: i64) i64 {
    var count: i64 = 0;
    var i: i64 = 1;
    while (i <= n) : (i += 1) {
        if (@mod(n, i) == 0) count += 1;
    }
    return count;
}

fn is_prime(n: i64) bool {
    if (n < 2) return false;
    var i: i64 = 2;
    while (i * i <= n) : (i += 1) {
        if (@mod(n, i) == 0) return false;
    }
    return true;
}

fn count_primes(limit: i64) i64 {
    var count: i64 = 0;
    var n: i64 = 2;
    while (n <= limit) : (n += 1) {
        if (is_prime(n)) count += 1;
    }
    return count;
}

fn collatz_length(n_in: i64) i64 {
    var steps: i64 = 0;
    var val: i64 = n_in;
    while (val != 1) {
        if (@mod(val, 2) == 0) {
            val = @divTrunc(val, 2);
        } else {
            val = val * 3 + 1;
        }
        steps += 1;
    }
    return steps;
}

fn digital_root(n_in: i64) i64 {
    var val: i64 = n_in;
    while (val >= 10) {
        var sum: i64 = 0;
        var tmp: i64 = val;
        while (tmp > 0) {
            sum += @mod(tmp, 10);
            tmp = @divTrunc(tmp, 10);
        }
        val = sum;
    }
    return val;
}

// String-related functions

fn repeat_char(buf: []u8, ch: u8, count: usize) []u8 {
    for (0..count) |i| { buf[i] = ch; }
    return buf[0..count];
}

fn describe_number(n: i64) []const u8 {
    if (n < 0) return "negative";
    if (n == 0) return "zero";
    if (n < 10) return "small";
    if (n < 100) return "medium";
    return "large";
}

fn classify_char(ch: u8) []const u8 {
    if (ch >= '0' and ch <= '9') return "digit";
    if (ch >= 'a' and ch <= 'z') return "lower";
    if (ch >= 'A' and ch <= 'Z') return "upper";
    return "other";
}

// Point operations

fn point_add(p1: Point, p2: Point) Point { return .{ .x = p1.x + p2.x, .y = p1.y + p2.y }; }
fn point_scale(p: Point, s: i64) Point { return .{ .x = p.x * s, .y = p.y * s }; }
fn point_manhattan(p1: Point, p2: Point) i64 { return abs_val(p1.x - p2.x) + abs_val(p1.y - p2.y); }
fn point_eq(p1: Point, p2: Point) bool { return p1.x == p2.x and p1.y == p2.y; }

// Vec3 operations

fn vec3_add(a: Vec3f, b: Vec3f) Vec3f { return .{ .x = a.x + b.x, .y = a.y + b.y, .z = a.z + b.z }; }
fn vec3_scale(v: Vec3f, s: f64) Vec3f { return .{ .x = v.x * s, .y = v.y * s, .z = v.z * s }; }
fn vec3_dot(a: Vec3f, b: Vec3f) f64 { return a.x * b.x + a.y * b.y + a.z * b.z; }
fn vec3_cross(a: Vec3f, b: Vec3f) Vec3f {
    return .{ .x = a.y * b.z - a.z * b.y, .y = a.z * b.x - a.x * b.z, .z = a.x * b.y - a.y * b.x };
}

// Color operations

fn color_blend(c1: Color, c2: Color) Color {
    return .{
        .r = @divTrunc(c1.r + c2.r, 2),
        .g = @divTrunc(c1.g + c2.g, 2),
        .b = @divTrunc(c1.b + c2.b, 2),
        .a = @divTrunc(c1.a + c2.a, 2),
    };
}

fn color_brightness(c: Color) i64 { return @divTrunc(c.r * 299 + c.g * 587 + c.b * 114, 1000); }

// Rect operations

fn rect_area(r: Rect) i64 { return r.width * r.height; }
fn rect_perimeter(r: Rect) i64 { return 2 * (r.width + r.height); }
fn rect_contains(r: Rect, px: i64, py: i64) bool {
    return px >= r.x and px < r.x + r.width and py >= r.y and py < r.y + r.height;
}

// Stats operations

fn stats_average(s: Stats) f64 { return if (s.count == 0) 0.0 else s.total / @as(f64, @floatFromInt(s.count)); }
fn stats_range_val(s: Stats) f64 { return s.max_val - s.min_val; }

// List operations (using arrays)

fn sum_array(items: []const i64) i64 {
    var total: i64 = 0;
    for (items) |item| { total += item; }
    return total;
}

fn max_array(items: []const i64) i64 {
    var best: i64 = items[0];
    for (items[1..]) |item| { if (item > best) best = item; }
    return best;
}

fn min_array(items: []const i64) i64 {
    var best: i64 = items[0];
    for (items[1..]) |item| { if (item < best) best = item; }
    return best;
}

// Matrix operations

fn matrix2_mul(a: Matrix2, b: Matrix2) Matrix2 {
    return .{
        .a = a.a * b.a + a.b * b.c,
        .b = a.a * b.b + a.b * b.d,
        .c = a.c * b.a + a.d * b.c,
        .d = a.c * b.b + a.d * b.d,
    };
}

fn matrix2_det(m: Matrix2) f64 { return m.a * m.d - m.b * m.c; }

// Validation functions

fn validate_point(p: Point) bool { return p.x >= 0 and p.y >= 0; }
fn validate_color(c: Color) bool {
    return c.r >= 0 and c.r <= 255 and c.g >= 0 and c.g <= 255 and
        c.b >= 0 and c.b <= 255 and c.a >= 0 and c.a <= 255;
}
fn validate_rect(r: Rect) bool { return r.width > 0 and r.height > 0; }

// Main function

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    // Arithmetic
    const a = add_vals(10, 20);
    const b = sub_vals(50, 17);
    const c = mul_vals(6, 7);
    const d = div_safe(100, 3);
    const e = modulo(17, 5);
    _ = negate_val(42);
    const g = abs_val(-99);
    const h = max_of(33, 77);
    const i_val = min_of(33, 77);
    const j = clamp_val(150, 0, 100);

    // Float arithmetic
    _ = fadd(1.5, 2.5);
    _ = fsub(10.0, 3.7);
    _ = fmul(2.0, 3.14);
    _ = fdiv(22.0, 7.0);
    _ = fabs_val(-5.5);
    _ = fmax_of(1.1, 2.2);
    _ = fmin_of(1.1, 2.2);
    _ = lerp(0.0, 100.0, 0.25);

    // Recursive
    const fib_val = fibonacci(20);
    const fact_val = factorial(10);
    const pow_val = power_val(2, 10);
    const gcd_val = gcd(48, 18);
    const sr = sum_range(1, 100);

    // Loops
    const st = sum_to(100);
    const pt = product_to(10);
    const cd = count_divisors(60);
    _ = is_prime(97);
    const cp = count_primes(100);
    const cl = collatz_length(27);
    const dr = digital_root(9999);

    // String
    var star_buf: [32]u8 = undefined;
    _ = repeat_char(&star_buf, '*', 10);
    _ = describe_number(50);
    _ = classify_char('A');

    // Points
    const p1 = Point{ .x = 1, .y = 2 };
    const p2 = Point{ .x = 3, .y = 4 };
    _ = point_add(p1, p2);
    _ = point_scale(p1, 3);
    const dist = point_manhattan(p1, p2);
    _ = point_eq(p1, p2);

    // Vec3
    const v1 = Vec3f{ .x = 1.0, .y = 0.0, .z = 0.0 };
    const v2 = Vec3f{ .x = 0.0, .y = 1.0, .z = 0.0 };
    _ = vec3_add(v1, v2);
    _ = vec3_scale(v1, 2.5);
    _ = vec3_dot(v1, v2);
    _ = vec3_cross(v1, v2);

    // Colors
    const red = Color{ .r = 255, .g = 0, .b = 0, .a = 255 };
    const blue = Color{ .r = 0, .g = 0, .b = 255, .a = 255 };
    _ = color_blend(red, blue);
    const bright = color_brightness(red);

    // Rects
    const r1 = Rect{ .x = 0, .y = 0, .width = 100, .height = 50 };
    const area = rect_area(r1);
    const peri = rect_perimeter(r1);
    _ = rect_contains(r1, 50, 25);

    // Stats
    const s = Stats{ .count = 5, .total = 25.0, .min_val = 1.0, .max_val = 10.0 };
    _ = stats_average(s);
    _ = stats_range_val(s);

    // Matrix
    const m1 = Matrix2{ .a = 1.0, .b = 2.0, .c = 3.0, .d = 4.0 };
    const m2 = Matrix2{ .a = 5.0, .b = 6.0, .c = 7.0, .d = 8.0 };
    _ = matrix2_mul(m1, m2);
    _ = matrix2_det(m1);

    // Lists
    const nums = [_]i64{ 5, 3, 8, 1, 9, 2, 7, 4, 6, 10 };
    const total = sum_array(&nums);
    const mx = max_array(&nums);
    const mn = min_array(&nums);

    // Validation
    _ = validate_point(p1);
    _ = validate_color(red);
    _ = validate_rect(r1);

    // Checksum
    const checksum = a + b + c + d + e + g + h + i_val + j +
        fib_val + fact_val + pow_val + gcd_val + sr + st + pt +
        cd + cp + cl + dr + dist + bright + area + peri + total + mx + mn;
    try stdout.print("{}\n", .{checksum});
}
