// Compile Speed Benchmark - Rust sample (~500 lines)
// Tests compiler throughput on struct definitions, functions, loops

// Struct definitions

struct Point { x: i64, y: i64 }
struct Vec3 { x: f64, y: f64, z: f64 }
struct Color { r: i64, g: i64, b: i64, a: i64 }
struct Rect { x: i64, y: i64, width: i64, height: i64 }
struct Circle { cx: f64, cy: f64, radius: f64 }
struct Matrix2 { a: f64, b: f64, c: f64, d: f64 }
struct Person { name: String, age: i64, score: f64 }
struct Config { width: i64, height: i64, title: String, fullscreen: bool }
struct Stats { count: i64, total: f64, min_val: f64, max_val: f64 }
struct Entry { key: String, value: i64, active: bool }

// Arithmetic functions

fn add(a: i64, b: i64) -> i64 { a + b }
fn sub(a: i64, b: i64) -> i64 { a - b }
fn mul(a: i64, b: i64) -> i64 { a * b }
fn div_safe(a: i64, b: i64) -> i64 { if b == 0 { 0 } else { a / b } }
fn modulo(a: i64, b: i64) -> i64 { if b == 0 { 0 } else { a % b } }
fn negate(a: i64) -> i64 { -a }
fn abs_val(a: i64) -> i64 { if a < 0 { -a } else { a } }
fn max_of(a: i64, b: i64) -> i64 { if a > b { a } else { b } }
fn min_of(a: i64, b: i64) -> i64 { if a < b { a } else { b } }
fn clamp_val(val: i64, lo: i64, hi: i64) -> i64 {
    if val < lo { lo } else if val > hi { hi } else { val }
}

// Float arithmetic

fn fadd(a: f64, b: f64) -> f64 { a + b }
fn fsub(a: f64, b: f64) -> f64 { a - b }
fn fmul(a: f64, b: f64) -> f64 { a * b }
fn fdiv(a: f64, b: f64) -> f64 { if b == 0.0 { 0.0 } else { a / b } }
fn fabs_val(a: f64) -> f64 { if a < 0.0 { -a } else { a } }
fn fmax_of(a: f64, b: f64) -> f64 { if a > b { a } else { b } }
fn fmin_of(a: f64, b: f64) -> f64 { if a < b { a } else { b } }
fn lerp(a: f64, b: f64, t: f64) -> f64 { a + (b - a) * t }

// Recursive functions

fn fibonacci(n: i64) -> i64 {
    if n <= 1 { return n; }
    fibonacci(n - 1) + fibonacci(n - 2)
}

fn factorial(n: i64) -> i64 {
    if n <= 1 { return 1; }
    n * factorial(n - 1)
}

fn power_val(base: i64, exp: i64) -> i64 {
    if exp == 0 { return 1; }
    base * power_val(base, exp - 1)
}

fn gcd(a: i64, b: i64) -> i64 {
    if b == 0 { return a; }
    gcd(b, a % b)
}

fn sum_range(lo: i64, hi: i64) -> i64 {
    if lo > hi { return 0; }
    lo + sum_range(lo + 1, hi)
}

// Loop-based functions

fn sum_to(n: i64) -> i64 {
    let mut total: i64 = 0;
    for i in 0..=n { total += i; }
    total
}

fn product_to(n: i64) -> i64 {
    let mut result: i64 = 1;
    for i in 1..=n { result *= i; }
    result
}

fn count_divisors(n: i64) -> i64 {
    let mut count = 0;
    for i in 1..=n {
        if n % i == 0 { count += 1; }
    }
    count
}

fn is_prime(n: i64) -> bool {
    if n < 2 { return false; }
    let mut i = 2;
    while i * i <= n {
        if n % i == 0 { return false; }
        i += 1;
    }
    true
}

fn count_primes(limit: i64) -> i64 {
    let mut count = 0;
    for n in 2..=limit {
        if is_prime(n) { count += 1; }
    }
    count
}

fn collatz_length(n: i64) -> i64 {
    let mut steps = 0;
    let mut val = n;
    while val != 1 {
        if val % 2 == 0 { val /= 2; }
        else { val = val * 3 + 1; }
        steps += 1;
    }
    steps
}

fn digital_root(n: i64) -> i64 {
    let mut val = n;
    while val >= 10 {
        let mut sum = 0;
        let mut tmp = val;
        while tmp > 0 {
            sum += tmp % 10;
            tmp /= 10;
        }
        val = sum;
    }
    val
}

// String-related functions

fn repeat_char(ch: char, count: usize) -> String {
    let mut result = String::new();
    for _ in 0..count { result.push(ch); }
    result
}

fn int_to_padded(n: i64, width: usize) -> String {
    let s = n.to_string();
    let slen = s.len();
    if slen >= width { return s; }
    let pad = repeat_char('0', width - slen);
    format!("{}{}", pad, s)
}

fn describe_number(n: i64) -> &'static str {
    if n < 0 { "negative" }
    else if n == 0 { "zero" }
    else if n < 10 { "small" }
    else if n < 100 { "medium" }
    else { "large" }
}

fn classify_char(ch: char) -> &'static str {
    if ch >= '0' && ch <= '9' { "digit" }
    else if ch >= 'a' && ch <= 'z' { "lower" }
    else if ch >= 'A' && ch <= 'Z' { "upper" }
    else { "other" }
}

// Point operations

fn point_add(p1: &Point, p2: &Point) -> Point {
    Point { x: p1.x + p2.x, y: p1.y + p2.y }
}

fn point_scale(p: &Point, s: i64) -> Point {
    Point { x: p.x * s, y: p.y * s }
}

fn point_manhattan(p1: &Point, p2: &Point) -> i64 {
    abs_val(p1.x - p2.x) + abs_val(p1.y - p2.y)
}

fn point_eq(p1: &Point, p2: &Point) -> bool {
    p1.x == p2.x && p1.y == p2.y
}

// Vec3 operations

fn vec3_add(a: &Vec3, b: &Vec3) -> Vec3 {
    Vec3 { x: a.x + b.x, y: a.y + b.y, z: a.z + b.z }
}

fn vec3_scale(v: &Vec3, s: f64) -> Vec3 {
    Vec3 { x: v.x * s, y: v.y * s, z: v.z * s }
}

fn vec3_dot(a: &Vec3, b: &Vec3) -> f64 {
    a.x * b.x + a.y * b.y + a.z * b.z
}

fn vec3_cross(a: &Vec3, b: &Vec3) -> Vec3 {
    Vec3 {
        x: a.y * b.z - a.z * b.y,
        y: a.z * b.x - a.x * b.z,
        z: a.x * b.y - a.y * b.x,
    }
}

// Color operations

fn color_blend(c1: &Color, c2: &Color) -> Color {
    Color {
        r: (c1.r + c2.r) / 2,
        g: (c1.g + c2.g) / 2,
        b: (c1.b + c2.b) / 2,
        a: (c1.a + c2.a) / 2,
    }
}

fn color_brightness(c: &Color) -> i64 {
    (c.r * 299 + c.g * 587 + c.b * 114) / 1000
}

// Rect operations

fn rect_area(r: &Rect) -> i64 { r.width * r.height }
fn rect_perimeter(r: &Rect) -> i64 { 2 * (r.width + r.height) }
fn rect_contains(r: &Rect, px: i64, py: i64) -> bool {
    px >= r.x && px < r.x + r.width && py >= r.y && py < r.y + r.height
}

// Stats operations

fn stats_average(s: &Stats) -> f64 {
    if s.count == 0 { 0.0 } else { s.total / s.count as f64 }
}

fn stats_range(s: &Stats) -> f64 { s.max_val - s.min_val }

// List operations

fn sum_vec(items: &[i64]) -> i64 {
    let mut total: i64 = 0;
    for &item in items { total += item; }
    total
}

fn max_vec(items: &[i64]) -> i64 {
    let mut best = items[0];
    for &item in &items[1..] {
        if item > best { best = item; }
    }
    best
}

fn min_vec(items: &[i64]) -> i64 {
    let mut best = items[0];
    for &item in &items[1..] {
        if item < best { best = item; }
    }
    best
}

fn reverse_vec(items: &[i64]) -> Vec<i64> {
    let mut result = Vec::new();
    for i in (0..items.len()).rev() {
        result.push(items[i]);
    }
    result
}

// Matrix operations

fn matrix2_mul(a: &Matrix2, b: &Matrix2) -> Matrix2 {
    Matrix2 {
        a: a.a * b.a + a.b * b.c,
        b: a.a * b.b + a.b * b.d,
        c: a.c * b.a + a.d * b.c,
        d: a.c * b.b + a.d * b.d,
    }
}

fn matrix2_det(m: &Matrix2) -> f64 { m.a * m.d - m.b * m.c }

// Validation functions

fn validate_point(p: &Point) -> bool { p.x >= 0 && p.y >= 0 }
fn validate_color(c: &Color) -> bool {
    c.r >= 0 && c.r <= 255 && c.g >= 0 && c.g <= 255 &&
    c.b >= 0 && c.b <= 255 && c.a >= 0 && c.a <= 255
}
fn validate_rect(r: &Rect) -> bool { r.width > 0 && r.height > 0 }

// Main function

fn main() {
    // Arithmetic
    let a = add(10, 20);
    let b = sub(50, 17);
    let c = mul(6, 7);
    let d = div_safe(100, 3);
    let e = modulo(17, 5);
    let _f = negate(42);
    let g = abs_val(-99);
    let h = max_of(33, 77);
    let i_val = min_of(33, 77);
    let j = clamp_val(150, 0, 100);

    // Float arithmetic
    let _fa = fadd(1.5, 2.5);
    let _fb = fsub(10.0, 3.7);
    let _fc = fmul(2.0, 3.14);
    let _fd = fdiv(22.0, 7.0);
    let _fe = fabs_val(-5.5);
    let _ff = fmax_of(1.1, 2.2);
    let _fg = fmin_of(1.1, 2.2);
    let _fh = lerp(0.0, 100.0, 0.25);

    // Recursive
    let fib_val = fibonacci(20);
    let fact_val = factorial(10);
    let pow_val = power_val(2, 10);
    let gcd_val = gcd(48, 18);
    let sr = sum_range(1, 100);

    // Loops
    let st = sum_to(100);
    let pt = product_to(10);
    let cd = count_divisors(60);
    let _ip = is_prime(97);
    let cp = count_primes(100);
    let cl = collatz_length(27);
    let dr = digital_root(9999);

    // String
    let _stars = repeat_char('*', 10);
    let _padded = int_to_padded(42, 6);
    let _desc = describe_number(50);
    let _cls = classify_char('A');

    // Points
    let p1 = Point { x: 1, y: 2 };
    let p2 = Point { x: 3, y: 4 };
    let _p3 = point_add(&p1, &p2);
    let _p4 = point_scale(&p1, 3);
    let dist = point_manhattan(&p1, &p2);
    let _peq = point_eq(&p1, &p2);

    // Vec3
    let v1 = Vec3 { x: 1.0, y: 0.0, z: 0.0 };
    let v2 = Vec3 { x: 0.0, y: 1.0, z: 0.0 };
    let _v3 = vec3_add(&v1, &v2);
    let _v4 = vec3_scale(&v1, 2.5);
    let _dot = vec3_dot(&v1, &v2);
    let _cross = vec3_cross(&v1, &v2);

    // Colors
    let red = Color { r: 255, g: 0, b: 0, a: 255 };
    let blue = Color { r: 0, g: 0, b: 255, a: 255 };
    let _mixed = color_blend(&red, &blue);
    let bright = color_brightness(&red);

    // Rects
    let r1 = Rect { x: 0, y: 0, width: 100, height: 50 };
    let area = rect_area(&r1);
    let peri = rect_perimeter(&r1);
    let _inside = rect_contains(&r1, 50, 25);

    // Stats
    let s = Stats { count: 5, total: 25.0, min_val: 1.0, max_val: 10.0 };
    let _avg = stats_average(&s);
    let _rng = stats_range(&s);

    // Matrix
    let m1 = Matrix2 { a: 1.0, b: 2.0, c: 3.0, d: 4.0 };
    let m2 = Matrix2 { a: 5.0, b: 6.0, c: 7.0, d: 8.0 };
    let _m3 = matrix2_mul(&m1, &m2);
    let _det = matrix2_det(&m1);

    // Lists
    let nums: Vec<i64> = vec![5, 3, 8, 1, 9, 2, 7, 4, 6, 10];
    let total = sum_vec(&nums);
    let mx = max_vec(&nums);
    let mn = min_vec(&nums);
    let _rev = reverse_vec(&nums);

    // Validation
    let _vp = validate_point(&p1);
    let _vc = validate_color(&red);
    let _vr = validate_rect(&r1);

    // Checksum
    let checksum = a + b + c + d + e + g + h + i_val + j
        + fib_val + fact_val + pow_val + gcd_val + sr + st + pt
        + cd + cp + cl + dr + dist + bright + area + peri + total + mx + mn;
    println!("{}", checksum);
}
