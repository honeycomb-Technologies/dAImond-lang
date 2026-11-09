module compile_speed_sample

-- Struct definitions

struct Point {
    x: int,
    y: int
}

struct Vec3 {
    x: float,
    y: float,
    z: float
}

struct Color {
    r: int,
    g: int,
    b: int,
    a: int
}

struct Rect {
    x: int,
    y: int,
    width: int,
    height: int
}

struct Circle {
    cx: float,
    cy: float,
    radius: float
}

struct Matrix2 {
    a: float,
    b: float,
    c: float,
    d: float
}

struct Person {
    name: string,
    age: int,
    score: float
}

struct Config {
    width: int,
    height: int,
    title: string,
    fullscreen: bool
}

struct Stats {
    count: int,
    total: float,
    min_val: float,
    max_val: float
}

struct Entry {
    key: string,
    value: int,
    active: bool
}

-- Arithmetic functions

fn add(a: int, b: int) -> int {
    return a + b
}

fn sub(a: int, b: int) -> int {
    return a - b
}

fn mul(a: int, b: int) -> int {
    return a * b
}

fn div_safe(a: int, b: int) -> int {
    if b == 0 { return 0 }
    return a / b
}

fn modulo(a: int, b: int) -> int {
    if b == 0 { return 0 }
    return a % b
}

fn negate(a: int) -> int {
    return 0 - a
}

fn abs_val(a: int) -> int {
    if a < 0 { return 0 - a }
    return a
}

fn max_of(a: int, b: int) -> int {
    if a > b { return a }
    return b
}

fn min_of(a: int, b: int) -> int {
    if a < b { return a }
    return b
}

fn clamp(val: int, lo: int, hi: int) -> int {
    if val < lo { return lo }
    if val > hi { return hi }
    return val
}

-- Float arithmetic

fn fadd(a: float, b: float) -> float {
    return a + b
}

fn fsub(a: float, b: float) -> float {
    return a - b
}

fn fmul(a: float, b: float) -> float {
    return a * b
}

fn fdiv(a: float, b: float) -> float {
    if b == 0.0 { return 0.0 }
    return a / b
}

fn fabs_val(a: float) -> float {
    if a < 0.0 { return 0.0 - a }
    return a
}

fn fmax_of(a: float, b: float) -> float {
    if a > b { return a }
    return b
}

fn fmin_of(a: float, b: float) -> float {
    if a < b { return a }
    return b
}

fn lerp(a: float, b: float, t: float) -> float {
    return a + (b - a) * t
}

-- Recursive functions

fn fibonacci(n: int) -> int {
    if n <= 1 { return n }
    return fibonacci(n - 1) + fibonacci(n - 2)
}

fn factorial(n: int) -> int {
    if n <= 1 { return 1 }
    return n * factorial(n - 1)
}

fn power(base: int, exp: int) -> int {
    if exp == 0 { return 1 }
    return base * power(base, exp - 1)
}

fn gcd(a: int, b: int) -> int {
    if b == 0 { return a }
    return gcd(b, a % b)
}

fn sum_range(lo: int, hi: int) -> int {
    if lo > hi { return 0 }
    return lo + sum_range(lo + 1, hi)
}

-- Loop-based functions

fn sum_to(n: int) -> int {
    let mut total = 0
    let mut i = 0
    while i <= n {
        total = total + i
        i = i + 1
    }
    return total
}

fn product_to(n: int) -> int {
    let mut result = 1
    let mut i = 1
    while i <= n {
        result = result * i
        i = i + 1
    }
    return result
}

fn count_divisors(n: int) -> int {
    let mut count = 0
    let mut i = 1
    while i <= n {
        if n % i == 0 {
            count = count + 1
        }
        i = i + 1
    }
    return count
}

fn is_prime(n: int) -> bool {
    if n < 2 { return false }
    let mut i = 2
    while i * i <= n {
        if n % i == 0 { return false }
        i = i + 1
    }
    return true
}

fn count_primes(limit: int) -> int {
    let mut count = 0
    let mut n = 2
    while n <= limit {
        if is_prime(n) {
            count = count + 1
        }
        n = n + 1
    }
    return count
}

fn collatz_length(n: int) -> int {
    let mut steps = 0
    let mut val = n
    while val != 1 {
        if val % 2 == 0 {
            val = val / 2
        } else {
            val = val * 3 + 1
        }
        steps = steps + 1
    }
    return steps
}

fn digital_root(n: int) -> int {
    let mut val = n
    while val >= 10 {
        let mut sum = 0
        let mut tmp = val
        while tmp > 0 {
            sum = sum + tmp % 10
            tmp = tmp / 10
        }
        val = sum
    }
    return val
}

-- String-related functions

fn repeat_char(ch: string, count: int) -> string {
    let mut result = ""
    let mut i = 0
    while i < count {
        result = result + ch
        i = i + 1
    }
    return result
}

fn int_to_padded(n: int, width: int) -> string {
    let s = int_to_string(n)
    let slen = len(s)
    if slen >= width { return s }
    return repeat_char("0", width - slen) + s
}

fn describe_number(n: int) -> string {
    if n < 0 { return "negative" }
    if n == 0 { return "zero" }
    if n < 10 { return "small" }
    if n < 100 { return "medium" }
    return "large"
}

fn classify_char(ch: string) -> string {
    if ch >= "0" and ch <= "9" { return "digit" }
    if ch >= "a" and ch <= "z" { return "lower" }
    if ch >= "A" and ch <= "Z" { return "upper" }
    return "other"
}

-- Point operations

fn point_add(p1: Point, p2: Point) -> Point {
    return Point { x: p1.x + p2.x, y: p1.y + p2.y }
}

fn point_scale(p: Point, s: int) -> Point {
    return Point { x: p.x * s, y: p.y * s }
}

fn point_manhattan(p1: Point, p2: Point) -> int {
    return abs_val(p1.x - p2.x) + abs_val(p1.y - p2.y)
}

fn point_eq(p1: Point, p2: Point) -> bool {
    return p1.x == p2.x and p1.y == p2.y
}

-- Vec3 operations

fn vec3_add(a: Vec3, b: Vec3) -> Vec3 {
    return Vec3 { x: a.x + b.x, y: a.y + b.y, z: a.z + b.z }
}

fn vec3_scale(v: Vec3, s: float) -> Vec3 {
    return Vec3 { x: v.x * s, y: v.y * s, z: v.z * s }
}

fn vec3_dot(a: Vec3, b: Vec3) -> float {
    return a.x * b.x + a.y * b.y + a.z * b.z
}

fn vec3_cross(a: Vec3, b: Vec3) -> Vec3 {
    return Vec3 {
        x: a.y * b.z - a.z * b.y,
        y: a.z * b.x - a.x * b.z,
        z: a.x * b.y - a.y * b.x
    }
}

-- Color operations

fn color_blend(c1: Color, c2: Color) -> Color {
    return Color {
        r: (c1.r + c2.r) / 2,
        g: (c1.g + c2.g) / 2,
        b: (c1.b + c2.b) / 2,
        a: (c1.a + c2.a) / 2
    }
}

fn color_brightness(c: Color) -> int {
    return (c.r * 299 + c.g * 587 + c.b * 114) / 1000
}

-- Rect operations

fn rect_area(r: Rect) -> int {
    return r.width * r.height
}

fn rect_perimeter(r: Rect) -> int {
    return 2 * (r.width + r.height)
}

fn rect_contains(r: Rect, px: int, py: int) -> bool {
    return px >= r.x and px < r.x + r.width and py >= r.y and py < r.y + r.height
}

-- Stats operations

fn stats_average(s: Stats) -> float {
    if s.count == 0 { return 0.0 }
    return s.total / s.count as float
}

fn stats_range(s: Stats) -> float {
    return s.max_val - s.min_val
}

-- Sorting helpers

fn bubble_sort_step(arr: List[int]) -> List[int] {
    let mut result: List[int] = []
    let mut i = 0
    while i < arr.len() {
        result.push(arr[i])
        i = i + 1
    }
    let n = result.len()
    let mut j = 0
    while j < n - 1 {
        if result[j] > result[j + 1] {
            let tmp = result[j]
            result[j] = result[j + 1]
            result[j + 1] = tmp
        }
        j = j + 1
    }
    return result
}

-- List operations

fn sum_list(items: List[int]) -> int {
    let mut total = 0
    let mut i = 0
    while i < items.len() {
        total = total + items[i]
        i = i + 1
    }
    return total
}

fn max_list(items: List[int]) -> int {
    let mut best = items[0]
    let mut i = 1
    while i < items.len() {
        if items[i] > best {
            best = items[i]
        }
        i = i + 1
    }
    return best
}

fn min_list(items: List[int]) -> int {
    let mut best = items[0]
    let mut i = 1
    while i < items.len() {
        if items[i] < best {
            best = items[i]
        }
        i = i + 1
    }
    return best
}

fn reverse_list(items: List[int]) -> List[int] {
    let mut result: List[int] = []
    let mut i = items.len() - 1
    while i >= 0 {
        result.push(items[i])
        i = i - 1
    }
    return result
}

-- Matrix operations

fn matrix2_mul(a: Matrix2, b: Matrix2) -> Matrix2 {
    return Matrix2 {
        a: a.a * b.a + a.b * b.c,
        b: a.a * b.b + a.b * b.d,
        c: a.c * b.a + a.d * b.c,
        d: a.c * b.b + a.d * b.d
    }
}

fn matrix2_det(m: Matrix2) -> float {
    return m.a * m.d - m.b * m.c
}

-- Validation functions

fn validate_point(p: Point) -> bool {
    return p.x >= 0 and p.y >= 0
}

fn validate_color(c: Color) -> bool {
    return c.r >= 0 and c.r <= 255 and c.g >= 0 and c.g <= 255 and c.b >= 0 and c.b <= 255 and c.a >= 0 and c.a <= 255
}

fn validate_rect(r: Rect) -> bool {
    return r.width > 0 and r.height > 0
}

-- Main function that exercises everything

fn main() {
    -- Arithmetic
    let a = add(10, 20)
    let b = sub(50, 17)
    let c = mul(6, 7)
    let d = div_safe(100, 3)
    let e = modulo(17, 5)
    let f = negate(42)
    let g = abs_val(0 - 99)
    let h = max_of(33, 77)
    let i_val = min_of(33, 77)
    let j = clamp(150, 0, 100)

    -- Float arithmetic
    let fa = fadd(1.5, 2.5)
    let fb = fsub(10.0, 3.7)
    let fc = fmul(2.0, 3.14)
    let fd = fdiv(22.0, 7.0)
    let fe = fabs_val(0.0 - 5.5)
    let ff = fmax_of(1.1, 2.2)
    let fg = fmin_of(1.1, 2.2)
    let fh = lerp(0.0, 100.0, 0.25)

    -- Recursive
    let fib_val = fibonacci(20)
    let fact_val = factorial(10)
    let pow_val = power(2, 10)
    let gcd_val = gcd(48, 18)
    let sr = sum_range(1, 100)

    -- Loops
    let st = sum_to(100)
    let pt = product_to(10)
    let cd = count_divisors(60)
    let ip = is_prime(97)
    let cp = count_primes(100)
    let cl = collatz_length(27)
    let dr = digital_root(9999)

    -- String
    let stars = repeat_char("*", 10)
    let padded = int_to_padded(42, 6)
    let desc = describe_number(50)
    let cls = classify_char("A")

    -- Points
    let p1 = Point { x: 1, y: 2 }
    let p2 = Point { x: 3, y: 4 }
    let p3 = point_add(p1, p2)
    let p4 = point_scale(p1, 3)
    let dist = point_manhattan(p1, p2)
    let peq = point_eq(p1, p2)

    -- Vec3
    let v1 = Vec3 { x: 1.0, y: 0.0, z: 0.0 }
    let v2 = Vec3 { x: 0.0, y: 1.0, z: 0.0 }
    let v3 = vec3_add(v1, v2)
    let v4 = vec3_scale(v1, 2.5)
    let dot = vec3_dot(v1, v2)
    let cross = vec3_cross(v1, v2)

    -- Colors
    let red = Color { r: 255, g: 0, b: 0, a: 255 }
    let blue = Color { r: 0, g: 0, b: 255, a: 255 }
    let mixed = color_blend(red, blue)
    let bright = color_brightness(red)

    -- Rects
    let r1 = Rect { x: 0, y: 0, width: 100, height: 50 }
    let area = rect_area(r1)
    let peri = rect_perimeter(r1)
    let inside = rect_contains(r1, 50, 25)

    -- Stats
    let s = Stats { count: 5, total: 25.0, min_val: 1.0, max_val: 10.0 }
    let avg = stats_average(s)
    let rng = stats_range(s)

    -- Matrix
    let m1 = Matrix2 { a: 1.0, b: 2.0, c: 3.0, d: 4.0 }
    let m2 = Matrix2 { a: 5.0, b: 6.0, c: 7.0, d: 8.0 }
    let m3 = matrix2_mul(m1, m2)
    let det = matrix2_det(m1)

    -- Lists
    let mut nums: List[int] = []
    nums.push(5)
    nums.push(3)
    nums.push(8)
    nums.push(1)
    nums.push(9)
    nums.push(2)
    nums.push(7)
    nums.push(4)
    nums.push(6)
    nums.push(10)
    let total = sum_list(nums)
    let mx = max_list(nums)
    let mn = min_list(nums)
    let rev = reverse_list(nums)

    -- Validation
    let vp = validate_point(p1)
    let vc = validate_color(red)
    let vr = validate_rect(r1)

    -- Output a checksum of sorts
    let checksum = a + b + c + d + e + g + h + i_val + j + fib_val + fact_val + pow_val + gcd_val + sr + st + pt + cd + cp + cl + dr + dist + bright + area + peri + total + mx + mn
    println(int_to_string(checksum))
}
