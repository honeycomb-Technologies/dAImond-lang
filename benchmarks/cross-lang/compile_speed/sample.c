#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Struct definitions */

typedef struct { int x, y; } Point;
typedef struct { double x, y, z; } Vec3;
typedef struct { int r, g, b, a; } Color;
typedef struct { int x, y, width, height; } Rect;
typedef struct { double cx, cy, radius; } Circle;
typedef struct { double a, b, c, d; } Matrix2;
typedef struct { char name[64]; int age; double score; } Person;
typedef struct { int width, height; char title[64]; int fullscreen; } Config;
typedef struct { int count; double total, min_val, max_val; } Stats;
typedef struct { char key[64]; int value; int active; } Entry;

/* Arithmetic functions */

int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }
int mul(int a, int b) { return a * b; }
int div_safe(int a, int b) { return b == 0 ? 0 : a / b; }
int modulo(int a, int b) { return b == 0 ? 0 : a % b; }
int negate(int a) { return -a; }
int abs_val(int a) { return a < 0 ? -a : a; }
int max_of(int a, int b) { return a > b ? a : b; }
int min_of(int a, int b) { return a < b ? a : b; }
int clamp_val(int val, int lo, int hi) {
    if (val < lo) return lo;
    if (val > hi) return hi;
    return val;
}

/* Float arithmetic */

double fl_add(double a, double b) { return a + b; }
double fl_sub(double a, double b) { return a - b; }
double fl_mul(double a, double b) { return a * b; }
double fl_div(double a, double b) { return b == 0.0 ? 0.0 : a / b; }
double fabs_val(double a) { return a < 0.0 ? -a : a; }
double fmax_of(double a, double b) { return a > b ? a : b; }
double fmin_of(double a, double b) { return a < b ? a : b; }
double lerp(double a, double b, double t) { return a + (b - a) * t; }

/* Recursive functions */

long long fibonacci(int n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

long long factorial(int n) {
    if (n <= 1) return 1;
    return (long long)n * factorial(n - 1);
}

long long power_val(int base, int exp) {
    if (exp == 0) return 1;
    return (long long)base * power_val(base, exp - 1);
}

int gcd(int a, int b) {
    if (b == 0) return a;
    return gcd(b, a % b);
}

long long sum_range(int lo, int hi) {
    if (lo > hi) return 0;
    return lo + sum_range(lo + 1, hi);
}

/* Loop-based functions */

long long sum_to(int n) {
    long long total = 0;
    for (int i = 0; i <= n; i++) total += i;
    return total;
}

long long product_to(int n) {
    long long result = 1;
    for (int i = 1; i <= n; i++) result *= i;
    return result;
}

int count_divisors(int n) {
    int count = 0;
    for (int i = 1; i <= n; i++) {
        if (n % i == 0) count++;
    }
    return count;
}

int is_prime(int n) {
    if (n < 2) return 0;
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) return 0;
    }
    return 1;
}

int count_primes(int limit) {
    int count = 0;
    for (int n = 2; n <= limit; n++) {
        if (is_prime(n)) count++;
    }
    return count;
}

int collatz_length(int n) {
    int steps = 0;
    long long val = n;
    while (val != 1) {
        if (val % 2 == 0) val /= 2;
        else val = val * 3 + 1;
        steps++;
    }
    return steps;
}

int digital_root(int n) {
    int val = n;
    while (val >= 10) {
        int sum = 0;
        int tmp = val;
        while (tmp > 0) {
            sum += tmp % 10;
            tmp /= 10;
        }
        val = sum;
    }
    return val;
}

/* String-related functions */

void repeat_char(char *buf, char ch, int count) {
    for (int i = 0; i < count; i++) buf[i] = ch;
    buf[count] = '\0';
}

void int_to_padded(char *buf, int n, int width) {
    char tmp[32];
    snprintf(tmp, sizeof(tmp), "%d", n);
    int slen = (int)strlen(tmp);
    int pad = width - slen;
    int idx = 0;
    for (int i = 0; i < pad; i++) buf[idx++] = '0';
    for (int i = 0; i < slen; i++) buf[idx++] = tmp[i];
    buf[idx] = '\0';
}

const char *describe_number(int n) {
    if (n < 0) return "negative";
    if (n == 0) return "zero";
    if (n < 10) return "small";
    if (n < 100) return "medium";
    return "large";
}

const char *classify_char(char ch) {
    if (ch >= '0' && ch <= '9') return "digit";
    if (ch >= 'a' && ch <= 'z') return "lower";
    if (ch >= 'A' && ch <= 'Z') return "upper";
    return "other";
}

/* Point operations */

Point point_add(Point p1, Point p2) { return (Point){p1.x + p2.x, p1.y + p2.y}; }
Point point_scale(Point p, int s) { return (Point){p.x * s, p.y * s}; }
int point_manhattan(Point p1, Point p2) { return abs_val(p1.x - p2.x) + abs_val(p1.y - p2.y); }
int point_eq(Point p1, Point p2) { return p1.x == p2.x && p1.y == p2.y; }

/* Vec3 operations */

Vec3 vec3_add(Vec3 a, Vec3 b) { return (Vec3){a.x+b.x, a.y+b.y, a.z+b.z}; }
Vec3 vec3_scale(Vec3 v, double s) { return (Vec3){v.x*s, v.y*s, v.z*s}; }
double vec3_dot(Vec3 a, Vec3 b) { return a.x*b.x + a.y*b.y + a.z*b.z; }
Vec3 vec3_cross(Vec3 a, Vec3 b) {
    return (Vec3){a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x};
}

/* Color operations */

Color color_blend(Color c1, Color c2) {
    return (Color){(c1.r+c2.r)/2, (c1.g+c2.g)/2, (c1.b+c2.b)/2, (c1.a+c2.a)/2};
}

int color_brightness(Color c) { return (c.r * 299 + c.g * 587 + c.b * 114) / 1000; }

/* Rect operations */

int rect_area(Rect r) { return r.width * r.height; }
int rect_perimeter(Rect r) { return 2 * (r.width + r.height); }
int rect_contains(Rect r, int px, int py) {
    return px >= r.x && px < r.x + r.width && py >= r.y && py < r.y + r.height;
}

/* Stats operations */

double stats_average(Stats s) { return s.count == 0 ? 0.0 : s.total / s.count; }
double stats_range(Stats s) { return s.max_val - s.min_val; }

/* List operations (using arrays) */

long long sum_array(int *arr, int n) {
    long long total = 0;
    for (int i = 0; i < n; i++) total += arr[i];
    return total;
}

int max_array(int *arr, int n) {
    int best = arr[0];
    for (int i = 1; i < n; i++) if (arr[i] > best) best = arr[i];
    return best;
}

int min_array(int *arr, int n) {
    int best = arr[0];
    for (int i = 1; i < n; i++) if (arr[i] < best) best = arr[i];
    return best;
}

void reverse_array(int *arr, int *out, int n) {
    for (int i = 0; i < n; i++) out[i] = arr[n - 1 - i];
}

/* Matrix operations */

Matrix2 matrix2_mul(Matrix2 a, Matrix2 b) {
    return (Matrix2){
        a.a*b.a + a.b*b.c, a.a*b.b + a.b*b.d,
        a.c*b.a + a.d*b.c, a.c*b.b + a.d*b.d
    };
}

double matrix2_det(Matrix2 m) { return m.a * m.d - m.b * m.c; }

/* Validation functions */

int validate_point(Point p) { return p.x >= 0 && p.y >= 0; }
int validate_color(Color c) {
    return c.r >= 0 && c.r <= 255 && c.g >= 0 && c.g <= 255 &&
           c.b >= 0 && c.b <= 255 && c.a >= 0 && c.a <= 255;
}
int validate_rect(Rect r) { return r.width > 0 && r.height > 0; }

/* Main function */

int main(void) {
    /* Arithmetic */
    int a = add(10, 20);
    int b = sub(50, 17);
    int c = mul(6, 7);
    int d = div_safe(100, 3);
    int e = modulo(17, 5);
    int f = negate(42);
    int g = abs_val(-99);
    int h = max_of(33, 77);
    int i_val = min_of(33, 77);
    int j = clamp_val(150, 0, 100);

    /* Float arithmetic */
    double fa = fl_add(1.5, 2.5);
    double fb = fl_sub(10.0, 3.7);
    double fc = fl_mul(2.0, 3.14);
    double fd = fl_div(22.0, 7.0);
    double fe = fabs_val(-5.5);
    double ff = fmax_of(1.1, 2.2);
    double fg = fmin_of(1.1, 2.2);
    double fh = lerp(0.0, 100.0, 0.25);

    /* Recursive */
    long long fib_val = fibonacci(20);
    long long fact_val = factorial(10);
    long long pow_val = power_val(2, 10);
    int gcd_val = gcd(48, 18);
    long long sr = sum_range(1, 100);

    /* Loops */
    long long st = sum_to(100);
    long long pt = product_to(10);
    int cd = count_divisors(60);
    int ip = is_prime(97);
    int cp = count_primes(100);
    int cl = collatz_length(27);
    int dr = digital_root(9999);

    /* String */
    char stars[32];
    repeat_char(stars, '*', 10);
    char padded[32];
    int_to_padded(padded, 42, 6);
    const char *desc = describe_number(50);
    const char *cls = classify_char('A');

    /* Points */
    Point p1 = {1, 2};
    Point p2 = {3, 4};
    Point p3 = point_add(p1, p2);
    Point p4 = point_scale(p1, 3);
    int dist = point_manhattan(p1, p2);
    int peq = point_eq(p1, p2);

    /* Vec3 */
    Vec3 v1 = {1.0, 0.0, 0.0};
    Vec3 v2 = {0.0, 1.0, 0.0};
    Vec3 v3 = vec3_add(v1, v2);
    Vec3 v4 = vec3_scale(v1, 2.5);
    double dot = vec3_dot(v1, v2);
    Vec3 cross = vec3_cross(v1, v2);

    /* Colors */
    Color red = {255, 0, 0, 255};
    Color blue = {0, 0, 255, 255};
    Color mixed = color_blend(red, blue);
    int bright = color_brightness(red);

    /* Rects */
    Rect r1 = {0, 0, 100, 50};
    int area = rect_area(r1);
    int peri = rect_perimeter(r1);
    int inside = rect_contains(r1, 50, 25);

    /* Stats */
    Stats s = {5, 25.0, 1.0, 10.0};
    double avg = stats_average(s);
    double rng = stats_range(s);

    /* Matrix */
    Matrix2 m1 = {1.0, 2.0, 3.0, 4.0};
    Matrix2 m2 = {5.0, 6.0, 7.0, 8.0};
    Matrix2 m3 = matrix2_mul(m1, m2);
    double det = matrix2_det(m1);

    /* Lists */
    int nums[] = {5, 3, 8, 1, 9, 2, 7, 4, 6, 10};
    int n_nums = 10;
    long long total = sum_array(nums, n_nums);
    int mx = max_array(nums, n_nums);
    int mn = min_array(nums, n_nums);
    int rev[10];
    reverse_array(nums, rev, n_nums);

    /* Validation */
    int vp = validate_point(p1);
    int vc = validate_color(red);
    int vr = validate_rect(r1);

    /* Checksum */
    long long checksum = a + b + c + d + e + g + h + i_val + j
        + fib_val + fact_val + pow_val + gcd_val + sr + st + pt
        + cd + cp + cl + dr + dist + bright + area + peri + total + mx + mn;
    printf("%lld\n", checksum);

    /* Suppress unused variable warnings */
    (void)f; (void)fa; (void)fb; (void)fc; (void)fd; (void)fe; (void)ff;
    (void)fg; (void)fh; (void)ip; (void)stars; (void)padded; (void)desc;
    (void)cls; (void)p3; (void)p4; (void)peq; (void)v3; (void)v4;
    (void)dot; (void)cross; (void)mixed; (void)inside; (void)avg; (void)rng;
    (void)m3; (void)det; (void)rev; (void)vp; (void)vc; (void)vr;

    return 0;
}
