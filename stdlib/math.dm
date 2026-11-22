module math

-- Standard Math Module
-- The functions of C's math.h, which the compiler links (-lm), as
-- functions that need no `unsafe`:
--
--   import std.math
--
--   let h = sqrt(a * a + b * b)
--   let turn = atan2(y, x) / math_tau()

extern fn libm_sin(x: float) -> float = "sin"
extern fn libm_cos(x: float) -> float = "cos"
extern fn libm_tan(x: float) -> float = "tan"
extern fn libm_asin(x: float) -> float = "asin"
extern fn libm_acos(x: float) -> float = "acos"
extern fn libm_atan(x: float) -> float = "atan"
extern fn libm_atan2(y: float, x: float) -> float = "atan2"
extern fn libm_sqrt(x: float) -> float = "sqrt"
extern fn libm_cbrt(x: float) -> float = "cbrt"
extern fn libm_log(x: float) -> float = "log"
extern fn libm_log2(x: float) -> float = "log2"
extern fn libm_log10(x: float) -> float = "log10"
extern fn libm_exp(x: float) -> float = "exp"
extern fn libm_exp2(x: float) -> float = "exp2"
extern fn libm_pow(base: float, exponent: float) -> float = "pow"
extern fn libm_floor(x: float) -> float = "floor"
extern fn libm_ceil(x: float) -> float = "ceil"
extern fn libm_round(x: float) -> float = "round"
extern fn libm_fabs(x: float) -> float = "fabs"
extern fn libm_fmod(x: float, y: float) -> float = "fmod"

-- The sine of an angle in radians
fn sin(x: float) -> float {
    unsafe { return libm_sin(x) }
}

-- The cosine of an angle in radians
fn cos(x: float) -> float {
    unsafe { return libm_cos(x) }
}

-- The tangent of an angle in radians
fn tan(x: float) -> float {
    unsafe { return libm_tan(x) }
}

-- The angle in radians whose sine is x
fn asin(x: float) -> float {
    unsafe { return libm_asin(x) }
}

-- The angle in radians whose cosine is x
fn acos(x: float) -> float {
    unsafe { return libm_acos(x) }
}

-- The angle in radians whose tangent is x
fn atan(x: float) -> float {
    unsafe { return libm_atan(x) }
}

-- The angle in radians of the point (x, y)
fn atan2(y: float, x: float) -> float {
    unsafe { return libm_atan2(y, x) }
}

-- The square root
fn sqrt(x: float) -> float {
    unsafe { return libm_sqrt(x) }
}

-- The cube root
fn cbrt(x: float) -> float {
    unsafe { return libm_cbrt(x) }
}

-- The natural logarithm
fn log(x: float) -> float {
    unsafe { return libm_log(x) }
}

-- The logarithm to base 2
fn log2(x: float) -> float {
    unsafe { return libm_log2(x) }
}

-- The logarithm to base 10
fn log10(x: float) -> float {
    unsafe { return libm_log10(x) }
}

-- e to the power x
fn exp(x: float) -> float {
    unsafe { return libm_exp(x) }
}

-- 2 to the power x
fn exp2(x: float) -> float {
    unsafe { return libm_exp2(x) }
}

-- base to the power exponent
fn pow(base: float, exponent: float) -> float {
    unsafe { return libm_pow(base, exponent) }
}

-- The largest whole number that is not above x
fn floor(x: float) -> float {
    unsafe { return libm_floor(x) }
}

-- The smallest whole number that is not below x
fn ceil(x: float) -> float {
    unsafe { return libm_ceil(x) }
}

-- The nearest whole number, halves away from zero
fn round(x: float) -> float {
    unsafe { return libm_round(x) }
}

-- x without its sign
fn fabs(x: float) -> float {
    unsafe { return libm_fabs(x) }
}

-- The remainder of x / y, with the sign of x
fn fmod(x: float, y: float) -> float {
    unsafe { return libm_fmod(x, y) }
}

-- Constants
fn math_pi() -> float {
    return 3.14159265358979323846
}

fn math_e() -> float {
    return 2.71828182845904523536
}

fn math_tau() -> float {
    return 6.28318530717958647692
}

-- Utility functions
fn math_min(a: float, b: float) -> float {
    if a < b { return a }
    return b
}

fn math_max(a: float, b: float) -> float {
    if a > b { return a }
    return b
}

fn math_clamp(x: float, lo: float, hi: float) -> float {
    if x < lo { return lo }
    if x > hi { return hi }
    return x
}

fn math_abs(x: float) -> float {
    if x < 0.0 { return 0.0 - x }
    return x
}
