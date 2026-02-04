module math

-- Standard Math Module
-- Wraps C math.h functions via extern fn declarations.
-- Requires -lm linking flag (automatically included by compiler).

extern fn sin(x: float) -> float
extern fn cos(x: float) -> float
extern fn tan(x: float) -> float
extern fn asin(x: float) -> float
extern fn acos(x: float) -> float
extern fn atan(x: float) -> float
extern fn atan2(y: float, x: float) -> float
extern fn sqrt(x: float) -> float
extern fn cbrt(x: float) -> float
extern fn log(x: float) -> float
extern fn log2(x: float) -> float
extern fn log10(x: float) -> float
extern fn exp(x: float) -> float
extern fn exp2(x: float) -> float
extern fn pow(base: float, exponent: float) -> float
extern fn floor(x: float) -> float
extern fn ceil(x: float) -> float
extern fn round(x: float) -> float
extern fn fabs(x: float) -> float
extern fn fmod(x: float, y: float) -> float

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
