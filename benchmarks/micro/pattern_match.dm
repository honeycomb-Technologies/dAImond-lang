-- Micro Benchmark: Pattern Matching
-- Dispatches 1,000,000 values through conditional chains.
-- Tests branch prediction and comparison performance.

module pattern_match_bench

-- Use integer tags to simulate enum dispatch (avoids enum match codegen issue)
-- 0 = circle, 1 = rect, 2 = triangle, 3 = point

fn area(kind: int, a: float, b: float) -> float {
    if kind == 0 {
        return 3.14159 * a * a
    }
    if kind == 1 {
        return a * b
    }
    if kind == 2 {
        -- Triangle with base a, height b
        return 0.5 * a * b
    }
    return 0.0
}

fn main() {
    let mut total = 0.0
    let mut i = 0
    while i < 1000000 {
        let kind = i % 4
        if kind == 0 {
            total = total + area(0, 5.0, 0.0)
        } else {
            if kind == 1 {
                total = total + area(1, 3.0, 4.0)
            } else {
                if kind == 2 {
                    total = total + area(2, 6.0, 3.0)
                } else {
                    total = total + area(3, 0.0, 0.0)
                }
            }
        }
        i = i + 1
    }
    println("total area = " + float_to_string(total))
}
