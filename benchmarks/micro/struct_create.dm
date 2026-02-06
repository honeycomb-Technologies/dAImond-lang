-- Micro Benchmark: Struct Creation
-- Creates 1,000,000 struct instances.
-- Tests struct allocation and field access performance.

module struct_create_bench

struct Point {
    x: float,
    y: float,
    z: float,
}

fn distance(a: Point, b: Point) -> float {
    let dx = a.x - b.x
    let dy = a.y - b.y
    let dz = a.z - b.z
    return dx * dx + dy * dy + dz * dz
}

fn main() {
    let mut total = 0.0
    let mut i = 0
    while i < 1000000 {
        let p1 = Point { x: 1.0, y: 2.0, z: 3.0 }
        let p2 = Point { x: 4.0, y: 5.0, z: 6.0 }
        total = total + distance(p1, p2)
        i = i + 1
    }
    println("total distance^2 = " + float_to_string(total))
}
