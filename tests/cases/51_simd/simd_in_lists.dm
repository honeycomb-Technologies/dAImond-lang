module simd_in_lists

-- SIMD vectors as the elements of a list, in a struct that is an
-- element, and in a box: each lies where its type must (a multiple
-- of 16 or 32 bytes).

struct Tagged {
    tag: int,
    v: f32x4
}

struct Wide {
    v: f64x4,
    n: int
}

fn main() {
    let mut xs: List[f32x4] = []
    let mut i = 0
    while i < 20 {
        xs.push(simd_splat_f32x4(1.5))
        i = i + 1
    }
    let s = simd_add(xs[0], xs[19])
    println(float_to_string(simd_extract(s, 1) as float))

    let mut ys: List[f64x4] = []
    i = 0
    while i < 20 {
        ys.push(simd_splat_f64x4(2.5))
        i = i + 1
    }
    let t = simd_add(ys[1], ys[18])
    println(float_to_string(simd_extract(t, 3)))

    let mut zs: List[i32x8] = []
    i = 0
    while i < 9 {
        zs.push(simd_splat_i32x8(i as i32))
        i = i + 1
    }
    println(int_to_string(simd_extract(simd_add(zs[3], zs[8]), 7) as int))

    let mut vs: List[Tagged] = []
    i = 0
    while i < 9 {
        vs.push(Tagged { tag: i, v: simd_splat_f32x4(0.25) })
        i = i + 1
    }
    let u = simd_add(vs[0].v, vs[8].v)
    println(float_to_string(simd_extract(u, 0) as float) + " " + int_to_string(vs[8].tag))

    let mut ws: List[Wide] = []
    i = 0
    while i < 9 {
        ws.push(Wide { v: simd_splat_f64x4(0.5), n: i })
        i = i + 1
    }
    println(float_to_string(simd_extract(simd_mul(ws[2].v, ws[7].v), 2)) + " " + int_to_string(ws[7].n))

    -- A copy that pushes moves to elements of its own
    let mut more = xs
    more.push(simd_splat_f32x4(4.0))
    xs.push(simd_splat_f32x4(8.0))
    println(float_to_string(simd_extract(more[20], 0) as float) + " " + float_to_string(simd_extract(xs[20], 0) as float))

    let mut boxes: List[Box[Wide]] = []
    i = 0
    while i < 9 {
        boxes.push(Box_new(Wide { v: simd_splat_f64x4(1.25), n: i }))
        i = i + 1
    }
    let last = *boxes[8]
    let first = *boxes[1]
    println(float_to_string(simd_extract(simd_add(last.v, first.v), 0)) + " " + int_to_string(last.n))
}
