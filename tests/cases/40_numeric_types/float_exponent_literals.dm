module main

fn main() -> int {
    let avogadro = 6.02214076e23
    let small = 1.5e-3
    let plain = 2e3
    let upper = 4.84143144246472090E+00
    println(float_to_string(avogadro))
    println(float_to_string(small))
    println(float_to_string(plain))
    println(float_to_string(upper))
    println(float_to_string(-1.16032004402742839e+00 * 2.0))
    -- A number followed by a name that begins with e is not an exponent
    let e = 5
    let xs = [1, 2, 3]
    println(int_to_string(xs[1] + e))
    return 0
}
