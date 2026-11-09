module mandelbrot_bench

-- Mandelbrot set iteration count over a 4000x4000 grid
-- Tests tight floating-point loops

fn main() {
    let width = 4000
    let height = 4000
    let xmin = -1.5
    let xmax = 0.5
    let ymin = -1.0
    let ymax = 1.0
    let max_iter = 50

    let mut total = 0
    let mut py = 0
    while py < height {
        let mut px = 0
        while px < width {
            let cr = xmin + (px as float / width as float) * (xmax - xmin)
            let ci = ymin + (py as float / height as float) * (ymax - ymin)
            let mut zr = 0.0
            let mut zi = 0.0
            let mut iter = 0
            while zr * zr + zi * zi <= 4.0 and iter < max_iter {
                let new_zr = zr * zr - zi * zi + cr
                zi = 2.0 * zr * zi + ci
                zr = new_zr
                iter = iter + 1
            }
            total = total + iter
            px = px + 1
        }
        py = py + 1
    }
    println(int_to_string(total))
}
