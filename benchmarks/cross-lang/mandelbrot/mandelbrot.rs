// Mandelbrot set iteration count over a 4000x4000 grid
// Tests tight floating-point loops

fn main() {
    let width: i64 = 4000;
    let height: i64 = 4000;
    let xmin: f64 = -1.5;
    let xmax: f64 = 0.5;
    let ymin: f64 = -1.0;
    let ymax: f64 = 1.0;
    let max_iter: i64 = 50;

    let mut total: i64 = 0;
    for py in 0..height {
        for px in 0..width {
            let cr = xmin + (px as f64 / width as f64) * (xmax - xmin);
            let ci = ymin + (py as f64 / height as f64) * (ymax - ymin);
            let mut zr: f64 = 0.0;
            let mut zi: f64 = 0.0;
            let mut iter: i64 = 0;
            while zr * zr + zi * zi <= 4.0 && iter < max_iter {
                let new_zr = zr * zr - zi * zi + cr;
                zi = 2.0 * zr * zi + ci;
                zr = new_zr;
                iter += 1;
            }
            total += iter;
        }
    }
    println!("{}", total);
}
