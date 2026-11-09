// Mandelbrot set iteration count over a 4000x4000 grid
// Tests tight floating-point loops

const width = 4000;
const height = 4000;
const xmin = -1.5;
const xmax = 0.5;
const ymin = -1.0;
const ymax = 1.0;
const maxIter = 50;

let total = 0;
for (let py = 0; py < height; py++) {
    for (let px = 0; px < width; px++) {
        const cr = xmin + (px / width) * (xmax - xmin);
        const ci = ymin + (py / height) * (ymax - ymin);
        let zr = 0.0;
        let zi = 0.0;
        let iter = 0;
        while (zr * zr + zi * zi <= 4.0 && iter < maxIter) {
            const newZr = zr * zr - zi * zi + cr;
            zi = 2.0 * zr * zi + ci;
            zr = newZr;
            iter++;
        }
        total += iter;
    }
}
console.log(total);
