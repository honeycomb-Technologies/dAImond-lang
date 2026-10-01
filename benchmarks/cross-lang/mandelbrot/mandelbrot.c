/* Mandelbrot set iteration count over a 4000x4000 grid */
/* Tests tight floating-point loops */

#include <stdio.h>

int main(void) {
    int width = 4000;
    int height = 4000;
    double xmin = -1.5;
    double xmax = 0.5;
    double ymin = -1.0;
    double ymax = 1.0;
    int max_iter = 50;

    long long total = 0;
    for (int py = 0; py < height; py++) {
        for (int px = 0; px < width; px++) {
            double cr = xmin + ((double)px / (double)width) * (xmax - xmin);
            double ci = ymin + ((double)py / (double)height) * (ymax - ymin);
            double zr = 0.0;
            double zi = 0.0;
            int iter = 0;
            while (zr * zr + zi * zi <= 4.0 && iter < max_iter) {
                double new_zr = zr * zr - zi * zi + cr;
                zi = 2.0 * zr * zi + ci;
                zr = new_zr;
                iter++;
            }
            total += iter;
        }
    }
    printf("%lld\n", total);
    return 0;
}
