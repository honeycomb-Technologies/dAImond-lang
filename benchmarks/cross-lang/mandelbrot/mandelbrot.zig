// Mandelbrot set iteration count over a 4000x4000 grid
// Tests tight floating-point loops

const std = @import("std");

pub fn main() !void {
    const width: i64 = 4000;
    const height: i64 = 4000;
    const xmin: f64 = -1.5;
    const xmax: f64 = 0.5;
    const ymin: f64 = -1.0;
    const ymax: f64 = 1.0;
    const max_iter: i64 = 50;

    var total: i64 = 0;
    var py: i64 = 0;
    while (py < height) : (py += 1) {
        var px: i64 = 0;
        while (px < width) : (px += 1) {
            const cr = xmin + (@as(f64, @floatFromInt(px)) / @as(f64, @floatFromInt(width))) * (xmax - xmin);
            const ci = ymin + (@as(f64, @floatFromInt(py)) / @as(f64, @floatFromInt(height))) * (ymax - ymin);
            var zr: f64 = 0.0;
            var zi: f64 = 0.0;
            var iter: i64 = 0;
            while (zr * zr + zi * zi <= 4.0 and iter < max_iter) {
                const new_zr = zr * zr - zi * zi + cr;
                zi = 2.0 * zr * zi + ci;
                zr = new_zr;
                iter += 1;
            }
            total += iter;
        }
    }
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{total});
}
