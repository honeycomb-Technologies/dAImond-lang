// Mandelbrot set iteration count over a 4000x4000 grid
// Tests tight floating-point loops

package main

import "fmt"

func main() {
	width := 4000
	height := 4000
	xmin := -1.5
	xmax := 0.5
	ymin := -1.0
	ymax := 1.0
	maxIter := 50

	total := int64(0)
	for py := 0; py < height; py++ {
		for px := 0; px < width; px++ {
			cr := xmin + (float64(px)/float64(width))*(xmax-xmin)
			ci := ymin + (float64(py)/float64(height))*(ymax-ymin)
			zr := 0.0
			zi := 0.0
			iter := 0
			for zr*zr+zi*zi <= 4.0 && iter < maxIter {
				newZr := zr*zr - zi*zi + cr
				zi = 2.0*zr*zi + ci
				zr = newZr
				iter++
			}
			total += int64(iter)
		}
	}
	fmt.Println(total)
}
