package main

import "fmt"

// Struct definitions

type Point struct{ X, Y int }
type Vec3 struct{ X, Y, Z float64 }
type Color struct{ R, G, B, A int }
type Rect struct{ X, Y, Width, Height int }
type Circle struct{ CX, CY, Radius float64 }
type Matrix2 struct{ A, B, C, D float64 }
type Person struct{ Name string; Age int; Score float64 }
type Config struct{ Width, Height int; Title string; Fullscreen bool }
type Stats struct{ Count int; Total, MinVal, MaxVal float64 }
type Entry struct{ Key string; Value int; Active bool }

// Arithmetic functions

func add(a, b int) int        { return a + b }
func sub(a, b int) int        { return a - b }
func mul(a, b int) int        { return a * b }
func divSafe(a, b int) int    { if b == 0 { return 0 }; return a / b }
func modulo(a, b int) int     { if b == 0 { return 0 }; return a % b }
func negate(a int) int        { return -a }
func absVal(a int) int        { if a < 0 { return -a }; return a }
func maxOf(a, b int) int      { if a > b { return a }; return b }
func minOf(a, b int) int      { if a < b { return a }; return b }
func clampVal(val, lo, hi int) int {
	if val < lo { return lo }
	if val > hi { return hi }
	return val
}

// Float arithmetic

func fadd(a, b float64) float64    { return a + b }
func fsub(a, b float64) float64    { return a - b }
func fmul(a, b float64) float64    { return a * b }
func fdiv(a, b float64) float64    { if b == 0.0 { return 0.0 }; return a / b }
func fabsVal(a float64) float64    { if a < 0.0 { return -a }; return a }
func fmaxOf(a, b float64) float64  { if a > b { return a }; return b }
func fminOf(a, b float64) float64  { if a < b { return a }; return b }
func lerp(a, b, t float64) float64 { return a + (b-a)*t }

// Recursive functions

func fibonacci(n int) int {
	if n <= 1 { return n }
	return fibonacci(n-1) + fibonacci(n-2)
}

func factorial(n int) int {
	if n <= 1 { return 1 }
	return n * factorial(n-1)
}

func powerVal(base, exp int) int {
	if exp == 0 { return 1 }
	return base * powerVal(base, exp-1)
}

func gcd(a, b int) int {
	if b == 0 { return a }
	return gcd(b, a%b)
}

func sumRange(lo, hi int) int {
	if lo > hi { return 0 }
	return lo + sumRange(lo+1, hi)
}

// Loop-based functions

func sumTo(n int) int {
	total := 0
	for i := 0; i <= n; i++ { total += i }
	return total
}

func productTo(n int) int {
	result := 1
	for i := 1; i <= n; i++ { result *= i }
	return result
}

func countDivisors(n int) int {
	count := 0
	for i := 1; i <= n; i++ {
		if n%i == 0 { count++ }
	}
	return count
}

func isPrime(n int) bool {
	if n < 2 { return false }
	for i := 2; i*i <= n; i++ {
		if n%i == 0 { return false }
	}
	return true
}

func countPrimes(limit int) int {
	count := 0
	for n := 2; n <= limit; n++ {
		if isPrime(n) { count++ }
	}
	return count
}

func collatzLength(n int) int {
	steps := 0
	val := n
	for val != 1 {
		if val%2 == 0 { val /= 2 } else { val = val*3 + 1 }
		steps++
	}
	return steps
}

func digitalRoot(n int) int {
	val := n
	for val >= 10 {
		sum := 0
		tmp := val
		for tmp > 0 {
			sum += tmp % 10
			tmp /= 10
		}
		val = sum
	}
	return val
}

// String-related functions

func repeatChar(ch byte, count int) string {
	result := make([]byte, count)
	for i := 0; i < count; i++ { result[i] = ch }
	return string(result)
}

func intToPadded(n, width int) string {
	s := fmt.Sprintf("%d", n)
	if len(s) >= width { return s }
	return repeatChar('0', width-len(s)) + s
}

func describeNumber(n int) string {
	if n < 0 { return "negative" }
	if n == 0 { return "zero" }
	if n < 10 { return "small" }
	if n < 100 { return "medium" }
	return "large"
}

func classifyChar(ch byte) string {
	if ch >= '0' && ch <= '9' { return "digit" }
	if ch >= 'a' && ch <= 'z' { return "lower" }
	if ch >= 'A' && ch <= 'Z' { return "upper" }
	return "other"
}

// Point operations

func pointAdd(p1, p2 Point) Point     { return Point{p1.X + p2.X, p1.Y + p2.Y} }
func pointScale(p Point, s int) Point { return Point{p.X * s, p.Y * s} }
func pointManhattan(p1, p2 Point) int { return absVal(p1.X-p2.X) + absVal(p1.Y-p2.Y) }
func pointEq(p1, p2 Point) bool       { return p1.X == p2.X && p1.Y == p2.Y }

// Vec3 operations

func vec3Add(a, b Vec3) Vec3         { return Vec3{a.X + b.X, a.Y + b.Y, a.Z + b.Z} }
func vec3Scale(v Vec3, s float64) Vec3 { return Vec3{v.X * s, v.Y * s, v.Z * s} }
func vec3Dot(a, b Vec3) float64      { return a.X*b.X + a.Y*b.Y + a.Z*b.Z }
func vec3Cross(a, b Vec3) Vec3 {
	return Vec3{a.Y*b.Z - a.Z*b.Y, a.Z*b.X - a.X*b.Z, a.X*b.Y - a.Y*b.X}
}

// Color operations

func colorBlend(c1, c2 Color) Color {
	return Color{(c1.R + c2.R) / 2, (c1.G + c2.G) / 2, (c1.B + c2.B) / 2, (c1.A + c2.A) / 2}
}

func colorBrightness(c Color) int { return (c.R*299 + c.G*587 + c.B*114) / 1000 }

// Rect operations

func rectArea(r Rect) int      { return r.Width * r.Height }
func rectPerimeter(r Rect) int { return 2 * (r.Width + r.Height) }
func rectContains(r Rect, px, py int) bool {
	return px >= r.X && px < r.X+r.Width && py >= r.Y && py < r.Y+r.Height
}

// Stats operations

func statsAverage(s Stats) float64 {
	if s.Count == 0 { return 0.0 }
	return s.Total / float64(s.Count)
}

func statsRange(s Stats) float64 { return s.MaxVal - s.MinVal }

// List operations

func sumSlice(items []int) int {
	total := 0
	for _, item := range items { total += item }
	return total
}

func maxSlice(items []int) int {
	best := items[0]
	for _, item := range items[1:] {
		if item > best { best = item }
	}
	return best
}

func minSlice(items []int) int {
	best := items[0]
	for _, item := range items[1:] {
		if item < best { best = item }
	}
	return best
}

func reverseSlice(items []int) []int {
	result := make([]int, len(items))
	for i := 0; i < len(items); i++ {
		result[i] = items[len(items)-1-i]
	}
	return result
}

// Matrix operations

func matrix2Mul(a, b Matrix2) Matrix2 {
	return Matrix2{
		a.A*b.A + a.B*b.C, a.A*b.B + a.B*b.D,
		a.C*b.A + a.D*b.C, a.C*b.B + a.D*b.D,
	}
}

func matrix2Det(m Matrix2) float64 { return m.A*m.D - m.B*m.C }

// Validation functions

func validatePoint(p Point) bool { return p.X >= 0 && p.Y >= 0 }
func validateColor(c Color) bool {
	return c.R >= 0 && c.R <= 255 && c.G >= 0 && c.G <= 255 &&
		c.B >= 0 && c.B <= 255 && c.A >= 0 && c.A <= 255
}
func validateRect(r Rect) bool { return r.Width > 0 && r.Height > 0 }

// Main function

func main() {
	// Arithmetic
	a := add(10, 20)
	b := sub(50, 17)
	c := mul(6, 7)
	d := divSafe(100, 3)
	e := modulo(17, 5)
	_ = negate(42)
	g := absVal(-99)
	h := maxOf(33, 77)
	iVal := minOf(33, 77)
	j := clampVal(150, 0, 100)

	// Float arithmetic
	_ = fadd(1.5, 2.5)
	_ = fsub(10.0, 3.7)
	_ = fmul(2.0, 3.14)
	_ = fdiv(22.0, 7.0)
	_ = fabsVal(-5.5)
	_ = fmaxOf(1.1, 2.2)
	_ = fminOf(1.1, 2.2)
	_ = lerp(0.0, 100.0, 0.25)

	// Recursive
	fibVal := fibonacci(20)
	factVal := factorial(10)
	powVal := powerVal(2, 10)
	gcdVal := gcd(48, 18)
	sr := sumRange(1, 100)

	// Loops
	st := sumTo(100)
	pt := productTo(10)
	cd := countDivisors(60)
	_ = isPrime(97)
	cp := countPrimes(100)
	cl := collatzLength(27)
	dr := digitalRoot(9999)

	// String
	_ = repeatChar('*', 10)
	_ = intToPadded(42, 6)
	_ = describeNumber(50)
	_ = classifyChar('A')

	// Points
	p1 := Point{1, 2}
	p2 := Point{3, 4}
	_ = pointAdd(p1, p2)
	_ = pointScale(p1, 3)
	dist := pointManhattan(p1, p2)
	_ = pointEq(p1, p2)

	// Vec3
	v1 := Vec3{1.0, 0.0, 0.0}
	v2 := Vec3{0.0, 1.0, 0.0}
	_ = vec3Add(v1, v2)
	_ = vec3Scale(v1, 2.5)
	_ = vec3Dot(v1, v2)
	_ = vec3Cross(v1, v2)

	// Colors
	red := Color{255, 0, 0, 255}
	blue := Color{0, 0, 255, 255}
	_ = colorBlend(red, blue)
	bright := colorBrightness(red)

	// Rects
	r1 := Rect{0, 0, 100, 50}
	area := rectArea(r1)
	peri := rectPerimeter(r1)
	_ = rectContains(r1, 50, 25)

	// Stats
	s := Stats{5, 25.0, 1.0, 10.0}
	_ = statsAverage(s)
	_ = statsRange(s)

	// Matrix
	m1 := Matrix2{1.0, 2.0, 3.0, 4.0}
	m2 := Matrix2{5.0, 6.0, 7.0, 8.0}
	_ = matrix2Mul(m1, m2)
	_ = matrix2Det(m1)

	// Lists
	nums := []int{5, 3, 8, 1, 9, 2, 7, 4, 6, 10}
	total := sumSlice(nums)
	mx := maxSlice(nums)
	mn := minSlice(nums)
	_ = reverseSlice(nums)

	// Validation
	_ = validatePoint(p1)
	_ = validateColor(red)
	_ = validateRect(r1)

	// Checksum
	checksum := a + b + c + d + e + g + h + iVal + j +
		fibVal + factVal + powVal + gcdVal + sr + st + pt +
		cd + cp + cl + dr + dist + bright + area + peri + total + mx + mn
	fmt.Println(checksum)
}
