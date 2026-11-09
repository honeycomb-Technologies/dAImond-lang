// String Concatenation Benchmark
// Concatenate a single character 100,000 times
// Tests string allocation overhead
//
// Uses s += "a" (JavaScript's natural approach; JS engines may optimize
// string concatenation internally).

const n = 100000;
let s = "";

for (let i = 0; i < n; i++) {
    s += "a";
}

console.log(`length: ${s.length}`);
