// List Stress Benchmark
// Push 10 million integers onto a dynamic array, compute sum
// Tests dynamic array resize performance

const n = 10000000;
const items = [];

for (let i = 0; i < n; i++) {
    items.push(i);
}

let sum = 0;
for (let j = 0; j < items.length; j++) {
    sum += items[j];
}

console.log(`sum: ${sum}`);
console.log(`len: ${items.length}`);
