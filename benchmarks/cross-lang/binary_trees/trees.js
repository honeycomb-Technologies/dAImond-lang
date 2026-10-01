// Binary Trees Benchmark
// Allocate and traverse complete binary trees of depth 21
// Tests allocation/deallocation performance

function makeTree(depth) {
    if (depth > 0) {
        return {
            left: makeTree(depth - 1),
            right: makeTree(depth - 1),
        };
    }
    return { left: null, right: null };
}

function countNodes(node) {
    if (node === null) {
        return 0;
    }
    return 1 + countNodes(node.left) + countNodes(node.right);
}

const maxDepth = 21;
const iterations = 10;
let count = 0;

for (let i = 0; i < iterations; i++) {
    const tree = makeTree(maxDepth);
    count = countNodes(tree);
}

console.log(`nodes: ${count}`);
console.log(`iterations: ${iterations}`);
