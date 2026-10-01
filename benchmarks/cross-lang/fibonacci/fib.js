// Naive recursive fibonacci
// Computes fib(40) to test function call overhead

function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

const result = fib(40);
console.log(result);
