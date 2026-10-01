/* Naive recursive fibonacci */
/* Computes fib(40) to test function call overhead */

#include <stdio.h>

static long long fib(long long n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

int main(void) {
    long long result = fib(40);
    printf("%lld\n", result);
    return 0;
}
