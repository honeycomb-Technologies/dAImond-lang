/* String Concatenation Benchmark
 * Concatenate a single character 100,000 times
 * Tests string allocation overhead
 *
 * Uses realloc-based string building (C's natural approach).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    long n = 100000;
    long len = 0;
    long cap = 16;
    char *s = (char *)malloc(cap);
    s[0] = '\0';

    for (long i = 0; i < n; i++) {
        if (len + 1 >= cap) {
            cap *= 2;
            s = (char *)realloc(s, cap);
        }
        s[len] = 'a';
        len++;
        s[len] = '\0';
    }

    printf("length: %ld\n", len);

    free(s);
    return 0;
}
