/* C Comparison Benchmark: Matrix Multiplication */
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    int n = 500;
    int total = n * n;

    double* a = malloc(total * sizeof(double));
    double* b = malloc(total * sizeof(double));
    double* c = calloc(total, sizeof(double));

    /* Initialize A and B */
    for (int i = 0; i < total; i++) {
        a[i] = (i % n) * 0.01;
        b[i] = (i / n) * 0.01;
    }

    /* Matrix multiply: C = A * B */
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            double sum = 0.0;
            for (int k = 0; k < n; k++) {
                sum += a[i * n + k] * b[k * n + j];
            }
            c[i * n + j] = sum;
        }
    }

    /* Print checksum */
    double checksum = 0.0;
    for (int i = 0; i < total; i++) {
        checksum += c[i];
    }
    printf("checksum = %g\n", checksum);

    free(a);
    free(b);
    free(c);
    return 0;
}
