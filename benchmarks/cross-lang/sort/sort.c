#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static int64_t seed = 42;

static int64_t next_rand(void) {
    seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
    return seed;
}

static void swap(int64_t *arr, int64_t i, int64_t j) {
    int64_t tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
}

static int64_t partition(int64_t *arr, int64_t low, int64_t high) {
    int64_t pivot = arr[high];
    int64_t i = low - 1;
    for (int64_t j = low; j < high; j++) {
        if (arr[j] <= pivot) {
            i++;
            swap(arr, i, j);
        }
    }
    swap(arr, i + 1, high);
    return i + 1;
}

static void quicksort(int64_t *arr, int64_t low, int64_t high) {
    if (low < high) {
        int64_t p = partition(arr, low, high);
        quicksort(arr, low, p - 1);
        quicksort(arr, p + 1, high);
    }
}

int main(void) {
    int64_t n = 2000000;
    int64_t *arr = (int64_t *)malloc(n * sizeof(int64_t));

    /* Generate random numbers */
    for (int64_t i = 0; i < n; i++) {
        arr[i] = next_rand();
    }

    /* Sort */
    quicksort(arr, 0, n - 1);

    /* Print first 5 sorted elements */
    for (int i = 0; i < 5; i++) {
        printf("sorted[%d]: %ld\n", i, arr[i]);
    }

    /* Compute checksum: sum of first 1000 elements */
    int64_t checksum = 0;
    for (int i = 0; i < 1000; i++) {
        checksum += arr[i];
    }
    printf("checksum: %ld\n", checksum);

    free(arr);
    return 0;
}
