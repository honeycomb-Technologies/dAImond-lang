#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

static long long fib(int n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

typedef struct {
    int n;
    long long result;
} WorkerArg;

static void *worker(void *arg) {
    WorkerArg *wa = (WorkerArg *)arg;
    wa->result = fib(wa->n);
    return NULL;
}

int main(void) {
    pthread_t threads[4];
    WorkerArg args[4];

    args[0].n = 35;
    args[1].n = 36;
    args[2].n = 37;
    args[3].n = 38;

    for (int i = 0; i < 4; i++) {
        pthread_create(&threads[i], NULL, worker, &args[i]);
    }

    for (int i = 0; i < 4; i++) {
        pthread_join(threads[i], NULL);
    }

    long long sum = 0;
    for (int i = 0; i < 4; i++) {
        sum += args[i].result;
    }

    printf("%lld\n", sum);
    return 0;
}
