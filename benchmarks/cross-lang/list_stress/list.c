/* List Stress Benchmark
 * Push 10 million integers onto a dynamic array, compute sum
 * Tests dynamic array resize performance
 */

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    long *data;
    long len;
    long cap;
} List;

void list_init(List *list) {
    list->cap = 16;
    list->len = 0;
    list->data = (long *)malloc(list->cap * sizeof(long));
}

void list_push(List *list, long value) {
    if (list->len == list->cap) {
        list->cap *= 2;
        list->data = (long *)realloc(list->data, list->cap * sizeof(long));
    }
    list->data[list->len] = value;
    list->len++;
}

void list_free(List *list) {
    free(list->data);
}

int main(void) {
    long n = 10000000;
    List items;
    list_init(&items);

    for (long i = 0; i < n; i++) {
        list_push(&items, i);
    }

    long sum = 0;
    for (long j = 0; j < items.len; j++) {
        sum += items.data[j];
    }

    printf("sum: %ld\n", sum);
    printf("len: %ld\n", items.len);

    list_free(&items);
    return 0;
}
