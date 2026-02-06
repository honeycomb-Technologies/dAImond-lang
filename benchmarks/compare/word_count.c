/* C Comparison Benchmark: Word Count */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/* Simple hash map for word counting */
#define HASH_SIZE 4096

typedef struct Entry {
    char* key;
    int count;
    struct Entry* next;
} Entry;

typedef struct {
    Entry* buckets[HASH_SIZE];
    int unique;
} WordMap;

static unsigned int hash_str(const char* s, int len) {
    unsigned int h = 5381;
    for (int i = 0; i < len; i++) {
        h = ((h << 5) + h) + (unsigned char)s[i];
    }
    return h % HASH_SIZE;
}

static void map_init(WordMap* m) {
    memset(m->buckets, 0, sizeof(m->buckets));
    m->unique = 0;
}

static void map_increment(WordMap* m, const char* word, int len) {
    unsigned int h = hash_str(word, len);
    Entry* e = m->buckets[h];
    while (e) {
        if ((int)strlen(e->key) == len && memcmp(e->key, word, len) == 0) {
            e->count++;
            return;
        }
        e = e->next;
    }
    /* New entry */
    Entry* ne = malloc(sizeof(Entry));
    ne->key = malloc(len + 1);
    memcpy(ne->key, word, len);
    ne->key[len] = '\0';
    ne->count = 1;
    ne->next = m->buckets[h];
    m->buckets[h] = ne;
    m->unique++;
}

static void map_find_max(WordMap* m, char** max_word, int* max_count) {
    *max_count = 0;
    *max_word = "";
    for (int i = 0; i < HASH_SIZE; i++) {
        Entry* e = m->buckets[i];
        while (e) {
            if (e->count > *max_count) {
                *max_count = e->count;
                *max_word = e->key;
            }
            e = e->next;
        }
    }
}

static void map_free(WordMap* m) {
    for (int i = 0; i < HASH_SIZE; i++) {
        Entry* e = m->buckets[i];
        while (e) {
            Entry* next = e->next;
            free(e->key);
            free(e);
            e = next;
        }
    }
}

int main(void) {
    const char* words = "the quick brown fox jumps over the lazy dog and the cat sat on the mat while the bird flew high in the sky above the green forest where the river flows gently through the meadow near the old stone bridge that connects the two villages";
    int words_len = strlen(words);

    /* Generate text: repeat 1000 times */
    int text_cap = (words_len + 1) * 1000 + 1;
    char* text = malloc(text_cap);
    int text_len = 0;
    for (int i = 0; i < 1000; i++) {
        if (i > 0) text[text_len++] = ' ';
        memcpy(text + text_len, words, words_len);
        text_len += words_len;
    }
    text[text_len] = '\0';

    printf("text length: %d\n", text_len);

    /* Count words */
    WordMap counts;
    map_init(&counts);
    int total_words = 0;
    int word_start = -1;

    for (int i = 0; i <= text_len; i++) {
        bool is_sep = (i == text_len) || text[i] == ' ' || text[i] == '\n' || text[i] == '\t';
        if (is_sep) {
            if (word_start >= 0) {
                map_increment(&counts, text + word_start, i - word_start);
                total_words++;
                word_start = -1;
            }
        } else {
            if (word_start < 0) word_start = i;
        }
    }

    printf("unique words: %d\n", counts.unique);
    printf("total words: %d\n", total_words);

    char* max_word;
    int max_count;
    map_find_max(&counts, &max_word, &max_count);
    printf("most frequent: \"%s\" (%d times)\n", max_word, max_count);
    printf("done, total = %d\n", total_words);

    map_free(&counts);
    free(text);
    return 0;
}
