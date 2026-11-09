#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* Simple hash map with open addressing and FNV-1a hash */

#define INITIAL_CAP 2097152  /* 2^21, enough for 1M entries at 0.5 load */

typedef struct {
    char *key;
    int64_t value;
    int occupied;
} Entry;

typedef struct {
    Entry *entries;
    size_t capacity;
    size_t size;
} HashMap;

static uint64_t fnv1a(const char *key) {
    uint64_t hash = 14695981039346656037ULL;
    for (const char *p = key; *p; p++) {
        hash ^= (uint64_t)(unsigned char)*p;
        hash *= 1099511628211ULL;
    }
    return hash;
}

static void hashmap_init(HashMap *m, size_t cap) {
    m->capacity = cap;
    m->size = 0;
    m->entries = (Entry *)calloc(cap, sizeof(Entry));
}

static void hashmap_resize(HashMap *m) {
    size_t old_cap = m->capacity;
    Entry *old_entries = m->entries;
    size_t new_cap = old_cap * 2;
    m->entries = (Entry *)calloc(new_cap, sizeof(Entry));
    m->capacity = new_cap;
    m->size = 0;
    for (size_t i = 0; i < old_cap; i++) {
        if (old_entries[i].occupied) {
            /* re-insert */
            uint64_t h = fnv1a(old_entries[i].key);
            size_t idx = h % new_cap;
            while (m->entries[idx].occupied) {
                idx = (idx + 1) % new_cap;
            }
            m->entries[idx] = old_entries[i];
            m->size++;
        }
    }
    free(old_entries);
}

static void hashmap_insert(HashMap *m, const char *key, int64_t value) {
    if (m->size * 4 >= m->capacity * 3) { /* load factor 0.75 */
        hashmap_resize(m);
    }
    uint64_t h = fnv1a(key);
    size_t idx = h % m->capacity;
    while (m->entries[idx].occupied) {
        if (strcmp(m->entries[idx].key, key) == 0) {
            m->entries[idx].value = value;
            return;
        }
        idx = (idx + 1) % m->capacity;
    }
    m->entries[idx].key = strdup(key);
    m->entries[idx].value = value;
    m->entries[idx].occupied = 1;
    m->size++;
}

static int64_t hashmap_get(HashMap *m, const char *key) {
    uint64_t h = fnv1a(key);
    size_t idx = h % m->capacity;
    while (m->entries[idx].occupied) {
        if (strcmp(m->entries[idx].key, key) == 0) {
            return m->entries[idx].value;
        }
        idx = (idx + 1) % m->capacity;
    }
    return -1; /* not found */
}

static void hashmap_remove(HashMap *m, const char *key) {
    uint64_t h = fnv1a(key);
    size_t idx = h % m->capacity;
    while (m->entries[idx].occupied) {
        if (strcmp(m->entries[idx].key, key) == 0) {
            free(m->entries[idx].key);
            m->entries[idx].key = NULL;
            m->entries[idx].occupied = 0;
            m->size--;
            /* Rehash subsequent entries to fix probe chain */
            size_t next = (idx + 1) % m->capacity;
            while (m->entries[next].occupied) {
                Entry tmp = m->entries[next];
                m->entries[next].occupied = 0;
                m->entries[next].key = NULL;
                m->size--;
                /* Re-insert */
                uint64_t rh = fnv1a(tmp.key);
                size_t ri = rh % m->capacity;
                while (m->entries[ri].occupied) {
                    ri = (ri + 1) % m->capacity;
                }
                m->entries[ri] = tmp;
                m->size++;
                next = (next + 1) % m->capacity;
            }
            return;
        }
        idx = (idx + 1) % m->capacity;
    }
}

static void hashmap_free(HashMap *m) {
    for (size_t i = 0; i < m->capacity; i++) {
        if (m->entries[i].occupied) {
            free(m->entries[i].key);
        }
    }
    free(m->entries);
}

int main(void) {
    HashMap m;
    hashmap_init(&m, INITIAL_CAP);

    char key[32];

    /* Insert 1,000,000 entries */
    for (int i = 0; i < 1000000; i++) {
        snprintf(key, sizeof(key), "key_%d", i);
        hashmap_insert(&m, key, i);
    }
    printf("inserted: 1000000\n");

    /* Lookup all entries, sum values */
    int64_t sum = 0;
    for (int i = 0; i < 1000000; i++) {
        snprintf(key, sizeof(key), "key_%d", i);
        sum += hashmap_get(&m, key);
    }
    printf("lookup sum: %ld\n", sum);

    /* Delete entries where i % 3 == 0 */
    for (int i = 0; i < 1000000; i++) {
        if (i % 3 == 0) {
            snprintf(key, sizeof(key), "key_%d", i);
            hashmap_remove(&m, key);
        }
    }
    printf("final: %zu\n", m.size);

    hashmap_free(&m);
    return 0;
}
