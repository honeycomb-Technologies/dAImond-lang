/**
 * dAImond Runtime Test Suite
 *
 * This file tests the dAImond C runtime library functionality.
 * Compile and run with:
 *   cc -o test_runtime test_runtime.c daimond_runtime.c -lm
 *   ./test_runtime
 */

#include "daimond_runtime.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

/* Test counters */
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name) static void test_##name(void)
#define RUN_TEST(name) do { \
    printf("Testing %s... ", #name); \
    test_##name(); \
    printf("PASSED\n"); \
    tests_passed++; \
} while(0)

#define ASSERT_TRUE(cond) do { \
    if (!(cond)) { \
        printf("FAILED at line %d: %s\n", __LINE__, #cond); \
        tests_failed++; \
        return; \
    } \
} while(0)

#define ASSERT_FALSE(cond) ASSERT_TRUE(!(cond))
#define ASSERT_EQ(a, b) ASSERT_TRUE((a) == (b))
#define ASSERT_NE(a, b) ASSERT_TRUE((a) != (b))

/* ============================================================================
 * String Tests
 * ============================================================================ */

TEST(string_new) {
    dm_string s = dm_string_new("hello");
    ASSERT_EQ(s.len, 5);
    ASSERT_EQ(s.cap, 0);  /* Static string */
    ASSERT_TRUE(memcmp(s.data, "hello", 5) == 0);
}

TEST(string_from_len) {
    dm_string s = dm_string_from_len("hello world", 5);
    ASSERT_EQ(s.len, 5);
    ASSERT_TRUE(memcmp(s.data, "hello", 5) == 0);
}

TEST(string_clone) {
    dm_string original = dm_string_new("test");
    dm_string cloned = dm_string_clone(original);

    ASSERT_EQ(cloned.len, 4);
    ASSERT_TRUE(cloned.cap > 0);  /* Owned string */
    ASSERT_TRUE(dm_string_eq(original, cloned));
    ASSERT_NE(original.data, cloned.data);  /* Different memory */

    dm_string_free(cloned);
}

TEST(string_concat) {
    dm_string a = dm_string_new("hello ");
    dm_string b = dm_string_new("world");
    dm_string result = dm_string_concat(a, b);

    ASSERT_EQ(result.len, 11);
    ASSERT_TRUE(result.cap > 0);
    ASSERT_TRUE(memcmp(result.data, "hello world", 11) == 0);

    dm_string_free(result);
}

TEST(string_eq) {
    dm_string a = dm_string_new("test");
    dm_string b = dm_string_new("test");
    dm_string c = dm_string_new("other");
    dm_string d = dm_string_new("tes");

    ASSERT_TRUE(dm_string_eq(a, b));
    ASSERT_FALSE(dm_string_eq(a, c));
    ASSERT_FALSE(dm_string_eq(a, d));
}

TEST(string_cmp) {
    dm_string a = dm_string_new("apple");
    dm_string b = dm_string_new("banana");
    dm_string c = dm_string_new("apple");

    ASSERT_TRUE(dm_string_cmp(a, b) < 0);
    ASSERT_TRUE(dm_string_cmp(b, a) > 0);
    ASSERT_EQ(dm_string_cmp(a, c), 0);
}

TEST(string_slice) {
    dm_string s = dm_string_new("hello world");
    dm_string slice = dm_string_slice(s, 0, 5);

    ASSERT_EQ(slice.len, 5);
    ASSERT_TRUE(memcmp(slice.data, "hello", 5) == 0);

    dm_string slice2 = dm_string_slice(s, 6, 11);
    ASSERT_EQ(slice2.len, 5);
    ASSERT_TRUE(memcmp(slice2.data, "world", 5) == 0);
}

TEST(string_len) {
    dm_string s = dm_string_new("test");
    ASSERT_EQ(dm_string_len(s), 4);

    dm_string empty = DM_STRING_EMPTY;
    ASSERT_EQ(dm_string_len(empty), 0);
}

TEST(string_is_empty) {
    dm_string s = dm_string_new("test");
    dm_string empty = DM_STRING_EMPTY;

    ASSERT_FALSE(dm_string_is_empty(s));
    ASSERT_TRUE(dm_string_is_empty(empty));
}

TEST(string_starts_with) {
    dm_string s = dm_string_new("hello world");
    dm_string prefix = dm_string_new("hello");
    dm_string wrong = dm_string_new("world");

    ASSERT_TRUE(dm_string_starts_with(s, prefix));
    ASSERT_FALSE(dm_string_starts_with(s, wrong));
    ASSERT_TRUE(dm_string_starts_with(s, DM_STRING_EMPTY));
}

TEST(string_ends_with) {
    dm_string s = dm_string_new("hello world");
    dm_string suffix = dm_string_new("world");
    dm_string wrong = dm_string_new("hello");

    ASSERT_TRUE(dm_string_ends_with(s, suffix));
    ASSERT_FALSE(dm_string_ends_with(s, wrong));
    ASSERT_TRUE(dm_string_ends_with(s, DM_STRING_EMPTY));
}

TEST(string_find) {
    dm_string s = dm_string_new("hello world");
    dm_string needle = dm_string_new("world");
    dm_string not_found = dm_string_new("xyz");

    ASSERT_EQ(dm_string_find(s, needle), 6);
    ASSERT_EQ(dm_string_find(s, not_found), (size_t)-1);
    ASSERT_EQ(dm_string_find(s, DM_STRING_EMPTY), 0);
}

TEST(string_char_at) {
    dm_string s = dm_string_new("hello");

    ASSERT_EQ(dm_string_char_at(s, 0), 'h');
    ASSERT_EQ(dm_string_char_at(s, 4), 'o');
    ASSERT_EQ(dm_string_char_at(s, 100), '\0');
}

TEST(string_literal) {
    dm_string s = DM_STRING_LIT("compile time");
    ASSERT_EQ(s.len, 12);
    ASSERT_EQ(s.cap, 0);
}

/* ============================================================================
 * Option Type Tests
 * ============================================================================ */

TEST(option_some) {
    dm_option_int opt = DM_SOME_VAL(dm_option_int, 42);

    ASSERT_TRUE(DM_IS_SOME(opt));
    ASSERT_FALSE(DM_IS_NONE(opt));
    ASSERT_EQ(DM_UNWRAP(opt), 42);
}

TEST(option_none) {
    dm_option_int opt = DM_NONE_VAL(dm_option_int);

    ASSERT_FALSE(DM_IS_SOME(opt));
    ASSERT_TRUE(DM_IS_NONE(opt));
}

TEST(option_unwrap_or) {
    dm_option_int some = DM_SOME_VAL(dm_option_int, 42);
    dm_option_int none = DM_NONE_VAL(dm_option_int);

    ASSERT_EQ(DM_UNWRAP_OR(some, 0), 42);
    ASSERT_EQ(DM_UNWRAP_OR(none, 99), 99);
}

TEST(option_init_macros) {
    dm_option_int opt;
    DM_OPTION_SOME(opt, 100);
    ASSERT_TRUE(DM_IS_SOME(opt));
    ASSERT_EQ(DM_UNWRAP(opt), 100);

    DM_OPTION_NONE(opt);
    ASSERT_TRUE(DM_IS_NONE(opt));
}

/* ============================================================================
 * Result Type Tests
 * ============================================================================ */

/* Define a result type for testing */
DM_DEFINE_RESULT(int, dm_string, IntStringResult);

TEST(result_ok) {
    IntStringResult res = DM_OK_VAL(IntStringResult, 42);

    ASSERT_TRUE(DM_IS_OK(res));
    ASSERT_FALSE(DM_IS_ERR(res));
    ASSERT_EQ(DM_UNWRAP_OK(res), 42);
}

TEST(result_err) {
    dm_string error = dm_string_new("error message");
    IntStringResult res = DM_ERR_VAL(IntStringResult, error);

    ASSERT_FALSE(DM_IS_OK(res));
    ASSERT_TRUE(DM_IS_ERR(res));
    ASSERT_TRUE(dm_string_eq(DM_UNWRAP_ERR(res), error));
}

TEST(result_init_macros) {
    IntStringResult res;
    DM_RESULT_OK(res, 99);
    ASSERT_TRUE(DM_IS_OK(res));
    ASSERT_EQ(DM_UNWRAP_OK(res), 99);

    dm_string err = dm_string_new("failed");
    DM_RESULT_ERR(res, err);
    ASSERT_TRUE(DM_IS_ERR(res));
    ASSERT_TRUE(dm_string_eq(DM_UNWRAP_ERR(res), err));
}

/* ============================================================================
 * Arena/Region Tests
 * ============================================================================ */

TEST(arena_create_destroy) {
    dm_arena* arena = dm_arena_create(1024);
    ASSERT_TRUE(arena != NULL);
    ASSERT_EQ(arena->size, 1024);
    ASSERT_EQ(arena->used, 0);
    ASSERT_TRUE(arena->next == NULL);

    dm_arena_destroy(arena);
}

TEST(arena_alloc) {
    dm_arena* arena = dm_arena_create(1024);

    void* ptr1 = dm_arena_alloc(arena, 100);
    ASSERT_TRUE(ptr1 != NULL);
    ASSERT_TRUE(arena->used >= 100);

    void* ptr2 = dm_arena_alloc(arena, 200);
    ASSERT_TRUE(ptr2 != NULL);
    ASSERT_TRUE(ptr2 != ptr1);

    dm_arena_destroy(arena);
}

TEST(arena_overflow) {
    dm_arena* arena = dm_arena_create(64);

    /* Allocate more than initial size to trigger overflow */
    void* ptr1 = dm_arena_alloc(arena, 100);
    ASSERT_TRUE(ptr1 != NULL);
    ASSERT_TRUE(arena->next != NULL);  /* New block allocated */

    dm_arena_destroy(arena);
}

TEST(arena_reset) {
    dm_arena* arena = dm_arena_create(1024);

    dm_arena_alloc(arena, 100);
    ASSERT_TRUE(arena->used >= 100);

    dm_arena_reset(arena);
    ASSERT_EQ(arena->used, 0);

    dm_arena_destroy(arena);
}

TEST(region_macros) {
    DM_REGION_BEGIN(test_region);

    void* ptr = dm_arena_alloc(test_region, 256);
    ASSERT_TRUE(ptr != NULL);

    DM_REGION_END(test_region);
}

/* ============================================================================
 * List Tests
 * ============================================================================ */

TEST(list_new) {
    dm_list list = dm_list_new(sizeof(int));

    ASSERT_TRUE(list.data == NULL);
    ASSERT_EQ(list.len, 0);
    ASSERT_EQ(list.cap, 0);
    ASSERT_EQ(list.elem_size, sizeof(int));
}

TEST(list_push_pop) {
    dm_list list = dm_list_new(sizeof(int));

    int val1 = 10, val2 = 20, val3 = 30;
    dm_list_push(&list, &val1);
    dm_list_push(&list, &val2);
    dm_list_push(&list, &val3);

    ASSERT_EQ(dm_list_len(&list), 3);

    int popped;
    ASSERT_TRUE(dm_list_pop(&list, &popped));
    ASSERT_EQ(popped, 30);

    ASSERT_TRUE(dm_list_pop(&list, &popped));
    ASSERT_EQ(popped, 20);

    ASSERT_EQ(dm_list_len(&list), 1);

    dm_list_free(&list);
}

TEST(list_get_set) {
    dm_list list = dm_list_new(sizeof(int));

    int val1 = 10, val2 = 20;
    dm_list_push(&list, &val1);
    dm_list_push(&list, &val2);

    int* ptr = (int*)dm_list_get(&list, 0);
    ASSERT_TRUE(ptr != NULL);
    ASSERT_EQ(*ptr, 10);

    int new_val = 99;
    ASSERT_TRUE(dm_list_set(&list, 0, &new_val));

    ptr = (int*)dm_list_get(&list, 0);
    ASSERT_EQ(*ptr, 99);

    dm_list_free(&list);
}

TEST(list_with_capacity) {
    dm_list list = dm_list_with_capacity(sizeof(int), 100);

    ASSERT_TRUE(list.data != NULL);
    ASSERT_EQ(list.len, 0);
    ASSERT_EQ(list.cap, 100);

    dm_list_free(&list);
}

TEST(list_clear) {
    dm_list list = dm_list_new(sizeof(int));

    int val = 42;
    dm_list_push(&list, &val);
    dm_list_push(&list, &val);

    ASSERT_EQ(dm_list_len(&list), 2);

    dm_list_clear(&list);
    ASSERT_EQ(dm_list_len(&list), 0);
    ASSERT_TRUE(list.cap > 0);  /* Capacity preserved */

    dm_list_free(&list);
}

/* ============================================================================
 * Numeric Conversion Tests
 * ============================================================================ */

TEST(int_to_string) {
    dm_string s1 = dm_int_to_string(42);
    ASSERT_TRUE(dm_string_eq(s1, dm_string_new("42")));
    dm_string_free(s1);

    dm_string s2 = dm_int_to_string(-123);
    ASSERT_TRUE(dm_string_eq(s2, dm_string_new("-123")));
    dm_string_free(s2);

    dm_string s3 = dm_int_to_string(0);
    ASSERT_TRUE(dm_string_eq(s3, dm_string_new("0")));
    dm_string_free(s3);
}

TEST(float_to_string) {
    dm_string s1 = dm_float_to_string(3.14);
    ASSERT_TRUE(s1.len > 0);
    dm_string_free(s1);

    dm_string s2 = dm_float_to_string(-2.5);
    ASSERT_TRUE(dm_string_starts_with(s2, dm_string_new("-")));
    dm_string_free(s2);
}

TEST(string_to_int) {
    int64_t val;

    ASSERT_TRUE(dm_string_to_int(dm_string_new("42"), &val));
    ASSERT_EQ(val, 42);

    ASSERT_TRUE(dm_string_to_int(dm_string_new("-123"), &val));
    ASSERT_EQ(val, -123);

    ASSERT_FALSE(dm_string_to_int(dm_string_new("not a number"), &val));
    ASSERT_FALSE(dm_string_to_int(DM_STRING_EMPTY, &val));
}

TEST(string_to_float) {
    double val;

    ASSERT_TRUE(dm_string_to_float(dm_string_new("3.14"), &val));
    ASSERT_TRUE(val > 3.13 && val < 3.15);

    ASSERT_TRUE(dm_string_to_float(dm_string_new("-2.5"), &val));
    ASSERT_TRUE(val < -2.4 && val > -2.6);

    ASSERT_FALSE(dm_string_to_float(dm_string_new("not a number"), &val));
}

/* ============================================================================
 * Math Macro Tests
 * ============================================================================ */

TEST(math_macros) {
    ASSERT_EQ(DM_MIN(5, 10), 5);
    ASSERT_EQ(DM_MIN(10, 5), 5);

    ASSERT_EQ(DM_MAX(5, 10), 10);
    ASSERT_EQ(DM_MAX(10, 5), 10);

    ASSERT_EQ(DM_CLAMP(5, 0, 10), 5);
    ASSERT_EQ(DM_CLAMP(-5, 0, 10), 0);
    ASSERT_EQ(DM_CLAMP(15, 0, 10), 10);

    ASSERT_EQ(DM_ABS(5), 5);
    ASSERT_EQ(DM_ABS(-5), 5);
}

TEST(math_constants) {
    ASSERT_TRUE(DM_PI > 3.14 && DM_PI < 3.15);
    ASSERT_TRUE(DM_E > 2.71 && DM_E < 2.72);
}

/* ============================================================================
 * I/O Tests (limited - just test that functions exist)
 * ============================================================================ */

TEST(print_functions) {
    /* Just verify they don't crash */
    dm_print(dm_string_new("test output: "));
    dm_println(dm_string_new("hello"));
    dm_printf("formatted: %d\n", 42);
}

/* ============================================================================
 * File I/O Tests
 * ============================================================================ */

TEST(file_io) {
    dm_string path = dm_string_new("/tmp/daimond_test_file.txt");
    dm_string content = dm_string_new("Hello, dAImond!");

    /* Write file */
    ASSERT_TRUE(dm_write_file(path, content));

    /* Check exists */
    ASSERT_TRUE(dm_file_exists(path));

    /* Read file */
    dm_string read_content = dm_read_file(path);
    ASSERT_TRUE(dm_string_eq(read_content, content));
    dm_string_free(read_content);

    /* Append to file */
    dm_string more = dm_string_new(" More text.");
    ASSERT_TRUE(dm_append_file(path, more));

    /* Read again */
    read_content = dm_read_file(path);
    ASSERT_EQ(read_content.len, content.len + more.len);
    dm_string_free(read_content);
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

int main(void) {
    printf("=== dAImond Runtime Test Suite ===\n\n");

    dm_runtime_init();

    /* String tests */
    printf("--- String Tests ---\n");
    RUN_TEST(string_new);
    RUN_TEST(string_from_len);
    RUN_TEST(string_clone);
    RUN_TEST(string_concat);
    RUN_TEST(string_eq);
    RUN_TEST(string_cmp);
    RUN_TEST(string_slice);
    RUN_TEST(string_len);
    RUN_TEST(string_is_empty);
    RUN_TEST(string_starts_with);
    RUN_TEST(string_ends_with);
    RUN_TEST(string_find);
    RUN_TEST(string_char_at);
    RUN_TEST(string_literal);
    printf("\n");

    /* Option tests */
    printf("--- Option Tests ---\n");
    RUN_TEST(option_some);
    RUN_TEST(option_none);
    RUN_TEST(option_unwrap_or);
    RUN_TEST(option_init_macros);
    printf("\n");

    /* Result tests */
    printf("--- Result Tests ---\n");
    RUN_TEST(result_ok);
    RUN_TEST(result_err);
    RUN_TEST(result_init_macros);
    printf("\n");

    /* Arena tests */
    printf("--- Arena/Region Tests ---\n");
    RUN_TEST(arena_create_destroy);
    RUN_TEST(arena_alloc);
    RUN_TEST(arena_overflow);
    RUN_TEST(arena_reset);
    RUN_TEST(region_macros);
    printf("\n");

    /* List tests */
    printf("--- List Tests ---\n");
    RUN_TEST(list_new);
    RUN_TEST(list_push_pop);
    RUN_TEST(list_get_set);
    RUN_TEST(list_with_capacity);
    RUN_TEST(list_clear);
    printf("\n");

    /* Numeric conversion tests */
    printf("--- Numeric Conversion Tests ---\n");
    RUN_TEST(int_to_string);
    RUN_TEST(float_to_string);
    RUN_TEST(string_to_int);
    RUN_TEST(string_to_float);
    printf("\n");

    /* Math tests */
    printf("--- Math Tests ---\n");
    RUN_TEST(math_macros);
    RUN_TEST(math_constants);
    printf("\n");

    /* I/O tests */
    printf("--- I/O Tests ---\n");
    RUN_TEST(print_functions);
    RUN_TEST(file_io);
    printf("\n");

    dm_runtime_cleanup();

    /* Summary */
    printf("=================================\n");
    printf("Tests passed: %d\n", tests_passed);
    printf("Tests failed: %d\n", tests_failed);
    printf("=================================\n");

    return tests_failed > 0 ? 1 : 0;
}
