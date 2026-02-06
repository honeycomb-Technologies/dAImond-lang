/* C Comparison Benchmark: JSON Parser */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    const char* input;
    int pos;
    int len;
} ParseCtx;

static inline bool is_ws(char ch) {
    return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';
}

static inline bool is_digit(char ch) {
    return ch >= '0' && ch <= '9';
}

static void skip_whitespace(ParseCtx* ctx) {
    while (ctx->pos < ctx->len && is_ws(ctx->input[ctx->pos])) {
        ctx->pos++;
    }
}

static void parse_value(ParseCtx* ctx);

static void parse_string(ParseCtx* ctx) {
    ctx->pos++; /* skip opening quote */
    while (ctx->pos < ctx->len) {
        char ch = ctx->input[ctx->pos];
        if (ch == '"') {
            ctx->pos++;
            return;
        }
        if (ch == '\\') {
            ctx->pos += 2;
        } else {
            ctx->pos++;
        }
    }
}

static void parse_number(ParseCtx* ctx) {
    if (ctx->pos < ctx->len && ctx->input[ctx->pos] == '-') ctx->pos++;
    while (ctx->pos < ctx->len && is_digit(ctx->input[ctx->pos])) ctx->pos++;
    if (ctx->pos < ctx->len && ctx->input[ctx->pos] == '.') {
        ctx->pos++;
        while (ctx->pos < ctx->len && is_digit(ctx->input[ctx->pos])) ctx->pos++;
    }
}

static void parse_array(ParseCtx* ctx) {
    ctx->pos++; /* skip [ */
    skip_whitespace(ctx);
    if (ctx->pos < ctx->len && ctx->input[ctx->pos] == ']') {
        ctx->pos++;
        return;
    }
    parse_value(ctx);
    skip_whitespace(ctx);
    while (ctx->pos < ctx->len && ctx->input[ctx->pos] == ',') {
        ctx->pos++;
        parse_value(ctx);
        skip_whitespace(ctx);
    }
    if (ctx->pos < ctx->len) ctx->pos++; /* skip ] */
}

static void parse_object(ParseCtx* ctx) {
    ctx->pos++; /* skip { */
    skip_whitespace(ctx);
    if (ctx->pos < ctx->len && ctx->input[ctx->pos] == '}') {
        ctx->pos++;
        return;
    }
    parse_string(ctx);
    skip_whitespace(ctx);
    if (ctx->pos < ctx->len) ctx->pos++; /* skip : */
    parse_value(ctx);
    skip_whitespace(ctx);
    while (ctx->pos < ctx->len && ctx->input[ctx->pos] == ',') {
        ctx->pos++;
        skip_whitespace(ctx);
        parse_string(ctx);
        skip_whitespace(ctx);
        if (ctx->pos < ctx->len) ctx->pos++; /* skip : */
        parse_value(ctx);
        skip_whitespace(ctx);
    }
    if (ctx->pos < ctx->len) ctx->pos++; /* skip } */
}

static void parse_value(ParseCtx* ctx) {
    skip_whitespace(ctx);
    if (ctx->pos >= ctx->len) return;
    char ch = ctx->input[ctx->pos];
    if (ch == '"') parse_string(ctx);
    else if (ch == '{') parse_object(ctx);
    else if (ch == '[') parse_array(ctx);
    else if (ch == 't') ctx->pos += 4;
    else if (ch == 'f') ctx->pos += 5;
    else if (ch == 'n') ctx->pos += 4;
    else parse_number(ctx);
}

static char* build_test_json(int* out_len) {
    size_t cap = 1024 * 1024 * 4;
    char* buf = malloc(cap);
    int pos = 0;
    buf[pos++] = '[';
    for (int i = 0; i < 1000; i++) {
        if (i > 0) buf[pos++] = ',';
        pos += snprintf(buf + pos, cap - pos,
            "{\"id\":%d,\"name\":\"item_%d\",\"value\":%g,\"active\":true,\"tags\":[\"a\",\"b\",\"c\"]}",
            i, i, 1.5 * i);
    }
    buf[pos++] = ']';
    buf[pos] = '\0';
    *out_len = pos;
    return buf;
}

int main(void) {
    int json_len;
    char* json = build_test_json(&json_len);
    printf("JSON length: %d\n", json_len);

    for (int i = 0; i < 10; i++) {
        ParseCtx ctx = { .input = json, .pos = 0, .len = json_len };
        parse_value(&ctx);
    }
    printf("Parsed 10 iterations\n");
    printf("Done\n");

    free(json);
    return 0;
}
