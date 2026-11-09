#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static char *json_buf = NULL;
static size_t json_len = 0;
static size_t json_cap = 0;

static void json_init(void) {
    json_cap = 1024 * 1024;
    json_buf = (char *)malloc(json_cap);
    json_len = 0;
    json_buf[0] = '\0';
}

static void json_append(const char *s) {
    size_t slen = strlen(s);
    while (json_len + slen + 1 > json_cap) {
        json_cap *= 2;
        json_buf = (char *)realloc(json_buf, json_cap);
    }
    memcpy(json_buf + json_len, s, slen);
    json_len += slen;
    json_buf[json_len] = '\0';
}

static int skip_whitespace(const char *input, int pos, int len) {
    while (pos < len && (input[pos] == ' ' || input[pos] == '\n' || input[pos] == '\t' || input[pos] == '\r')) {
        pos++;
    }
    return pos;
}

static int parse_string(const char *input, int pos, int len) {
    pos++; /* skip opening quote */
    while (pos < len && input[pos] != '"') {
        if (input[pos] == '\\') {
            pos += 2;
        } else {
            pos++;
        }
    }
    pos++; /* skip closing quote */
    return pos;
}

static int parse_number(const char *input, int pos, int len) {
    if (pos < len && input[pos] == '-') pos++;
    while (pos < len && input[pos] >= '0' && input[pos] <= '9') pos++;
    if (pos < len && input[pos] == '.') {
        pos++;
        while (pos < len && input[pos] >= '0' && input[pos] <= '9') pos++;
    }
    return pos;
}

static int parse_value(const char *input, int pos, int len);
static int parse_array(const char *input, int pos, int len);
static int parse_object(const char *input, int pos, int len);

static int parse_value(const char *input, int pos, int len) {
    pos = skip_whitespace(input, pos, len);
    if (pos >= len) return pos;
    char ch = input[pos];
    if (ch == '"') return parse_string(input, pos, len);
    if (ch == '{') return parse_object(input, pos, len);
    if (ch == '[') return parse_array(input, pos, len);
    if (ch == 't') return pos + 4;
    if (ch == 'f') return pos + 5;
    if (ch == 'n') return pos + 4;
    return parse_number(input, pos, len);
}

static int parse_array(const char *input, int pos, int len) {
    pos++; /* skip [ */
    pos = skip_whitespace(input, pos, len);
    if (pos < len && input[pos] == ']') return pos + 1;
    pos = parse_value(input, pos, len);
    pos = skip_whitespace(input, pos, len);
    while (pos < len && input[pos] == ',') {
        pos++;
        pos = parse_value(input, pos, len);
        pos = skip_whitespace(input, pos, len);
    }
    pos++; /* skip ] */
    return pos;
}

static int parse_object(const char *input, int pos, int len) {
    pos++; /* skip { */
    pos = skip_whitespace(input, pos, len);
    if (pos < len && input[pos] == '}') return pos + 1;
    pos = parse_string(input, pos, len);
    pos = skip_whitespace(input, pos, len);
    pos++; /* skip : */
    pos = parse_value(input, pos, len);
    pos = skip_whitespace(input, pos, len);
    while (pos < len && input[pos] == ',') {
        pos++;
        pos = skip_whitespace(input, pos, len);
        pos = parse_string(input, pos, len);
        pos = skip_whitespace(input, pos, len);
        pos++; /* skip : */
        pos = parse_value(input, pos, len);
        pos = skip_whitespace(input, pos, len);
    }
    pos++; /* skip } */
    return pos;
}

int main(void) {
    json_init();

    char tmp[256];
    json_append("[");
    for (int i = 0; i < 10000; i++) {
        if (i > 0) json_append(",");
        snprintf(tmp, sizeof(tmp),
            "{\"id\":%d,\"name\":\"item_%d\",\"value\":%g,\"active\":true,\"tags\":[\"a\",\"b\",\"c\"]}",
            i, i, i * 1.5);
        json_append(tmp);
    }
    json_append("]");

    printf("json length: %zu\n", json_len);

    int count = 0;
    while (count < 100) {
        parse_value(json_buf, 0, (int)json_len);
        count++;
    }

    printf("parsed: %d\n", count);

    free(json_buf);
    return 0;
}
