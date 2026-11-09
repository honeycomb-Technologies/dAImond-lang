package main

import (
	"fmt"
	"strings"
)

func skipWhitespace(input string, pos int) int {
	for pos < len(input) && (input[pos] == ' ' || input[pos] == '\n' || input[pos] == '\t' || input[pos] == '\r') {
		pos++
	}
	return pos
}

func parseString(input string, pos int) int {
	pos++ // skip opening quote
	for pos < len(input) && input[pos] != '"' {
		if input[pos] == '\\' {
			pos += 2
		} else {
			pos++
		}
	}
	pos++ // skip closing quote
	return pos
}

func parseNumber(input string, pos int) int {
	if pos < len(input) && input[pos] == '-' {
		pos++
	}
	for pos < len(input) && input[pos] >= '0' && input[pos] <= '9' {
		pos++
	}
	if pos < len(input) && input[pos] == '.' {
		pos++
		for pos < len(input) && input[pos] >= '0' && input[pos] <= '9' {
			pos++
		}
	}
	return pos
}

func parseValue(input string, pos int) int {
	pos = skipWhitespace(input, pos)
	if pos >= len(input) {
		return pos
	}
	ch := input[pos]
	if ch == '"' {
		return parseString(input, pos)
	}
	if ch == '{' {
		return parseObject(input, pos)
	}
	if ch == '[' {
		return parseArray(input, pos)
	}
	if ch == 't' {
		return pos + 4
	}
	if ch == 'f' {
		return pos + 5
	}
	if ch == 'n' {
		return pos + 4
	}
	return parseNumber(input, pos)
}

func parseArray(input string, pos int) int {
	pos++ // skip [
	pos = skipWhitespace(input, pos)
	if pos < len(input) && input[pos] == ']' {
		return pos + 1
	}
	pos = parseValue(input, pos)
	pos = skipWhitespace(input, pos)
	for pos < len(input) && input[pos] == ',' {
		pos++
		pos = parseValue(input, pos)
		pos = skipWhitespace(input, pos)
	}
	pos++ // skip ]
	return pos
}

func parseObject(input string, pos int) int {
	pos++ // skip {
	pos = skipWhitespace(input, pos)
	if pos < len(input) && input[pos] == '}' {
		return pos + 1
	}
	pos = parseString(input, pos)
	pos = skipWhitespace(input, pos)
	pos++ // skip :
	pos = parseValue(input, pos)
	pos = skipWhitespace(input, pos)
	for pos < len(input) && input[pos] == ',' {
		pos++
		pos = skipWhitespace(input, pos)
		pos = parseString(input, pos)
		pos = skipWhitespace(input, pos)
		pos++ // skip :
		pos = parseValue(input, pos)
		pos = skipWhitespace(input, pos)
	}
	pos++ // skip }
	return pos
}

func main() {
	var b strings.Builder
	b.Grow(2 * 1024 * 1024)
	b.WriteByte('[')
	for i := 0; i < 10000; i++ {
		if i > 0 {
			b.WriteByte(',')
		}
		fmt.Fprintf(&b,
			`{"id":%d,"name":"item_%d","value":%g,"active":true,"tags":["a","b","c"]}`,
			i, i, float64(i)*1.5)
	}
	b.WriteByte(']')
	json := b.String()

	fmt.Printf("json length: %d\n", len(json))

	count := 0
	for count < 100 {
		parseValue(json, 0)
		count++
	}

	fmt.Printf("parsed: %d\n", count)
}
