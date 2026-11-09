function skipWhitespace(input, pos) {
    while (pos < input.length && (input[pos] === ' ' || input[pos] === '\n' || input[pos] === '\t' || input[pos] === '\r')) {
        pos++;
    }
    return pos;
}

function parseString(input, pos) {
    pos++; // skip opening quote
    while (pos < input.length && input[pos] !== '"') {
        if (input[pos] === '\\') {
            pos += 2;
        } else {
            pos++;
        }
    }
    pos++; // skip closing quote
    return pos;
}

function parseNumber(input, pos) {
    if (pos < input.length && input[pos] === '-') pos++;
    while (pos < input.length && input[pos] >= '0' && input[pos] <= '9') pos++;
    if (pos < input.length && input[pos] === '.') {
        pos++;
        while (pos < input.length && input[pos] >= '0' && input[pos] <= '9') pos++;
    }
    return pos;
}

function parseValue(input, pos) {
    pos = skipWhitespace(input, pos);
    if (pos >= input.length) return pos;
    const ch = input[pos];
    if (ch === '"') return parseString(input, pos);
    if (ch === '{') return parseObject(input, pos);
    if (ch === '[') return parseArray(input, pos);
    if (ch === 't') return pos + 4;
    if (ch === 'f') return pos + 5;
    if (ch === 'n') return pos + 4;
    return parseNumber(input, pos);
}

function parseArray(input, pos) {
    pos++; // skip [
    pos = skipWhitespace(input, pos);
    if (pos < input.length && input[pos] === ']') return pos + 1;
    pos = parseValue(input, pos);
    pos = skipWhitespace(input, pos);
    while (pos < input.length && input[pos] === ',') {
        pos++;
        pos = parseValue(input, pos);
        pos = skipWhitespace(input, pos);
    }
    pos++; // skip ]
    return pos;
}

function parseObject(input, pos) {
    pos++; // skip {
    pos = skipWhitespace(input, pos);
    if (pos < input.length && input[pos] === '}') return pos + 1;
    pos = parseString(input, pos);
    pos = skipWhitespace(input, pos);
    pos++; // skip :
    pos = parseValue(input, pos);
    pos = skipWhitespace(input, pos);
    while (pos < input.length && input[pos] === ',') {
        pos++;
        pos = skipWhitespace(input, pos);
        pos = parseString(input, pos);
        pos = skipWhitespace(input, pos);
        pos++; // skip :
        pos = parseValue(input, pos);
        pos = skipWhitespace(input, pos);
    }
    pos++; // skip }
    return pos;
}

function main() {
    const parts = ['['];
    for (let i = 0; i < 10000; i++) {
        if (i > 0) parts.push(',');
        parts.push(`{"id":${i},"name":"item_${i}","value":${i * 1.5},"active":true,"tags":["a","b","c"]}`);
    }
    parts.push(']');
    const json = parts.join('');

    console.log(`json length: ${json.length}`);

    let count = 0;
    while (count < 100) {
        parseValue(json, 0);
        count++;
    }

    console.log(`parsed: ${count}`);
}

main();
