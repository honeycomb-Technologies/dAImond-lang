const std = @import("std");

fn skipWhitespace(input: []const u8, pos_in: usize) usize {
    var pos = pos_in;
    while (pos < input.len and (input[pos] == ' ' or input[pos] == '\n' or input[pos] == '\t' or input[pos] == '\r')) {
        pos += 1;
    }
    return pos;
}

fn parseString(input: []const u8, pos_in: usize) usize {
    var pos = pos_in + 1; // skip opening quote
    while (pos < input.len and input[pos] != '"') {
        if (input[pos] == '\\') {
            pos += 2;
        } else {
            pos += 1;
        }
    }
    pos += 1; // skip closing quote
    return pos;
}

fn parseNumber(input: []const u8, pos_in: usize) usize {
    var pos = pos_in;
    if (pos < input.len and input[pos] == '-') pos += 1;
    while (pos < input.len and input[pos] >= '0' and input[pos] <= '9') pos += 1;
    if (pos < input.len and input[pos] == '.') {
        pos += 1;
        while (pos < input.len and input[pos] >= '0' and input[pos] <= '9') pos += 1;
    }
    return pos;
}

fn parseValue(input: []const u8, pos_in: usize) usize {
    const pos = skipWhitespace(input, pos_in);
    if (pos >= input.len) return pos;
    const ch = input[pos];
    if (ch == '"') return parseString(input, pos);
    if (ch == '{') return parseObject(input, pos);
    if (ch == '[') return parseArray(input, pos);
    if (ch == 't') return pos + 4;
    if (ch == 'f') return pos + 5;
    if (ch == 'n') return pos + 4;
    return parseNumber(input, pos);
}

fn parseArray(input: []const u8, pos_in: usize) usize {
    var pos = pos_in + 1; // skip [
    pos = skipWhitespace(input, pos);
    if (pos < input.len and input[pos] == ']') return pos + 1;
    pos = parseValue(input, pos);
    pos = skipWhitespace(input, pos);
    while (pos < input.len and input[pos] == ',') {
        pos += 1;
        pos = parseValue(input, pos);
        pos = skipWhitespace(input, pos);
    }
    pos += 1; // skip ]
    return pos;
}

fn parseObject(input: []const u8, pos_in: usize) usize {
    var pos = pos_in + 1; // skip {
    pos = skipWhitespace(input, pos);
    if (pos < input.len and input[pos] == '}') return pos + 1;
    pos = parseString(input, pos);
    pos = skipWhitespace(input, pos);
    pos += 1; // skip :
    pos = parseValue(input, pos);
    pos = skipWhitespace(input, pos);
    while (pos < input.len and input[pos] == ',') {
        pos += 1;
        pos = skipWhitespace(input, pos);
        pos = parseString(input, pos);
        pos = skipWhitespace(input, pos);
        pos += 1; // skip :
        pos = parseValue(input, pos);
        pos = skipWhitespace(input, pos);
    }
    pos += 1; // skip }
    return pos;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var json = std.ArrayList(u8).init(std.heap.page_allocator);
    defer json.deinit();
    try json.ensureTotalCapacity(2 * 1024 * 1024);

    try json.append('[');
    var buf: [256]u8 = undefined;
    var i: usize = 0;
    while (i < 10000) : (i += 1) {
        if (i > 0) try json.append(',');
        const val: f64 = @as(f64, @floatFromInt(i)) * 1.5;
        const s = std.fmt.bufPrint(&buf,
            "{{\"id\":{},\"name\":\"item_{}\",\"value\":{d},\"active\":true,\"tags\":[\"a\",\"b\",\"c\"]}}",
            .{ i, i, val }) catch unreachable;
        try json.appendSlice(s);
    }
    try json.append(']');

    const input = json.items;
    try stdout.print("json length: {}\n", .{input.len});

    var count: usize = 0;
    while (count < 100) {
        _ = parseValue(input, 0);
        count += 1;
    }

    try stdout.print("parsed: {}\n", .{count});
}
