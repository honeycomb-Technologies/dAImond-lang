module tables

import buffer
import text

-- ============================================================
-- What the language server knows without reading a program: the
-- builtin functions, the keywords and the builtin types
-- ============================================================

-- The builtin functions. Each entry is a declaration and, after a tab,
-- what the function does.
fn builtin_functions() -> List[string] {
    let mut b: List[string] = []
    b.push("fn print(s: string)\tPrint to standard output, without a newline.")
    b.push("fn println(s: string)\tPrint to standard output, with a newline.")
    b.push("fn eprint(s: string)\tPrint to standard error, without a newline.")
    b.push("fn eprintln(s: string)\tPrint to standard error, with a newline.")
    b.push("fn panic(message: string)\tPrint `PANIC: <message>` to standard error and exit with code 1.")
    b.push("fn exit(code: int)\tEnd the program with this exit code.")
    b.push("fn flush()\tWrite out what standard output still holds.")
    b.push("fn read_line() -> string\tThe next line of standard input without its newline, or \"\" at the end of input.")
    b.push("fn assert(condition: bool)\tPanic with `assertion failed` unless the condition is true.")
    b.push("fn assert_eq(actual: T, expected: T)\tPanic unless the two values are equal.")
    b.push("fn len(s: string) -> int\tThe length of a string in bytes.")
    b.push("fn char_at(s: string, i: int) -> string\tThe one-byte string at index `i`.")
    b.push("fn byte_at(s: string, i: int) -> int\tThe byte at index `i`.")
    b.push("fn chr(n: int) -> string\tThe one-byte string whose byte is `n`.")
    b.push("fn substr(s: string, start: int, length: int) -> string\t`length` bytes of the string from index `start`.")
    b.push("fn int_to_string(n: int) -> string\tThe number in decimal.")
    b.push("fn float_to_string(f: float) -> string\tThe number as text.")
    b.push("fn bool_to_string(b: bool) -> string\t`\"true\"` or `\"false\"`.")
    b.push("fn parse_int(s: string) -> int\tThe whole number the string spells.")
    b.push("fn parse_float(s: string) -> float\tThe number the string spells.")
    b.push("fn string_contains(s: string, sub: string) -> bool\tWhether `sub` occurs in the string.")
    b.push("fn string_find(s: string, sub: string) -> int\tThe index of the first `sub` in the string, or -1.")
    b.push("fn starts_with(s: string, prefix: string) -> bool\tWhether the string starts with `prefix`.")
    b.push("fn ends_with(s: string, suffix: string) -> bool\tWhether the string ends with `suffix`.")
    b.push("fn string_trim(s: string) -> string\tThe string without the white space at its ends.")
    b.push("fn string_replace(s: string, old: string, new: string) -> string\tThe string with every `old` replaced by `new`.")
    b.push("fn string_to_upper(s: string) -> string\tThe string in upper case.")
    b.push("fn string_to_lower(s: string) -> string\tThe string in lower case.")
    b.push("fn string_split(s: string, delimiter: string) -> List[string]\tThe pieces of the string between occurrences of `delimiter`.")
    b.push("fn is_alpha(s: string) -> bool\tWhether the first byte is a letter.")
    b.push("fn is_digit(s: string) -> bool\tWhether the first byte is a digit.")
    b.push("fn is_alnum(s: string) -> bool\tWhether the first byte is a letter or a digit.")
    b.push("fn is_whitespace(s: string) -> bool\tWhether the first byte is white space.")
    b.push("fn file_read(path: string) -> string\tThe whole contents of a file.")
    b.push("fn file_write(path: string, content: string)\tMake `content` the whole of a file.")
    b.push("fn file_append(path: string, content: string)\tAdd `content` to the end of a file.")
    b.push("fn file_exists(path: string) -> bool\tWhether the file can be opened for reading.")
    b.push("fn fs_mkdir(path: string) -> int\tCreate a directory. 0 when it worked.")
    b.push("fn fs_readdir(path: string) -> string\tThe names in a directory, one per line.")
    b.push("fn fs_remove(path: string) -> int\tDelete a file or an empty directory. 0 when it worked.")
    b.push("fn fs_rename(from: string, to: string) -> int\tRename a file. 0 when it worked.")
    b.push("fn fs_getcwd() -> string\tThe current directory.")
    b.push("fn env_get(name: string) -> string\tThe value of an environment variable, or \"\".")
    b.push("fn path_dirname(path: string) -> string\tThe directory part of a path.")
    b.push("fn path_basename(path: string) -> string\tThe last part of a path.")
    b.push("fn path_extension(path: string) -> string\tThe extension of a path's last part.")
    b.push("fn path_stem(path: string) -> string\tThe last part of a path without its extension.")
    b.push("fn path_join(a: string, b: string) -> string\tThe two paths as one.")
    b.push("fn args_len() -> int\tThe number of command-line arguments, the program's name included.")
    b.push("fn args_get(i: int) -> string\tThe command-line argument at index `i`.")
    b.push("fn system(command: string) -> int\tRun a shell command and give its wait status.")
    b.push("fn run_command(args: List[string]) -> int\tRun the program `args[0]` with the arguments after it, without a shell, and give its exit code.")
    b.push("fn run_command_io(args: List[string], input: string, output: string, errors: string) -> int\tRun a program as `run_command` does, with files as its standard input, output and error (\"\" leaves a stream as it is).")
    b.push("fn read_key() -> int\tThe next byte of standard input, waiting for it; -1 at the end of input.")
    b.push("fn read_key_nb() -> int\tThe next byte of standard input without waiting; -1 when none is waiting.")
    b.push("fn term_width() -> int\tThe terminal's width in columns.")
    b.push("fn term_height() -> int\tThe terminal's height in rows.")
    b.push("fn time_ms() -> int\tMilliseconds on a monotonic clock.")
    b.push("fn sleep_ms(ms: int)\tWait for `ms` milliseconds.")
    b.push("fn tcp_listen(address: string) -> int\tA socket that listens on `HOST:PORT`, or -1.")
    b.push("fn tcp_accept(listener: int) -> int\tWait for a connection to a listening socket and give its socket, or -1.")
    b.push("fn tcp_connect(address: string) -> int\tA socket connected to `HOST:PORT`, or -1.")
    b.push("fn tcp_read(fd: int, max_bytes: int) -> string\tUp to `max_bytes` bytes from a socket; \"\" when the connection is closed.")
    b.push("fn tcp_write(fd: int, data: string) -> int\tSend `data` on a socket and give the number of bytes sent.")
    b.push("fn tcp_close(fd: int)\tClose a socket.")
    b.push("fn thread_spawn(f: fn() -> void) -> int\tRun the function `f` on a new thread and give the thread.")
    b.push("fn thread_join(thread: int)\tWait for a thread to end.")
    b.push("fn mutex_new() -> int\tA new, unlocked mutex.")
    b.push("fn mutex_lock(mutex: int)\tWait until the mutex is unlocked, and lock it.")
    b.push("fn mutex_unlock(mutex: int)\tUnlock the mutex.")
    b.push("fn mutex_destroy(mutex: int)\tFree the mutex.")
    b.push("fn Box_new(value: T) -> Box[T]\tA value on the heap.")
    b.push("fn Box_null() -> Box[T]\tA Box that holds nothing.")
    b.push("fn Map_new() -> Map[K, V]\tAn empty map.")
    b.push("fn simd_splat_f32x4(value: f32) -> f32x4\tA vector whose 4 lanes all hold the value.")
    b.push("fn simd_splat_f32x8(value: f32) -> f32x8\tA vector whose 8 lanes all hold the value.")
    b.push("fn simd_splat_f64x2(value: float) -> f64x2\tA vector whose 2 lanes all hold the value.")
    b.push("fn simd_splat_f64x4(value: float) -> f64x4\tA vector whose 4 lanes all hold the value.")
    b.push("fn simd_splat_i32x4(value: i32) -> i32x4\tA vector whose 4 lanes all hold the value.")
    b.push("fn simd_splat_i32x8(value: i32) -> i32x8\tA vector whose 8 lanes all hold the value.")
    b.push("fn simd_splat_i64x2(value: int) -> i64x2\tA vector whose 2 lanes all hold the value.")
    b.push("fn simd_splat_i64x4(value: int) -> i64x4\tA vector whose 4 lanes all hold the value.")
    b.push("fn simd_set_f32x4(a: f32, b: f32, c: f32, d: f32) -> f32x4\tA vector of the 4 values, lane 0 first.")
    b.push("fn simd_set_f32x8(a: f32, b: f32, c: f32, d: f32, e: f32, f: f32, g: f32, h: f32) -> f32x8\tA vector of the 8 values, lane 0 first.")
    b.push("fn simd_set_f64x2(a: float, b: float) -> f64x2\tA vector of the 2 values, lane 0 first.")
    b.push("fn simd_set_f64x4(a: float, b: float, c: float, d: float) -> f64x4\tA vector of the 4 values, lane 0 first.")
    b.push("fn simd_set_i32x4(a: i32, b: i32, c: i32, d: i32) -> i32x4\tA vector of the 4 values, lane 0 first.")
    b.push("fn simd_set_i32x8(a: i32, b: i32, c: i32, d: i32, e: i32, f: i32, g: i32, h: i32) -> i32x8\tA vector of the 8 values, lane 0 first.")
    b.push("fn simd_set_i64x2(a: int, b: int) -> i64x2\tA vector of the 2 values, lane 0 first.")
    b.push("fn simd_set_i64x4(a: int, b: int, c: int, d: int) -> i64x4\tA vector of the 4 values, lane 0 first.")
    b.push("fn simd_add(a: V, b: V) -> V\tThe sums of the lanes of two vectors of one type.")
    b.push("fn simd_sub(a: V, b: V) -> V\tThe differences of the lanes of two vectors of one type.")
    b.push("fn simd_mul(a: V, b: V) -> V\tThe products of the lanes of two vectors of one type.")
    b.push("fn simd_div(a: V, b: V) -> V\tThe quotients of the lanes of two vectors of one type.")
    b.push("fn simd_extract(vector: V, lane: int) -> T\tThe value in the lane of a vector, counted from 0.")
    return b
}

-- The methods of the builtin types. Each entry is the method as it is
-- called and, after a tab, what it does.
fn builtin_methods() -> List[string] {
    let mut m: List[string] = []
    m.push("push(item)\tAdd an item to the end of a List.")
    m.push("pop()\tRemove the last item of a List and give it.")
    m.push("len()\tThe number of items of a List or Map, or the length of a string.")
    m.push("insert(key, value)\tSet the value of a key of a Map.")
    m.push("set(key, value)\tSet the value of a key of a Map.")
    m.push("get(key)\tThe value of a key of a Map. Panics when the Map has no such key.")
    m.push("contains(key)\tWhether a Map has the key, or a List the item.")
    m.push("remove(key)\tRemove a key of a Map.")
    m.push("keys()\tThe keys of a Map, in the order they were first set.")
    m.push("values()\tThe values of a Map, in the order of its keys.")
    return m
}

-- The keywords and builtin types. Each entry is the word and, after a
-- tab, what an editor shows for it, in Markdown. A type's text starts
-- with `**`.
fn known_words() -> List[string] {
    let mut w: List[string] = []
    w.push("fn\t```daimond\nfn name(params) -> ReturnType { body }\n```\nDeclare a function.")
    w.push("let\t```daimond\nlet x = value\nlet mut x = value\n```\nDeclare a variable. It cannot be assigned to again unless it is `mut`.")
    w.push("mut\tMarks a variable or parameter that can be assigned to.")
    w.push("const\t```daimond\nconst NAME: Type = value\n```\nDeclare a constant.")
    w.push("struct\t```daimond\nstruct Name { field: Type, ... }\n```\nDeclare a struct type.")
    w.push("enum\t```daimond\nenum Name { Variant1, Variant2(Type), ... }\n```\nDeclare an enum type.")
    w.push("trait\t```daimond\ntrait Name { fn method(self) -> Type }\n```\nDeclare a trait.")
    w.push("impl\t```daimond\nimpl TypeName { fn method(self) { ... } }\nimpl TraitName for TypeName { ... }\n```\nGive a type methods, or implement a trait for it.")
    w.push("match\t```daimond\nmatch value { Pattern => expr, ... }\n```\nChoose by pattern.")
    w.push("if\t```daimond\nif condition { ... } else { ... }\n```")
    w.push("else\tThe branch of an `if` taken when its condition is false.")
    w.push("while\t```daimond\nwhile condition { ... }\n```")
    w.push("for\t```daimond\nfor item in list { ... }\n```")
    w.push("in\tSeparates the variable of a `for` loop from what it goes through.")
    w.push("loop\t```daimond\nloop { ... }\n```\nRepeat until a `break`.")
    w.push("break\tLeave the innermost loop.")
    w.push("continue\tGo to the next turn of the innermost loop.")
    w.push("return\tLeave the function, with a value if it has a return type.")
    w.push("region\t```daimond\nregion name { ... }\n```\nA scope whose values must not outlive it.")
    w.push("unsafe\t```daimond\nunsafe { ... }\n```\nA block that may use raw memory and call extern functions.")
    w.push("comptime\t```daimond\ncomptime { ... }\n```\nAn expression evaluated when the program is compiled.")
    w.push("extern\t```daimond\nextern fn name(params) -> ReturnType\n```\nDeclare a C function.")
    w.push("import\t```daimond\nimport module_name\n```\nUse the declarations of another file.")
    w.push("module\t```daimond\nmodule name\n```\nNames the file's module. Every file starts with it.")
    w.push("with\t```daimond\nfn name() with [IO, FileSystem] { ... }\n```\nThe effects a function may have.")
    w.push("as\t```daimond\nvalue as Type\n```\nConvert a number to another numeric type.")
    w.push("and\tTrue when both sides are true. The right side is evaluated only when the left is true.")
    w.push("or\tTrue when either side is true. The right side is evaluated only when the left is false.")
    w.push("not\tTrue when its operand is false.")
    w.push("true\tThe `bool` value true.")
    w.push("false\tThe `bool` value false.")
    w.push("self\tThe value a method is called on.")
    w.push("async\tMarks a function that returns a `Future`.")
    w.push("await\tWait for the value of a `Future`.")
    w.push("dyn\t```daimond\ndyn TraitName\n```\nA value of any type that implements the trait.")
    w.push("int\t**int** is a 64-bit signed integer.")
    w.push("float\t**float** is a 64-bit floating-point number.")
    w.push("bool\t**bool** is `true` or `false`.")
    w.push("string\t**string** is an immutable string of bytes.")
    w.push("void\t**void** is the return type of a function that returns nothing.")
    w.push("i8\t**i8** is an 8-bit signed integer.")
    w.push("i16\t**i16** is a 16-bit signed integer.")
    w.push("i32\t**i32** is a 32-bit signed integer, C's `int`.")
    w.push("i64\t**i64** is a 64-bit signed integer, the same as `int`.")
    w.push("u8\t**u8** is an 8-bit unsigned integer.")
    w.push("u16\t**u16** is a 16-bit unsigned integer.")
    w.push("u32\t**u32** is a 32-bit unsigned integer.")
    w.push("u64\t**u64** is a 64-bit unsigned integer.")
    w.push("f32\t**f32** is a 32-bit floating-point number.")
    w.push("f64\t**f64** is a 64-bit floating-point number, the same as `float`.")
    w.push("List\t**List[T]** is a growable array. Methods: `push(item)`, `pop()`, `len()`; `xs[i]` reads and writes an item.")
    w.push("Map\t**Map[K, V]** is a hash map. Methods: `insert(k, v)`, `get(k)`, `contains(k)`, `remove(k)`, `len()`, `keys()`, `values()`.")
    w.push("Box\t**Box[T]** is a value on the heap. `Box_new(value)` makes one, `Box_null()` an empty one, and `*box` reads it.")
    w.push("Option\t**Option[T]** is `Some(T)` or `None`.")
    w.push("Result\t**Result[T, E]** is `Ok(T)` or `Err(E)`. `?` returns the error from the function.")
    w.push("Future\t**Future[T]** is the result of an `async` function.")
    return w
}

-- The part of a table entry before its tab
fn entry_head(entry: string) -> string {
    let tab = string_find(entry, "\t")
    if tab < 0 {
        return entry
    }
    return view(entry, 0, tab)
}

-- The part of a table entry after its tab
fn entry_text(entry: string) -> string {
    let tab = string_find(entry, "\t")
    if tab < 0 {
        return ""
    }
    return view(entry, tab + 1, len(entry) - tab - 1)
}

-- The name of the function a declaration such as
-- "extern fn add[T](a: T) -> T" declares
fn declared_name(declaration: string) -> string {
    let mut at = string_find(declaration, "fn ")
    if at < 0 {
        return ""
    }
    at = at + 3
    let mut end = at
    while end < len(declaration) and is_word_byte(byte_at(declaration, end)) {
        end = end + 1
    }
    return view(declaration, at, end - at)
}

-- The entry of the builtin function `name`; "" when there is none
fn find_builtin(builtins: List[string], name: string) -> string {
    let mut i = 0
    while i < builtins.len() {
        if declared_name(entry_head(builtins[i])) == name {
            return builtins[i]
        }
        i = i + 1
    }
    return ""
}

-- The entry of the keyword or type `word`; "" when there is none
fn find_known_word(words: List[string], word: string) -> string {
    let mut i = 0
    while i < words.len() {
        if entry_head(words[i]) == word {
            return words[i]
        }
        i = i + 1
    }
    return ""
}
