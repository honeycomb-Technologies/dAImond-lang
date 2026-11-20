module harness

-- ============================================================
-- The integration test harness
-- Compiles every test case with a dAImond compiler, runs the program
-- and compares what it does with what the case expects.
--
-- Usage: harness <compiler> [<cases directory> [<filter>]]
--   <compiler>         the compiler to test, such as build/daimond
--   <cases directory>  default: tests/cases
--   <filter>           run only the cases whose "suite/name" contains
--                      this text
--
-- The cases directory holds one directory per suite. In a suite, a
-- case named X is either the file X.dm, or the directory X with the
-- program main.dm and the files it imports. Next to it are:
--   X.stdout   what the program must print (the case compiles and runs)
--   X.exit     the exit code the program must end with (default 0)
--   X.stderr   what the program must write to standard error (not
--              compared when the file is missing)
--   X.args     the arguments the program is run with, as shell words
--   X.errors   the case must fail to compile, and each line of this
--              file must appear in what the compiler prints
--   X.cmd      a shell command to run in place of compiling and running
--              the program, for the compiler's own commands. In it,
--              {compiler} stands for the compiler, {source} for the
--              case's program and {work} for an empty directory, the
--              first two as absolute paths. X.stdout, X.exit and
--              X.stderr are what the command must print and end with.
-- A case has X.stdout or X.errors, not both.
--
-- The harness runs the compiler and the programs through the shell,
-- with their output sent to files. It exits with 0 when every case
-- passes and with 1 otherwise.
-- ============================================================

extern fn getpid() -> i32

struct Outcome {
    passed: bool,
    message: string
}

fn pass() -> Outcome {
    return Outcome { passed: true, message: "" }
}

fn fail(message: string) -> Outcome {
    return Outcome { passed: false, message: message }
}

-- The strings in ascending order
fn sorted(items: List[string]) -> List[string] {
    let mut out = items
    let mut i = 1
    while i < out.len() {
        let item = out[i]
        let mut j = i
        while j > 0 and out[j - 1] > item {
            out[j] = out[j - 1]
            j = j - 1
        }
        out[j] = item
        i = i + 1
    }
    return out
}

-- The names in a directory, in ascending order
fn list_dir(path: string) -> List[string] {
    let mut names: List[string] = []
    let listing = fs_readdir(path)
    if listing == "" {
        return names
    }
    let entries = string_split(listing, "\n")
    let mut i = 0
    while i < entries.len() {
        if entries[i] != "" {
            names.push(entries[i])
        }
        i = i + 1
    }
    return sorted(names)
}

-- A word of a shell command that stands for exactly this text
fn shell_quote(text: string) -> string {
    return "'" + string_replace(text, "'", "'\\''") + "'"
}

-- Run a shell command. Gives the exit code of what it ran, 128 plus
-- the signal's number when a signal ended it, or 254 when the shell
-- could not be started.
fn run(command: string) -> int {
    let status = system(command)
    if status < 0 {
        return 254
    }
    if status % 128 != 0 {
        return 128 + status % 128
    }
    return (status / 256) % 256
}

-- The file's contents; "" when there is no such file
fn read_or_empty(path: string) -> string {
    if file_exists(path) {
        return file_read(path)
    }
    return ""
}

-- Delete a file, or a directory and everything in it
fn remove_tree(path: string) {
    if fs_remove(path) == 0 {
        return
    }
    let names = list_dir(path)
    let mut i = 0
    while i < names.len() {
        remove_tree(path + "/" + names[i])
        i = i + 1
    }
    fs_remove(path)
}

-- A path that names the same file from any directory
fn absolute_path(path: string) -> string {
    if starts_with(path, "/") {
        return path
    }
    return fs_getcwd() + "/" + path
}

-- Compare what a command did with the case's .exit, .stdout and
-- .stderr files. `code` is its exit code; its output is in the files
-- `out_file` and `err_file`.
fn compare_run(base: string, code: int, out_file: string, err_file: string) -> Outcome {
    let mut expected_code = 0
    if file_exists(base + ".exit") {
        expected_code = parse_int(string_trim(file_read(base + ".exit")))
    }
    if code != expected_code {
        return fail("Expected exit code " + int_to_string(expected_code) + ", got " + int_to_string(code) + "\nStderr: '" + read_or_empty(err_file) + "'")
    }

    let expected_out = file_read(base + ".stdout")
    let actual_out = read_or_empty(out_file)
    if actual_out != expected_out {
        return fail("Output mismatch.\nExpected: '" + expected_out + "'\nGot: '" + actual_out + "'")
    }

    if file_exists(base + ".stderr") {
        let expected_err = file_read(base + ".stderr")
        let actual_err = read_or_empty(err_file)
        if actual_err != expected_err {
            return fail("Stderr mismatch.\nExpected: '" + expected_err + "'\nGot: '" + actual_err + "'")
        }
    }
    return pass()
}

-- Run one case: its program is the file `source`, and its expectation
-- files are `base`.stdout and so on. `work` is an empty directory for
-- the program and for what it writes next to itself.
fn run_case(compiler: string, source: string, base: string, work: string) -> Outcome {
    let expects_error = file_exists(base + ".errors")
    if expects_error == false and file_exists(base + ".stdout") == false {
        return fail("the case has neither a .stdout nor a .errors file")
    }
    if expects_error and file_exists(base + ".stdout") {
        return fail("the case has both a .stdout and a .errors file")
    }

    let run_out = work + "/run.stdout"
    let run_err = work + "/run.stderr"

    -- A case with its own command
    if file_exists(base + ".cmd") {
        if expects_error {
            return fail("the case has both a .cmd and a .errors file")
        }
        let command_dir = work + "/cmd"
        fs_mkdir(command_dir)
        let mut command = string_trim(file_read(base + ".cmd"))
        command = string_replace(command, "{compiler}", shell_quote(absolute_path(compiler)))
        command = string_replace(command, "{source}", shell_quote(absolute_path(source)))
        command = string_replace(command, "{work}", shell_quote(command_dir))
        let command_code = run("( " + command + " ) > " + shell_quote(run_out) + " 2> " + shell_quote(run_err) + " < /dev/null")
        return compare_run(base, command_code, run_out, run_err)
    }

    let binary = work + "/test_bin"
    let compile_out = work + "/compile.stdout"
    let compile_err = work + "/compile.stderr"
    let compile_code = run(shell_quote(compiler) + " " + shell_quote(source) + " -o " + shell_quote(binary) + " > " + shell_quote(compile_out) + " 2> " + shell_quote(compile_err) + " < /dev/null")
    let compile_output = read_or_empty(compile_out) + read_or_empty(compile_err)

    if expects_error {
        if compile_code == 0 {
            return fail("Expected compile error but compilation succeeded")
        }
        if compile_code >= 128 {
            return fail("Expected compile error but the compiler crashed (exit code " + int_to_string(compile_code) + ")\n" + compile_output)
        }
        let needles = string_split(file_read(base + ".errors"), "\n")
        let mut i = 0
        while i < needles.len() {
            if needles[i] != "" and string_contains(compile_output, needles[i]) == false {
                return fail("Compile error output missing expected text: '" + needles[i] + "'\nGot: '" + compile_output + "'")
            }
            i = i + 1
        }
        return pass()
    }

    if compile_code != 0 {
        return fail("Compilation failed (exit code " + int_to_string(compile_code) + ")\n" + compile_output)
    }

    let mut arguments = ""
    if file_exists(base + ".args") {
        arguments = " " + string_trim(file_read(base + ".args"))
    }
    let code = run(binary + arguments + " > " + shell_quote(run_out) + " 2> " + shell_quote(run_err) + " < /dev/null")
    return compare_run(base, code, run_out, run_err)
}

fn main() -> int {
    if args_len() < 2 or args_get(1) == "-h" or args_get(1) == "--help" {
        println("Usage: harness <compiler> [<cases directory> [<filter>]]")
        println("")
        println("Compiles and runs every test case in the cases directory (default:")
        println("tests/cases) with the compiler. With a filter, runs only the cases")
        println("whose \"suite/name\" contains it.")
        if args_len() < 2 {
            return 2
        }
        return 0
    }
    let compiler = args_get(1)
    let mut cases_dir = "tests/cases"
    if args_len() > 2 {
        cases_dir = args_get(2)
    }
    let mut filter = ""
    if args_len() > 3 {
        filter = args_get(3)
    }
    if file_exists(compiler) == false {
        eprintln("Error: the compiler " + compiler + " was not found")
        return 2
    }
    let suites = list_dir(cases_dir)
    if suites.len() == 0 {
        eprintln("Error: no test suites in " + cases_dir)
        return 2
    }

    -- A directory of this run's own, in /tmp so that its path needs no
    -- quoting: the cases put the path of their program into commands
    let mut pid = 0
    unsafe {
        pid = getpid() as int
    }
    let work_root = "/tmp/daimond_test_" + int_to_string(pid) + "_" + int_to_string(time_ms())
    if fs_mkdir(work_root) != 0 {
        eprintln("Error: cannot create " + work_root)
        return 2
    }

    println("")
    println("============================================================")
    println("  dAImond Integration Tests")
    println("============================================================")
    println("")

    let mut passed = 0
    let mut failed = 0
    let mut si = 0
    while si < suites.len() {
        let suite = suites[si]
        let suite_dir = cases_dir + "/" + suite
        let entries = list_dir(suite_dir)
        let mut printed_suite = false
        let mut ei = 0
        while ei < entries.len() {
            let entry = entries[ei]
            -- A case is X.dm, or a directory X that holds main.dm
            let mut name = ""
            let mut source = ""
            if ends_with(entry, ".dm") {
                name = substr(entry, 0, len(entry) - 3)
                source = suite_dir + "/" + entry
            } else if file_exists(suite_dir + "/" + entry + "/main.dm") {
                name = entry
                source = suite_dir + "/" + entry + "/main.dm"
            }
            if name != "" and string_contains(suite + "/" + name, filter) {
                if printed_suite == false {
                    println("--- " + suite + " ---")
                    printed_suite = true
                }
                let work = work_root + "/case"
                fs_mkdir(work)
                let outcome = run_case(compiler, source, suite_dir + "/" + name, work)
                remove_tree(work)
                if outcome.passed {
                    println("  PASS: " + name)
                    passed = passed + 1
                } else {
                    println("  FAIL: " + name)
                    println("        " + outcome.message)
                    failed = failed + 1
                }
                flush()
            }
            ei = ei + 1
        }
        if printed_suite {
            println("")
        }
        si = si + 1
    }
    remove_tree(work_root)

    println("============================================================")
    println("  Results: " + int_to_string(passed) + " passed, " + int_to_string(failed) + " failed")
    println("============================================================")

    if passed + failed == 0 {
        eprintln("Error: no test case matches '" + filter + "'")
        return 2
    }
    if failed > 0 {
        return 1
    }
    return 0
}
