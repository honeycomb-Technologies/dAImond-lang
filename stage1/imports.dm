module imports

-- ============================================================
-- MULTI-FILE IMPORT RESOLVER
-- ============================================================

-- Extract directory from a file path (everything up to and including last '/')
fn dir_of(path: string) -> string {
    let mut last_slash = 0 - 1
    let mut i = 0
    let path_len = len(path)
    while i < path_len {
        let ch = char_at(path, i)
        if ch == "/" {
            last_slash = i
        }
        i = i + 1
    }
    if last_slash < 0 {
        return ""
    }
    return substr(path, 0, last_slash + 1)
}

-- Scan source text for import lines, return list of module names
fn find_imports(source: string) -> List[string] {
    let mut imports: List[string] = []
    let src_len = len(source)
    let mut pos = 0

    while pos < src_len {
        -- Skip to start of line
        -- Check if line starts with "import " (possibly after whitespace)
        let mut lp = pos
        while lp < src_len and (char_at(source, lp) == " " or char_at(source, lp) == "\t") {
            lp = lp + 1
        }

        -- Check for "import " at current position
        let mut is_import = false
        if lp + 7 <= src_len {
            let word = substr(source, lp, 7)
            if word == "import " {
                is_import = true
            }
        }

        if is_import {
            -- Extract module name: everything after "import " until newline
            let mut name_start = lp + 7
            -- Skip whitespace after "import "
            while name_start < src_len and char_at(source, name_start) == " " {
                name_start = name_start + 1
            }
            let mut name_end = name_start
            while name_end < src_len and char_at(source, name_end) != "\n" and char_at(source, name_end) != "\r" {
                name_end = name_end + 1
            }
            -- Trim trailing whitespace from name
            let mut trim_end = name_end
            while trim_end > name_start and (char_at(source, trim_end - 1) == " " or char_at(source, trim_end - 1) == "\t") {
                trim_end = trim_end - 1
            }
            if trim_end > name_start {
                let module_name = substr(source, name_start, trim_end - name_start)
                imports.push(module_name)
            }
        }

        -- Advance to next line
        while pos < src_len and char_at(source, pos) != "\n" {
            pos = pos + 1
        }
        if pos < src_len {
            pos = pos + 1
        }
    }

    return imports
}

-- Remove "module X" line from source (first occurrence only)
fn strip_module_decl(source: string) -> string {
    let src_len = len(source)
    let mut pos = 0

    while pos < src_len {
        -- Skip whitespace at start of line
        let mut lp = pos
        while lp < src_len and (char_at(source, lp) == " " or char_at(source, lp) == "\t") {
            lp = lp + 1
        }

        -- Check for "module " at current position
        let mut is_module = false
        if lp + 7 <= src_len {
            let word = substr(source, lp, 7)
            if word == "module " {
                is_module = true
            }
        }

        if is_module {
            -- Find end of line
            let mut line_end = lp
            while line_end < src_len and char_at(source, line_end) != "\n" {
                line_end = line_end + 1
            }
            if line_end < src_len {
                line_end = line_end + 1
            }
            -- Remove this line: before + after
            let before = substr(source, 0, pos)
            let after = substr(source, line_end, src_len - line_end)
            return before + after
        }

        -- Advance to next line
        while pos < src_len and char_at(source, pos) != "\n" {
            pos = pos + 1
        }
        if pos < src_len {
            pos = pos + 1
        }
    }

    return source
}

-- Remove "import X" lines from source
fn strip_import_decls(source: string) -> string {
    let src_len = len(source)
    let mut result = ""
    let mut pos = 0

    while pos < src_len {
        let mut line_start = pos
        -- Find end of line
        let mut line_end = pos
        while line_end < src_len and char_at(source, line_end) != "\n" {
            line_end = line_end + 1
        }
        if line_end < src_len {
            line_end = line_end + 1
        }

        -- Check if this line is an import
        let mut lp = pos
        while lp < src_len and (char_at(source, lp) == " " or char_at(source, lp) == "\t") {
            lp = lp + 1
        }
        let mut is_import = false
        if lp + 7 <= src_len {
            let word = substr(source, lp, 7)
            if word == "import " {
                is_import = true
            }
        }

        if is_import == false {
            let line = substr(source, line_start, line_end - line_start)
            result = result + line
        }

        pos = line_end
    }

    return result
}

-- Resolve module name to file path using dots as directory separators
fn resolve_module_path(base_dir: string, module_name: string) -> string {
    -- Replace dots with '/' for nested modules: "std.io" -> "std/io"
    let path_part = string_replace(module_name, ".", "/")
    return base_dir + path_part + ".dm"
}

-- Recursively resolve imports and concatenate sources
-- imported_modules is a pipe-delimited string tracking already-imported modules
struct ResolveResult {
    source: string,
    imported: string
}

fn collect_imports_recursive(base_dir: string, source: string, imported: string) -> ResolveResult {
    let imports = find_imports(source)
    let mut combined_imports = ""
    let mut current_imported = imported

    let mut i = 0
    while i < imports.len() {
        let module_name = "" + imports[i]
        let marker = "|" + module_name + "|"

        -- Skip if already imported
        if string_contains(current_imported, marker) == false {
            current_imported = current_imported + marker

            let file_path = resolve_module_path(base_dir, module_name)
            let mod_source = file_read(file_path)

            -- Determine the base directory for the imported file
            let mod_dir = dir_of(file_path)

            -- Recursively collect imports from this module first
            let inner = collect_imports_recursive(mod_dir, mod_source, current_imported)
            current_imported = inner.imported
            combined_imports = combined_imports + inner.source

            -- Strip module declaration and import lines from imported source
            let cleaned = strip_import_decls(strip_module_decl(mod_source))

            combined_imports = combined_imports + "\n-- [imported: " + module_name + "]\n" + cleaned + "\n"
        }
        i = i + 1
    }

    return ResolveResult { source: combined_imports, imported: current_imported }
}

fn resolve_imports(source: string, filename: string) -> string {
    let imports = find_imports(source)
    -- If no imports, return source unchanged
    if imports.len() == 0 {
        return source
    }

    let base_dir = dir_of(filename)

    -- Collect all imported code
    let result = collect_imports_recursive(base_dir, source, "")

    -- Build final source: module decl from main, then imports, then rest of main
    -- The main source keeps its module declaration, but imports are inserted after it
    let main_no_imports = strip_import_decls(source)

    -- Find the end of the module declaration line to insert imports after it
    let src_len = len(main_no_imports)
    let mut mod_end = 0
    let mut pos = 0
    let mut found_module = false

    while pos < src_len and found_module == false {
        let mut lp = pos
        while lp < src_len and (char_at(main_no_imports, lp) == " " or char_at(main_no_imports, lp) == "\t") {
            lp = lp + 1
        }
        let mut is_module = false
        if lp + 7 <= src_len {
            if substr(main_no_imports, lp, 7) == "module " {
                is_module = true
            }
        }
        -- Find end of this line
        let mut line_end = pos
        while line_end < src_len and char_at(main_no_imports, line_end) != "\n" {
            line_end = line_end + 1
        }
        if line_end < src_len {
            line_end = line_end + 1
        }
        if is_module {
            mod_end = line_end
            found_module = true
        }
        pos = line_end
    }

    if found_module {
        let before = substr(main_no_imports, 0, mod_end)
        let after = substr(main_no_imports, mod_end, src_len - mod_end)
        return before + "\n" + result.source + "\n" + after
    }
    -- No module decl found: just prepend imports
    return result.source + "\n" + main_no_imports
}
