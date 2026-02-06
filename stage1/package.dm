module package

-- ============================================================
-- PACKAGE MANAGER (TOML Manifest + Dependency Resolution)
-- ============================================================

-- Package manifest data stored as parallel lists for checker compatibility.
-- Each index i represents one dependency with:
--   dep_names[i], dep_versions[i], dep_paths[i], dep_gits[i]
struct PackageManifest {
    name: string,
    version: string,
    dep_names: List[string],
    dep_versions: List[string],
    dep_paths: List[string],
    dep_gits: List[string]
}

-- Parse a simple TOML file into a PackageManifest struct
-- Supports: [package] name/version, [dependencies] name = "version" or name = { path = "..." }
fn parse_toml_manifest(content: string) -> PackageManifest {
    let mut pkg_name = ""
    let mut pkg_version = ""
    let mut dep_names: List[string] = []
    let mut dep_versions: List[string] = []
    let mut dep_paths: List[string] = []
    let mut dep_gits: List[string] = []
    let mut in_package = false
    let mut in_deps = false

    let lines = string_split(content, "\n")
    let mut i = 0
    while i < lines.len() {
        let line = string_trim(lines[i])
        -- Skip empty lines and comments
        if line == "" or starts_with(line, "#") {
            i = i + 1
            continue
        }
        -- Section headers
        if line == "[package]" {
            in_package = true
            in_deps = false
            i = i + 1
            continue
        }
        if line == "[dependencies]" {
            in_package = false
            in_deps = true
            i = i + 1
            continue
        }
        if starts_with(line, "[") {
            in_package = false
            in_deps = false
            i = i + 1
            continue
        }
        -- Key-value pairs
        let eq_pos = string_find(line, "=")
        if eq_pos > 0 {
            let key = string_trim(substr(line, 0, eq_pos))
            let val_raw = string_trim(substr(line, eq_pos + 1, len(line) - eq_pos - 1))
            -- Remove quotes from value
            let mut val = val_raw
            if starts_with(val, "\"") and ends_with(val, "\"") {
                val = substr(val, 1, len(val) - 2)
            }
            if in_package {
                if key == "name" { pkg_name = val }
                if key == "version" { pkg_version = val }
            }
            if in_deps {
                -- Simple version dep: name = "version"
                -- Path dep: name = { path = "..." }
                -- Git dep: name = { git = "..." }
                let dname = key
                if starts_with(val_raw, "{") {
                    -- Inline table: parse path/git/version
                    let mut dp = ""
                    let mut dg = ""
                    let mut dv = ""
                    if string_contains(val_raw, "path") {
                        let path_pos = string_find(val_raw, "path")
                        let after_path = substr(val_raw, path_pos + 4, len(val_raw) - path_pos - 4)
                        let eq2 = string_find(after_path, "=")
                        if eq2 >= 0 {
                            let path_val_raw = string_trim(substr(after_path, eq2 + 1, len(after_path) - eq2 - 1))
                            let mut pv = path_val_raw
                            -- Remove quotes and trailing }
                            pv = string_replace(pv, "\"", "")
                            pv = string_replace(pv, "}", "")
                            pv = string_replace(pv, ",", "")
                            dp = string_trim(pv)
                        }
                    }
                    if string_contains(val_raw, "git") {
                        let git_pos = string_find(val_raw, "git")
                        let after_git = substr(val_raw, git_pos + 3, len(val_raw) - git_pos - 3)
                        let eq3 = string_find(after_git, "=")
                        if eq3 >= 0 {
                            let git_val_raw = string_trim(substr(after_git, eq3 + 1, len(after_git) - eq3 - 1))
                            let mut gv = git_val_raw
                            gv = string_replace(gv, "\"", "")
                            gv = string_replace(gv, "}", "")
                            gv = string_replace(gv, ",", "")
                            dg = string_trim(gv)
                        }
                    }
                    if string_contains(val_raw, "version") {
                        let ver_pos = string_find(val_raw, "version")
                        let after_ver = substr(val_raw, ver_pos + 7, len(val_raw) - ver_pos - 7)
                        let eq4 = string_find(after_ver, "=")
                        if eq4 >= 0 {
                            let ver_val_raw = string_trim(substr(after_ver, eq4 + 1, len(after_ver) - eq4 - 1))
                            let mut vv = ver_val_raw
                            vv = string_replace(vv, "\"", "")
                            vv = string_replace(vv, "}", "")
                            vv = string_replace(vv, ",", "")
                            dv = string_trim(vv)
                        }
                    }
                    dep_names.push(dname)
                    dep_versions.push(dv)
                    dep_paths.push(dp)
                    dep_gits.push(dg)
                } else {
                    dep_names.push(dname)
                    dep_versions.push(val)
                    dep_paths.push("")
                    dep_gits.push("")
                }
            }
        }
        i = i + 1
    }
    return PackageManifest { name: pkg_name, version: pkg_version, dep_names: dep_names, dep_versions: dep_versions, dep_paths: dep_paths, dep_gits: dep_gits }
}

-- Generate a default daimond.toml content
fn generate_default_toml(project_name: string) -> string {
    let mut content = "[package]\n"
    content = content + "name = \"" + project_name + "\"\n"
    content = content + "version = \"0.1.0\"\n\n"
    content = content + "[dependencies]\n"
    return content
}

-- Add a dependency to the toml content
fn add_dependency_to_toml(content: string, dep_name: string, dep_version: string) -> string {
    -- Find [dependencies] section and add after it
    let deps_pos = string_find(content, "[dependencies]")
    if deps_pos < 0 {
        return content + "\n[dependencies]\n" + dep_name + " = \"" + dep_version + "\"\n"
    }
    -- Find end of [dependencies] line
    let after_deps = substr(content, deps_pos, len(content) - deps_pos)
    let nl_pos = string_find(after_deps, "\n")
    if nl_pos < 0 {
        return content + "\n" + dep_name + " = \"" + dep_version + "\"\n"
    }
    let insert_pos = deps_pos + nl_pos + 1
    let before = substr(content, 0, insert_pos)
    let after = substr(content, insert_pos, len(content) - insert_pos)
    return before + dep_name + " = \"" + dep_version + "\"\n" + after
}

-- Handle 'pkg init' command: create daimond.toml
fn pkg_init() {
    if file_exists("daimond.toml") {
        println("daimond.toml already exists")
        return
    }
    let content = generate_default_toml("my-project")
    file_write("daimond.toml", content)
    println("Created daimond.toml")
}

-- Handle 'pkg add <name>' command: add dependency
fn pkg_add(dep_name: string) {
    if file_exists("daimond.toml") == false {
        eprintln("Error: no daimond.toml found. Run 'pkg init' first.")
        exit(1)
    }
    let content = file_read("daimond.toml")
    let new_content = add_dependency_to_toml(content, dep_name, "*")
    file_write("daimond.toml", new_content)
    println("Added dependency: " + dep_name)
}

-- Handle 'pkg list' command: list dependencies
fn pkg_list() {
    if file_exists("daimond.toml") == false {
        eprintln("Error: no daimond.toml found. Run 'pkg init' first.")
        exit(1)
    }
    let content = file_read("daimond.toml")
    let pkg = parse_toml_manifest(content)
    println("Package: " + pkg.name + " v" + pkg.version)
    if pkg.dep_names.len() == 0 {
        println("No dependencies")
        return
    }
    println("Dependencies:")
    let mut i = 0
    while i < pkg.dep_names.len() {
        let mut info = "  " + pkg.dep_names[i]
        let dver = "" + pkg.dep_versions[i]
        let dpath = "" + pkg.dep_paths[i]
        let dgit = "" + pkg.dep_gits[i]
        if dver != "" {
            info = info + " = \"" + dver + "\""
        }
        if dpath != "" {
            info = info + " (path: " + dpath + ")"
        }
        if dgit != "" {
            info = info + " (git: " + dgit + ")"
        }
        println(info)
        i = i + 1
    }
}
