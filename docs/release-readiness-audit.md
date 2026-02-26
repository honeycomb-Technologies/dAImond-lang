# dAImond Front-to-Back Release Readiness Audit

## Scope

This audit evaluates what is required to ship dAImond as a **releasable, downloadable language distribution** (compiler + runtime + standard library + tooling + packaging + release process), similar to mature language ecosystems.

## Executive Summary

dAImond already has strong core assets (multi-stage compiler architecture, runtime, stdlib modules, and sizable test corpus), but it is not yet in “download-and-use” final form.

The highest-impact blockers are:

1. **No automated release pipeline** (no CI/workflows checked into repo).
2. **No binary distribution/install story** (users currently build from source with Zig/GCC/LLVM).
3. **Bootstrap complexity remains high for end users** (Stage 4 build process is manual and multi-step).
4. **Packaging ecosystem is local-first and not yet registry-backed** (manifest/dependency model exists, but discoverability and trust infrastructure are immature).
5. **Environment validation is not encoded as a first-class doctor/preflight command** for predictable downloads.

## What Is Already Strong

- Stage 0 exposes a full CLI (build/run/lex/parse/check/fmt/test/pkg/clean), which is a good foundation for a distributable SDK command surface.  
- The project includes compiler unit tests plus integration tests in documented workflows, and Stage 3/Stage 4 architecture docs are present.  
- Runtime and stdlib are present in-repo with clear module layout, reducing external packaging complexity for v1 if bundled as a single distribution artifact.  
- Package manager commands include `init/add/list/install/update`, and lock-file generation is implemented, which is a good base to evolve into a real ecosystem.

## Readiness Matrix

| Area | Status | Why it matters for a downloadable language | Current observation | Priority |
|---|---|---|---|---|
| Compiler correctness | **Amber** | Must be predictably testable on every commit/release | Tests are defined, but this environment cannot run Zig-based test suite | P0 |
| Release automation | **Red** | Needed for reproducible tagged binaries + checksums | No `.github/workflows` present in repo | P0 |
| Binary distribution | **Red** | Users expect prebuilt downloads (Linux/macOS/Windows) | Current docs are source-build oriented (`zig build`) | P0 |
| Install UX | **Red** | “curl/powershell/package manager install” is baseline expectation | No installer scripts or package-manager formulas in-tree | P0 |
| Toolchain dependency minimization | **Amber** | Lower friction = broader adoption | Stage 4 exists but build path is still manual and LLVM-heavy | P1 |
| Package ecosystem maturity | **Amber** | Registry + provenance unlock third-party adoption | Manifest/lock exist; registry/security workflow not defined | P1 |
| Security & supply chain | **Red** | Signed artifacts and provenance are expected | No release signing/provenance policy documented | P1 |
| Docs for operators/contributors | **Amber** | Clear release/operator playbooks reduce bus factor | Strong architecture docs, but no release runbook | P1 |
| IDE/editor distribution | **Amber** | Modern language adoption depends on editor setup | `daimond-lsp` exists, but extension packaging/distribution path not documented | P2 |

## Gap Analysis and Migration Plan

### Phase 1 (P0): Make releases real (2–4 weeks)

1. **Add CI + release workflows**
   - Build and test Stage 0 on Linux/macOS/Windows.
   - Build release artifacts (`daimond`, `daimond-lsp`, runtime/stdlib bundle).
   - Publish checksums and SBOM per release.

2. **Define distribution artifacts**
   - `daimond-vX.Y.Z-{os}-{arch}.tar.gz` / `.zip` containing:
     - compiler binaries
     - runtime files
     - stdlib files
     - version manifest
   - Add `daimond --version --verbose` to print embedded commit/build metadata.

3. **Create installers**
   - POSIX install script + uninstall script.
   - PowerShell installer for Windows.
   - Homebrew tap + Scoop/winget/Nix package definitions (can start community-maintained but linked officially).

4. **Add a `daimond doctor` command**
   - Detect required host tooling and versions.
   - Check runtime/stdlib install integrity.
   - Validate C compiler/LLVM availability where required.

### Phase 2 (P1): Stabilize self-hosting and packaging ecosystem (4–8 weeks)

1. **Promote a canonical production backend**
   - Decide/declare primary backend for users (Stage 0 C backend or Stage 4 LLVM backend), and make the other advanced/experimental if needed.
   - Publish explicit support matrix by OS/arch and backend.

2. **Bootstrap simplification**
   - Replace manual Stage 4 command chain with scripted build entrypoint.
   - Ensure deterministic bootstrap in CI (fixed-point verification optional but recommended for release branches).

3. **Package registry v1 plan**
   - Keep local/path/git dependencies now, but define roadmap for:
     - package namespace ownership
     - immutable versioning
     - checksum verification in lock file
     - signature support

4. **Security hardening**
   - Artifact signing (cosign/minisign/GPG).
   - SLSA-style provenance attestation for release artifacts.

### Phase 3 (P2): Final-form developer experience (ongoing)

1. **Editor distribution**
   - Publish and version VS Code extension tied to `daimond-lsp` release cadence.

2. **Project templates**
   - `daimond new` starter templates (app/lib/tests/ffi).

3. **Compatibility contract**
   - Language semver policy (breaking changes only in major releases).
   - Standard library stability guarantees.

## Concrete “Definition of Done” for Final Form

dAImond is in final releasable/downloadable form when all are true:

- Tagged releases automatically produce signed binaries for at least Linux/macOS/Windows.
- Installers/package-manager entries exist and are documented on the main README.
- `daimond doctor` gives deterministic setup diagnostics for users.
- CI runs unit + integration suites on all supported targets prior to release.
- One backend is clearly documented as production/default with support policy.
- Package lockfile includes integrity metadata and install path is reproducible.
- Release notes include migration guidance and compatibility statements.

## Evidence Snapshot (Repository)

- Stage 0 build config defines executables and unit/integration test steps.  
- CLI command set includes language tooling and package management commands.  
- Package manager supports `install` and `update` workflows and writes `daimond.lock`.  
- Current docs emphasize source builds and manual Stage 4 build steps.  
- No in-repo GitHub Actions workflow directory was found.

## Immediate Next Actions (Recommended This Week)

1. Add `docs/release-process.md` with release checklist and artifact contract.
2. Add CI workflow to run Stage 0 tests on every PR.
3. Add release workflow for tagged builds with checksums.
4. Add `daimond doctor` command skeleton and first checks.
5. Publish first “preview” binary release to validate install flow.
