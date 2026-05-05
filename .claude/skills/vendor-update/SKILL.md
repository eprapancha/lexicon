# Vendor Update Skill

Manage vendored external Rust crates in `packages/lexicon-engine/vendor/`.

## When to Vendor

Only vendor code when **all** of these apply:
1. The crate requires WASM-specific patches to compile/run in browser
2. Upstream won't accept the patches (platform-specific, too niche)
3. No alternative crate exists that works on wasm32 out of the box

If the patch is small and upstreamable, submit a PR upstream first.

## Current Inventory

See `packages/lexicon-engine/vendor/MANIFEST.edn` for the authoritative manifest.

Current vendors (from BurntSushi/ripgrep):
- **grep-matcher** v0.1.8 — regex matching trait (unmodified)
- **grep-regex** v0.1.14 — regex implementation (unmodified)
- **grep-searcher** v0.1.16 — line-oriented search (WASM mmap patch)

## Quick Commands

```bash
bb vendor:check    # Show status, check for upstream updates
bb vendor:sync     # Pull updates from fork into vendor/
```

## Update Process

### 1. Rebase the Fork

```bash
cd /tmp && git clone <fork-url> ripgrep-fork
cd ripgrep-fork
git fetch upstream
git rebase upstream/<new-tag> lexicon-wasm-patches
# Resolve conflicts in patch files
git push --force-with-lease origin lexicon-wasm-patches
```

### 2. Sync Vendor Directory

```bash
bb vendor:sync              # Uses fork HEAD
bb vendor:sync 14.2.0       # Target a specific tag
```

### 3. Verify

```bash
cargo test --manifest-path packages/lexicon-engine/Cargo.toml
bb vendor:check
```

### 4. Update MANIFEST.edn

Update the `:fork` section with the new SHA and the `:upstream` section with the new tag/SHA. Update `:vendored-date` to today.

### 5. Commit

```
deps(vendor): update ripgrep crates to <version>

- Rebased WASM patches onto <tag>
- Updated MANIFEST.edn with new SHAs
```

## Adding a New Vendored Crate

1. **Fork** the upstream repo (if not already forked)
2. **Create a patch branch** (e.g., `lexicon-wasm-patches`)
3. **Make minimal patches** — see Patch Conventions below
4. **Copy** the crate directory into `packages/lexicon-engine/vendor/`
5. **Add** the crate as a workspace member in `packages/lexicon-engine/Cargo.toml`
6. **Add** an entry to `MANIFEST.edn` with all provenance fields
7. **Run** `cargo test` and `bb vendor:check`
8. **Commit** with type `deps(vendor):`

## Patch Conventions

- **Minimal** — only change what's necessary for WASM compilation
- **Feature-gated** — use `#[cfg(target_arch = "wasm32")]` where possible
- **Documented** — every patch file listed in MANIFEST.edn with description and reason
- **Isolated** — don't modify code paths that work fine on wasm32
- **Commented** — add `// LEXICON PATCH:` comments at modification points

## Troubleshooting

### Edition Mismatch
If upstream bumps Rust edition (e.g., 2021 -> 2024), update `Cargo.toml` in the vendored crate and verify the workspace `rust-version` supports it.

### New OS-Specific Imports
If upstream adds new platform-specific code (mmap, signals, etc.), add `#[cfg(not(target_arch = "wasm32"))]` guards in the patch branch.

### Cargo Workspace Resolution
Vendored crates must be listed in the workspace `[patch]` or `[dependencies]` with `path = "vendor/<crate>"`. Check `packages/lexicon-engine/Cargo.toml` if builds fail after updating.

### Version Conflicts
If other dependencies pull in a different version of the same crate, use `[patch.crates-io]` in the workspace root to force the vendored version.
