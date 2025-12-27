# Nix Policy

This document clarifies where Nix references are allowed and forbidden in the project.

## ❌ Nix-Free Zones (Build System)

These files MUST NOT contain any Nix-specific code or references:

- ✅ `bb.edn` - Build tasks
- ✅ `package.json` - Root package file
- ✅ `packages/*/package.json` - Workspace packages
- ✅ `.github/workflows/*.yml` - GitHub Actions
- ✅ `BUILD.md` - Build documentation (can mention other options, but shouldn't push Nix)

**Why:** These files define the portable build system that works on any platform.

## ✅ Nix-Allowed Zones (Local Dev Only)

These files CAN contain Nix-specific code:

- `shell.nix` - Local development shell (Nix-only file)
- `.claude/settings.local.json` - Local editor config (gitignored)
- `DEVELOPMENT_ENVIRONMENT.md` - Explains local dev setup options
- `.github/LOCAL_TESTING.md` - Can suggest Nix as one option among others

**Why:** These are local development conveniences that don't affect the build system.

## Documentation Guidelines

When documenting tool installation:

**❌ Don't do this:**
```bash
# Install wasm-pack
nix-shell -p wasm-pack
```

**✅ Do this instead:**
```bash
# Install wasm-pack
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
```

**✅ Or this (showing multiple options):**
```bash
# Install wasm-pack

# Universal (works everywhere)
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# Or use your package manager
brew install wasm-pack              # macOS
cargo install wasm-pack             # Rust
nix-shell -p wasm-pack              # NixOS (local dev only)
```

## Verification

To verify the build system is Nix-free:

```bash
# Should return only ✅ Clean for all files
for file in bb.edn package.json packages/*/package.json .github/workflows/*.yml; do
  echo -n "$file: "
  grep -i "nix" "$file" > /dev/null 2>&1 && echo "❌ CONTAINS NIX" || echo "✅ Clean"
done
```

## Rationale

**Nix is a great local development tool**, but:

1. Not everyone uses NixOS
2. CI/CD environments (GitHub Actions, Docker) have their own sandboxing
3. Mixing environment managers creates confusion
4. Platform-agnostic builds are more maintainable

**Use Nix for local convenience, but keep it out of the build system.**
