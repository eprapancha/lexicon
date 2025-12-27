# Development Environment Setup

This document explains the separation between local development tooling and the portable build system.

## Philosophy

The build system (Babashka tasks) is **environment-agnostic** and works on any platform with the required tools installed. Local development conveniences (like Nix) are kept separate and never leak into CI/CD or distribution.

## Build System (Platform Agnostic)

The following files define the **portable** build system that works everywhere:

- `bb.edn` - Babashka build tasks (no platform-specific paths)
- `package.json` - npm scripts (delegates to Babashka)
- `.github/workflows/` - GitHub Actions (uses standard actions)

**Required tools** (install however you prefer):
- Node.js >= 16
- Java >= 17
- Babashka >= 1.3.0
- Rust + wasm-pack
- tree-sitter-cli (optional)

These files contain **zero** platform-specific code or paths.

## Local Development (Optional)

### For NixOS Users

The `shell.nix` file is provided for **local convenience only**:

```bash
# Enter development shell (provides all tools)
nix-shell

# Then use standard build commands
bb dev
bb build
```

**Important:** `shell.nix` is NOT required for building. It simply provides a convenient way to get all tools in one command on NixOS.

### For Non-Nix Users

Just install the required tools using your preferred method:

```bash
# macOS with Homebrew
brew install babashka node openjdk@17 rust wasm-pack

# Ubuntu/Debian
# Install Node, Java, Rust via your package manager
# Install Babashka: https://github.com/babashka/babashka#installation

# Then use the same build commands
bb dev
bb build
```

## CI/CD Environments

GitHub Actions installs tools using standard actions:
- `actions/setup-node` for Node.js
- `actions/setup-java` for Java
- `turtlequeue/setup-babashka` for Babashka
- `dtolnay/rust-toolchain` for Rust
- `jetli/wasm-pack-action` for wasm-pack

**No Nix**, **no Docker**, **no platform-specific magic**.

## Claude Code Settings

The `.claude/settings.local.json` file contains PATH manipulations for local development only. This file:
- Is in `.gitignore` (local only)
- Helps Claude Code find tools in your local Nix environment
- Does NOT affect the build system
- Does NOT get pushed to GitHub

## Summary

```
┌─────────────────────────────────────┐
│   Portable Build System             │
│   - bb.edn                          │
│   - package.json                    │
│   - .github/workflows/*.yml         │
│                                     │
│   Works everywhere, no assumptions  │
└─────────────────────────────────────┘
              ▲
              │
              │ uses
              │
┌─────────────────────────────────────┐
│   Local Development (Optional)      │
│   - shell.nix (NixOS convenience)   │
│   - .claude/ (editor config)        │
│                                     │
│   Never affects builds or CI        │
└─────────────────────────────────────┘
```

## Verification

To verify no platform-specific code leaked into the build system:

```bash
# Check for hardcoded paths in build files
grep -r "nix-profile\|/home/" bb.edn package.json .github/workflows/

# Should return nothing (or only documentation)
```

The build system is clean and portable! 🎉
