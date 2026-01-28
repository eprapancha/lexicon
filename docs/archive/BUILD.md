# Lexicon Build System

This project uses [Babashka](https://babashka.org/) for build orchestration.

## Prerequisites

- **Babashka** (>= 1.3.0): `nix-shell -p babashka` or see [installation](https://github.com/babashka/babashka#installation)
- **Node.js** (>= 16.0.0)
- **Java** (>= 17) - for ClojureScript compilation
- **Rust** + **wasm-pack** - for WASM builds
- **tree-sitter-cli** (optional) - for grammar builds

## Quick Start

```bash
# Install dependencies and start development
npm run dev

# Or use bb directly
bb dev
```

## Available Tasks

Run `bb tasks` to see all available tasks:

### Development
- `bb dev` - Start ClojureScript watch mode
- `bb dev-server` - Start development server
- `bb build-dev` - Development build (faster, debug mode)

### Production Build
- `bb build` - Full production build (WASM + ClojureScript)
- `bb build-wasm` - Build Rust WASM module (release)
- `bb build-cljs` - Build ClojureScript (production)
- `bb build-grammars` - Build Tree-sitter grammars (optional)

### Individual Components
- `bb install-cljs` - Install npm dependencies
- `bb setup-cljs-deps` - Copy static assets
- `bb build-wasm-debug` - Build WASM in debug mode
- `bb build-cljs-dev` - Build ClojureScript in dev mode

### Testing & Utilities
- `bb test` - Run all tests
- `bb ci-test` - Test CI workflow locally (simulates GitHub Actions)
- `bb clean` - Clean build artifacts
- `bb serve` - Serve built application on :8000
- `bb build-and-serve` - Build and serve

## npm Scripts

All tasks are also available via npm:

```bash
npm run build         # bb build
npm run dev           # bb dev
npm run test          # bb test
npm run clean         # bb clean
npm run build-wasm    # bb build-wasm
# ... etc
```

## Project Structure

```
lexicon/
├── bb.edn                          # Babashka build configuration
├── package.json                    # Root package (workspaces)
└── packages/
    ├── editor-cljs/                # ClojureScript frontend
    │   ├── shadow-cljs.edn
    │   └── package.json
    ├── lexicon-engine/             # Rust WASM engine
    │   ├── Cargo.toml
    │   ├── core/                   # Core Rust library
    │   └── wasm/                   # WASM bindings
    ├── language-grammars/          # Tree-sitter grammars
    ├── backend-server/             # Clojure backend
    ├── evil-mode/                  # Vim emulation
    └── lexicon-bridge/             # Bridge utilities
```

## Build Pipeline

### Full Build (`bb build`)
1. Install ClojureScript npm dependencies
2. Setup static assets (web-tree-sitter)
3. Build Rust WASM module (release)
4. Build ClojureScript frontend (production)

**Note:** Tree-sitter grammars are optional and can be built separately with `bb build-grammars`

### Development Build (`bb build-dev`)
1. Install ClojureScript npm dependencies
2. Setup static assets
3. Build Rust WASM module (debug)
4. Build ClojureScript frontend (dev)

## CI/CD

GitHub Actions workflows use Babashka:

- **CI** (`.github/workflows/ci.yml`) - Build and test on PR/push
- **Deploy** (`.github/workflows/deploy.yml`) - Build and deploy to GitHub Pages

Both workflows:
1. Setup Node.js, Java, Rust, and wasm-pack (via official actions)
2. Install Babashka (via official installer script)
3. Run `bb install-cljs`
4. Run `bb build`
5. Run `bb test` (CI only)
6. Deploy to GitHub Pages (deploy only)

All tools are installed using official sources - no third-party actions with version bugs.

## Testing CI Locally

You have two options for testing before pushing to GitHub:

### Option 1: Quick Test (Fast)
```bash
bb ci-test
```

Simulates GitHub Actions locally without Docker (fast, but not 100% identical).

### Option 2: Full Simulation with act (Accurate)
```bash
# Install act (one-time setup)
bb install-act

# List available workflows
bb act-list

# Run the full CI workflow in Docker (exactly like GitHub)
bb act-ci
```

**Recommended workflow:**
1. Use `bb ci-test` for quick iterations
2. Use `bb act-ci` for final verification before pushing

See `.github/LOCAL_TESTING.md` for detailed instructions.

## Troubleshooting

### Missing wasm-pack
```bash
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
```

See: https://rustwasm.github.io/wasm-pack/installer/

### Missing tree-sitter (optional)
Tree-sitter grammars are optional for development. To build them:

```bash
npm install -g tree-sitter-cli
bb build-grammars
```

### Clean build
```bash
bb clean
bb build
```

## Migration from Shell Scripts

All shell scripts have been removed and replaced with Babashka tasks:

| Old Script | New Command |
|------------|-------------|
| `./scripts/build.sh` | `bb build` |
| `./scripts/build-wasm.sh` | `bb build-wasm` |
| `./scripts/dev.sh` | `bb dev` |
| `./scripts/test.sh` | `bb test` |
| `./scripts/build-and-serve.sh` | `bb build-and-serve` |
| `./test-integration.sh` | (removed - covered by `bb test`) |
