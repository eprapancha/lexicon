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
- `bb build` - Full production build (WASM + grammars + ClojureScript)
- `bb build-wasm` - Build Rust WASM module (release)
- `bb build-grammars` - Build Tree-sitter grammars
- `bb build-cljs` - Build ClojureScript (production)

### Individual Components
- `bb install-cljs` - Install npm dependencies
- `bb setup-cljs-deps` - Copy static assets
- `bb build-wasm-debug` - Build WASM in debug mode
- `bb build-cljs-dev` - Build ClojureScript in dev mode

### Testing & Utilities
- `bb test` - Run all tests
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
4. Build Tree-sitter grammars to WASM
5. Build ClojureScript frontend (production)

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
1. Setup Node.js, Java, Babashka, Rust, and wasm-pack
2. Run `bb install-cljs`
3. Run `bb build`
4. Run `bb test` (CI only)
5. Deploy to GitHub Pages (deploy only)

## Troubleshooting

### Missing wasm-pack
```bash
# NixOS
nix-shell -p wasm-pack

# Or install directly
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
```

### Missing tree-sitter
```bash
npm install -g tree-sitter-cli
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
