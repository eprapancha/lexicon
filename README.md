# Lexicon: Emacs for the Modern Web

> **A faithful recreation of GNU Emacs in the browser, built with Rust, ClojureScript, and WebAssembly**

---

## What is Lexicon?

Lexicon is **GNU Emacs running in the browser** - not Emacs-inspired, not Emacs-like, but **actual Emacs** with its core architecture faithfully implemented:

- **Gap buffer** text storage (like Emacs C core)
- **Hierarchical keymaps** with exact Emacs precedence
- **Major and minor modes** with buffer-local state
- **Command-oriented** editing model
- **Package system** where Evil-mode, Vertico, etc. are external packages
- **Minibuffer** for interactive commands

**Technology Stack:** Rust/WASM (gap buffer), ClojureScript/re-frame (Emacs Lisp layer), Reagent (UI)

---

## Current Status

**Phase:** E2E Testing & Quality Assurance

**What Works:**
- Full Emacs-style editing (insert, delete, undo, kill/yank)
- Multi-buffer/window management (C-x b, C-x 2/3, C-x o, C-x 0/1)
- Hierarchical keymaps with Emacs precedence
- Interactive minibuffer with completion (M-x, TAB)
- Help system (C-h k/f/b/a/?)
- Package system with SCI sandbox
- Per-window mode-lines
- Hook system with priorities
- Query-replace with regexp support

**Current Work:**
- Comprehensive E2E test suite ([Issue #94](https://github.com/eprapancha/lexicon/issues/94))
- UI tests (keyboard simulation) and Lisp API tests
- Test-driven development for remaining features

**See [GitHub Issues](https://github.com/eprapancha/lexicon/issues) for detailed status**

---

## Quick Start

### Prerequisites
- **Nix** (recommended) or Node.js 18+, Java 17+, Rust
- **Firefox** (for E2E tests)

### Run Lexicon

```bash
# Clone and enter nix shell
git clone https://github.com/eprapancha/lexicon.git
cd lexicon
nix-shell

# Build WASM
cd packages/lexicon-engine/wasm && wasm-pack build --target web && cd ../../..

# Install deps and start dev server
cd packages/editor-cljs && npm install && cd ../..
bb dev

# Open http://localhost:8080
```

### Try It Out

Once loaded, try these commands:
- Type text naturally
- `C-x C-f` - Open file
- `C-x 2` / `C-x 3` - Split window
- `C-x o` - Switch windows
- `M-x` - Execute command (with TAB completion)
- `C-h k` - Describe what a key does
- `C-h b` - List all keybindings
- `M-%` - Query replace

---

## Documentation

| Document | Description |
|----------|-------------|
| [DEVELOPMENT.md](./docs/DEVELOPMENT.md) | Build, test, and contribute |
| [ROADMAP.md](./docs/ROADMAP.md) | Development phases and status |
| [ARCHITECTURE_BOUNDARY.md](./docs/ARCHITECTURE_BOUNDARY.md) | Core/package boundaries |
| [ARCHITECTURE.md](./docs/ARCHITECTURE.md) | Technical details |

---

## Project Structure

```
lexicon/
├── docs/                      # Documentation
├── packages/
│   ├── editor-cljs/           # ClojureScript core (Emacs Lisp layer)
│   ├── lexicon-engine/        # Rust/WASM gap buffer (Emacs C layer)
│   ├── evil-mode/             # Vim emulation package (future)
│   └── backend-server/        # Bridge server (LSP, git)
├── e2e_tests/                 # E2E tests (Etaoin/Firefox)
│   └── lexicon/
│       ├── ui/                # Keyboard simulation tests
│       └── lisp/              # Lisp API tests
└── scripts/                   # Build scripts
```

---

## Philosophy

Lexicon is an **experiment in architectural fidelity**. Most "Emacs-inspired" editors borrow Emacs's keybindings but not its architecture. We're doing the opposite - building the actual Emacs architecture in the browser.

**Why?** Because Emacs got it right. After 40+ years, Emacs's architecture remains the gold standard for extensible text editors:

- **Gap buffers** for text storage
- **Command-oriented** editing with interactive specifications
- **Hierarchical keymaps** with prefix keys
- **Mode-based** extensibility
- **Lisp-powered** configuration (ClojureScript instead of Elisp)

The web deserves this same power.

---

## Contributing

We welcome contributions! Before starting:

1. Check [GitHub Issues](https://github.com/eprapancha/lexicon/issues) for current work
2. Read [DEVELOPMENT.md](./docs/DEVELOPMENT.md) for setup instructions
3. Run `bb test:e2e` to verify tests pass
4. Study Emacs source before implementing features

**Workflow:**
- Write tests first (TDD)
- Commit after each green test
- Zero warnings policy

---

## Technology Stack

```
┌─────────────────────────────────────┐
│         Browser (localhost:8080)    │
│  ┌───────────────────────────────┐  │
│  │   ClojureScript (re-frame)    │  │  ← Emacs Lisp layer
│  │   - Buffers, windows, modes   │  │
│  │   - Commands, keymaps         │  │
│  │   - UI rendering (Reagent)    │  │
│  └───────────────┬───────────────┘  │
│                  │ WASM FFI          │
│  ┌───────────────┴───────────────┐  │
│  │   Rust/WASM (Gap Buffer)      │  │  ← Emacs C core
│  │   - Text storage & operations │  │
│  │   - UTF-8 handling            │  │
│  └───────────────────────────────┘  │
└─────────────────────────────────────┘
         │ WebSocket (future)
┌─────────────────────────────────────┐
│   Lexicon Bridge (localhost:30303) │  ← LSP, git, etc.
└─────────────────────────────────────┘
```

---

## License

MIT License - See [LICENSE](./LICENSE)

---

**Status:** Active Development
**Last Updated:** 2026-01-28

*Building Emacs for the web, one gap buffer at a time.*
