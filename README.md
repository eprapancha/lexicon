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

**Phase 6D of 11** - Missing Emacs Infrastructure ✅ COMPLETE | **Next: Phase 6.5 - Testing & QA**

**What Works:**
- ✅ **Phases 0-6D Complete** - Full Emacs infrastructure ready for packages!
- ✅ **Phase 6D Complete** - Buffer-local variables, hooks, advice, thing-at-point, advanced undo!
  - Text editing (insert, delete, undo, kill ring)
  - Navigation (C-f/b/n/p, C-a/e, M-f/b, M-</>, arrows)
  - Multi-buffer support (C-x b, C-x C-f, C-x C-s)
  - Window management (C-x 2/3, C-x o, C-x 0/1)
  - Minibuffer with completion (M-x, TAB completion)
  - Help system (C-h k/f/b/a/?)
  - Major/minor modes with hooks
  - Universal argument (C-u)
  - Mark and region (C-SPC, C-w, M-w, C-y)
  - Completion framework (metadata, styles, tables, CAPFs)
  - Built-in packages (project.el, imenu, recentf, savehist)
  - Buffer-local variables & enhanced hooks
  - Advice system for function wrapping
  - Thing-at-point subsystem
  - Advanced undo with boundaries and groups

**Current Work:**
- ✅ **Phase 6A-6D Complete** - All core Emacs infrastructure implemented!
- 📋 **Next: Phase 6.5** - Testing & Quality Assurance (CRITICAL before external contributions)

**See [docs/ROADMAP.md](./docs/ROADMAP.md) for detailed progress and next steps**

---

## Quick Start

### Prerequisites
- **Nix** (with flakes enabled)
- **Modern browser** (Chrome, Firefox, Edge)
- **Node.js 18+** and **npm**

### Run Lexicon

```bash
# Clone the repository
git clone https://github.com/yourusername/lexicon.git
cd lexicon

# Enter Nix development shell
nix develop

# Install dependencies and build WASM
./scripts/setup.sh

# Start development server
cd packages/editor-cljs
npx shadow-cljs watch app

# Open browser to http://localhost:8080
```

### Try It Out

Once loaded, try these commands:
- Type text naturally
- `C-x C-f` - Open file
- `C-x 2` - Split window horizontally
- `C-x o` - Switch windows
- `M-x` - Execute command (with TAB completion)
- `C-h k` - Describe what a key does
- `C-h b` - List all keybindings
- `C-h ?` - Help menu

---

## Documentation

- **[ROADMAP.md](./docs/ROADMAP.md)** - Detailed phased development plan (read this to understand project status)
- **[CORE_PRINCIPLES.md](./docs/CORE_PRINCIPLES.md)** - Architectural philosophy and guidelines
- **[architecture.md](./docs/architecture.md)** - Technical architecture details

---

## Project Structure

```
lexicon/
├── docs/
│   ├── CORE_PRINCIPLES.md      # Architectural guidelines
│   ├── ROADMAP.md              # Detailed phased plan
│   └── architecture.md         # Technical architecture
│
├── packages/
│   ├── editor-cljs/            # ClojureScript core (Emacs Lisp layer)
│   ├── lexicon-engine/         # Rust/WASM gap buffer (Emacs C layer)
│   ├── evil-mode/              # Vim emulation package
│   └── backend-server/         # Bridge server (LSP, git, etc.)
│
└── scripts/                    # Build and dev scripts
```

---

## Philosophy

Lexicon is an **experiment in architectural fidelity**. Most "Emacs-inspired" editors borrow Emacs's keybindings but not its architecture. We're doing the opposite - building the actual Emacs architecture in the browser.

**Why?** Because Emacs got it right. After 40+ years, Emacs's architecture remains the gold standard for extensible text editors:

- **Gap buffers** for text storage
- **Command-oriented** editing model with interactive specifications
- **Hierarchical keymaps** with prefix keys
- **Mode-based** extensibility (major modes, minor modes)
- **Lisp-powered** configuration (ClojureScript instead of Elisp)
- **Package system** for clean extensibility

The web deserves this same power.

**We may fail**, but we'll learn a lot trying. And if we succeed, we'll have Emacs in the browser.

---

## Contributing

**Current Status:** Phase 6B complete - display & theming foundation complete. Phase 6C (Completion Framework) next.

We're **not yet ready for external contributions**, but you can:

- ⭐ **Star the repo** to follow progress
- 👀 **Watch** for updates
- 💬 **Open issues** for questions or suggestions
- 📖 **Read [CORE_PRINCIPLES.md](./docs/CORE_PRINCIPLES.md)** to understand our approach

**After Phase 6.5** (Testing & Quality Assurance), we'll welcome contributions! This phase is CRITICAL before accepting PRs - we need automated tests to ensure changes don't break existing functionality.

See [ROADMAP.md](./docs/ROADMAP.md) Phase 6.5 for testing plan.

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
│   - Language servers               │
│   - Native tool integration        │
└─────────────────────────────────────┘
```

**Core Technologies:**
- **ClojureScript** + **re-frame** - Functional reactive UI & state management
- **Rust** + **WebAssembly** - High-performance text engine
- **Reagent** - React wrapper for ClojureScript
- **Nix** - Reproducible development environment

---

## Learning Resources

Understanding Lexicon requires understanding Emacs:

**Emacs Internals:**
- [GNU Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
- [Emacs source code](https://github.com/emacs-mirror/emacs) - Especially `src/buffer.c`, `src/keyboard.c`

**Gap Buffers:**
- [The Text Editor Sam](http://doc.cat-v.org/plan_9/4th_edition/papers/sam/)
- [Emacs Buffer Implementation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Internals.html)

**Project Inspirations:**
- [GNU Emacs](https://www.gnu.org/software/emacs/) - The original
- [CodeMirror 6](https://codemirror.net/6/) - Modern web editor architecture

---

## License

MIT License - See [LICENSE](./LICENSE) for details.

---

## Acknowledgments

- **GNU Emacs** - For 40 years of editorial excellence
- **Rich Hickey** - For Clojure(Script) and functional sanity
- **The Rust Community** - For WebAssembly tooling
- **You** - For being curious about this experiment

---

**Current Phase:** Phase 6D Complete | Next: Phase 6.5 - Testing & QA
**Last Updated:** 2025-12-25
**Status:** 🟢 Active Development - Phases 0-6D Complete!

*Building Emacs for the web, one gap buffer at a time.*
