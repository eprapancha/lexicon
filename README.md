# Lexicon: Emacs for the Modern Web

> **A shameless recreation of GNU Emacs in the browser, built with Rust, ClojureScript, and WebAssembly**

---

## Current Status: Architecture Reset (Phase 0)

**⚠️ PROJECT UNDER ACTIVE REFACTORING ⚠️**

Lexicon is currently undergoing a significant architectural realignment to stay true to Emacs principles. We got ahead of ourselves by implementing complex features (Evil-mode) before establishing a solid core, and basic functionality is currently broken.

**What's Working:**
- ✅ Development environment (Nix-based, reproducible)
- ✅ WASM loading infrastructure
- ✅ re-frame state management setup
- ✅ Basic DOM rendering

**What's Broken/In Progress:**
- ⚠️ **Basic text input** - Enter key doesn't create newline (being fixed)
- ⚠️ **Architecture** - Evil-mode code mixed into core (being separated)
- ⚠️ **Buffer engine** - Piece tree being replaced with gap buffer

**Current Focus:** Establishing a minimal, working Emacs core before adding packages.

See [ROADMAP.md](./docs/ROADMAP.md) for detailed plan and [CORE_PRINCIPLES.md](./docs/CORE_PRINCIPLES.md) for architectural guidelines.

---

## Vision

Lexicon aims to be **GNU Emacs for the web** - not inspired by Emacs, not Emacs-like, but **actual Emacs** running in the browser. We faithfully implement Emacs's architecture:

- **Gap buffer** for text storage (like Emacs C core)
- **Hierarchical keymaps** with exact Emacs precedence
- **Major and minor modes** with buffer-local state
- **Command dispatcher** with interactive specifications
- **Minibuffer** for user interaction
- **Package system** where Evil-mode, Vertico, etc. are external packages

**Why?** Because Emacs has the best editing architecture ever designed, and the web deserves it.

---

## Core Principles

### 1. Emacs Purity
When in doubt, do what Emacs does. Study `emacs/src/*.c` and Elisp before implementing.

### 2. Core is Sacred
Core = bare Emacs (`emacs -Q`). Everything else is a package.
- ✅ Core: Buffers, windows, keymaps, commands, modes, minibuffer
- ❌ Not Core: Evil-mode, Vertico, LSP, themes

### 3. Gap Buffer
We use gap buffers like Emacs, not piece trees. Simplicity and Emacs-alignment matter more than theoretical performance.

### 4. Userspace/Kernel Split
- **Rust/WASM**: Low-level gap buffer operations (like Emacs C core)
- **ClojureScript**: Everything else (like Emacs Lisp layer)

### 5. Packages are External
Evil-mode lives in `packages/evil-mode/`, not in core. Packages interact with core via clean APIs.

Read the full principles: [CORE_PRINCIPLES.md](./docs/CORE_PRINCIPLES.md)

---

## Technology Stack

```
┌─────────────────────────────────────┐
│         Browser (localhost:8080)    │
│  ┌───────────────────────────────┐  │
│  │   ClojureScript (re-frame)    │  │  ← Emacs Lisp layer equivalent
│  │   - Buffers, windows, modes   │  │
│  │   - Commands, keymaps         │  │
│  │   - UI rendering (Reagent)    │  │
│  └───────────────┬───────────────┘  │
│                  │ WASM FFI          │
│  ┌───────────────┴───────────────┐  │
│  │   Rust/WASM (Gap Buffer)      │  │  ← Emacs C core equivalent
│  │   - Text storage & operations │  │
│  │   - UTF-8 handling            │  │
│  └───────────────────────────────┘  │
└─────────────────────────────────────┘
         │ WebSocket (future)
┌─────────────────────────────────────┐
│   Lexicon Bridge (localhost:30303) │  ← For LSP, git, etc.
│   - Language servers               │
│   - Native tool integration        │
└─────────────────────────────────────┘
```

**Core Technologies:**
- **ClojureScript** + **re-frame** - Functional reactive UI & state
- **Rust** + **WebAssembly** - High-performance text engine
- **Reagent** - React wrapper for ClojureScript
- **Nix** - Reproducible development environment

---

## Roadmap

We're building Lexicon in phases, each delivering working software:

### Phase 0: Architecture Reset (Current - Dec 2025)
🔄 **In Progress**
- Extract Evil-mode from core to `packages/evil-mode/`
- Replace piece tree with gap buffer
- Fix basic text input (typing, Enter, Backspace)
- Establish clean core/package boundary

### Phase 1: Basic Editing (Jan 2025)
🔲 Planned
- Navigation: C-f, C-b, C-n, C-p, C-a, C-e
- Editing: insert, delete, newline, kill-line
- Selection: C-SPC, C-w, M-w, C-y
- Undo: C-/

### Phase 2: Buffers & Files (Jan-Feb 2025)
🔲 Planned
- Multi-buffer support: C-x b, C-x k
- File I/O: C-x C-f, C-x C-s
- Buffer switching with minibuffer completion

### Phase 3: Windows (Feb 2025)
🔲 Planned
- Window splitting: C-x 2, C-x 3
- Window navigation: C-x o
- Window deletion: C-x 0, C-x 1

### Phase 4: Minibuffer & Completion (Feb-Mar 2025)
🔲 Planned
- M-x command execution
- Completion (TAB)
- File/buffer name completion

### Phase 5: Modes & Keymaps (Mar 2025)
🔲 Planned
- Major modes: fundamental, text, clojure
- Minor modes: auto-fill, line-number
- Mode hooks
- Keymap inheritance

### Phase 6: Package System & Evil-mode (Mar-Apr 2025)
🔲 Planned
- Package loading system
- Evil-mode as external package
- Normal, Insert, Visual modes
- Vim operators and motions

**See [ROADMAP.md](./docs/ROADMAP.md) for complete phased plan**

---

## Project Structure

```
lexicon/
├── docs/
│   ├── CORE_PRINCIPLES.md      # Architectural guidelines (READ THIS)
│   ├── ROADMAP.md              # Detailed phased plan
│   └── architecture.md         # Technical architecture
│
├── packages/
│   ├── lexicon-core/           # THE BARE EMACS (future)
│   │   ├── wasm/              # Rust gap buffer
│   │   └── cljs/              # ClojureScript core
│   │
│   ├── editor-cljs/           # Current code (being refactored)
│   ├── lexicon-engine/        # Current WASM (piece tree → gap buffer)
│   │
│   ├── evil-mode/             # Vim emulation package (future)
│   ├── vertico/               # Completion UI (future)
│   └── lexicon-bridge/        # LSP bridge server
│
└── scripts/                   # Build and dev scripts
```

---

## Getting Started (For Developers)

### Prerequisites
- **Nix** (with flakes enabled)
- **Modern browser** (Chrome, Firefox, Edge)
- **Node.js 18+** and **npm**

### Development Setup

```bash
# Clone the repository
git clone https://github.com/yourusername/lexicon.git
cd lexicon

# Enter Nix development shell
nix-shell

# Install JavaScript dependencies
npm install

# Build Rust/WASM engine
cd packages/lexicon-engine/wasm
wasm-pack build --target web --out-dir pkg
cd ../../..

# Start development server
npm run dev

# In another terminal, start bridge server (optional, for LSP)
cd packages/lexicon-bridge
node index.js
```

Open browser to `http://localhost:8080`

**⚠️ Current State:** You'll see a basic editor, but Enter key doesn't work yet. We're fixing this!

---

## Current Implementation Status

### What Actually Works

✅ **Infrastructure:**
- Nix-based reproducible dev environment
- Shadow-cljs build pipeline
- WASM module loading
- re-frame state management
- Basic DOM rendering
- Bridge server (for future LSP integration)

### What's Being Fixed (Phase 0)

🔄 **Core Editor:**
- Gap buffer implementation (replacing piece tree)
- Basic text input (Enter key broken)
- Cursor positioning
- Character insertion/deletion

🔄 **Architecture:**
- Extracting Evil-mode to separate package
- Cleaning core namespace
- Defining core API for packages

### What's Coming Next (Phase 1)

🔲 **Basic Editing:**
- Navigation commands (C-f, C-b, C-n, C-p)
- Kill ring (C-w, M-w, C-y)
- Undo (C-/)
- Line editing (C-k, C-a, C-e)

---

## Documentation

- **[CORE_PRINCIPLES.md](./docs/CORE_PRINCIPLES.md)** - Read this first! Architectural philosophy and guidelines
- **[ROADMAP.md](./docs/ROADMAP.md)** - Detailed phased development plan
- **[architecture.md](./docs/architecture.md)** - Technical architecture details
- **[DEPLOYMENT.md](./DEPLOYMENT.md)** - Deployment guide (outdated, will update)

---

## Contributing

Lexicon is in active refactoring. We're not ready for external contributions yet, but you can:

- ⭐ **Star the repo** to follow progress
- 👀 **Watch** for updates
- 💬 **Open issues** for questions or suggestions
- 📖 **Read the principles** to understand our approach

Once Phase 0 is complete (working basic editor), we'll welcome contributions!

---

## Philosophy

Lexicon is an **experiment in architectural fidelity**. Most "Emacs-inspired" editors borrow Emacs's keybindings but not its architecture. We're doing the opposite - building the actual Emacs architecture in the browser:

- **Gap buffers** for text storage
- **Command-oriented** editing model
- **Hierarchical keymaps** with prefix keys
- **Mode-based** extensibility
- **Lisp-powered** configuration (ClojureScript instead of Elisp)

Why? Because Emacs got it right. After 40+ years, Emacs's architecture remains the gold standard for extensible text editors. The web deserves that same power.

**We may fail**, but we'll learn a lot trying. And if we succeed, we'll have Emacs in the browser.

---

## Learning Resources

Understanding Lexicon requires understanding Emacs:

**Emacs Internals:**
- [GNU Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
- [Emacs source code](https://github.com/emacs-mirror/emacs) - Especially `src/buffer.c`, `src/keyboard.c`
- [Emacs Wiki - Architecture](https://www.emacswiki.org/)

**Gap Buffers:**
- [The Text Editor Sam](http://doc.cat-v.org/plan_9/4th_edition/papers/sam/) - Another gap buffer implementation
- [Emacs Buffer Implementation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Internals.html)

**Project Inspirations:**
- [GNU Emacs](https://www.gnu.org/software/emacs/) - The original
- [CodeMirror 6](https://codemirror.net/6/) - Modern web editor architecture
- [Xi Editor](https://github.com/xi-editor/xi-editor) - High-performance editor (archived)

---

## Status Updates

Follow development progress:

- **GitHub Issues** - Track bugs and features
- **Commit History** - See daily progress
- **ROADMAP.md** - Updated weekly with phase progress

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

**Current Phase:** Architecture Reset (Phase 0)
**Last Updated:** 2025-12-20
**Status:** 🔄 Active Development

*Building Emacs for the web, one gap buffer at a time.*
