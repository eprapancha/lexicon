# Lexicon: Emacs for the Modern Web

> **A faithful recreation of GNU Emacs in the browser, built with Rust, ClojureScript, and WebAssembly**

**[Try it now: https://eprapancha.github.io/lexicon](https://eprapancha.github.io/lexicon)**

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

## Implemented Features

### Core Editing Commands

| Command | Keybinding | Description |
|---------|------------|-------------|
| `forward-char` | `C-f`, `<right>` | Move forward one character |
| `backward-char` | `C-b`, `<left>` | Move backward one character |
| `next-line` | `C-n`, `<down>` | Move to next line |
| `previous-line` | `C-p`, `<up>` | Move to previous line |
| `forward-word` | `M-f` | Move forward one word |
| `backward-word` | `M-b` | Move backward one word |
| `beginning-of-line` | `C-a`, `<home>` | Move to beginning of line |
| `end-of-line` | `C-e`, `<end>` | Move to end of line |
| `beginning-of-buffer` | `M-<` | Move to beginning of buffer |
| `end-of-buffer` | `M->` | Move to end of buffer |
| `scroll-up-command` | `C-v`, `<PageDown>` | Scroll down one screen |
| `scroll-down-command` | `M-v`, `<PageUp>` | Scroll up one screen |

### Kill, Yank, and Editing

| Command | Keybinding | Description |
|---------|------------|-------------|
| `kill-line` | `C-k` | Kill to end of line |
| `kill-region` | `C-w` | Kill region (cut) |
| `copy-region-as-kill` | `M-w` | Copy region |
| `yank` | `C-y` | Yank (paste) |
| `yank-pop` | `M-y` | Cycle through kill ring |
| `kill-word` | `M-d` | Kill word forward |
| `backward-kill-word` | `M-DEL` | Kill word backward |
| `undo` | `C-/`, `C-_` | Undo last change |
| `delete-backward-char` | `DEL` | Delete character before point |
| `delete-forward-char` | `C-d` | Delete character at point |
| `open-line` | `C-o` | Insert newline after point |
| `newline` | `RET` | Insert newline |
| `delete-indentation` | `M-^` | Join line with previous |
| `dabbrev-expand` | `M-/` | Dynamic abbreviation expansion |

### Mark and Region

| Command | Keybinding | Description |
|---------|------------|-------------|
| `set-mark-command` | `C-SPC` | Set mark at point |
| `exchange-point-and-mark` | `C-x C-x` | Swap point and mark |
| Shift+arrow | `S-<arrow>` | Extend selection |

### Search and Replace

| Command | Keybinding | Description |
|---------|------------|-------------|
| `isearch-forward` | `C-s` | Incremental search forward |
| `isearch-backward` | `C-r` | Incremental search backward |
| `query-replace` | `M-%` | Query replace |
| `query-replace-regexp` | `C-M-%` | Query replace with regexp |
| `replace-string` | - | Replace all occurrences |
| `replace-regexp` | - | Replace with regexp |

### Buffer Management

| Command | Keybinding | Description |
|---------|------------|-------------|
| `switch-to-buffer` | `C-x b` | Switch to buffer |
| `kill-buffer` | `C-x k` | Kill buffer |
| `list-buffers` | `C-x C-b` | List all buffers |
| `buffer-menu` | `M-x buffer-menu` | Open buffer menu |

**Buffer Menu Mode Keys:**
| Key | Description |
|-----|-------------|
| `RET` | Select buffer at point |
| `q` | Quit buffer menu |
| `d` | Mark for deletion |
| `x` | Execute deletions |
| `s` | Save buffer |
| `u` | Unmark |
| `g` | Refresh list |
| `n`/`p` | Navigate up/down |

### Window Management

| Command | Keybinding | Description |
|---------|------------|-------------|
| `split-window-below` | `C-x 2` | Split window horizontally |
| `split-window-right` | `C-x 3` | Split window vertically |
| `delete-window` | `C-x 0` | Delete current window |
| `delete-other-windows` | `C-x 1` | Delete other windows |
| `other-window` | `C-x o` | Switch to next window |
| `windmove-left` | `S-<left>` | Select window to left |
| `windmove-right` | `S-<right>` | Select window to right |
| `windmove-up` | `S-<up>` | Select window above |
| `windmove-down` | `S-<down>` | Select window below |
| `winner-undo` | `C-c <left>` | Undo window config (winner-mode) |
| `winner-redo` | `C-c <right>` | Redo window config (winner-mode) |

### File Operations

| Command | Keybinding | Description |
|---------|------------|-------------|
| `find-file` | `C-x C-f` | Open file (with path completion) |
| `save-buffer` | `C-x C-s` | Save buffer |
| `write-file` | `C-x C-w` | Save as... |
| `revert-buffer` | - | Reload from disk |
| `dired` | `M-x dired` | Directory editor |
| `grant-directory-access` | - | Grant FS Access API permission |

**Dired Mode Keys:**
| Key | Description |
|-----|-------------|
| `RET` | Open file/directory |
| `n`/`p` | Navigate up/down |
| `^` | Go to parent directory |
| `g` | Refresh listing |

### Rectangle Operations

| Command | Keybinding | Description |
|---------|------------|-------------|
| `kill-rectangle` | `C-x r k` | Kill rectangle |
| `delete-rectangle` | `C-x r d` | Delete rectangle |
| `yank-rectangle` | `C-x r y` | Yank rectangle |
| `open-rectangle` | `C-x r o` | Insert blank rectangle |
| `clear-rectangle` | `C-x r c` | Clear rectangle (spaces) |
| `string-rectangle` | `C-x r t` | Replace rectangle with string |

### Keyboard Macros

| Command | Keybinding | Description |
|---------|------------|-------------|
| `kmacro-start-macro` | `F3`, `C-x (` | Start recording macro |
| `kmacro-end-or-call-macro` | `F4`, `C-x )` | Stop recording / replay |
| `call-last-kbd-macro` | `C-x e` | Replay last macro |

### Help System

| Command | Keybinding | Description |
|---------|------------|-------------|
| `describe-key` | `C-h k` | Describe what key does |
| `describe-function` | `C-h f` | Describe function |
| `describe-variable` | `C-h v` | Describe variable |
| `describe-bindings` | `C-h b` | List all keybindings |
| `apropos-command` | `C-h a` | Search commands |
| `help-for-help` | `C-h ?` | Help on help |

### Prefix Arguments

| Command | Keybinding | Description |
|---------|------------|-------------|
| `universal-argument` | `C-u` | Prefix argument (4x) |
| `digit-argument` | `C-0` to `C-9` | Numeric argument |
| `negative-argument` | `C--` | Negative argument |

### Occur Mode (Matching Lines)

| Command | Keybinding | Description |
|---------|------------|-------------|
| `occur` | `M-x occur` | List lines matching regexp |

**Occur Mode Keys:**
| Key | Description |
|-----|-------------|
| `RET` | Go to occurrence |
| `n`/`p` | Navigate occurrences |
| `q` | Quit occur buffer |

### Outline Mode (Folding)

| Command | Keybinding | Description |
|---------|------------|-------------|
| `outline-hide-body` | `C-c @ C-t` | Hide all body text |
| `outline-show-all` | `C-c @ C-a` | Show everything |
| `outline-hide-entry` | `C-c @ C-c` | Hide current entry |
| `outline-show-entry` | `C-c @ C-e` | Show current entry |
| `outline-toggle-children` | `C-c @ TAB` | Toggle visibility |

**Hideshow (Code Folding):**
| Command | Keybinding | Description |
|---------|------------|-------------|
| `hs-hide-block` | `C-c @ C-h` | Hide block |
| `hs-show-block` | `C-c @ C-s` | Show block |
| `hs-hide-all` | `C-c @ C-M-h` | Hide all blocks |
| `hs-show-all` | `C-c @ C-M-s` | Show all blocks |

### Miscellaneous

| Command | Keybinding | Description |
|---------|------------|-------------|
| `execute-extended-command` | `M-x` | Run command by name |
| `keyboard-quit` | `C-g` | Cancel current operation |

---

## Implemented Minor Modes

| Mode | Description |
|------|-------------|
| `line-number-mode` | Show line number in mode-line |
| `column-number-mode` | Show column number in mode-line |
| `display-line-numbers-mode` | Show line numbers in margin |
| `electric-pair-mode` | Auto-insert matching brackets |
| `delete-selection-mode` | Replace selection when typing |
| `show-paren-mode` | Highlight matching parentheses |
| `hl-line-mode` | Highlight current line |
| `whitespace-mode` | Show whitespace characters |
| `auto-save-mode` | Auto-save buffers |
| `read-only-mode` | Toggle read-only (`C-x C-q`) |
| `icomplete-mode` | Incremental completion in minibuffer |
| `winner-mode` | Window configuration undo/redo |
| `which-function-mode` | Show current function in mode-line |
| `font-lock-mode` | Syntax highlighting |
| `hs-minor-mode` | Code folding (hideshow) |
| `outline-minor-mode` | Outline navigation |

---

## Implemented Major Modes

| Mode | Description |
|------|-------------|
| `fundamental-mode` | Default mode |
| `text-mode` | Plain text editing |
| `clojure-mode` | Clojure editing |
| `javascript-mode` | JavaScript editing |
| `python-mode` | Python editing |
| `rust-mode` | Rust editing |
| `html-mode` | HTML editing |
| `css-mode` | CSS editing |
| `markdown-mode` | Markdown editing |
| `help-mode` | Help buffer display |
| `special-mode` | Read-only special buffers |
| `buffer-menu-mode` | Buffer list display |
| `dired-mode` | Directory editor |
| `occur-mode` | Occur results display |
| `outline-mode` | Hierarchical document editing |
| `completion-list-mode` | *Completions* buffer navigation |

---

## Completion System

- **Minibuffer completion** with TAB
- **Completion styles**: basic, substring, flex
- **icomplete-mode**: Show candidates inline as you type
- **Completion cycling**: `C-.` / `C-,` (with icomplete)

---

## Architecture

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
```

---

## Quick Start

### Prerequisites
- **Nix** (recommended) or Node.js 18+, Java 17+, Rust
- **Firefox** (for E2E tests)

### Run Locally

```bash
# Clone and enter nix shell
git clone https://github.com/eprapancha/lexicon.git
cd lexicon
nix-shell

# Build and run
bb build
bb dev

# Open http://localhost:8080
```

### Run Tests

```bash
bb test        # All tests
bb test:e2e    # E2E tests only
bb lint        # Linters
```

---

## Project Structure

```
lexicon/
├── docs/                      # Documentation
├── packages/
│   ├── editor-cljs/           # ClojureScript core (Emacs Lisp layer)
│   ├── lexicon-engine/        # Rust/WASM gap buffer (Emacs C core)
│   ├── evil-mode/             # Vim emulation package (future)
│   └── backend-server/        # Bridge server (LSP, git)
├── e2e_tests/                 # E2E tests (Etaoin/Firefox)
└── scripts/                   # Build scripts
```

---

## Roadmap

**Current Phase:** 6.6 - Emacs Semantic Compatibility

**Recently Completed:**
- File System Access API for Emacs-style find-file (#135)
- occur-mode - List matching lines (#131)
- Buffer menu with full functionality (#115)
- Outline mode and code folding (#114)
- Font-lock and which-function modes (#130)
- Emacs completion framework (#136, #137, #138)

**In Progress:**
- Version control integration (vc.el, vc-git.el) (#113)
- Project management (project.el, xref.el) (#116)
- Programming support (compile, flymake, imenu) (#122)

**Upcoming:**
- LSP client (eglot.el) (#129)
- Eldoc and documentation (#124)

**See [GitHub Issues](https://github.com/eprapancha/lexicon/issues) for detailed status**

---

## Contributing

We welcome contributions! Before starting:

1. Check [GitHub Issues](https://github.com/eprapancha/lexicon/issues)
2. Read [DEVELOPMENT.md](./docs/DEVELOPMENT.md)
3. Run `bb test:e2e` to verify tests pass
4. Study Emacs source before implementing features

**Workflow:** Write tests first (TDD), commit after each green test, zero warnings policy.

---

## License

GNU General Public License v3.0 - See [LICENSE](./LICENSE)

---

**Status:** Active Development
**Last Updated:** 2026-02-03

*Building Emacs for the web, one gap buffer at a time.*
