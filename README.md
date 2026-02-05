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
| `ibuffer` | `M-x ibuffer` | Advanced buffer management |

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

**ibuffer** provides filter groups (by mode, name, starred buffers), sorting (alphabetic, size, mode, recency), and bulk mark/delete operations.

**uniquify**: Buffer names are automatically disambiguated when multiple files share the same name (e.g., `config.cljs<src>` vs `config.cljs<test>`). Supports forward, reverse, post-forward, and post-forward-angle-brackets styles.

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
| `m` | Mark file |
| `u` | Unmark file |
| `U` | Unmark all |
| `t` | Toggle marks |
| `d` | Flag for deletion |
| `x` | Execute flagged deletions |
| `C` | Copy file |
| `R` | Rename/move file |
| `D` | Delete file |
| `+` | Create directory |

**File Persistence:**
- **recentf**: Tracks recently opened files (persisted to localStorage)
- **saveplace**: Remembers cursor position per file across sessions
- **autorevert**: Auto-refreshes unmodified buffers when files change on disk (5s interval, requires FS Access API)

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

### Programming Support

| Command | Keybinding | Description |
|---------|------------|-------------|
| `compile` | `M-x compile` | Run compilation command |
| `recompile` | `M-x recompile` | Repeat last compilation |
| `next-error` | `C-x \`` | Jump to next error |
| `previous-error` | - | Jump to previous error |
| `first-error` | - | Jump to first error |
| `imenu` | `M-x imenu` | Jump to definition in buffer |

**Compilation Mode Keys:**
| Key | Description |
|-----|-------------|
| `g` | Recompile |
| `n`/`p` | Navigate errors |
| `RET` | Jump to error location |
| `q` | Quit compilation buffer |

### Diff Mode

View and navigate unified diffs:

| Command | Description |
|---------|-------------|
| `diff-hunk-next` / `n` | Navigate to next hunk |
| `diff-hunk-prev` / `p` | Navigate to previous hunk |
| `diff-file-next` / `N` | Navigate to next file |
| `diff-file-prev` / `P` | Navigate to previous file |
| `diff-goto-source` / `RET` | Jump to source location |
| `diff-hunk-kill` / `k` | Remove current hunk |
| `q` | Quit diff buffer |

*Note: ediff (visual side-by-side diff) and smerge-mode (merge conflict resolution) are not yet implemented.*

### Interactive Highlighting (hi-lock)

| Command | Keybinding | Description |
|---------|------------|-------------|
| `highlight-regexp` | `M-s h r` | Highlight matches of a regexp |
| `highlight-phrase` | `M-s h p` | Highlight a phrase |
| `highlight-lines-matching-regexp` | `M-s h l` | Highlight entire matching lines |
| `unhighlight-regexp` | `M-s h u` | Remove highlighting |
| `highlight-symbol-at-point` | `M-s h .` | Highlight symbol at point |

### Eldoc

`eldoc-mode` shows function signatures and documentation in the echo area after a brief idle delay. Supports `global-eldoc-mode` and a pluggable documentation provider system.

### Info Documentation Browser

`M-x info` opens the Info documentation browser with built-in Lexicon documentation.

| Key | Description |
|-----|-------------|
| `n`/`p` | Next/previous node |
| `u` | Go up in hierarchy |
| `l`/`r` | History back/forward |
| `t` | Top node |
| `d` | Directory node |
| `s` | Search |
| `q` | Quit |

*Note: Menu navigation (m) and cross-reference following (f) are not yet implemented. Only built-in documentation nodes are available.*

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
| `font-lock-mode` | Syntax highlighting (with multiline and JIT support) |
| `hs-minor-mode` | Code folding (hideshow) |
| `outline-minor-mode` | Outline navigation |
| `eldoc-mode` | Show documentation in echo area |
| `hi-lock-mode` | Interactive regexp highlighting |
| `recentf-mode` | Track recently opened files |
| `save-place-mode` | Remember cursor position per file |
| `auto-revert-mode` | Auto-refresh when file changes on disk |

---

## Implemented Major Modes

**Core Modes (fully implemented):**
| Mode | Description |
|------|-------------|
| `fundamental-mode` | Default mode |
| `text-mode` | Plain text editing |
| `help-mode` | Help buffer display |
| `special-mode` | Read-only special buffers |
| `buffer-menu-mode` | Buffer list with mark/delete/execute |
| `dired-mode` | Directory editor with file operations |
| `occur-mode` | Occur results with navigation |
| `outline-mode` | Hierarchical document editing |
| `completion-list-mode` | *Completions* buffer navigation |
| `diff-mode` | Unified diff viewing with hunk navigation |
| `info-mode` | Info documentation browser |
| `shell-mode` | Shell buffer (built-in commands only) |
| `ibuffer-mode` | Advanced buffer filtering and grouping |

**Programming Modes (basic support):**

*These modes provide syntax highlighting via font-lock and basic editing. They do NOT include language-specific features like LSP integration, debugging, or advanced refactoring - those require additional packages.*

| Mode | Includes |
|------|----------|
| `clojure-mode` | Syntax highlighting, paren matching |
| `javascript-mode` | Syntax highlighting |
| `python-mode` | Syntax highlighting |
| `rust-mode` | Syntax highlighting |
| `html-mode` | Syntax highlighting |
| `css-mode` | Syntax highlighting |
| `markdown-mode` | Syntax highlighting |

---

## Completion System

- **Minibuffer completion** with TAB
- **Completion styles**: basic, substring, flex
- **icomplete-mode**: Show candidates inline as you type
- **Completion cycling**: `C-.` / `C-,` (with icomplete)

---

## Features with Browser Limitations

The following features have command infrastructure in place but are constrained by the browser environment. They provide the UI framework and will become fully functional when connected to a backend server.

### Version Control (vc.el)

Commands registered: `vc-next-action`, `vc-diff`, `vc-log`, `vc-revert`, `vc-register`, `vc-annotate`. Mode-line shows VC status indicator (`%v` construct, e.g., `Git-main`).

**Limitation:** Cannot execute git commands in the browser. Commands show informational messages but do not perform actual version control operations. Requires a backend server bridge.

### Shell and Eshell

`M-x shell` and `M-x eshell` open shell buffers. `M-x shell-command` (`M-!`) executes commands. Built-in eshell commands work: `echo`, `pwd`, `date`, `help`, `whoami`, `history`, `clear`.

**Limitation:** Cannot spawn real processes in the browser. Only built-in commands execute; external commands (ls, grep, etc.) are not available without a backend server.

### Project Management (project.el)

Commands registered: `project-find-file`, `project-switch-project`, `project-search`. Project registry and detection framework in place.

**Limitation:** Project root detection and file listing require file system traversal not available in browser context. Currently returns placeholder data.

### Cross-References (xref.el)

Commands registered: `xref-find-definitions` (`M-.`), `xref-find-references` (`M-?`), `xref-go-back` (`M-,`). Marker stack for navigation history works.

**Limitation:** Actual definition/reference lookup requires an LSP backend or tag system. Currently extracts identifier at point but cannot resolve definitions.

### LSP Client (eglot)

Commands registered: `eglot`, `eglot-shutdown`, `eglot-reconnect`, `eglot-rename`, `eglot-code-actions`, `eglot-format-buffer`. Server program configuration for multiple languages defined.

**Limitation:** No LSP protocol implementation yet. Requires WebSocket bridge to communicate with language servers. This is framework/command registration only.

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
- Buffer name uniquification (#141)
- ibuffer - advanced buffer management (#140)
- Font-lock multiline patterns and JIT fontification (#143, #144)
- Eldoc documentation in echo area (#124)
- File persistence: recentf, saveplace, autorevert (#118)
- Diff mode with hunk navigation (#119, partial - diff-mode only)
- Hi-lock interactive highlighting (#125, partial - hi-lock only)
- Dired mode with file operations via FS Access API (#139)
- Programming support: compile, flymake, imenu (#122)
- File System Access API for Emacs-style find-file (#135)
- occur-mode - List matching lines (#131)
- Buffer menu with full functionality (#115)
- Outline mode and code folding (#114)
- Font-lock and which-function modes (#130)
- Emacs completion framework (#136, #137, #138)

**In Progress (framework in place, needs backend):**
- Version control integration - vc.el commands and mode-line (#113)
- Project management - project.el, xref.el commands (#116)
- Shell/Eshell - built-in commands work (#112)
- Info browser - basic navigation works (#142)
- LSP client - eglot command framework (#129)

**Not Started:**
- Terminal emulation - term.el, comint.el (#127)
- Remote files - tramp.el (#126)
- grep.el - grep output parsing (part of #125)
- ediff / smerge-mode - visual diff/merge (part of #119)

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
**Last Updated:** 2026-02-05

*Building Emacs for the web, one gap buffer at a time.*
