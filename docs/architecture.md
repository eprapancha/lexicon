# Lexicon Technical Architecture

**Last Updated:** 2025-12-20
**Status:** Phase 0 (Architecture Reset)

---

## Table of Contents

1. [Overview](#overview)
2. [Core Principles](#core-principles)
3. [System Architecture](#system-architecture)
4. [Text Buffer Engine](#text-buffer-engine)
5. [State Management](#state-management)
6. [Command System](#command-system)
7. [Keymap System](#keymap-system)
8. [Mode System](#mode-system)
9. [Package Architecture](#package-architecture)
10. [Future: LSP Integration](#future-lsp-integration)

---

## Overview

Lexicon faithfully recreates GNU Emacs's architecture for the modern web. We don't just copy Emacs's keybindings - we implement its entire architectural philosophy.

### Design Philosophy

**Emacs C Core → Rust/WASM**
- Gap buffer text storage
- Low-level primitive operations
- UTF-8 text handling
- Performance-critical code

**Emacs Lisp Layer → ClojureScript**
- User-facing commands
- Modes and keymaps
- UI rendering
- Extensibility system

### Technology Stack

| Layer | Technology | Purpose |
|-------|-----------|---------|
| **UI** | Reagent (React) | View rendering |
| **State** | re-frame | Functional reactive state management |
| **Language** | ClojureScript | Userspace ("Elisp layer") |
| **Engine** | Rust + WASM | Kernel ("C core") |
| **Build** | Shadow-cljs | ClojureScript compilation |
| **Packaging** | wasm-pack | Rust→WASM compilation |
| **Dev Env** | Nix | Reproducible development environment |

---

## Core Principles

See [CORE_PRINCIPLES.md](./CORE_PRINCIPLES.md) for complete architectural guidelines. Key principles:

1. **Emacs Purity** - When in doubt, do what Emacs does
2. **Core is Sacred** - Bare Emacs only, everything else is a package
3. **Gap Buffer** - Use Emacs's proven text representation
4. **Userspace/Kernel Split** - Clear boundary between Rust and ClojureScript
5. **Packages are External** - Evil-mode, Vertico, etc. live in `packages/`

---

## System Architecture

### High-Level Structure

```
┌──────────────────────────────────────────────────────────┐
│                    Browser (localhost:8080)              │
│                                                          │
│  ┌────────────────────────────────────────────────────┐ │
│  │              ClojureScript Layer                   │ │
│  │              (Emacs Lisp analog)                   │ │
│  │                                                    │ │
│  │  ┌──────────────────────────────────────────────┐ │ │
│  │  │  Reagent Components (React)                  │ │ │
│  │  │  - Editor view                               │ │ │
│  │  │  - Minibuffer                                │ │ │
│  │  │  - Mode line                                 │ │ │
│  │  │  - Buffer tabs                               │ │ │
│  │  └──────────────────────────────────────────────┘ │ │
│  │                       ↕                            │ │
│  │  ┌──────────────────────────────────────────────┐ │ │
│  │  │  re-frame State & Events                     │ │ │
│  │  │  - Buffer management                         │ │ │
│  │  │  - Window management                         │ │ │
│  │  │  - Command dispatcher                        │ │ │
│  │  │  - Keymap lookup                             │ │ │
│  │  │  - Mode system                               │ │ │
│  │  └──────────────────────────────────────────────┘ │ │
│  │                       ↕ FFI                        │ │
│  │  ┌──────────────────────────────────────────────┐ │ │
│  │  │  Rust/WASM Text Engine                       │ │ │
│  │  │  (Emacs C core analog)                       │ │ │
│  │  │                                              │ │ │
│  │  │  - Gap buffer implementation                 │ │ │
│  │  │  - Insert/delete operations                  │ │ │
│  │  │  - Line/column conversion                    │ │ │
│  │  │  - UTF-8 validation                          │ │ │
│  │  └──────────────────────────────────────────────┘ │ │
│  └────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────┘
                           ↕ WebSocket (future)
┌──────────────────────────────────────────────────────────┐
│         Lexicon Bridge (localhost:30303)                 │
│         - Language servers (LSP)                         │
│         - Native tool integration                        │
└──────────────────────────────────────────────────────────┘
```

### Directory Structure

```
lexicon/
├── packages/
│   ├── lexicon-core/              # Future: Reorganized core
│   │   ├── wasm/                  # Rust gap buffer
│   │   │   ├── src/
│   │   │   │   ├── gap_buffer.rs  # Gap buffer implementation
│   │   │   │   └── lib.rs         # WASM bindings
│   │   │   └── Cargo.toml
│   │   └── cljs/                  # ClojureScript core
│   │       └── src/lexicon/core/
│   │           ├── buffer.cljs    # Buffer management
│   │           ├── command.cljs   # Command dispatcher
│   │           ├── keymap.cljs    # Keymap system
│   │           ├── mode.cljs      # Major/minor modes
│   │           ├── window.cljs    # Window (splits)
│   │           └── minibuffer.cljs
│   │
│   ├── editor-cljs/               # Current code (refactoring)
│   ├── lexicon-engine/            # Current WASM (refactoring)
│   │
│   ├── evil-mode/                 # Future: Vim emulation package
│   ├── vertico/                   # Future: Completion UI
│   └── lexicon-bridge/            # LSP bridge server
│
├── docs/
│   ├── CORE_PRINCIPLES.md         # Read this first!
│   ├── ROADMAP.md                 # Development phases
│   └── architecture.md            # This file
│
└── scripts/                       # Build scripts
```

---

## Text Buffer Engine

### Gap Buffer Implementation

Emacs uses gap buffers - we do too. A gap buffer is a contiguous array with an invisible gap at the editing point.

#### Structure

```rust
pub struct GapBuffer {
    text: Vec<u8>,        // UTF-8 encoded text
    gap_start: usize,     // Beginning of gap (in bytes)
    gap_end: usize,       // End of gap (exclusive, in bytes)
}
```

**Visual Representation:**

```
Before insertion:
[H][e][l][l][o][_][ ][ ][ ][ ][ ][W][o][r][l][d]
                  ^gap_start      ^gap_end

After inserting "there ":
[H][e][l][l][o][_][t][h][e][r][e][ ][W][o][r][l][d]
                                  ^gap_start/gap_end
                                  (gap exhausted)
```

#### Operations

**Insert at Point:**
```rust
fn insert(&mut self, position: usize, text: &str) {
    self.move_gap_to(position);
    // Copy text bytes into gap
    let bytes = text.as_bytes();
    self.text[self.gap_start..self.gap_start + bytes.len()]
        .copy_from_slice(bytes);
    self.gap_start += bytes.len();
}
```

**Delete:**
```rust
fn delete(&mut self, position: usize, length: usize) {
    self.move_gap_to(position);
    // Expand gap to include deleted text
    self.gap_end += length;
}
```

**Move Gap:**
```rust
fn move_gap_to(&mut self, position: usize) {
    if position < self.gap_start {
        // Move gap backward
        let distance = self.gap_start - position;
        // Use memmove to shift text
        self.text.copy_within(position..self.gap_start,
                             self.gap_end - distance);
        self.gap_start = position;
        self.gap_end -= distance;
    } else if position > self.gap_start {
        // Move gap forward
        let distance = position - self.gap_start;
        self.text.copy_within(self.gap_end..self.gap_end + distance,
                             self.gap_start);
        self.gap_start += distance;
        self.gap_end += distance;
    }
}
```

#### Why Gap Buffer?

| Consideration | Gap Buffer | Piece Tree | Our Choice |
|---------------|-----------|------------|------------|
| Simplicity | ✅ ~100 LOC | ❌ ~500 LOC | Gap Buffer |
| Localized edits | ✅ O(1) amortized | 🟡 O(log n) | Gap Buffer |
| Non-local edits | ❌ O(n) | ✅ O(log n) | Acceptable |
| Emacs alignment | ✅ Exact match | ❌ Different | Gap Buffer |
| Debuggability | ✅ Simple | ❌ Tree invariants | Gap Buffer |

**Decision:** Gap buffer for Phase 0-5. Reconsider if we add multi-cursor or collaboration.

#### WASM FFI

```rust
#[wasm_bindgen]
pub struct WasmBuffer {
    buffer: GapBuffer,
}

#[wasm_bindgen]
impl WasmBuffer {
    #[wasm_bindgen(constructor)]
    pub fn new(initial_text: &str) -> WasmBuffer {
        WasmBuffer { buffer: GapBuffer::new(initial_text) }
    }

    #[wasm_bindgen]
    pub fn insert(&mut self, position: usize, text: &str) {
        self.buffer.insert(position, text);
    }

    #[wasm_bindgen]
    pub fn delete(&mut self, position: usize, length: usize) {
        self.buffer.delete(position, length);
    }

    #[wasm_bindgen]
    pub fn get_text(&self) -> String {
        self.buffer.get_text()
    }

    #[wasm_bindgen]
    pub fn length(&self) -> usize {
        self.buffer.length()
    }
}
```

---

## State Management

### re-frame Architecture

Lexicon uses re-frame for state management - a functional, reactive framework for ClojureScript.

#### App Database (db)

```clojure
{:buffers {buffer-id {:id 1
                      :wasm-instance WasmBuffer
                      :name "*scratch*"
                      :file-handle nil
                      :is-modified? false
                      :point 0                    ; Cursor position
                      :mark nil                   ; Mark for region
                      :major-mode :fundamental-mode
                      :minor-modes #{}            ; Set of active minor modes
                      :local-vars {}}}            ; Buffer-local variables

 :windows {window-id {:id 1
                      :buffer-id 1
                      :point 0                    ; Window's point (may differ from buffer)
                      :viewport {:start-line 0
                                :end-line 50}}}

 :active-window-id 1

 :keymaps {:global {key-sequence command-name}
           :major {mode-name {key-sequence command-name}}
           :minor {mode-name {key-sequence command-name}}}

 :commands {command-name {:handler event-vector
                         :docstring "..."
                         :interactive spec}}

 :kill-ring []                                    ; Clipboard history

 :minibuffer {:active? false
              :prompt ""
              :input ""
              :completions []
              :on-confirm nil
              :on-cancel [:minibuffer/deactivate]}

 ;; NOTE: Minibuffer redesign planned for Phase 7.8.1
 ;; See ROADMAP.md Phase 7.8.1 for new architecture
 ;; Future: Always-visible, dynamic height, unified with echo-area}
```

#### Event Flow

```clojure
User Action (keypress)
    ↓
Event: [:handle-key-sequence "C-f"]
    ↓
Keymap Lookup
    ↓
Event: [:execute-command :forward-char]
    ↓
Command Handler
    ↓
WASM Call: buffer.move_point_forward(1)
    ↓
State Update: assoc-in [:buffers 1 :point] new-position
    ↓
Subscription Update
    ↓
View Re-render
```

---

## Command System

### Interactive Commands (Emacs-style)

Commands are the core abstraction - every user-facing operation is a command.

#### Command Registration

```clojure
(register-command! :forward-char
  {:docstring "Move cursor forward one character"
   :interactive ""                    ; No arguments
   :handler [:editor/forward-char]})

(register-command! :kill-region
  {:docstring "Kill (cut) region between point and mark"
   :interactive "r"                   ; Read region (point and mark)
   :handler [:editor/kill-region]})
```

#### Interactive Specification

| Code | Meaning |
|------|---------|
| `""` | No arguments |
| `"p"` | Prefix argument (numeric) |
| `"r"` | Region (point and mark) |
| `"s"` | Read string from minibuffer |
| `"f"` | Read filename |
| `"b"` | Read buffer name |

#### Command Execution

```clojure
(execute-command :forward-char)
  → Dispatch [:editor/forward-char]

(execute-command :kill-region)
  → Read region boundaries (point and mark)
  → Dispatch [:editor/kill-region start end]
```

---

## Keymap System

### Hierarchical Keymaps (Emacs Precedence)

Keymap lookup follows Emacs's exact precedence order:

1. **Text/overlay properties at point** (highest)
2. **Active minor mode keymaps** (reverse activation order)
3. **Buffer's major mode keymap**
4. **Global keymap** (lowest, always active)

#### Keymap Structure

```clojure
{:global {"C-f" :forward-char
          "C-b" :backward-char
          "C-n" :next-line
          "C-p" :previous-line
          "C-x C-f" :find-file     ; Multi-key sequence
          "C-x C-s" :save-buffer}

 :major {:text-mode {"TAB" :indent-line}
         :clojure-mode {"C-c C-k" :clojure-eval-buffer}}

 :minor {:auto-fill-mode {"SPC" :auto-fill-space}
         :evil-normal-mode {"h" :evil-backward-char
                           "j" :evil-next-line}}}
```

#### Multi-Key Sequences

Prefix keys work via nested keymaps. `C-x` is bound to a keymap, not a command:

```clojure
;; Internal representation
{"C-x" {:keymap-type :prefix
        :bindings {"C-f" :find-file
                   "C-s" :save-buffer
                   "b" :switch-to-buffer
                   "k" :kill-buffer}}}
```

**Lookup process for `C-x C-f`:**
1. Receive `C-x` → Lookup returns prefix keymap
2. Enter "prefix key state", wait for next key
3. Receive `C-f` → Lookup in prefix keymap → Returns `:find-file`
4. Execute command

---

## Mode System

### Major Modes

**One per buffer, mutually exclusive.**

```clojure
(define-major-mode :text-mode
  {:parent :fundamental-mode
   :name "Text"
   :keymap {"TAB" :indent-line}
   :syntax-table {...}
   :on-activate (fn [buffer-id]
                  ;; Run when mode activates
                  )})
```

Activation sequence:
1. Deactivate current major mode
2. Set buffer's `:major-mode` to new mode
3. Install mode's keymap
4. Run mode hooks
5. Update mode line

### Minor Modes

**Multiple per buffer, composable.**

```clojure
(define-minor-mode :auto-fill-mode
  {:name "Fill"
   :lighter " Fill"          ; Mode line indicator
   :keymap {"SPC" :auto-fill-space}
   :on-enable (fn [buffer-id]
                ;; Run when enabled
                (add-hook :after-change-hook #'auto-fill-function))
   :on-disable (fn [buffer-id]
                 ;; Run when disabled
                 (remove-hook :after-change-hook #'auto-fill-function))})
```

Toggle: `(toggle-minor-mode :auto-fill-mode)`

---

## Package Architecture

### Core vs Package Boundary

**Core provides:**
- Buffer API (create, insert, delete, get-text)
- Command API (register, execute)
- Keymap API (define, lookup)
- Mode API (define-major-mode, define-minor-mode)
- Hook API (add-hook, remove-hook, run-hooks)

**Packages use the API:**

```clojure
;; packages/evil-mode/src/lexicon/evil/core.cljs
(ns lexicon.evil.core
  (:require [lexicon.core.command :as cmd]
            [lexicon.core.mode :as mode]
            [lexicon.core.keymap :as keymap]))

(defn initialize! []
  ;; Register Evil commands
  (cmd/register-command! :evil-forward-char ...)

  ;; Define Evil normal mode as minor mode
  (mode/define-minor-mode :evil-normal-mode
    {:keymap {"h" :evil-backward-char
              "j" :evil-next-line
              "k" :evil-previous-line
              "l" :evil-forward-char}})

  ;; Set up FSM (internal to evil-mode package)
  (evil.fsm/initialize!))
```

### Package Loading

```clojure
;; User's init file
(require '[lexicon.packages :as packages])

(packages/load-package! :evil-mode)
(packages/load-package! :vertico)
(packages/load-package! :orderless)
```

Packages are **namespaced, loadable, and removable** without affecting core.

---

## Future: LSP Integration

### Architecture

```
Lexicon (Browser)
    ↕ WebSocket
Lexicon Bridge (localhost:30303)
    ↕ stdio pipes
Language Servers (typescript-language-server, rust-analyzer, etc.)
```

### Bridge Protocol

```json
{
  "method": "lsp/initialize",
  "params": {
    "language": "typescript",
    "rootUri": "file:///path/to/project"
  }
}
```

Bridge spawns language server process, forwards JSON-RPC messages between browser and LSP server.

---

## Performance Considerations

### Current Targets (Phase 0-1)

- **Input latency:** < 16ms (60 FPS)
- **Insert character:** < 5ms
- **Cursor movement:** < 5ms
- **File load (1MB):** < 500ms

### Optimization Strategy

1. **Phase 0-2:** Correctness first, optimize later
2. **Phase 3+:** Profile and optimize hot paths
3. **Future:** Virtual scrolling, Web Workers for parsing

---

## Testing Strategy

### Phase 0-1 (Manual Testing)

- Manual test checklist for each feature
- Browser DevTools debugging
- No automated tests yet

### Phase 2+ (Automated Testing)

- Unit tests for gap buffer (Rust)
- Integration tests for commands (ClojureScript)
- E2E tests with browser automation

---

## References

- [GNU Emacs Source Code](https://github.com/emacs-mirror/emacs)
- [Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
- [re-frame Documentation](https://day8.github.io/re-frame/)
- [wasm-bindgen Guide](https://rustwasm.github.io/wasm-bindgen/)

---

**Last Updated:** 2025-12-20
**Status:** Living document, updated as architecture evolves

*See [CORE_PRINCIPLES.md](./CORE_PRINCIPLES.md) for architectural philosophy.*
