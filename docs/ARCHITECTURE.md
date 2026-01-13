# Lexicon Architecture

**Last Updated:** 2026-01-13

---

## Core Philosophy

**Lexicon is a faithful recreation of GNU Emacs for the modern web.**

"When in doubt, do what Emacs does" - We study Emacs source code before implementing features. This is not Emacs-inspired, this IS Emacs for the browser.

---

## Architectural Principles

### 1. Userspace/Kernel Split (Emacs C / Elisp Analogy)

**Rust/WASM "Kernel":**
- Gap buffer operations
- UTF-8 text handling
- Performance-critical primitives
- Marker management at engine level

**ClojureScript "Userspace":**
- All user-facing commands (even `self-insert-command`)
- Mode definitions, keymap configurations
- UI rendering (Reagent/React)
- Extensibility and package system

**Rule:** Only functions that need speed go in Rust. Everything else in ClojureScript.

### 2. Core vs Package

**Core** (what you get with `emacs -Q`):
- Buffer/window/frame management
- Keymap system, command dispatcher
- Mode system (major/minor)
- Minibuffer, basic editing
- File I/O, kill ring

**Packages** (external, loadable):
- Evil-mode (vim emulation)
- Vertico/Orderless/Consult (completion)
- LSP integration
- Syntax highlighting (beyond basic)
- Themes

**Rule:** If Emacs doesn't have it by default, it's a package.

### 3. State Ownership (CRITICAL)

**Problem:** Shared mutable state creates tight coupling, debugging nightmares, and regressions.

**Rule:** Each state variable OWNED by exactly ONE module. Others MUST dispatch events.

**Ownership Map:**
- `:minibuffer` → `ui.cljs` events: `:minibuffer/activate`, `:minibuffer/deactivate`
- `:echo-area` → `ui.cljs` events: `:echo/message`, `:echo/clear`
- `:mark-position` → `edit.cljs` events: `:set-mark`, `:deactivate-mark`
- `:window-tree` → `ui.cljs` events: `:window/set-buffer`, `:window/set-mark`, `:window/set-cursor`
- `:ui :cursor-position` → `edit.cljs` events: `:cursor/set-position`
- `:buffers` → `buffer.cljs` events: `:buffer/set-mode`, `:buffer/update-version`
- `:kill-ring` → `edit.cljs` (kill/yank commands only)

**Enforcement:**
```clojure
;; ❌ WRONG: Direct state manipulation
(assoc db :minibuffer {...})

;; ✅ CORRECT: Dispatch to owner
[:dispatch [:minibuffer/activate {...}]]
```

See `packages/editor-cljs/src/lexicon/db.cljs` for full ownership documentation.

---

## System Architecture

### Gap Buffer (Rust/WASM)

**Why Gap Buffer?**
- Simpler than piece table/rope
- Faster for localized edits (99% of editing)
- Exact Emacs implementation
- Well-understood performance characteristics

**Interface:**
```rust
// packages/lexicon-engine/wasm/src/lib.rs
pub struct WasmGapBuffer {
    buffer: GapBuffer,
}

impl WasmGapBuffer {
    pub fn insert(&mut self, pos: usize, text: &str);
    pub fn delete(&mut self, pos: usize, length: usize);
    pub fn get_text(&self) -> String;
    pub fn length(&self) -> usize;  // Property, not method: use .-length in JS
}
```

**ClojureScript Integration:**
```clojure
;; Create instance
(let [buffer (WasmGapBuffer. "")]
  ;; Insert
  (.insert buffer 0 "Hello")
  ;; Delete
  (.delete buffer 0 5)
  ;; Get text
  (.getText buffer)
  ;; Length
  (.-length buffer))  ; NOT (.getLength buffer)
```

### Re-frame State Management

**App-DB Schema:**
```clojure
{:buffers {"*scratch*" {:name "*scratch*"
                        :wasm-instance <WasmGapBuffer>
                        :major-mode :text-mode
                        :local-vars {}        ; Phase 6.5 Week 5-6
                        :cursor-position {:line 0 :column 0}}}
 
 :window-tree {:type :window
               :buffer-id "*scratch*"
               :cursor-position {:line 0 :column 0}
               :mark-position nil}
 
 :active-window-id <window-id>
 
 :minibuffer []  ; Phase 6.5 Week 3-4: Stack of frames
 
 :prefix-arg nil  ; Phase 6.5 Week 1-2
 :current-prefix-arg nil
 
 :kill-ring []
 
 :ui {:cursor-position 0}}  ; Linear position for WASM
```

### Cursor Position (3 Locations)

**Why 3 locations?**
1. **`[:ui :cursor-position]`** - Linear integer for WASM operations
2. **`[:buffers id :cursor-position]`** - Per-buffer line/column
3. **Window `:cursor-position`** - Per-window, survives buffer switches

**Conversion:**
```clojure
(defn linear-to-line-col [text linear-pos]
  ;; Convert linear position to {:line N :column M}
  ...)

(defn line-col-to-linear [text line col]
  ;; Convert {:line N :column M} to linear position
  ...)
```

### Keymap System

**Hierarchical Precedence (Exact Emacs Order):**
1. Character property keymaps
2. Minor mode keymaps (in reverse order of activation)
3. Local keymap (major mode)
4. Global keymap

**Key Sequence Handling:**
```clojure
;; Multi-key sequences
"C-x C-f"  → find-file
"C-x 4 b"  → switch-to-buffer-other-window

;; Transient keymaps (Phase 6.5 Week 1-2)
;; C-u activates universal-argument-map temporarily
```

### Command System

**Interactive Specs (Phase 6.5 Week 1-2):**
```clojure
;; "P" - Raw prefix argument
(defn my-command []
  (interactive "P")
  (let [arg (current-prefix-arg)]
    ...))

;; "p" - Numeric prefix (nil → 1, (4) → 4, etc.)
(defn forward-char [n]
  (interactive "p")
  ...)
```

### Minibuffer (Phase 6.5 Week 3-4)

**Stack-Based Architecture:**
```clojure
;; Current: Single state (broken)
{:minibuffer {:active? true
              :prompt "M-x "
              :input ""}}

;; Phase 6.5: Stack of frames
{:minibuffer [{:prompt "M-x "
               :input ""
               :completion-table [...]
               :on-confirm [:execute-command-by-name]
               :on-cancel [:minibuffer/restore-previous]}]}

;; Depth tracking
(defn minibuffer-depth [] (count (:minibuffer @db)))
```

**Recursive Support:**
```clojure
;; Config variable
(setq enable-recursive-minibuffers true)

;; Command can activate minibuffer while minibuffer active
;; Each level has independent state
```

### Buffer-Local Variables (Phase 6.5 Week 5-6)

**Lookup Order:**
```clojure
;; 1. Check buffer-local
(get-in db [:buffers current-buffer :local-vars var-name])

;; 2. Fall back to global
(get-in db [:global-vars var-name])
```

**API:**
```clojure
(make-local-variable 'foo)  ; Mark as buffer-local
(setq foo 42)               ; Sets in current buffer
(buffer-local-value 'foo buffer)  ; Read from specific buffer
(kill-local-variable 'foo)  ; Remove local binding
```

### Runtime Evaluation (Phase 6.5 Week 7-8)

**SCI Sandbox:**
```clojure
;; Safe eval with restricted namespaces
(eval-string "(+ 1 2)")  ; => 3

;; Access to lexicon.core API
(eval-string "(message \"Hello\")")

;; User init file: ~/.lexicon/init.cljs
(ns user
  (:require [lexicon.core :refer [message defun]]))

(defun my-command []
  (interactive)
  (message "Custom command!"))
```

**Security Model:**
- Allow-list: `lexicon.core`, `lexicon.api.*`, safe `clojure.core` subset
- Block: Filesystem (except approved), network, dangerous JS interop
- Sandboxed by default

---

## File Organization

```
packages/
├── editor-cljs/
│   └── src/lexicon/
│       ├── core.cljs              # Entry point, API re-exports
│       ├── db.cljs                # Schema, state ownership docs
│       ├── events/                # Event handlers by domain
│       │   ├── buffer.cljs
│       │   ├── command.cljs
│       │   ├── edit.cljs
│       │   ├── minibuffer.cljs
│       │   └── ...
│       ├── eval.cljs              # SCI wrapper (Phase 6.5)
│       ├── modes/                 # Mode definitions
│       ├── api/                   # Public API modules
│       │   ├── buffer.cljs
│       │   └── message.cljs
│       └── ...
└── lexicon-engine/
    └── wasm/
        └── src/
            └── lib.rs             # Gap buffer WASM bindings
```

**Size Limit:** ~500 lines per namespace - split when exceeded

---

## Emacs Fidelity

### Research Process

Before implementing features:
1. Read Emacs source: `/tmp/emacs-source`
2. Study implementation details
3. Replicate behavior, not approximate

**Key Emacs Files:**
- `lisp/simple.el` - Basic commands, universal-argument
- `src/callint.c` - Interactive calling, prefix args
- `lisp/minibuffer.el` - Completion framework
- `src/minibuf.c` - Minibuffer depth, state
- `src/buffer.c` - Buffer-local variables
- `lisp/emacs-lisp/easy-mmode.el` - Mode macros

### Phase 6.5 Research

**Prefix Arguments:**
- Emacs uses transient keymap for accumulation
- `prefix-arg` (raw) vs `current-prefix-arg` (during execution)
- Interactive spec codes: "P" (raw), "p" (numeric)
- Value types: nil, (4), (16), numbers, negative

**Minibuffer Stack:**
- Each level has independent state
- `enable-recursive-minibuffers` controls nesting
- Depth tracking via frame count
- Command can replace or push new frame

**Buffer-Local Vars:**
- Per-buffer bindings, global fallback
- `make-local-variable` marks variable
- Minor modes are buffer-local by default

**Runtime Eval:**
- SCI provides safe ClojureScript evaluation
- Used by Babashka, Logseq, Maria.cloud
- Configurable namespaces, sandboxed by default

---

## Security Model

### Package Trust Levels

- **:core** - Built-in packages, full access, native eval
- **:local** - User filesystem, full access, native eval
- **:external** - Internet packages, **SCI sandbox only**, Core API only

### Runtime Eval (Phase 6.5)

- User init file: `~/.lexicon/init.cljs` runs in SCI sandbox
- Allow-list: `lexicon.core`, `lexicon.api.*`, safe `clojure.core`
- Block: Arbitrary filesystem, network, dangerous JS (`eval`, `Function`)

---

## Performance Standards

- ✅ **Editor startup:** < 2 seconds
- ✅ **Command execution:** < 16ms (60 FPS)
- ✅ **File loading:** < 1 second for 10k line files
- ✅ **Package loading:** < 100ms per package (warn if slower)

### Optimization Guidelines

- Profile before optimizing
- Gap buffer in Rust for speed
- Minimize re-renders (React.memo, memoize subscriptions)
- Lazy load packages
- Future: Web workers for heavy computation

---

## Known Quirks

### WASM API

- `WasmGapBuffer` only - simple gap buffer
- `.-length` is a property, NOT `.getLength()` method
- `WasmEditorCore` was deleted (unused complexity)

### Cursor Synchronization

- Three locations must stay in sync
- Linear ↔ line/column conversions are expensive
- Cache conversions where possible

### Log Bus (Issue #73)

- Lifecycle-independent logging
- Messages replay to *Messages* buffer when ready
- No dependencies on buffer/minibuffer state

---

**For implementation details, see GitHub issues with label `phase-6.5`.**
