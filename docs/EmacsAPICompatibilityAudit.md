# Emacs API Compatibility Audit

**Date:** 2026-01-04
**Purpose:** Comprehensive comparison of Lexicon's core APIs with GNU Emacs to identify gaps for the Elisp compatibility layer.

## Executive Summary

This audit compares Lexicon's implementation against Emacs' C-level primitives and core Elisp functions. The goal is to identify which functions Elisp packages would expect to exist when running on Lexicon's compatibility layer.

**Key Findings:**
- **Buffer API:** ~30% coverage - core functions exist but many utility functions missing
- **Keymap API:** ~40% coverage - basic keymap operations present, advanced features missing
- **Window API:** ~25% coverage - basic window splitting implemented, most window query/manipulation missing
- **Command/Interactive API:** ~50% coverage - command registry exists, but interactive specification not implemented

---

## 1. Buffer API

**Emacs Source:** `/tmp/emacs-source/src/buffer.c`
**Lexicon Implementation:** `/home/nixos/projects/lexicon/packages/editor-cljs/src/lexicon/db.cljs`, `/home/nixos/projects/lexicon/packages/editor-cljs/src/lexicon/events/buffer.cljs`

### Core Buffer Functions

| Emacs Function | Lexicon Equivalent | Priority | Status | Notes |
|----------------|-------------------|----------|--------|-------|
| **Buffer Identity & Access** |
| `buffer-name` | `:name` in buffer map | CRITICAL | ✓ IMPLEMENTED | Direct access via buffer map |
| `buffer-file-name` | `:file-handle` in buffer map | CRITICAL | ✓ IMPLEMENTED | Uses File System Access API handle |
| `buffer-live-p` | Check in `:buffers` map | HIGH | PARTIAL | No explicit function, can check map |
| `current-buffer` | Active buffer from window-tree | CRITICAL | ✓ IMPLEMENTED | Via `find-window-in-tree` |
| `set-buffer` | `:switch-buffer` event | CRITICAL | ✓ IMPLEMENTED | |
| `get-buffer` | Lookup by name in `:buffers` | HIGH | PARTIAL | No dedicated function |
| `get-buffer-create` | `:switch-to-buffer-by-name` | HIGH | ✓ IMPLEMENTED | Creates if not exists |
| `buffer-list` | Keys of `:buffers` map | MEDIUM | ✓ IMPLEMENTED | Via `:list-buffers` |
| **Buffer Content Access** |
| `point` | `:cursor-position` (linear) | CRITICAL | **MISSING** | Need accessor function |
| `point-min` | Always 0 | CRITICAL | **MISSING** | Need constant/function |
| `point-max` | `.getLength wasm-instance` | CRITICAL | **MISSING** | Need accessor function |
| `buffer-size` | `.getLength wasm-instance` | HIGH | PARTIAL | No dedicated function |
| `buffer-string` | `.getText wasm-instance` | HIGH | PARTIAL | No dedicated function |
| `buffer-substring` | Not implemented | HIGH | **MISSING** | Need for text extraction |
| `buffer-substring-no-properties` | Not implemented | MEDIUM | **MISSING** | |
| **Point/Region Movement** |
| `goto-char` | Cursor position update | CRITICAL | PARTIAL | Via events, no direct function |
| `forward-char` | `:forward-char` command | HIGH | ✓ IMPLEMENTED | |
| `backward-char` | `:backward-char` command | HIGH | ✓ IMPLEMENTED | |
| `beginning-of-line` | `:beginning-of-line` command | HIGH | ✓ IMPLEMENTED | |
| `end-of-line` | `:end-of-line` command | HIGH | ✓ IMPLEMENTED | |
| `beginning-of-buffer` | `:beginning-of-buffer` command | MEDIUM | ✓ IMPLEMENTED | |
| `end-of-buffer` | `:end-of-buffer` command | MEDIUM | ✓ IMPLEMENTED | |
| `line-beginning-position` | Not implemented | HIGH | **MISSING** | |
| `line-end-position` | Not implemented | HIGH | **MISSING** | |
| **Buffer Modification** |
| `insert` | `:editor/queue-transaction` | CRITICAL | ✓ IMPLEMENTED | Via WASM `.insert` |
| `delete-char` | `:delete-forward-char` | HIGH | ✓ IMPLEMENTED | |
| `delete-backward-char` | `:delete-backward-char` | HIGH | ✓ IMPLEMENTED | |
| `delete-region` | `:kill-region` | HIGH | ✓ IMPLEMENTED | |
| `erase-buffer` | DEFUN exists in Emacs | MEDIUM | **MISSING** | |
| `insert-buffer-substring` | Not implemented | MEDIUM | **MISSING** | |
| **Buffer State** |
| `buffer-modified-p` | `:is-modified?` | HIGH | ✓ IMPLEMENTED | |
| `set-buffer-modified-p` | Update in buffer map | MEDIUM | PARTIAL | No dedicated function |
| `buffer-modified-tick` | `:editor-version` | LOW | ✓ IMPLEMENTED | For cache invalidation |
| `restore-buffer-modified-p` | Not implemented | LOW | **MISSING** | |
| **Buffer-Local Variables** |
| `buffer-local-value` | `:buffer-local-vars` map | HIGH | ✓ IMPLEMENTED | |
| `buffer-local-variables` | Keys of buffer-local-vars | MEDIUM | PARTIAL | |
| `kill-all-local-variables` | DEFUN exists | MEDIUM | **MISSING** | |
| **Buffer Management** |
| `kill-buffer` | `:kill-buffer` event | HIGH | ✓ IMPLEMENTED | |
| `bury-buffer-internal` | Not implemented | LOW | **MISSING** | |
| `rename-buffer` | Not implemented | MEDIUM | **MISSING** | |
| `generate-new-buffer-name` | Not implemented | LOW | **MISSING** | |
| `other-buffer` | DEFUN exists | MEDIUM | **MISSING** | |
| **Overlays** |
| `make-overlay` | `:overlays` map structure | MEDIUM | ✓ IMPLEMENTED | Phase 6B |
| `delete-overlay` | Map dissoc | MEDIUM | PARTIAL | No dedicated function |
| `overlay-start` | Overlay `:start` | MEDIUM | PARTIAL | |
| `overlay-end` | Overlay `:end` | MEDIUM | PARTIAL | |
| `overlay-put` | Overlay property update | MEDIUM | PARTIAL | |
| `overlay-get` | Overlay property access | MEDIUM | PARTIAL | |
| `overlays-at` | Not implemented | LOW | **MISSING** | |
| `overlays-in` | Not implemented | LOW | **MISSING** | |

### Critical Missing Functions for Elisp Compatibility

**Priority: CRITICAL**
1. `point` - Current cursor position (linear offset)
2. `point-min` - Minimum point value (always 0 in Lexicon)
3. `point-max` - Maximum point value (buffer length)
4. `goto-char` - Move point to position
5. `buffer-substring` - Extract text between positions
6. `insert` - Already implemented via transactions, need direct API

**Priority: HIGH**
7. `get-buffer` - Get buffer by name
8. `line-beginning-position` - Position at start of line
9. `line-end-position` - Position at end of line
10. `buffer-size` - Total buffer length
11. `save-excursion` - Save/restore point (macro, not in C)
12. `with-current-buffer` - Execute in buffer context (macro)

---

## 2. Keymap API

**Emacs Source:** `/tmp/emacs-source/src/keymap.c`
**Lexicon Implementation:** `/home/nixos/projects/lexicon/packages/editor-cljs/src/lexicon/events/keymap.cljs`, `:keymaps` in `db.cljs`

### Core Keymap Functions

| Emacs Function | Lexicon Equivalent | Priority | Status | Notes |
|----------------|-------------------|----------|--------|-------|
| **Keymap Creation** |
| `make-keymap` | Create map in `:keymaps` | CRITICAL | PARTIAL | No dense keymap support |
| `make-sparse-keymap` | Default keymap structure | CRITICAL | ✓ IMPLEMENTED | Sparse maps used throughout |
| `copy-keymap` | Clojure `assoc` | MEDIUM | PARTIAL | No deep copy function |
| **Keymap Hierarchy** |
| `keymap-parent` | Not implemented | HIGH | **MISSING** | No parent chain |
| `set-keymap-parent` | Not implemented | HIGH | **MISSING** | Critical for inheritance |
| `keymapp` | Check if map | LOW | **MISSING** | |
| **Key Definition** |
| `define-key` | Direct map assoc | CRITICAL | ✓ IMPLEMENTED | String key -> command keyword |
| `lookup-key` | `resolve-keybinding` | CRITICAL | ✓ IMPLEMENTED | Precedence: minor > major > global |
| `key-binding` | `resolve-keybinding` | HIGH | ✓ IMPLEMENTED | |
| `command-remapping` | Not implemented | MEDIUM | **MISSING** | |
| **Active Keymaps** |
| `current-global-map` | `:keymaps :global` | HIGH | ✓ IMPLEMENTED | |
| `use-global-map` | Update `:keymaps :global` | MEDIUM | PARTIAL | No dedicated function |
| `current-local-map` | `:keymaps :major` for mode | HIGH | ✓ IMPLEMENTED | |
| `use-local-map` | Set major mode keymap | MEDIUM | PARTIAL | |
| `current-minor-mode-maps` | `:keymaps :minor` | HIGH | ✓ IMPLEMENTED | |
| `current-active-maps` | Combined resolution | HIGH | ✓ IMPLEMENTED | Via precedence order |
| **Key Sequences** |
| `key-description` | Not implemented | MEDIUM | **MISSING** | Convert vector to string |
| `single-key-description` | Not implemented | LOW | **MISSING** | |
| `text-char-description` | Not implemented | LOW | **MISSING** | |
| **Keymap Introspection** |
| `where-is-internal` | Reverse lookup in maps | MEDIUM | **MISSING** | Find key for command |
| `describe-buffer-bindings` | `:describe-bindings` command | MEDIUM | ✓ IMPLEMENTED | |
| `accessible-keymaps` | Not implemented | LOW | **MISSING** | |
| `map-keymap` | Iterate over keymap | LOW | **MISSING** | |

### Critical Missing Functions

**Priority: CRITICAL**
1. `set-keymap-parent` - Keymap inheritance (essential for mode hierarchy)
2. `make-keymap` - Full/dense keymap creation

**Priority: HIGH**
3. `keymap-parent` - Query parent chain
4. `where-is-internal` - Reverse key lookup (for help system)
5. Current keymap accessors need proper function wrappers

**Implementation Notes:**
- Lexicon uses flat precedence order (minor > major > global) rather than parent chains
- No support for keymap inheritance - each keymap is independent
- String-based key sequences ("C-x C-f") vs Emacs vectors [?\C-x ?\C-f]
- Prefix key handling implemented via `:prefix-key-state`

---

## 3. Window API

**Emacs Source:** `/tmp/emacs-source/src/window.c`
**Lexicon Implementation:** `/home/nixos/projects/lexicon/packages/editor-cljs/src/lexicon/events/ui.cljs`, `:window-tree` in `db.cljs`

### Core Window Functions

| Emacs Function | Lexicon Equivalent | Priority | Status | Notes |
|----------------|-------------------|----------|--------|-------|
| **Window Identity** |
| `windowp` | Check node type | LOW | PARTIAL | |
| `window-live-p` | Check leaf node | MEDIUM | PARTIAL | |
| `window-valid-p` | Exists in tree | MEDIUM | PARTIAL | |
| `selected-window` | `:active-window-id` | CRITICAL | ✓ IMPLEMENTED | |
| `select-window` | `:set-active-window` | CRITICAL | ✓ IMPLEMENTED | |
| `old-selected-window` | Not implemented | LOW | **MISSING** | |
| **Window Hierarchy** |
| `window-parent` | Binary tree parent | MEDIUM | PARTIAL | Tree structure exists |
| `window-top-child` | First child of hsplit | LOW | PARTIAL | |
| `window-left-child` | First child of vsplit | LOW | PARTIAL | |
| `window-next-sibling` | Not implemented | LOW | **MISSING** | |
| `window-prev-sibling` | Not implemented | LOW | **MISSING** | |
| **Window/Buffer** |
| `window-buffer` | Window's `:buffer-id` | CRITICAL | ✓ IMPLEMENTED | |
| `set-window-buffer` | Update in tree | HIGH | PARTIAL | Via switch-buffer |
| `get-buffer-window` | Search tree for buffer | MEDIUM | **MISSING** | |
| **Window Splitting** |
| `split-window-internal` | `:split-window-below/right` | CRITICAL | ✓ IMPLEMENTED | Binary tree splits |
| `split-window-below` | Creates `:hsplit` node | HIGH | ✓ IMPLEMENTED | C-x 2 |
| `split-window-right` | Creates `:vsplit` node | HIGH | ✓ IMPLEMENTED | C-x 3 |
| **Window Deletion** |
| `delete-window-internal` | `:delete-window` | HIGH | **MISSING** | Stub only |
| `delete-other-windows-internal` | `:delete-other-windows` | HIGH | ✓ IMPLEMENTED | C-x 1 |
| **Window Selection** |
| `next-window` | `:other-window` | HIGH | ✓ IMPLEMENTED | C-x o |
| `previous-window` | Not implemented | MEDIUM | **MISSING** | |
| `window-list` | `get-all-leaf-windows` | MEDIUM | ✓ IMPLEMENTED | |
| `window-list-1` | Not implemented | LOW | **MISSING** | |
| **Window Point** |
| `window-point` | Window's `:cursor-position` | CRITICAL | ✓ IMPLEMENTED | Line/col format |
| `set-window-point` | Update in window | HIGH | PARTIAL | Via cursor movement |
| `window-start` | `:viewport :start-line` | HIGH | ✓ IMPLEMENTED | |
| `window-end` | `:viewport :end-line` | HIGH | ✓ IMPLEMENTED | |
| `set-window-start` | Update viewport | MEDIUM | **MISSING** | |
| **Window Dimensions** |
| `window-pixel-width` | `:dimensions :width` | MEDIUM | ✓ IMPLEMENTED | |
| `window-pixel-height` | `:dimensions :height` | MEDIUM | ✓ IMPLEMENTED | |
| `window-total-width` | Not implemented | MEDIUM | **MISSING** | In characters |
| `window-total-height` | Not implemented | MEDIUM | **MISSING** | In lines |
| `window-body-width` | Not implemented | LOW | **MISSING** | |
| `window-body-height` | Not implemented | LOW | **MISSING** | |
| **Window Position** |
| `window-pixel-left` | `:dimensions :x` | LOW | ✓ IMPLEMENTED | |
| `window-pixel-top` | `:dimensions :y` | LOW | ✓ IMPLEMENTED | |
| `window-left-column` | Not implemented | LOW | **MISSING** | |
| `window-top-line` | Not implemented | LOW | **MISSING** | |
| `coordinates-in-window-p` | Not implemented | LOW | **MISSING** | |
| `window-at` | Not implemented | MEDIUM | **MISSING** | |
| **Window Display** |
| `window-hscroll` | Not implemented | LOW | **MISSING** | Horizontal scroll |
| `set-window-hscroll` | Not implemented | LOW | **MISSING** | |
| `window-vscroll` | Not implemented | LOW | **MISSING** | Vertical scroll |
| `set-window-vscroll` | Not implemented | LOW | **MISSING** | |
| `pos-visible-in-window-p` | Viewport check | MEDIUM | **MISSING** | |
| `recenter` | Not implemented | MEDIUM | **MISSING** | |
| **Scrolling** |
| `scroll-up` | `:scroll-up-command` | HIGH | ✓ IMPLEMENTED | C-v |
| `scroll-down` | `:scroll-down-command` | HIGH | ✓ IMPLEMENTED | M-v |
| `scroll-left` | Not implemented | LOW | **MISSING** | |
| `scroll-right` | Not implemented | LOW | **MISSING** | |
| **Window Configuration** |
| `current-window-configuration` | Serialize window-tree | MEDIUM | **MISSING** | Save/restore |
| `set-window-configuration` | Deserialize to tree | MEDIUM | **MISSING** | |
| `window-configuration-p` | Type check | LOW | **MISSING** | |

### Critical Missing Functions

**Priority: CRITICAL**
None - core window operations implemented

**Priority: HIGH**
1. `delete-window-internal` - Remove window from tree (stub exists)
2. `get-buffer-window` - Find window displaying buffer
3. `set-window-point` - Move point in specific window
4. `window-total-width/height` - Size in characters/lines (not pixels)

**Priority: MEDIUM**
5. `window-at` - Find window at coordinates
6. `recenter` - Center point in window
7. `set-window-start` - Scroll to position
8. `pos-visible-in-window-p` - Check if position visible
9. `current-window-configuration` / `set-window-configuration` - Save/restore layouts

**Implementation Notes:**
- Lexicon uses binary tree for window splits (`:hsplit`, `:vsplit`)
- Each window stores its own cursor position and viewport
- No support for window parameters, dedicated flag, or fringes
- Pixel-based dimensions but missing character-based sizing
- Window deletion requires tree rebalancing (not yet implemented)

---

## 4. Command/Interactive API

**Emacs Source:** `/tmp/emacs-source/src/keyboard.c`, `/tmp/emacs-source/lisp/simple.el`
**Lexicon Implementation:** `/home/nixos/projects/lexicon/packages/editor-cljs/src/lexicon/events/command.cljs`

### Command System

| Emacs Function/Concept | Lexicon Equivalent | Priority | Status | Notes |
|------------------------|-------------------|----------|--------|-------|
| **Command Definition** |
| `defun` with `interactive` | `:register-command` | CRITICAL | PARTIAL | No interactive spec parsing |
| `interactive` spec string | Not implemented | CRITICAL | **MISSING** | e.g., "p", "r", "sPrompt:" |
| `call-interactively` | `:execute-command` | HIGH | ✓ IMPLEMENTED | |
| `commandp` | Check `:commands` registry | MEDIUM | PARTIAL | |
| **Command Execution** |
| Command dispatch | `:execute-command` | CRITICAL | ✓ IMPLEMENTED | |
| Before/after hooks | `:before-command-hook` | HIGH | ✓ IMPLEMENTED | Phase 7.3 hook system |
| Prefix argument | `:prefix-argument` | HIGH | ✓ IMPLEMENTED | C-u |
| `this-command` | Dynamic context | MEDIUM | PARTIAL | Via exec-context |
| `last-command` | Not implemented | MEDIUM | **MISSING** | |
| **Interactive Specs** |
| `(interactive)` | Handler: event vector | HIGH | ✓ IMPLEMENTED | Simple case |
| `(interactive "p")` | Prefix arg as number | HIGH | **MISSING** | Needs spec parser |
| `(interactive "r")` | Region start/end | HIGH | **MISSING** | |
| `(interactive "s...")` | Read string | HIGH | **MISSING** | Minibuffer prompt |
| `(interactive "f...")` | Read file name | MEDIUM | **MISSING** | |
| `(interactive "b...")` | Read buffer name | MEDIUM | **MISSING** | |
| `(interactive "k...")` | Read key sequence | MEDIUM | **MISSING** | |
| `(interactive "c...")` | Read character | LOW | **MISSING** | |
| `(interactive "d...")` | Point position | LOW | **MISSING** | |
| **Key Reading** |
| `read-key-sequence` | Not implemented | HIGH | **MISSING** | Read multi-key |
| `read-key-sequence-vector` | Not implemented | MEDIUM | **MISSING** | |
| `read-char` | Not implemented | MEDIUM | **MISSING** | |
| `read-event` | Not implemented | MEDIUM | **MISSING** | |
| **Input State** |
| `this-command-keys` | Prefix state | MEDIUM | PARTIAL | Via `:prefix-key-state` |
| `this-command-keys-vector` | Not implemented | LOW | **MISSING** | |
| `clear-this-command-keys` | `:clear-prefix-key-state` | LOW | ✓ IMPLEMENTED | |
| `input-pending-p` | Not implemented | LOW | **MISSING** | |
| **Recursive Edit** |
| `recursive-edit` | Minibuffer activation | MEDIUM | PARTIAL | Via minibuffer |
| `exit-recursive-edit` | Minibuffer deactivate | MEDIUM | ✓ IMPLEMENTED | |
| `abort-recursive-edit` | `:keyboard-quit` | HIGH | ✓ IMPLEMENTED | C-g |
| `recursion-depth` | Not implemented | LOW | **MISSING** | |

### Interactive Specification Codes (Critical for Compatibility)

Emacs `interactive` specs are strings that describe how to collect arguments:

```elisp
;; Emacs
(defun my-command (arg)
  (interactive "p")  ; Numeric prefix argument
  ...)

(defun my-search (start end pattern)
  (interactive "r\nsSearch: ")  ; Region + string prompt
  ...)
```

**Current Lexicon Approach:**
```clojure
;; Lexicon - handlers are event vectors, arguments passed explicitly
(rf/reg-event-fx
 :register-command
 (fn [db [_ command-name command-definition]]
   {:handler [:some-event arg1 arg2]}))
```

**Critical Missing Interactive Codes:**

| Code | Meaning | Priority | Use Case |
|------|---------|----------|----------|
| `p` | Prefix arg as number | CRITICAL | Most commands with repeat |
| `r` | Region start/end | CRITICAL | All region commands |
| `s...` | String from minibuffer | HIGH | Search, replace, prompts |
| `b...` | Buffer name | HIGH | Buffer switching |
| `f...` | File name | HIGH | File operations |
| `d` | Point position | MEDIUM | Jump commands |
| `n` | Number from minibuffer | MEDIUM | Numeric input |
| `k...` | Key sequence | MEDIUM | Key remapping |

### Critical Missing Functions

**Priority: CRITICAL**
1. **Interactive spec parser** - Parse `(interactive "...")` strings
2. **Spec code handlers** - Implement each interactive code (`p`, `r`, `s`, etc.)

**Priority: HIGH**
3. `read-key-sequence` - Read multi-key sequences
4. `commandp` - Check if symbol is command
5. Region argument passing (for `r` spec)

**Implementation Recommendations:**

```clojure
;; Proposed: Interactive spec processor
(defn parse-interactive-spec [spec-string]
  "Parse interactive spec string into argument collection plan.
   Returns sequence of {:type :prefix-arg/:region/:string/:etc
                        :prompt \"...\" (if applicable)}"
  ...)

;; Example transformation:
;; (interactive \"r\\nsSearch: \") =>
;; [{:type :region}
;;  {:type :string :prompt \"Search: \"}]

;; Then collect args before executing handler:
(defn collect-interactive-args [spec-plan db]
  "Collect arguments according to interactive spec.
   May dispatch minibuffer prompts and return async."
  ...)
```

---

## 5. Missing Critical Primitives Summary

### Tier 1: Blocking Elisp Compatibility

These functions MUST be implemented for basic Elisp packages to work:

1. **Buffer Access:**
   - `point`, `point-min`, `point-max`
   - `goto-char`
   - `buffer-substring`
   - `buffer-string`

2. **Interactive System:**
   - Interactive spec parser
   - Interactive code handlers (especially `p`, `r`, `s`)

3. **Keymap Inheritance:**
   - `set-keymap-parent`
   - `keymap-parent`

### Tier 2: Required for Most Packages

4. **Buffer Utilities:**
   - `line-beginning-position`, `line-end-position`
   - `get-buffer` (by name)
   - `save-excursion` (macro)
   - `with-current-buffer` (macro)

5. **Window Operations:**
   - `delete-window` (complete implementation)
   - `get-buffer-window`
   - `window-total-width`, `window-total-height`

6. **Command System:**
   - `commandp`
   - `last-command`
   - `read-key-sequence`

### Tier 3: Nice to Have

7. **Buffer/Window Query:**
   - `window-at`
   - `pos-visible-in-window-p`
   - `recenter`
   - `overlays-at`, `overlays-in`

8. **Help System:**
   - `where-is-internal` (reverse key lookup)
   - `key-description` (vector to string)

---

## 6. Architectural Differences

### Coordinate Systems

**Emacs:**
- Linear byte/character positions (0 to buffer-size)
- Functions: `point`, `point-min`, `point-max`, `goto-char POS`

**Lexicon:**
- Line/column coordinates `{:line N :column M}`
- Linear positions used in `:ui :cursor-position`
- Need conversion functions in both directions

**Recommendation:** Provide both APIs but use linear positions as primary for Elisp compatibility.

### Buffer-Local Variables

**Emacs:**
- Variables can be made buffer-local via `make-variable-buffer-local`
- Automatic local binding per buffer
- Complex inheritance rules

**Lexicon:**
- Explicit `:buffer-local-vars` map in each buffer
- No automatic propagation
- Simpler but less compatible

**Recommendation:** Implement buffer-local variable infrastructure with `make-variable-buffer-local` and `buffer-local-value`.

### Keymap Inheritance

**Emacs:**
- Parent keymap chains
- Lookup walks up chain
- Complex precedence with multiple active keymaps

**Lexicon:**
- Flat precedence order (minor > major > global)
- No parent chains
- Simpler but breaks packages expecting inheritance

**Recommendation:** Add parent keymap support while keeping current precedence order.

### Interactive Specifications

**Emacs:**
- Declarative string specs
- Automatic argument collection
- Built-in prompts and validation

**Lexicon:**
- Event-based handlers
- Manual argument passing
- Explicit minibuffer activation

**Recommendation:** Build interactive spec parser that translates to Lexicon's event system.

---

## 7. Implementation Roadmap

### Phase 1: Critical Primitives (Blocking)

**Week 1-2: Buffer Position API**
- [ ] `point` - Return linear cursor position
- [ ] `point-min` - Return 0
- [ ] `point-max` - Return buffer length
- [ ] `goto-char` - Set cursor position
- [ ] `buffer-substring` - Extract text range
- [ ] Line/column <-> linear position converters

**Week 3-4: Interactive Specification Parser**
- [ ] Parse interactive spec strings
- [ ] Implement `p` code (prefix argument)
- [ ] Implement `r` code (region)
- [ ] Implement `s` code (string prompt)
- [ ] Integrate with command execution

### Phase 2: Core Functions (High Priority)

**Week 5-6: Buffer/Window Utilities**
- [ ] `get-buffer` (by name)
- [ ] `line-beginning-position` / `line-end-position`
- [ ] `window-total-width` / `window-total-height` (characters)
- [ ] `get-buffer-window`
- [ ] Complete `delete-window` implementation

**Week 7-8: Keymap Inheritance**
- [ ] Add `:parent` to keymap structure
- [ ] `set-keymap-parent` / `keymap-parent`
- [ ] Update `resolve-keybinding` to walk parent chain
- [ ] Test with inherited keymaps

### Phase 3: Compatibility Layer (Medium Priority)

**Week 9-10: Macros and Special Forms**
- [ ] `save-excursion` macro
- [ ] `with-current-buffer` macro
- [ ] `save-restriction` (if narrowing implemented)
- [ ] `condition-case` (error handling)

**Week 11-12: Additional Interactive Codes**
- [ ] `b` - Buffer name
- [ ] `f` - File name
- [ ] `k` - Key sequence
- [ ] `n` - Number
- [ ] `d` - Point

### Phase 4: Polish (Low Priority)

**Week 13-14: Advanced Features**
- [ ] `window-at`
- [ ] `pos-visible-in-window-p`
- [ ] `recenter`
- [ ] `where-is-internal`
- [ ] `overlays-at` / `overlays-in`
- [ ] Window configuration save/restore

---

## 8. Testing Strategy

### Compatibility Test Suite

Create test packages that exercise common Elisp patterns:

1. **Basic Package:**
   ```elisp
   ;; Test point movement, buffer manipulation
   (defun test-basic ()
     (goto-char (point-min))
     (insert "Hello")
     (buffer-substring (point-min) (point-max)))
   ```

2. **Interactive Commands:**
   ```elisp
   ;; Test interactive specs
   (defun test-interactive (arg start end pattern)
     (interactive "p\nr\nsPattern: ")
     ...)
   ```

3. **Keymap Package:**
   ```elisp
   ;; Test keymap inheritance
   (defvar my-mode-map
     (let ((map (make-sparse-keymap)))
       (set-keymap-parent map text-mode-map)
       (define-key map (kbd "C-c C-c") 'my-command)
       map))
   ```

4. **Buffer-Local Package:**
   ```elisp
   ;; Test buffer-local variables
   (defvar-local my-buffer-var nil)
   (defun my-mode ()
     (setq my-buffer-var 'value))
   ```

### Metrics for Success

- **API Coverage:** 80%+ of functions used by popular packages (magit, org-mode, etc.)
- **Behavior Compatibility:** 95%+ identical behavior for implemented functions
- **Performance:** Interactive commands respond in <16ms (60fps)
- **Package Loading:** Top 50 MELPA packages load without errors

---

## 9. Conclusion

Lexicon has a solid foundation with ~40% coverage of core Emacs primitives. The main gaps are:

1. **Point-based API** - Critical for Elisp compatibility
2. **Interactive specification system** - Required for command definition
3. **Keymap inheritance** - Expected by mode packages
4. **Buffer utility functions** - Used extensively in text manipulation

**Estimated Effort:**
- Phase 1 (Critical): 4 weeks
- Phase 2 (Core): 4 weeks
- Phase 3 (Compatibility): 4 weeks
- Phase 4 (Polish): 2 weeks
- **Total: ~14 weeks (3.5 months)**

With these implementations, Lexicon would support the majority of well-behaved Elisp packages that don't rely on deep Emacs internals (display engine, process management, networking).

---

## Appendix A: Function Reference by File

### Emacs C Sources Analyzed
- `/tmp/emacs-source/src/buffer.c` - 54 DEFUN declarations
- `/tmp/emacs-source/src/keymap.c` - 28 DEFUN declarations
- `/tmp/emacs-source/src/window.c` - 92 DEFUN declarations
- `/tmp/emacs-source/src/keyboard.c` - 30 DEFUN declarations

### Lexicon Sources Analyzed
- `/home/nixos/projects/lexicon/packages/editor-cljs/src/lexicon/db.cljs`
- `/home/nixos/projects/lexicon/packages/editor-cljs/src/lexicon/events/buffer.cljs`
- `/home/nixos/projects/lexicon/packages/editor-cljs/src/lexicon/events/keymap.cljs`
- `/home/nixos/projects/lexicon/packages/editor-cljs/src/lexicon/events/ui.cljs`
- `/home/nixos/projects/lexicon/packages/editor-cljs/src/lexicon/events/command.cljs`

---

**End of Audit**
