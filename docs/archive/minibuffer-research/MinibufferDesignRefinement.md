# Minibuffer Design Refinement

**Date:** 2026-01-04
**Purpose:** Critical analysis and refinement of minibuffer architecture based on Emacs source code review

---

## Executive Summary

After reviewing your three research documents and cross-referencing with actual Emacs C source (`minibuf.c`), Elisp implementation (`minibuffer.el`), and modern package implementations (Consult, Vertico), I can confirm:

**✅ Your core insights are architecturally sound**
**⚠️ Some refinements needed for Lexicon's specific constraints**
**🔍 Several implementation details require clarification**

---

## What You Got Absolutely Right

### 1. **Minibuffer as Control Plane** ✅

**Your claim:** "The minibuffer is not just a UI widget — it is a control plane, a protocol, and a social contract."

**Validation from source:**
- `src/minibuf.c` confirms: Minibuffers are real buffers (`get_minibuffer()` creates actual buffer objects)
- Named `" *Minibuf-0*"`, `" *Minibuf-1*"`, etc. (space-prefixed = internal buffer)
- Each has full buffer-local state, modes, keymaps
- NOT a special UI component

**Verdict:** 100% correct. Emacs treats minibuffers as first-class buffers with special lifecycle rules.

---

### 2. **Recursive by Design** ✅

**Your claim:** "The minibuffer is a stackable interaction context."

**Validation:**
- `minibuf_level` global variable tracks recursion depth
- `Vminibuffer_list` maintains list of all minibuffer buffers
- `nth_minibuffer(depth)` retrieves buffer at specific recursion level
- `read_minibuf_unwind()` implements stack unwinding

**Emacs implementation:**
```c
// From minibuf.c
int minibuf_level;  // Current recursion depth

static Lisp_Object
read_minibuf_unwind (void)
{
  /* Restore previous minibuffer */
  minibuf_window = old_minibuf_window;
  minibuf_level--;
  /* ... cleanup ... */
}
```

**Verdict:** Correct. Stack discipline is fundamental, not optional.

---

### 3. **Dynamic Expansion** ✅

**Your claim:** "The minibuffer can temporarily steal space from the frame."

**Validation from source:**
- `resize-mini-windows` variable with values: `nil`, `grow-only`, `t`
- `max-mini-window-height` caps expansion (default: 0.25 of frame height)
- Window system negotiates space allocation
- Layout restoration is **automatic** on exit

**Critical insight from Emacs:**
> "The values of these variables take effect at display time, so let-binding them around code which produces echo-area messages will not work."

This means expansion is a **display-time property**, not a runtime mutation.

**Verdict:** Correct architecture, but implementation must happen in rendering phase.

---

### 4. **Completion is External** ✅

**Your claim:** "The minibuffer orchestrates; other windows render."

**Validation:**
- `minibuffer.el` has 4000+ lines of completion logic, but minibuffer itself is ~200 lines
- Completion styles are pluggable: `completion-styles-alist`
- Completion metadata system allows rich customization
- Vertico/Consult work by **hooking into completion**, not replacing minibuffer

**Verdict:** Spot on. Separation of concerns is critical.

---

## Where Refinement Is Needed

### 1. **"Always Visible" Claim** ⚠️

**Your Phase 7.8.1 claim:** "Minibuffer always visible (1 empty line below mode line)"

**Emacs reality check:**
- The minibuffer **window** is always visible (fixed at bottom of frame)
- The minibuffer **buffer** is only shown when active
- When inactive, the window shows `" *Minibuf-0*"` (empty buffer)
- This creates the **illusion** of always-visible, but it's actually "always-allocated window"

**Critical distinction:**
```
┌─────────────────────┐
│  Main buffer        │
├─────────────────────┤
│  Mode line          │
├─────────────────────┤  ← Minibuffer window (ALWAYS EXISTS)
│  [Empty]            │     Buffer content (CHANGES)
└─────────────────────┘
```

**Refinement for Lexicon:**
- Don't make minibuffer "always visible" in app-db state
- Instead: Reserve a **fixed window slot** in UI layout
- That window displays different content: idle (empty), message (text), input (prompt + text)

**Corrected architecture:**
```clojure
;; NOT THIS:
{:minibuffer {:always-visible true  ; ❌ Wrong abstraction
              :active? false}}

;; THIS:
{:ui/layout {:minibuffer-window-id "mb-window"   ; Always allocated
             :minibuffer-buffer-id nil}}         ; Changes on activation

{:minibuffer {:active-session nil                ; nil when idle
              :session-stack []}}                ; For recursion
```

---

### 2. **Preview Contract** ⚠️

**Your Plan2 claim:**
```clojure
{:preview/start   (fn [])
 :preview/update  (fn [candidate])
 :preview/cancel  (fn [])
 :preview/commit  (fn [candidate])}
```

**Consult's actual implementation:**
- Uses `:state` function (not separate start/update/cancel/commit)
- State function returns a **closure** that handles all preview lifecycle
- No explicit "start" - setup happens on first call
- Cleanup is **automatic** via hooks, not manual cancel

**Emacs pattern (from Consult docs):**
```elisp
;; State function pattern
(defun my-consult-state ()
  (let ((preview-buffer nil))
    (lambda (action candidate)
      (pcase action
        ('preview (setq preview-buffer (open-preview candidate)))
        ('return  (when preview-buffer (kill-buffer preview-buffer)))
        ('exit    (when preview-buffer (kill-buffer preview-buffer)))))))
```

**Refinement for Lexicon:**
```clojure
;; Your session should include:
{:minibuffer/session
 {:state-fn (fn [action candidate]
              (case action
                :preview  (preview candidate)
                :exit     (cleanup-preview)
                :return   (commit-preview candidate)))}}
```

This is **more flexible** because:
- Single function handles all lifecycle
- Actions can be extended (`preview`, `exit`, `return`, `narrow`, etc.)
- Closure captures preview state naturally
- Matches Emacs/Consult convention

---

### 3. **Modeline vs Echo Area Confusion** 🔍

**Your DeepDive doc:** "The modeline and minibuffer cooperate subtly."

**Clarification needed:**
- **Modeline** = status line above **each window** (shows mode, filename, position)
- **Echo area** = bottom of minibuffer window (shows transient messages)
- **Minibuffer** = editable input area in same window as echo area

These are **3 different things** in the same frame:

```
┌─────────────────────┐
│  Buffer content     │
├─────────────────────┤ ← Modeline (per-window)
│ -U:** *scratch*     │
└─────────────────────┘
        ...
┌─────────────────────┐ ← Minibuffer window (shared, bottom of frame)
│ M-x │command▯       │   ← Minibuffer (when active)
│ Saved file foo.txt  │   ← Echo area (transient messages)
└─────────────────────┘
```

**Critical insight:**
- Messages flash in echo area (same window as minibuffer)
- When minibuffer activates, it **replaces** echo area content
- When minibuffer deactivates, echo area becomes visible again
- This is why it feels like "dual purpose" - it's the same window!

**Refinement for Lexicon:**
```clojure
;; Current broken model:
{:echo-area {:message "..."}      ; ❌ Separate component
 :minibuffer {:prompt "M-x "}}    ; ❌ Separate component

;; Correct model:
{:minibuffer-window {
   :mode :idle        ; :idle | :message | :input | :completion
   :content ""        ; Changes based on mode
   :prompt nil}}      ; Only present in :input/:completion mode
```

---

## Critical Missing Pieces

### 1. **Minibuffer Hooks** 🆕

**What your docs missed:**

Emacs has critical hooks that make packages work:

```elisp
minibuffer-setup-hook      ; Run when entering minibuffer
minibuffer-exit-hook       ; Run when exiting (always)
minibuffer-help-form       ; What to show on '?'
```

**Why this matters:**
- Vertico installs itself via `minibuffer-setup-hook`
- Consult uses hooks to inject preview state
- Packages can customize without modifying core

**Lexicon needs:**
```clojure
{:minibuffer/hooks {
   :on-setup  []    ; Vector of event vectors to dispatch
   :on-exit   []    ; Always runs, even on abort
   :on-abort  []}}  ; Runs only on C-g
```

---

### 2. **Minibuffer-Local Keymaps** 🆕

**Missing from your design:**

When minibuffer is active, keybindings change:
- `C-g` → `abort-minibuffers` (not `keyboard-quit`)
- `RET` → `exit-minibuffer`
- `TAB` → `minibuffer-complete`
- `M-n`/`M-p` → next/previous-history-element

These are defined in `minibuffer-local-map`, which is **buffer-local** to minibuffer buffer.

**Lexicon implementation:**
```clojure
;; In your keymap system:
(when (minibuffer-active?)
  (use-keymap :minibuffer-local-map))  ; Higher priority than global
```

---

### 3. **History Management** 🆕

**Completely absent from your docs:**

Emacs minibuffer has **per-command history**:
- `M-x` has command history
- `C-x C-f` has file history
- `switch-to-buffer` has buffer history

History is saved via `savehist-mode` and restored between sessions.

**Access with:**
- `M-p` (previous-history-element)
- `M-n` (next-history-element)
- `M-r` (previous-matching-history-element)

**Lexicon needs:**
```clojure
{:minibuffer/session {
   :history-var :command-history    ; Which history list to use
   :history-pos nil}}               ; Current position in history

{:minibuffer/history {
   :command-history ["previous" "commands"]
   :file-history ["/path/to/file"]
   :buffer-history ["*scratch*" "foo.txt"]}}
```

---

## Phasing Critique

**Your v0-v6 plan is sound, but reorder:**

### Suggested revision:

**v0: Minimal Session (Unchanged)** ✅
- Single-line input
- Accept/abort
- Layout snapshot/restore

**v1: Hooks + Keymaps (NEW - insert before completion)** 🆕
- Minibuffer-local keymaps
- Setup/exit hooks
- This enables packages to extend

**v2: Completion as Service (Your v1)** ✅
- TAB completion
- Separate completions window
- Pluggable completion engine

**v3: History (NEW - insert before expansion)** 🆕
- Per-command history
- M-p/M-n navigation
- History persistence

**v4: Expanded Minibuffer (Your v2)** ✅
- Vertical expansion
- Inline completion rendering

**v5: Preview (Your v3)** ✅
- State function pattern
- Speculative interaction

**v6: Recursion (Your v4)** ✅
- Stack discipline
- Nested sessions

**v7: Actions & Metadata (Your v5)** ✅
- Context-aware dispatch

---

## Implementation Corrections

### 1. Window Restoration

**Your claim:** "Layout snapshots are mandatory."

**Refinement:** Emacs doesn't snapshot full layout - it saves **window configuration**:

```elisp
(let ((old-config (current-window-configuration)))
  (unwind-protect
      (do-minibuffer-stuff)
    (set-window-configuration old-config)))
```

**For Lexicon:**
```clojure
;; Capture window tree structure
(defn capture-window-config [frame-id]
  {:window-tree (get-in db [:frames frame-id :window-tree])
   :active-window-id (:active-window-id db)})

;; Restore on exit
(defn restore-window-config [config]
  (-> db
      (assoc-in [:frames frame-id :window-tree] (:window-tree config))
      (assoc :active-window-id (:active-window-id config))))
```

This is **cheaper** than full snapshot and matches Emacs behavior.

---

### 2. Resize Policy

**Your Plan2:** `:resize-policy :grow-only | :elastic`

**Emacs actual values:** `nil | grow-only | t`

Use Emacs names for familiarity:

```clojure
{:minibuffer/resize-policy :grow-only   ; Default
 :minibuffer/max-height 0.25}           ; Fraction of frame height
```

---

## Final Recommendations

### 1. **Update Your Data Model**

Combine your insights with my refinements:

```clojure
{:minibuffer {
   ;; Session management
   :active-session {:id uuid
                    :prompt "M-x "
                    :buffer-id buffer-id
                    :completion-spec {...}
                    :state-fn (fn [action cand] ...)
                    :history-var :command-history
                    :window-config {...}
                    :on-accept [:execute-command]
                    :on-abort [:minibuffer/abort]}

   :session-stack []   ; For recursion

   ;; Display configuration
   :resize-policy :grow-only
   :max-height 0.25

   ;; History (persisted)
   :history {:command-history []
             :file-history []
             :buffer-history []}

   ;; Hooks (for packages)
   :hooks {:on-setup []
           :on-exit []
           :on-abort []}}}

;; Separate: Minibuffer window is part of UI layout
{:ui/layout {:minibuffer-window-id "mb"}}
```

---

### 2. **Start with v0, but plan for v1 (hooks)**

Don't implement v0 without **extensibility hooks**. Otherwise you'll have to retrofit them, breaking packages.

Add hooks from day one, even if unused:

```clojure
(defn minibuffer-setup []
  (run-hooks :minibuffer/on-setup)   ; ← Empty initially, but present
  (show-minibuffer))
```

---

### 3. **Follow Emacs naming conventions**

Use Emacs variable names where possible:
- `resize-mini-windows` → `:resize-mini-windows` (not `:resize-policy`)
- `minibuffer-setup-hook` → `:minibuffer/on-setup`
- `enable-recursive-minibuffers` → `:minibuffer/recursive?`

This helps users transitioning from Emacs understand your system.

---

### 4. **Document the window vs buffer distinction**

Your docs blur "minibuffer" (the buffer) and "minibuffer window" (the UI slot).

Be explicit:
- **Minibuffer buffer** = text content (created on demand)
- **Minibuffer window** = fixed UI slot (always exists in frame)
- **Echo area** = same window as minibuffer, but showing messages when minibuffer inactive

---

## Conclusion

**Your research is excellent.** The core architecture is sound. My refinements address:

1. ✅ Terminology precision (window vs buffer vs echo area)
2. ✅ Missing features (hooks, keymaps, history)
3. ✅ Implementation details (state function pattern, resize values)
4. ✅ Phase ordering (hooks before completion)

**Next step:** Integrate these refinements into Phase 7.8.1 plan in ROADMAP.md.

---

## Appendix: Key Emacs Source References

- `src/minibuf.c` - C implementation, buffer creation, recursion stack
- `lisp/minibuffer.el` - Completion system, hooks, keymaps
- `lisp/simple.el` - `read-from-minibuffer`, history
- Consult/Vertico - Modern package patterns

**Further reading:**
- GNU Emacs Lisp Reference Manual: Chapter 20 (Minibuffers)
- `describe-function read-from-minibuffer` in Emacs
- Consult README (preview state pattern)
