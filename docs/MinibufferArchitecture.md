# Minibuffer Source Code Analysis

**Date:** 2026-01-04
**Source:** GNU Emacs master branch (cloned locally to /tmp/emacs-source)
**Files analyzed:** `src/minibuf.c` (2548 lines), `lisp/minibuffer.el` (5773 lines)

---

## Key Findings from Emacs Source Code

### 1. Buffer Management (`src/minibuf.c`)

**Minibuffer buffers are named:** `" *Minibuf-N*"` where N is the depth (0-indexed)

```c
// Line 1046: get_minibuffer() function
static char const name_fmt[] = " *Minibuf-%"pI"d*";
char name[sizeof name_fmt + INT_STRLEN_BOUND (EMACS_INT)];
AUTO_STRING_WITH_LEN (lname, name, sprintf (name, name_fmt, depth));
buf = Fget_buffer_create (lname, Qnil);
```

**Critical insight:**
- Depth 0 = `" *Minibuf-0*"` = inactive/idle minibuffer
- Depth 1 = `" *Minibuf-1*"` = first active session
- Depth 2 = `" *Minibuf-2*"` = recursive session
- Buffers are **created on-demand** and **reused** (reset on each invocation)

**Your doc claimed:** "Minibuffer is always a buffer"
**Validated:** ✅ Correct - it's a real buffer with undo, modes, local vars

---

### 2. Recursive Stack Management

**Global variables (lines 47-56):**
```c
Lisp_Object Vminibuffer_list;           // List of all minibuffer buffers
static Lisp_Object Vcommand_loop_level_list;  // Command loop levels per depth
static Lisp_Object minibuf_save_list;   // Saved state stack
EMACS_INT minibuf_level;                // Current recursion depth
```

**Stack discipline:**
- `minibuf_level` starts at 0 (no minibuffer active)
- Increments on entry (line 664): `minibuf_level++`
- Decrements on exit (line 1140): `minibuf_level--`
- `get_minibuffer(depth)` retrieves/creates buffer at depth

**Your doc claimed:** "Recursive by design, stackable interaction contexts"
**Validated:** ✅ Correct - explicit stack with save/restore

---

### 3. Unwind/Cleanup on Exit (`read_minibuf_unwind`)

**Critical restoration (lines 1099-1189):**

```c
read_minibuf_unwind (void)
{
  Fset_buffer (expired_MB);
  minibuf_level--;  // Pop stack

  // Restore ALL saved state from minibuf_save_list:
  this_command_keys = Fcar (minibuf_save_list);
  minibuf_prompt = Fcar (minibuf_save_list);
  minibuf_prompt_width = XFIXNAT (Fcar (minibuf_save_list));
  Vhelp_form = Fcar (minibuf_save_list);
  Vcurrent_prefix_arg = Fcar (minibuf_save_list);
  Vminibuffer_history_position = Fcar (minibuf_save_list);
  Vminibuffer_history_variable = Fcar (minibuf_save_list);
  Voverriding_local_map = Fcar (minibuf_save_list);
  // ... more state restoration ...

  // Erase the minibuffer buffer
  Ferase_buffer ();

  // Resize minibuffer window back to normal
  if (minibuf_level == 0)
    resize_mini_window (XWINDOW (minibuf_window), 0);
}
```

**Key insight:** This is **Emacs' unwind-protect pattern** for minibuffer!

**Your Plan2 claimed:** "All minibuffer effects are reversible (I2)"
**Validated:** ✅ Correct - Emacs saves 10+ pieces of state and restores on exit

**What Lexicon needs:**
```clojure
{:minibuf-save-stack [
  {:command-keys [...]
   :prompt "M-x "
   :prefix-arg nil
   :history-position 0
   :history-variable :command-history
   :keymap-override nil
   :window-config {...}}]}
```

---

### 4. Mode Management

**Minibuffer has TWO modes (lines 1004-1023):**

```c
static void set_minibuffer_mode (Lisp_Object buf, EMACS_INT depth)
{
  if (depth > 0)
    call0 (Qminibuffer_mode);          // Active minibuffer
  else
    call0 (Qminibuffer_inactive_mode); // Inactive (depth 0)
}
```

**Critical revelation:**
- `minibuffer-mode` for active sessions (depth ≥ 1)
- `minibuffer-inactive-mode` for idle state (depth = 0)
- Mode switch happens automatically on entry/exit

**Your doc missed this distinction!**

**Lexicon implication:**
- Don't model as "visible" vs "invisible"
- Model as **buffer mode switch**: inactive-mode → minibuffer-mode

---

### 5. Window Resize (`resize-mini-window`)

**From line 1189:**
```c
if (minibuf_level == 0 || ...)
  resize_mini_window (XWINDOW (minibuf_window), 0);  // Reset to 1 line
```

**This confirms:**
- Minibuffer window resizes **automatically** on entry/exit
- When `minibuf_level == 0`, window shrinks to 1 line
- Resize is a **display-time property**, not state mutation

**Your claim:** "Dynamic expansion - can temporarily steal space"
**Validated:** ✅ Correct - but resize happens in **display/rendering**, not in session setup

---

### 6. Completion Metadata (`lisp/minibuffer.el`)

**From lines 117-147:**

```elisp
(defun completion-metadata (string table pred)
  "Return the metadata of elements to complete at the end of STRING.
   This metadata is an alist.  Currently understood keys are:
   - `category': the kind of objects returned by `all-completions'.
   - `annotation-function': function to add annotations.
   - `affixation-function': function to prepend/append prefix/suffix.
   - `group-function': function for grouping candidates.
   - `display-sort-function': function to sort entries in *Completions*.
   - `cycle-sort-function': function to sort entries when cycling.
   - `eager-display': non-nil to request eager display.
   - `eager-update': non-nil to update display as user types.
   ..."
```

**This is the completion plugin contract!**

**Your claim:** "Completion is external, pluggable"
**Validated:** ✅ Correct - metadata system enables Vertico/Consult/etc

---

## Critical Corrections to Your Docs

### Correction 1: "Always Visible" Terminology

**You wrote (Phase 7.8.1):** "Minibuffer always visible (1 empty line below mode line)"

**Actual Emacs behavior:**
- **Minibuffer window** is always allocated (fixed at bottom of frame)
- **Minibuffer buffer** changes: `" *Minibuf-0*"` when idle, `" *Minibuf-1*"` when active
- The window **displays different buffers**, not different visibility states

**Corrected model for Lexicon:**
```clojure
;; DON'T model as visibility:
{:minibuffer {:always-visible true}}  ; ❌ Wrong

;; DO model as window allocation + buffer switching:
{:ui/layout {:minibuffer-window-id "mb-window"}  ; Always exists
 :minibuffer {:active-buffer-id nil}}            ; nil = " *Minibuf-0*" (idle)
                                                  ; uuid = " *Minibuf-1*" (active)
```

---

### Correction 2: Preview Pattern

**You proposed (Plan2):**
```clojure
{:preview/start   (fn [])
 :preview/update  (fn [candidate])
 :preview/cancel  (fn [])
 :preview/commit  (fn [candidate])}
```

**Better pattern (from Emacs unwind-protect + Consult):**
```clojure
;; Single state function, not four separate functions
(defn preview-state-fn []
  (let [preview-state (atom nil)]
    (fn [action candidate]
      (case action
        :preview (reset! preview-state (show-preview candidate))
        :exit    (cleanup-preview @preview-state)
        :return  (commit-preview @preview-state candidate)))))

{:minibuffer/session {:state-fn (preview-state-fn)}}
```

**Why this is better:**
- Closure naturally captures preview state
- Single function = simpler contract
- Matches Consult's `:state` pattern
- Actions are extensible (`:preview`, `:exit`, `:return`, `:narrow`, etc.)

---

### Correction 3: History Variables

**Missing from your docs:** Minibuffer history is per-command

**From `read_minibuf_unwind` (lines 1155-1158):**
```c
Vminibuffer_history_position = Fcar (minibuf_save_list);
Vminibuffer_history_variable = Fcar (minibuf_save_list);
```

**Each command specifies which history to use:**
- `M-x` → `command-history`
- `C-x C-f` → `file-name-history`
- `switch-to-buffer` → `buffer-name-history`

**Lexicon needs:**
```clojure
{:minibuffer/session {
   :history-var :command-history    ; Which history list
   :history-pos 0}}                 ; Current position in history

{:minibuffer/history {
   :command-history ["previous-cmd" "another-cmd"]
   :file-name-history ["/path/to/file"]
   :buffer-name-history ["*scratch*" "foo.txt"]}}
```

**Access with:** `M-p` (previous), `M-n` (next)

---

## New Findings Not in Your Docs

### Finding 1: Minibuffer Prompt Width Tracking

**From minibuf.c (line 75):**
```c
static ptrdiff_t minibuf_prompt_width;
```

**This is saved/restored on stack (line 1149):**
```c
minibuf_prompt_width = XFIXNAT (Fcar (minibuf_save_list));
```

**Why this matters:**
- Emacs tracks **column offset** for cursor positioning
- Prompt like `"M-x "` has width 4
- Input cursor position accounts for prompt width

**Lexicon needs:**
```clojure
{:minibuffer/session {:prompt "M-x "
                      :prompt-width 4}}  ; For cursor calc
```

---

### Finding 2: Frame-Awareness

**From minibuf.c (lines 115-131):**
```c
static void choose_minibuf_frame (void)
{
  // Each frame can have its own minibuffer window
  minibuf_window = sf->minibuffer_window;
}
```

**Multi-frame behavior:**
- Each frame can have its own minibuffer window
- OR frames can share a minibuffer window
- Configurable via `minibuffer-follows-selected-frame`

**Lexicon (browser = single frame):**
- Simplification: Single minibuffer window per app
- But architecture should **allow** future multi-frame support

---

### Finding 3: Buffer Reuse and Reset

**From get_minibuffer() (lines 1056-1063):**
```c
else  // Buffer already exists
{
  delete_all_overlays (XBUFFER (buf));
  reset_buffer (XBUFFER (buf));
}
```

**Minibuffer buffers are REUSED, not recreated:**
- First invocation: Create `" *Minibuf-1*"`
- Exit: Erase buffer content
- Next invocation: Reuse `" *Minibuf-1*"` (reset overlays, local vars)

**Lexicon should:**
- Create minibuffer buffers lazily
- Reset on exit, not destroy
- Reuse for next invocation

---

## Recommended Data Model (Refined)

Based on source code analysis:

```clojure
{:minibuffer {
   ;; Stack of active sessions (grows on recursion)
   :session-stack []

   ;; Current active session (nil when idle)
   :active-session nil  ; or {:id ...}

   ;; Session structure:
   {:id uuid
    :depth 1                           ; 1 = first level, 2 = recursive, etc.
    :buffer-id buffer-id               ; Corresponds to " *Minibuf-N*"
    :prompt "M-x "
    :prompt-width 4
    :initial-input ""

    ;; Saved state (for restoration on exit)
    :saved-state {:command-keys [...]
                  :prefix-arg nil
                  :window-config {...}
                  :help-form nil}

    ;; History
    :history-var :command-history
    :history-pos 0

    ;; Completion
    :completion {:enabled? false
                 :table nil             ; Function or list
                 :metadata {}           ; category, annotation-fn, etc.
                 :candidates []}

    ;; Preview (Consult-style)
    :state-fn (fn [action candidate] ...)

    ;; Hooks (for packages like Vertico)
    :on-setup []                       ; Event vectors
    :on-exit  []

    ;; Callbacks
    :on-accept [:execute-command]
    :on-abort  [:minibuffer/abort]}}

   ;; Global history (persisted)
   :history {:command-history []
             :file-name-history []
             :buffer-name-history []}

   ;; Display configuration
   :resize-mini-windows :grow-only     ; nil | :grow-only | t
   :max-mini-window-height 0.25}}      ; Fraction of frame

;; Separate: UI layout (always has minibuffer window slot)
{:ui/layout {:minibuffer-window-id "mb"}}
```

---

## Implementation Recommendations

### 1. Start with Idle Buffer

Create `" *Minibuf-0*"` on startup:

```clojure
(defn init-minibuffer []
  (let [idle-buffer (create-buffer " *Minibuf-0*")]
    (set-buffer-mode idle-buffer :minibuffer-inactive-mode)
    {:buffers {(:id idle-buffer) idle-buffer}
     :ui/layout {:minibuffer-window-id "mb"
                 :minibuffer-buffer-id (:id idle-buffer)}}))
```

---

### 2. Session Entry (with unwind-protect)

```clojure
(defn minibuffer-enter [db {:keys [prompt on-accept on-abort history-var]}]
  (let [depth (inc (count (get-in db [:minibuffer :session-stack])))
        buffer (get-or-create-minibuffer-buffer depth)
        session {:id (random-uuid)
                 :depth depth
                 :buffer-id (:id buffer)
                 :prompt prompt
                 :prompt-width (count prompt)
                 :saved-state (capture-state db)  ; Save everything!
                 :history-var history-var
                 :history-pos 0
                 :on-accept on-accept
                 :on-abort on-abort}]

    ;; Push session onto stack
    (-> db
        (update-in [:minibuffer :session-stack] conj session)
        (assoc-in [:minibuffer :active-session] session)
        (assoc-in [:ui/layout :minibuffer-buffer-id] (:id buffer))
        (run-hooks :minibuffer/on-setup))))  ; Run setup hooks!
```

---

### 3. Session Exit (unwind-protect pattern)

```clojure
(defn minibuffer-exit [db commit?]
  (let [session (:active-session (:minibuffer db))
        {:keys [buffer-id saved-state on-accept on-abort state-fn]} session]

    ;; Call preview cleanup
    (when state-fn
      (state-fn :exit nil))

    ;; Run exit hooks
    (run-hooks db :minibuffer/on-exit)

    ;; Erase minibuffer buffer
    (erase-buffer buffer-id)

    ;; Restore saved state
    (-> db
        (restore-state saved-state)
        (update-in [:minibuffer :session-stack] pop)
        (assoc-in [:minibuffer :active-session]
                  (peek (get-in db [:minibuffer :session-stack])))  ; Pop to previous
        (cond-> commit? (dispatch-event on-accept)
                (not commit?) (dispatch-event on-abort)))))
```

---

## Next Steps

1. ✅ Your core architecture is sound
2. ⚠️ Update Phase 7.8.1 with these refinements:
   - Change "always visible" to "always-allocated window"
   - Add minibuffer modes (inactive-mode vs minibuffer-mode)
   - Add history management (v1 priority)
   - Use state-fn pattern for preview
   - Add hooks from day 1
3. 📖 Reference this document in your implementation
4. 🔬 Consider cloning Consult/Vertico for pattern study

---

## Source References

- `src/minibuf.c:47-56` - Global variables
- `src/minibuf.c:1030-1066` - `get_minibuffer()` - Buffer creation
- `src/minibuf.c:1099-1189` - `read_minibuf_unwind()` - Cleanup pattern
- `src/minibuf.c:1004-1023` - `set_minibuffer_mode()` - Mode switching
- `lisp/minibuffer.el:117-147` - Completion metadata

**Full Emacs source:** `/tmp/emacs-source/`
