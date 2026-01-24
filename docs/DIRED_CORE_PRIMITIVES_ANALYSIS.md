# Dired Core Primitives Analysis

**Date:** 2026-01-24
**Source:** Emacs 29.4 `~/projects/emacs-source/lisp/dired.el` (4,964 lines)
**Purpose:** Identify CORE editor primitives needed for packages like Dired

---

## Executive Summary

**The Goal is NOT to implement Dired** - it's to identify what's missing in Lexicon's CORE that would prevent us from implementing packages like Dired, Magit, Org-mode, etc.

After analyzing ~2000 lines of dired.el, the core primitives needed fall into these categories:

### 1. **Text Properties** (CRITICAL MISSING PRIMITIVE)
- Dired heavily uses text properties to mark regions invisibly
- Used for: hide-details mode, file name detection, mouse interaction
- **Why needed:** Packages need to attach metadata to buffer regions without visible markup
- **Examples from dired.el:**
  - Line 1894: `(put-text-property ... 'invisible 'dired-hide-details-information)`
  - Line 1912: `'dired-filename t` property marks where filenames are
  - Line 1939: `keymap` property for clickable directory segments

### 2. **Buffer-Local Variables** (PARTIALLY IMPLEMENTED)
- Dired uses many buffer-local vars: `dired-directory`, `dired-subdir-alist`, `buffer-read-only`
- **Status:** Lexicon has `:local-vars` but may need more infrastructure
- **Examples from dired.el:**
  - Line 1442: `(setq-local file-name-coding-system ...)`
  - Line 1461: `(setq dired-subdir-alist nil)` - buffer-specific state
  - Line 2232: `buffer-read-only` - mode-specific behavior

### 3. **Hooks System** (MISSING)
- Dired uses hooks for extensibility: `dired-before-readin-hook`, `dired-after-readin-hook`
- **Why critical:** Packages extend behavior without modifying core code
- **Examples from dired.el:**
  - Line 1439: `(run-hooks 'dired-before-readin-hook)`
  - Line 2002: `(run-hooks 'dired-after-readin-hook)`
  - Line 2009: `(hack-dir-local-variables-non-file-buffer)` - directory-local hooks

### 4. **Markers** (MISSING)
- Emacs markers are positions that move with text edits
- **Why needed:** Preserve positions during buffer modifications
- **Examples from dired.el:**
  - Line 1552: `(setq other (copy-marker other))`
  - Line 1553: `(setq file (copy-marker file))`
  - Line 2043: Window-point markers in `window-prev-buffers`

### 5. **Overlays vs Text Properties** (CONCEPTUAL GAP)
- Emacs has TWO systems for invisible metadata: text properties (permanent) and overlays (temporary)
- Dired uses text properties for structural markup
- **Decision needed:** Do we need both, or can text properties suffice?

### 6. **Save-Excursion** (MISSING PRIMITIVE)
- Critical control flow: execute code then restore point/mark
- **Why needed:** Temporary navigation without disrupting user's cursor
- **Examples from dired.el:**
  - Line 1258: `(save-excursion (goto-char (point-min)) ...)`
  - Used 30+ times in dired.el
  - Line 2052: Nested save-excursion for complex position tracking

### 7. **Buffer Modification Hooks** (MISSING)
- `before-change-functions`, `after-change-functions`, `inhibit-read-only`
- **Why needed:** Modes need to react to/prevent edits
- **Examples from dired.el:**
  - Line 1453: `(let ((inhibit-read-only t) ...)`  - bypass read-only temporarily
  - Line 1440: `(if (consp buffer-undo-list) (setq buffer-undo-list nil))` - undo integration

### 8. **Window/Buffer Navigation API** (PARTIALLY MISSING)
- `pop-to-buffer-same-window`, `switch-to-buffer-other-window`, `with-current-buffer`
- **Status:** Lexicon has basic window management, may need more APIs
- **Examples from dired.el:**
  - Line 1153: `(pop-to-buffer-same-window (dired-noselect dirname switches))`
  - Line 1317: `(set-buffer buffer)` - non-displaying buffer switch
  - Line 2076: `(with-selected-window ...)`  - temporary window context

### 9. **Revert Buffer** (FRAMEWORK MISSING)
- Buffers can be refreshed from source (file/directory)
- **Why needed:** Sync buffer with external state changes
- **Examples from dired.el:**
  - Line 1969: `(defun dired-revert ...)` - custom revert for dired
  - Line 1328: `(revert-buffer)` - standard revert call
  - Line 2232: `dired-buffer-stale-p` - detect when refresh needed

### 10. **Keymap Inheritance & Remapping** (PARTIALLY IMPLEMENTED)
- Mode keymaps, local keymaps, text-property keymaps
- **Status:** Lexicon has keymaps, but may lack property-based bindings
- **Examples from dired.el:**
  - Line 1875: Keymap defined as variable `dired-mouse-drag-files-map`
  - Line 1903: `'keymap dired-mouse-drag-files-map` as text property
  - Line 1960: Inline keymap definition with `define-keymap`

---

## CRITICAL CORE GAPS (Must Fix Before Packages)

### Priority 1: Text Properties
**Impact:** Affects ALL packages (org-mode, magit, dired, vertico completion)

**Required Functions:**
```clojure
(put-text-property start end property value)  ; Set property on range
(get-text-property pos property)              ; Query property at position
(text-properties-at pos)                      ; All properties at position
(propertize string &rest properties)          ; Create string with properties
```

**Properties Used by Dired:**
- `invisible` - hide text without deleting
- `dired-filename` - mark filename regions
- `mouse-face` - highlight on hover
- `help-echo` - tooltip text
- `keymap` - region-specific keybindings

**Test Strategy:**
- Semantic test: Set invisible property, verify not rendered but still in buffer
- Semantic test: Put keymap property, verify keys work in that region only
- NOT a Dired test - this is CORE functionality

### Priority 2: Save-Excursion
**Impact:** Needed by virtually all interactive commands

**Required Macro:**
```clojure
(save-excursion & body)  ; Save point/mark, execute body, restore
```

**Why Critical:**
- Dired uses it 30+ times
- Any command that "looks around" needs this
- Without it, every command must manually save/restore state

**Test Strategy:**
- Semantic test: Move point in save-excursion, verify restored after
- Semantic test: Errors in body still restore position
- Core test, not package test

### Priority 3: Markers
**Impact:** Required for any mode that tracks positions across edits

**Required API:**
```clojure
(make-marker)                    ; Create marker
(set-marker marker pos &optional buffer)  ; Position marker
(marker-position marker)         ; Query marker
(marker-buffer marker)           ; Which buffer
(copy-marker marker)             ; Duplicate
(move-marker marker pos buf)     ; Reposition
```

**Why Critical:**
- Window-point must survive buffer edits
- Undo system needs position tracking
- Multi-window editing requires stable references

**Test Strategy:**
- Semantic test: Insert text before marker, verify marker moves
- Semantic test: Delete text containing marker, verify marker behavior
- Semantic test: Markers in different buffers don't interfere

### Priority 4: Hooks System
**Impact:** Package extensibility, mode initialization

**Required Infrastructure:**
```clojure
(defhook hook-name)              ; Declare hook
(add-hook 'hook-name function)   ; Register callback
(remove-hook 'hook-name function) ; Unregister
(run-hooks 'hook-name)           ; Execute all callbacks
```

**Hooks Dired Uses:**
- `dired-before-readin-hook` - before directory listing
- `dired-after-readin-hook` - after directory listing
- `before-change-functions` - before any edit
- `after-change-functions` - after any edit

**Test Strategy:**
- Semantic test: Add hook, trigger event, verify callback ran
- Semantic test: Remove hook, verify not called
- Semantic test: Hook errors don't crash editor

### Priority 5: Inhibit-Read-Only
**Impact:** Modes need to modify "read-only" buffers internally

**Required Mechanism:**
```clojure
(let ((inhibit-read-only t))  ; Temporarily allow edits
  ... modify read-only buffer ...)
```

**Why Critical:**
- Dired buffer is read-only to users
- But Dired itself must update the listing
- Without this, no mode can manage its own buffer

**Test Strategy:**
- Semantic test: Read-only buffer rejects normal insert
- Semantic test: Insert with inhibit-read-only succeeds
- Already partially tested in existing read-only tests

---

## MEDIUM PRIORITY (Can Defer)

### Buffer-Local Setq
- `setq-local` - set variable locally without explicit buffer parameter
- Convenience wrapper, not strictly required

### Window Point vs Buffer Point
- Dired tracks positions per-window AND per-buffer
- May need `:window-point` distinct from `:buffer-point`

### Combine-Change-Calls
- Batch change notifications for performance
- Line 1452: Optimization, not semantic requirement

---

## IMPLEMENTATION ROADMAP

### Phase 1: Core Primitives (Week 1-2)
**Goal:** Add missing primitives to `lexicon.core`

1. **Text Properties** (3 days)
   - Add `:properties` map to text representation
   - Implement `put-text-property`, `get-text-property`
   - Update rendering to respect `invisible` property
   - **Tests:** `test/lexicon/semantic/text_properties_test.cljs`

2. **Save-Excursion** (1 day)
   - Macro that captures `:cursor-position`, `:mark-position`
   - Restore in `finally` block
   - **Tests:** `test/lexicon/semantic/excursion_test.cljs`

3. **Markers** (3 days)
   - New `:markers` atom in db (id -> {:buffer :position})
   - WASM integration: markers adjust on insert/delete
   - **Tests:** `test/lexicon/semantic/markers_test.cljs`

4. **Hooks** (2 days)
   - Add `:hooks` map to db
   - `run-hooks` event handler
   - **Tests:** `test/lexicon/semantic/hooks_test.cljs`

### Phase 2: Dired Scaffolding (Week 3)
**Goal:** Minimal Dired that proves primitives work

1. **Dired Mode** (2 days)
   - Buffer with text properties for filenames
   - Read-only with `inhibit-read-only` for refresh
   - Uses hooks for before/after readin

2. **E2E Tests** (3 days)
   - Test that Dired buffer appears
   - Test that buffer is read-only
   - Test that text properties are present
   - **NOT testing actual file operations yet**

### Phase 3: Full Implementation (Deferred)
**Goal:** Document, don't implement

1. **Dired Tests (Deferred)**
   - Mark files (NOT IMPLEMENTED)
   - Delete files (NOT IMPLEMENTED)
   - File operations (NOT IMPLEMENTED)
   - **Each has detailed comment about what primitives it needs**

2. **Filesystem Provider** (Deferred)
   - Issue #88 already exists
   - Don't conflate Dired with filesystem
   - Dired should use abstract FS API

---

## SEMANTIC TESTS TO WRITE

### Test Suite: `test/lexicon/semantic/text_properties_test.cljs`

```clojure
(deftest text-properties-invisible
  "Text with invisible property is not rendered but remains in buffer"
  (testing "Invisible text hidden from display"
    (with-buffer "*test*"
      (insert "Hello World")
      (put-text-property 6 11 'invisible t)  ; Hide "World"
      (is (= "Hello World" (buffer-text)))   ; Still in buffer
      (is (= "Hello " (visible-text))))))    ; Not rendered

(deftest text-properties-survive-edits
  "Text properties move with text during edits"
  (testing "Property range adjusts on insert"
    (with-buffer "*test*"
      (insert "Hello World")
      (put-text-property 6 11 'face 'bold)
      (goto-char 6)
      (insert "Beautiful ")
      ;; Property should now be at 16-21 (shifted by 10 chars)
      (is (= 'bold (get-text-property 16 'face))))))

(deftest text-property-keymaps
  "Keymap property enables region-specific bindings"
  (testing "Keys work differently in property region"
    (with-buffer "*test*"
      (let ((special-map (make-keymap)))
        (define-key special-map "x" :special-x-command)
        (insert "Normal text")
        (put-text-property 7 11 'keymap special-map)
        (goto-char 5)
        (is (= :self-insert (lookup-key "x")))  ; Normal binding
        (goto-char 8)
        (is (= :special-x-command (lookup-key "x")))))))  ; Property binding
```

### Test Suite: `test/lexicon/semantic/markers_test.cljs`

```clojure
(deftest markers-track-insertions
  "Markers move when text inserted before them"
  (testing "Marker advances with insertion"
    (with-buffer "*test*"
      (insert "Hello")
      (let ((m (make-marker)))
        (set-marker m 5)  ; After "Hello"
        (goto-char 0)
        (insert "XXX")
        (is (= 8 (marker-position m)))))))  ; Moved by 3

(deftest markers-survive-deletions
  "Markers adjust when surrounding text deleted"
  (testing "Marker before deletion unaffected"
    (with-buffer "*test*"
      (insert "Hello World")
      (let ((m (make-marker)))
        (set-marker m 5)  ; After "Hello"
        (delete-region 6 11)  ; Delete "World"
        (is (= 5 (marker-position m)))))))  ; Unchanged
```

### Test Suite: `test/lexicon/semantic/hooks_test.cljs`

```clojure
(deftest hooks-execute-in-order
  "Hooks run in order they were added"
  (testing "Multiple hooks on same event"
    (let ((calls (atom []))]
      (add-hook 'test-hook (fn [] (swap! calls conj :first)))
      (add-hook 'test-hook (fn [] (swap! calls conj :second)))
      (run-hooks 'test-hook)
      (is (= [:first :second] @calls)))))

(deftest buffer-change-hooks
  "before-change and after-change hooks fire on edits"
  (testing "Hooks capture edit parameters"
    (with-buffer "*test*"
      (let ((changes (atom []))]
        (add-hook 'before-change-functions
                  (fn [beg end] (swap! changes conj [:before beg end])))
        (add-hook 'after-change-functions
                  (fn [beg end len] (swap! changes conj [:after beg end len])))
        (insert "Hello")
        (is (= [[:before 0 0] [:after 0 5 0]] @changes))))))
```

---

## KEY INSIGHTS

### 1. Dired is NOT Special
- It's just a mode that uses core primitives
- Same primitives needed by org-mode, magit, ibuffer
- **Don't implement Dired - implement the primitives**

### 2. Text Properties are Foundation
- Used by: completion UI, syntax highlighting, clickable links, hide-show
- Without them, Lexicon can't support rich packages
- **Most critical missing feature**

### 3. Markers Enable Multi-Window
- Window-point must be stable across buffer edits
- Undo needs position tracking
- **Required for reliable editing experience**

### 4. Hooks Enable Packages
- Packages extend editor without modifying core
- Directory-local variables depend on hooks
- **Package ecosystem depends on this**

### 5. Save-Excursion is Ubiquitous
- Every command that "looks around" uses it
- Prevents user confusion from cursor jumping
- **Basic ergonomic requirement**

---

## ANTI-PATTERNS TO AVOID

### ❌ DON'T: Implement Dired File Operations Yet
- That's Issue #88 (Filesystem)
- Separate concern from core primitives
- **Focus: Can we RENDER a directory? Not: Can we DELETE files?**

### ❌ DON'T: Add Dired-Specific Code to Core
- No special cases for Dired in buffer.cljs
- Dired should be 100% userspace code
- **If Dired needs it, make it a general primitive**

### ❌ DON'T: Skip Semantic Tests
- These primitives affect ALL packages
- Dired tests come AFTER primitive tests
- **Test the foundation, not the building**

### ✅ DO: Write Deferred Tests with Comments
- Document what full Dired test WOULD look like
- Mark as `^:deferred` or skip with detailed comment
- **Ensures we don't forget the end goal**

### ✅ DO: Check Against Other Packages
- If org-mode also needs text properties → high priority
- If only Dired needs something → maybe defer
- **Validate against multiple use cases**

---

## CONCLUSION

**Dired revealed 5 CRITICAL missing primitives:**
1. Text Properties (affects all packages)
2. Markers (multi-window reliability)
3. Save-Excursion (ergonomics)
4. Hooks (extensibility)
5. Inhibit-Read-Only (mode self-management)

**These are NOT Dired features** - they're core editor capabilities that Dired happens to expose the need for.

**Next Steps:**
1. Write semantic tests for each primitive (Week 1)
2. Implement primitives in core (Week 2)
3. Create minimal Dired scaffold to prove it works (Week 3)
4. Document full Dired tests for future implementation (Week 3)

**Success Criteria:**
- ✅ Text properties render correctly (invisible works)
- ✅ Markers survive buffer edits
- ✅ Save-excursion restores state
- ✅ Hooks fire at right times
- ✅ Dired buffer appears with read-only enforcement
- ❌ NOT YET: File operations (that's Issue #88)

This analysis shows we're not far from package support - we just need the right primitives.
