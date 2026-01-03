# Core Command Audit Session - Phase 7.8

**Date:** 2026-01-03
**Status:** In Progress

## Commands Currently Registered

Based on `lexicon.events.command/initialize-commands`:

### Files & Buffers
- ✅ `find-file` (C-x C-f)
- ✅ `save-buffer` (C-x C-s)
- ✅ `write-file` (C-x C-w)
- ✅ `switch-to-buffer` (C-x b)
- ✅ `kill-buffer` (C-x k)
- ✅ `list-buffers` (C-x C-b)

### Windows
- ✅ `split-window-below` (C-x 2)
- ✅ `split-window-right` (C-x 3)
- ✅ `delete-window` (C-x 0)
- ✅ `delete-other-windows` (C-x 1)
- ✅ `other-window` (C-x o)

### Cursor Motion
- ✅ `forward-char` (C-f)
- ✅ `backward-char` (C-b)
- ✅ `next-line` (C-n)
- ✅ `previous-line` (C-p)
- ✅ `beginning-of-line` (C-a)
- ✅ `end-of-line` (C-e)
- ✅ `forward-word` (M-f)
- ✅ `backward-word` (M-b)
- ✅ `beginning-of-buffer` (M-<)
- ✅ `end-of-buffer` (M->)

### Editing
- ✅ `delete-backward-char` (DEL/Backspace)
- ✅ `delete-forward-char` (C-d)
- ✅ `kill-line` (C-k)
- ✅ `open-line` (C-o)

### Kill/Yank
- ✅ `kill-region` (C-w)
- ✅ `copy-region-as-kill` (M-w)
- ✅ `yank` (C-y)
- ✅ `yank-pop` (M-y)

### Mark
- ✅ `set-mark-command` (C-SPC)

### Undo
- ✅ `undo` (C-/, C-x u)

### Minibuffer
- ✅ `execute-extended-command` (M-x)
- ✅ `keyboard-quit` (C-g)

### Help
- ✅ `describe-bindings` (C-h b)
- ✅ `describe-key` (C-h k)
- ✅ `describe-function` (C-h f)
- ✅ `apropos-command` (C-h a)
- ✅ `help-for-help` (C-h ?)

### Other
- ✅ `universal-argument` (C-u)

## Commands MISSING from Reference Card

### High Priority (P0)

**Files:**
- ❌ `save-some-buffers` (C-x s) - Save all modified buffers
- ❌ `insert-file` (C-x i) - Insert file contents
- ❌ `find-alternate-file` (C-x C-v) - Replace buffer with different file

**Buffers:**
- ❌ `revert-buffer` (M-x revert-buffer) - Reload from file

**Scrolling:**
- ❌ `scroll-up-command` (C-v) - Scroll forward one screen
- ❌ `scroll-down-command` (M-v) - Scroll backward one screen

**Editing:**
- ❌ `kill-word` (M-d) - Kill word forward
- ❌ `backward-kill-word` (M-DEL) - Kill word backward

**Search/Replace:**
- ❌ `isearch-forward` (C-s) - Incremental search forward
- ❌ `isearch-backward` (C-r) - Incremental search backward
- ❌ `query-replace` (M-%) - Interactive replace
- ❌ `replace-string` (M-x replace-string)
- ❌ `replace-regexp` (M-x replace-regexp)

**Mark/Region:**
- ❌ `exchange-point-and-mark` (C-x C-x) - Swap point and mark

**Minibuffer:**
- ❌ Minibuffer completion (TAB) - needs verification
- ❌ Minibuffer history navigation

**Help:**
- ❌ `describe-variable` (C-h v) - Describe variable

### Medium Priority (P1)

**Keyboard Macros:**
- ❌ `kmacro-start-macro` (C-x ()
- ❌ `kmacro-end-macro` (C-x ))
- ❌ `kmacro-end-and-call-macro` (C-x e)

**Rectangles:**
- ❌ Rectangle commands (C-x r ...)

**Case Conversion:**
- ❌ `upcase-word` (M-u)
- ❌ `downcase-word` (M-l)
- ❌ `capitalize-word` (M-c)

**Transpose:**
- ❌ `transpose-chars` (C-t)
- ❌ `transpose-words` (M-t)
- ❌ `transpose-lines` (C-x C-t)

**Lisp Evaluation:**
- ❌ `eval-last-sexp` (C-x C-e)
- ❌ `eval-defun` (C-M-x)
- ❌ `eval-region` (M-x eval-region)
- ❌ `load-file` (M-x load-file)

### Summary

**Total Reference Card Commands:** ~60-70
**Currently Implemented:** ~40
**Coverage:** ~60-65%

**P0 Gaps:** 15 commands
**P1 Gaps:** 10+ commands

## Audit Results Summary (UPDATED)

### Overall Statistics

**Total Reference Card Commands:** 58
**Implemented (✅):** 34 commands
**Missing (❌):** 20 commands
**Partial/Needs Verification (🟡):** 2 commands
**Not Applicable (🚫):** 4 commands (web-based context)

**Coverage:** ~59% (34/58)

### Priority Breakdown

**P0 (Critical - Must Have):** 15 missing + 2 partial = 17 gaps
**P1 (Important - Should Have):** 7 missing
**P2 (Nice to Have):** 1 missing
**P3 (Not Applicable):** 4 commands

### P0 Gaps - Critical Missing Commands

**Files (3 commands):**
1. `save-some-buffers` (C-x s) - Save all modified buffers
2. `insert-file` (C-x i) - Insert file contents at point
3. `find-alternate-file` (C-x C-v) - Replace current buffer with different file

**Buffers (1 command):**
4. `revert-buffer` (M-x revert-buffer) - Reload buffer from disk

**Scrolling (2 commands):**
5. `scroll-up-command` (C-v) - Scroll forward one screen
6. `scroll-down-command` (M-v) - Scroll backward one screen

**Deleting (1 command):**
7. `kill-word` (M-d) - Kill word forward

**Mark (1 command):**
8. `exchange-point-and-mark` (C-x C-x) - Swap point and mark

**Search (4 commands) - ENTIRE SUBSYSTEM MISSING:**
9. `isearch-forward` (C-s) - Incremental search forward
10. `isearch-backward` (C-r) - Incremental search backward
11. Repeat search (C-s / C-r again) - Depends on isearch
12. Exit search (RET) - Depends on isearch

**Replace (3 commands) - ENTIRE SUBSYSTEM MISSING:**
13. `query-replace` (M-%) - Interactive find-replace
14. `replace-string` (M-x replace-string) - Non-interactive replace
15. `replace-regexp` (M-x replace-regexp) - Regexp replace

**Help (1 command):**
16. `describe-variable` (C-h v) - Describe variable

**Minibuffer (2 commands needing verification):**
17. Minibuffer completion (TAB) - 🟡 Needs verification
18. Exit minibuffer (RET) - 🟡 Should work, needs verification

### P1 Gaps - Important Missing Commands

**Lisp Evaluation (4 commands):**
1. `eval-last-sexp` (C-x C-e) - Evaluate expression before point
2. `eval-defun` (C-M-x) - Evaluate top-level form
3. `eval-region` (M-x eval-region) - Evaluate selected region
4. `load-file` (M-x load-file) - Load Clojure/ClojureScript file

**Keyboard Macros (3 commands):**
5. `kmacro-start-macro` (C-x () - Start recording macro
6. `kmacro-end-macro` (C-x )) - End recording macro
7. `kmacro-end-and-call-macro` (C-x e) - Execute last macro

### P2 Gaps - Nice to Have

1. `repeat` (C-x z) - Repeat last command

### Critical Findings

**Search/Replace Subsystem Completely Missing:**
- No incremental search (C-s, C-r)
- No query-replace (M-%)
- No replace-string or replace-regexp
- This is a FUNDAMENTAL editor capability
- **Recommendation:** Top priority for Phase 7.8

**Scrolling Missing:**
- C-v and M-v are essential navigation commands
- Used constantly in Emacs workflows
- **Recommendation:** High priority, easy to implement

**Reference Card Errors Found:**
- Section 6 incorrectly lists C-y as "Scroll one line up" (actually `yank`)
- Section 6 incorrectly lists C-e as "Scroll one line down" (actually `end-of-line`)

### Implementation Priority Order

**Tier 1 (Immediate - Week 1):**
1. Search subsystem (isearch-forward, isearch-backward)
2. Replace subsystem (query-replace, replace-string, replace-regexp)
3. Scrolling commands (scroll-up-command, scroll-down-command)

**Tier 2 (High Priority - Week 2):**
4. File commands (save-some-buffers, insert-file, find-alternate-file)
5. Buffer commands (revert-buffer)
6. kill-word (M-d)
7. exchange-point-and-mark (C-x C-x)
8. describe-variable (C-h v)

**Tier 3 (Important - Week 3):**
9. Lisp evaluation commands (eval-last-sexp, eval-defun, eval-region, load-file)

**Tier 4 (Enhancement - Future):**
10. Keyboard macros (kmacro-start-macro, kmacro-end-macro, kmacro-end-and-call-macro)
11. repeat command (C-x z)

## Next Steps

1. ✅ Fill in EmacsReferenceCard.md with status column - COMPLETE
2. ✅ Prioritize P0 gaps by implementation difficulty - COMPLETE
3. Begin implementing Tier 1 commands (Search/Replace/Scrolling)
4. Add E2E tests for each implemented command
5. Update ROADMAP.md Phase 7.8 progress tracking
