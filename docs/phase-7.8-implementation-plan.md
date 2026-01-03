# Phase 7.8 Implementation Plan - Complete Emacs Core

**Goal:** 100% Emacs Reference Card Coverage
**Current Coverage:** 59% (34/58 commands)
**Target Coverage:** 100% (58/58 commands)
**Missing Commands:** 24 (20 ❌ + 2 🟡 + 2 bonus)
**Timeline:** 3 weeks
**Status:** Planning

---

## Success Criteria

- ✅ All P0 commands from EmacsReferenceCard.md implemented
- ✅ All P1 commands from EmacsReferenceCard.md implemented
- ✅ E2E tests for each new command
- ✅ Zero regressions in existing tests
- ✅ EmacsReferenceCard.md shows 100% coverage
- ✅ Can use Lexicon for real editing work (search, replace, scroll, save-all)

---

## Command Inventory by Difficulty

### EASY (1-2 days each) - 8 commands

| Command | Binding | Why Easy | Effort |
|---------|---------|----------|--------|
| `exchange-point-and-mark` | C-x C-x | Swap two variables | 2 hours |
| `kill-word` | M-d | Like kill-line, use forward-word | 3 hours |
| `backward-kill-word` | M-DEL | Mirror of kill-word | 2 hours |
| `scroll-up-command` | C-v | Update viewport offset | 4 hours |
| `scroll-down-command` | M-v | Mirror of scroll-up | 3 hours |
| `describe-variable` | C-h v | Clone describe-function | 3 hours |
| `replace-string` | M-x replace-string | Loop + replace, no interaction | 6 hours |
| `replace-regexp` | M-x replace-regexp | Like replace-string, use JS regex | 6 hours |

**Total: 8 commands, ~30 hours (4 days)**

### MEDIUM (2-4 days each) - 8 commands

| Command | Binding | Why Medium | Effort |
|---------|---------|------------|--------|
| `save-some-buffers` | C-x s | Loop buffers, minibuffer prompts | 8 hours |
| `insert-file` | C-x i | File I/O + insert at point | 6 hours |
| `find-alternate-file` | C-x C-v | Kill buffer + find-file combo | 6 hours |
| `revert-buffer` | M-x revert-buffer | Re-read file, confirm prompt | 8 hours |
| `eval-last-sexp` | C-x C-e | Eval CLJS, show result | 8 hours |
| `eval-defun` | C-M-x | Find defun bounds, eval | 8 hours |
| `eval-region` | M-x eval-region | Get region text, eval | 6 hours |
| `load-file` | M-x load-file | Read file, eval contents | 6 hours |

**Total: 8 commands, ~56 hours (7 days)**

### HARD (4-7 days each) - 4 commands

| Command | Binding | Why Hard | Effort |
|---------|---------|----------|--------|
| `query-replace` | M-% | State machine, special keymap, prompts | 20 hours |
| `isearch-forward` | C-s | Modal mode, incremental feedback, special keymap | 24 hours |
| `isearch-backward` | C-r | Mirror of isearch-forward | 16 hours |
| `kmacro-start-macro` | C-x ( | Record command sequence | 12 hours |
| `kmacro-end-macro` | C-x ) | Stop recording | 4 hours |
| `kmacro-end-and-call-macro` | C-x e | Replay recorded commands | 8 hours |

**Total: 6 commands, ~84 hours (10 days)**

### BONUS (Not on refcard, but needed) - 2 commands

| Command | Binding | Why Needed | Effort |
|---------|---------|------------|--------|
| `backward-kill-word` | M-DEL | Listed in audit as P0 gap | 2 hours |
| `open-line` | C-o | Already implemented, verify | 1 hour |

**Total: 2 commands, ~3 hours**

---

## Recommended Implementation Order

### Batch 1: Quick Wins (Days 1-2) - Build Momentum

**Goal:** Get 6 commands done FAST to prove we can do this

1. ✅ `exchange-point-and-mark` (C-x C-x) - 2 hours
2. ✅ `kill-word` (M-d) - 3 hours
3. ✅ `backward-kill-word` (M-DEL) - 2 hours
4. ✅ `describe-variable` (C-h v) - 3 hours
5. ✅ `scroll-up-command` (C-v) - 4 hours
6. ✅ `scroll-down-command` (M-v) - 3 hours

**Total: 17 hours (2 days)**
**New Coverage: 45% → 55%**

**Rationale:**
- All ClojureScript, no Rust changes
- Clear implementation patterns
- Immediate usability improvement
- Psychological boost

---

### Batch 2: File/Buffer Commands (Days 3-5) - Essential Operations

**Goal:** Complete file management subsystem

7. ✅ `insert-file` (C-x i) - 6 hours
8. ✅ `revert-buffer` (M-x revert-buffer) - 8 hours
9. ✅ `save-some-buffers` (C-x s) - 8 hours
10. ✅ `find-alternate-file` (C-x C-v) - 6 hours

**Total: 28 hours (3.5 days)**
**New Coverage: 55% → 62%**

**Rationale:**
- Users need these for real workflows
- Builds on existing file I/O infrastructure
- Test file operations thoroughly

---

### Batch 3: Basic Replace (Days 6-7) - Foundation for Search ✅ COMPLETE

**Goal:** Non-interactive replace working

11. ✅ `replace-string` (M-x replace-string) - 6 hours ✅ IMPLEMENTED
12. ✅ `replace-regexp` (M-x replace-regexp) - 6 hours ✅ IMPLEMENTED

**Total: 12 hours (1.5 days)**
**New Coverage: 62% → 66%**
**Status:** COMPLETE - Both commands implemented and registered

**Rationale:**
- Simpler than query-replace
- Validates search loop logic
- Needed for batch operations

---

### Batch 4: Query Replace (Days 8-10) - Interactive Replace ✅ COMPLETE

**Goal:** Interactive find-replace working

13. ✅ `query-replace` (M-%) - 20 hours ✅ IMPLEMENTED

**Total: 20 hours (2.5 days)**
**New Coverage: 66% → 69%**
**Status:** COMPLETE - query-replace implemented with full state machine and key handlers (y/n/!/q/^/.)

**Implementation Plan:**
- Study Emacs `replace.el` carefully
- Design state machine: idle → searching → found → prompting → replace/skip
- Special keymap active during query-replace
- Minibuffer shows: "Query replacing 'foo' with 'bar': (y/n/!/q/^)"
- Support keys:
  - `y` = replace this match and continue
  - `n` = skip this match and continue
  - `!` = replace all remaining without asking
  - `q` = quit (stop replacing)
  - `^` = go back to previous match
  - `.` = replace this match and quit

**Rationale:**
- Users expect interactive replace
- Foundation for understanding modal editing
- Prepares us for isearch

---

### Batch 5: Incremental Search (Days 11-16) - The Big One

**Goal:** C-s and C-r working with basic features

14. ✅ `isearch-forward` (C-s) - 24 hours
15. ✅ `isearch-backward` (C-r) - 16 hours

**Total: 40 hours (5 days)**
**New Coverage: 69% → 76%**

**Implementation Plan (Phased):**

**Phase 1: Basic Isearch (Day 11-12)**
- Enter isearch mode (special state)
- Show "I-search: " in echo area/minibuffer
- Accumulate search string as user types
- Search forward/backward on each keystroke
- Move point to match
- Highlight current match (simple)
- RET exits and leaves point at match
- C-g aborts and returns to start position

**Phase 2: Repeat Search (Day 13)**
- C-s again = find next match
- C-r = find previous match
- DEL = remove last char from search string, return to previous match position
- Wrap around at end/beginning of buffer

**Phase 3: Polish (Day 14-15)**
- Case-insensitive search (if search string is all lowercase)
- Case-sensitive if any uppercase letters
- Show failure message: "Failing I-search: "
- Show wrapped message: "Overwrapped I-search: "
- Any command key exits isearch and executes that command

**Phase 4: Lazy Highlighting (Day 16)**
- Dim highlights for all matches in viewport
- Bright highlight for current match
- Update highlights as search string changes

**What We're SKIPPING (for now):**
- Regexp isearch (C-M-s)
- Word/symbol search (M-s w, M-s _)
- Search ring history (M-n, M-p)
- Occur integration (M-s o)
- Search highlighting across multiple buffers

**Rationale:**
- Most critical missing feature
- Users NEED search to do real work
- Foundation for other search features
- Complex but achievable in 5 days

---

### Batch 6: Lisp Evaluation (Days 17-19) - REPL Integration

**Goal:** Eval commands for CLJS development

16. ✅ `eval-last-sexp` (C-x C-e) - 8 hours
17. ✅ `eval-defun` (C-M-x) - 8 hours
18. ✅ `eval-region` (M-x eval-region) - 6 hours
19. ✅ `load-file` (M-x load-file) - 6 hours

**Total: 28 hours (3.5 days)**
**New Coverage: 76% → 83%**

**Implementation Plan:**
- Use SCI for safe evaluation (already have it)
- eval-last-sexp: find sexp before point, eval, show in echo area
- eval-defun: find top-level form, eval
- eval-region: get region text, eval
- load-file: read file contents, eval
- Handle errors gracefully, show in echo area

**Rationale:**
- Needed for dogfooding (developing Lexicon in Lexicon)
- Users expect REPL integration
- SCI infrastructure already exists
- Validates package system

---

### Batch 7: Keyboard Macros (Days 20-21) - Advanced Feature

**Goal:** Record and replay command sequences

20. ✅ `kmacro-start-macro` (C-x () - 12 hours
21. ✅ `kmacro-end-macro` (C-x )) - 4 hours
22. ✅ `kmacro-end-and-call-macro` (C-x e) - 8 hours

**Total: 24 hours (3 days)**
**New Coverage: 83% → 88%**

**Implementation Plan:**
- Track recording state in app-db
- During recording, capture all command invocations
- Store as vector of command keywords
- Replay by dispatching stored commands in sequence
- Support C-u prefix arg for repeat count
- Study Emacs `kmacro.el`

**Rationale:**
- Power user feature
- Demonstrates command system flexibility
- Relatively self-contained

---

### Batch 8: Final Polish (Day 22) - Verification

**Goal:** 100% coverage confirmed

23. ✅ Verify minibuffer TAB completion - 2 hours
24. ✅ Verify minibuffer RET exit - 1 hour
25. ✅ `repeat` (C-x z) - 4 hours

**Total: 7 hours (1 day)**
**New Coverage: 88% → 100%**

---

## Timeline Summary

| Week | Days | Batches | Commands | Coverage |
|------|------|---------|----------|----------|
| Week 1 | 1-7 | Batches 1-3 | 12 commands | 45% → 66% |
| Week 2 | 8-14 | Batches 4-5 | 3 commands (huge) | 66% → 76% |
| Week 3 | 15-21 | Batches 6-8 | 9 commands | 76% → 100% |

**Total: 21 days, 24 commands, 45% → 100% coverage**

---

## Testing Strategy

**For Each Command:**
1. Write E2E test FIRST (test-driven)
2. Implement command
3. Verify test passes
4. Commit
5. Mark ✅ in this document

**E2E Test Template:**
```clojure
(deftest test-COMMAND-NAME
  (test/goto-editor!)
  (test/insert-text! "test content")
  ;; Setup
  (test/send-keys! "C-a")  ; go to beginning
  ;; Execute command
  (test/send-keys! "BINDING")
  ;; Verify behavior
  (is (= expected (test/get-buffer-content))))
```

**Test Categories:**
- Basic functionality
- Edge cases (empty buffer, no matches, etc.)
- Interaction with prefix args (C-u)
- Undo/redo integration
- Multiple buffer scenarios

---

## Risk Mitigation

### High-Risk Commands

**isearch (C-s, C-r):**
- **Risk:** Most complex, easy to get wrong
- **Mitigation:** Phased implementation, study Emacs source extensively, allocate 5 days
- **Fallback:** Ship basic version first, enhance later

**query-replace (M-%):**
- **Risk:** State machine complexity
- **Mitigation:** Draw state diagram first, implement incrementally, test each state
- **Fallback:** Ship without `^` (go back) feature if needed

**kmacro:**
- **Risk:** Command recording might interact poorly with other systems
- **Mitigation:** Test with all command types, disable during macro replay if needed
- **Fallback:** Defer to Phase 7.9 if blocking

### General Risks

**Scope Creep:**
- **Risk:** Adding features beyond reference card
- **Mitigation:** Strictly stick to this plan, defer enhancements

**Test Failures:**
- **Risk:** Breaking existing functionality
- **Mitigation:** Run full test suite after each batch, fix immediately

**Time Overruns:**
- **Risk:** Commands take longer than estimated
- **Mitigation:** Re-prioritize after each batch, cut P2 features if needed

---

## Success Metrics

**Week 1 Goals:**
- ✅ 12 commands implemented
- ✅ 21% coverage increase
- ✅ All tests passing
- ✅ Can scroll and navigate comfortably

**Week 2 Goals:**
- ✅ Search and replace working
- ✅ Can do real editing workflows
- ✅ 10% coverage increase
- ✅ Query-replace functional

**Week 3 Goals:**
- ✅ 100% reference card coverage
- ✅ All E2E tests passing
- ✅ Documentation updated
- ✅ Phase 7.8 complete

---

## Next Steps

1. Review and approve this plan
2. Start with Batch 1 (Quick Wins)
3. Update this document after each command
4. Commit after each ✅
5. Celebrate milestones (50%, 75%, 100%)

---

## Notes

- Estimated hours are conservative (include learning, testing, debugging)
- Can parallelize some work (e.g., forward/backward versions)
- Should commit after each command, not batch
- Update EmacsReferenceCard.md as we go
- This is achievable - we have the infrastructure, just need to execute
