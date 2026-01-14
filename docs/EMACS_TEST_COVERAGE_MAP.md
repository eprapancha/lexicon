# Emacs Test Coverage Map

**Purpose:** Classification of Emacs test suite for semantic compatibility validation.

**Emacs Version:** 29.4 (tag: emacs-29.4, commit: 6a299b3caceb)
**Source:** `/tmp/emacs-source/`

---

## Test Categories

Tests are classified into three categories based on semantic portability:

### Category A — Pure Semantic (Portable)
Tests that validate editor semantics independent of UI or implementation details:
- Buffer contents and manipulation
- Point and mark movement
- Kill ring behavior
- Text insertion and deletion
- Command lifecycle

**Priority:** High — These tests form the semantic foundation.

### Category B — Semantic but UI-Coupled
Tests that validate semantics mediated by UI or control plane:
- Minibuffer interaction
- Completion systems
- Window selection and management
- Display-related behavior

**Priority:** Medium — Implement after Category A is stable.

### Category C — Implementation-Specific
Tests tied to Emacs implementation details or non-portable constraints:
- Redisplay internals
- Frame geometry
- Terminal escape sequences
- Native process management
- Timers and async primitives

**Priority:** Deferred — Informative but not binding.

---

## Test File: `simple-tests.el`

**File:** `/tmp/emacs-source/test/lisp/simple-tests.el`
**Category:** Primarily A (pure semantic), some B (minibuffer-coupled)
**Total Tests:** 45+
**Notes:** Covers fundamental buffer editing primitives from `simple.el`

### Tests Classified

#### Category A — Pure Semantic

| Test Name | Lines | Covers | Priority | Notes |
|-----------|-------|--------|----------|-------|
| `simple-test-count-lines` | 53-61 | Buffer line counting | High | Pure buffer semantics, newline handling |
| `simple-test-count-lines/ignore-invisible-lines` | 63-68 | Line counting with text properties | Medium | Requires text property support |
| `simple-text-count-lines-non-ascii` | 70-73 | Unicode line counting | Medium | UTF-8 handling |
| `newline` | 120-137 | Text insertion, point movement | **Critical** | Core insertion semantics, prefix args |
| `newline-indent` | 139-167 | Indentation behavior | Medium | Mode-dependent (electric-indent) |
| `open-line` | 171-189 | Insertion without point movement | **Critical** | Subtle semantics, historically tricky |
| `open-line-margin-and-prefix` | 191-209 | Margin and fill-prefix handling | Low | Advanced formatting |
| `simple-delete-indentation-no-region` | 278-295 | Join lines, whitespace handling | **High** | Multi-line mutations |
| `simple-delete-indentation-inactive-region` | 297-305 | Mark handling without active region | Medium | Mark semantics |
| `simple-delete-indentation-blank-line` | 307-335 | Blank line edge cases | High | Whitespace semantics |
| `simple-delete-indentation-boundaries` | 337-360 | Buffer boundary behavior | Medium | BOB/EOB handling, narrowing |
| `simple-delete-indentation-region` | 362-393 | Region-based deletion | Medium | Region semantics |
| `line-number-at-pos-in-widen-buffer` | 816-823 | Line number calculation | Low | Utility function |
| `line-number-at-pos-in-narrow-buffer` | 825-833 | Line numbers with narrowing | Low | Narrowing semantics |
| `line-number-at-pos-keeps-restriction` | 835-843 | Narrowing preservation | Low | Side-effect testing |
| `line-number-at-pos-keeps-point` | 845-853 | Point preservation | Low | Side-effect testing |

#### Category B — Semantic but UI-Coupled

| Test Name | Lines | Covers | Priority | Notes |
|-----------|-------|--------|----------|-------|
| `simple-execute-extended-command--shorter` | 78-84 | M-x abbreviation | Low | Minibuffer/completion coupling |
| `simple-execute-extended-command--describe-binding-msg` | 87-96 | Key binding description | Low | M-x message formatting |

#### Category C — Implementation-Specific

| Test Name | Lines | Covers | Priority | Notes |
|-----------|-------|--------|----------|-------|
| `simple-transpose-subr` | 112-116 | S-expression transposition | Deferred | Syntax table dependency |
| `simple-tests--undo` | 460-488 | Undo system | Deferred | Complex command boundary coupling |
| `simple-tests--undo-in-region` | 490-529 | Regional undo | Deferred | Undo + region interaction |
| `simple-tests--undo-equiv-table` | 531-606 | Undo equivalence classes | Deferred | Advanced undo semantics |
| `undo-auto-boundary-timer` | 608-610 | Timer-based undo boundaries | Deferred | Timer dependency |
| `undo-auto--boundaries-added` | 612-672 | Automatic undo boundaries | Deferred | Hook timing, command lifecycle edge cases |

---

## Initial Test Selection (5-7 Tests)

For Phase 6.6 Step 3, translate these tests first:

### Selected for Translation

1. **`newline`** (lines 120-137)
   - **Rationale:** Core insertion semantics, prefix argument handling, point movement
   - **Contract clauses:** 2.4 (Prefix arguments), 4.2 (Point), 4.1 (Buffers)

2. **`open-line`** (lines 171-189)
   - **Rationale:** Insertion without point movement—subtle and historically tricky
   - **Contract clauses:** 4.2 (Point), 4.1 (Buffers)

3. **`simple-delete-indentation-no-region`** (lines 278-295)
   - **Rationale:** Multi-line buffer mutations, whitespace handling
   - **Contract clauses:** 4.1 (Buffers), 4.2 (Point)

4. **`simple-test-count-lines`** (lines 53-61)
   - **Rationale:** Pure buffer semantics, newline edge cases
   - **Contract clauses:** 4.1 (Buffers)

5. **`simple-delete-indentation-blank-line`** (lines 307-335)
   - **Rationale:** Whitespace edge cases, blank line handling
   - **Contract clauses:** 4.1 (Buffers), 4.2 (Point)

**Total:** 5 tests (starting conservatively)

**Deferred for later:**
- `newline-indent` — Mode-dependent behavior (electric-indent)
- `simple-transpose-subr` — Syntax table dependency
- Undo tests — Command boundary coupling, do after simpler tests pass

---

## Test Translation Guidelines

When translating tests from Emacs to Lexicon:

1. **Use editor-facing APIs only**
   - No internal state peeking
   - Commands and public functions only

2. **Explicit buffer lifecycle**
   - `with-temp-buffer` is not just sugar—it encodes lifecycle semantics
   - Create, select, destroy must be observable

3. **Preserve semantic intent**
   - Translate behavior, not implementation
   - Test what packages will use

4. **Document divergence**
   - When tests fail, map to contract clause
   - Unknown failures are acceptable initially
   - Don't fix immediately—understand first

---

## Success Criteria

Phase 6.6 Step 3 is complete when:

- ✅ 5 tests translated to Lexicon semantics
- ✅ Test harness uses editor-facing APIs only
- ✅ Buffer lifecycle is explicit
- ✅ Failures are documented with contract clause references
- ✅ At least one surprising failure identified
- ✅ At least one "oh, Emacs does *that*?" moment

---

## Future Test Files

Additional Category A test files to classify (later phases):

- `test/lisp/buff-menu-tests.el` — Buffer management
- `test/lisp/emacs-lisp/cl-lib-tests.el` — ClojureScript/elisp semantic overlap
- `test/lisp/subr-tests.el` — Core editor functions

Additional Category B test files:

- `test/lisp/minibuffer-tests.el` — Minibuffer state machine
- `test/lisp/icomplete-tests.el` — Completion UI

---

## Notes

- **Version pinning:** Lexicon semantic compatibility targets Emacs 29.4
- **Test count discipline:** Start with 5-7 tests, not more
- **Failure interpretation:** Map failures to contract clauses before fixing
- **Category boundaries:** Some tests span categories—classify by primary dependency
