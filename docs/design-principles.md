# Design Principles - Lexicon Editor

**Date:** 2026-01-11
**Purpose:** Core design constraints to maintain architectural integrity as complexity grows

---

## Core Principles

### 1. Single Source of Truth (SSOT)

**Principle:** Every piece of state should have exactly ONE authoritative source.

**Problem We Had:** Dual cursor state (`:ui :cursor-position` AND `:window-tree :cursor-position`) with no clear ownership or synchronization.

**Rules:**
- ✅ **Prefer derived state over duplicated state**
  - Use re-frame subscriptions to compute values from canonical source
  - Example: Derive linear cursor position from window cursor + buffer text
- ✅ **If duplication is unavoidable, document the synchronization contract**
  - State ownership rules in `db.cljs`
  - Helper functions that maintain invariants atomically
  - Clear comments explaining WHY duplication exists
- ❌ **Never have two fields with same semantic meaning but different formats**
  - If needed, one must be clearly marked as "cache" or "derived"

**Action Items:**
- [ ] Eliminate `:ui :cursor-position` in favor of derived subscription (Issue #66)
- [x] Document all state synchronization contracts in `db.cljs`

---

### 2. Format Consistency

**Principle:** Related data should use consistent formats throughout the system.

**Problem We Had:**
- `:ui :cursor-position` = linear integer
- `:window-tree :cursor-position` = `{:line N :column M}` map
- `:window-tree :mark-position` = linear integer

**Rules:**
- ✅ **Choose ONE canonical format per data type**
  - Position data: Use line-col maps everywhere (more UI-friendly)
  - Convert at boundaries (WASM interface, legacy APIs)
- ✅ **Document format contracts in type specs**
  - Use `clojure.spec` to enforce position format
  - Spec: `::position (s/keys :req-un [::line ::column])`
- ✅ **Centralize conversions in dedicated namespace**
  - `lexicon.position` namespace for all position operations
  - No inline conversion logic scattered across events

**Action Items:**
- [ ] Create `lexicon.position` namespace with all conversion utilities
- [ ] Add clojure.spec definitions for position types
- [ ] Audit all position fields for format consistency

---

### 3. Explicit State Ownership

**Principle:** Every state path must have exactly ONE module that owns it.

**What We Got Right:** `db.cljs` lines 1-40 document ownership rules clearly.

**What We Got Wrong:** No enforcement of these rules - events violated ownership freely.

**Rules:**
- ✅ **Document ownership in `db.cljs` for EVERY state path**
- ✅ **Only owning module may directly update its state**
  - Other modules MUST dispatch events to the owner
  - Example: Only `ui.cljs` updates `:window-tree`, others dispatch `:window/set-cursor`
- ✅ **Create "sync" helper functions when cross-cutting updates needed**
  - Example: `sync-window-and-global-cursor` updates BOTH owned paths atomically
  - Document in comments WHY the exception exists
- ❌ **Never inline cross-module state updates**
  - Reject PRs that violate ownership without justification

**Enforcement:**
- Add code review checklist: "Does this event respect state ownership?"
- Consider: Pre-commit hook to detect cross-module updates

---

### 4. Atomic State Updates

**Principle:** Related state changes must happen atomically, never across multiple event dispatches.

**Problem We Had:** Commit 9358d79 changed synchronous updates to async `:fx` dispatches, breaking atomicity.

**Rules:**
- ✅ **Keep related updates in same event handler**
  - Cursor + mark updates in single event
  - Window tree + global UI state in single transaction
- ✅ **Use helper functions for multi-field updates**
  - `sync-window-and-global-cursor` updates both fields atomically
  - Prevents intermediate inconsistent states
- ❌ **Never use `:fx` dispatches for state that must stay synchronized**
  - `:fx` is async and can be reordered
  - Only use `:fx` for independent side effects
- ✅ **Test state invariants, not just final UI output**
  - Check cursor sync: `(= linear-cursor (line-col->linear window-cursor))`

**Action Items:**
- [ ] Audit all `:fx` dispatches that update related state
- [ ] Write state invariant tests for critical paths
- [ ] Document when `:fx` is appropriate vs `reg-event-db`

---

### 5. Test Both State AND UI

**Principle:** E2E tests must verify actual state, not just rendered output.

**Problem We Had:** Manual testing (UI rendering) worked, but E2E tests (state checks) failed - we trusted manual testing alone.

**Rules:**
- ✅ **E2E tests should check BOTH:**
  1. UI rendering (what user sees)
  2. Internal state (what app stores)
- ✅ **Test state invariants explicitly**
  - Cursor synchronization: window cursor ≡ global cursor
  - Mark activation: mark set ⟹ region exists
  - Buffer modification: dirty flag set after edit
- ✅ **Never trust manual testing alone**
  - Manual testing reads different code paths than tests
  - Humans check happy paths, tests check edge cases
- ✅ **Add state assertions to existing tests**
  - After every command, verify cursor position
  - After every edit, verify buffer state

**Action Items:**
- [ ] Add state verification helpers to `e2e_tests/lexicon/test_helpers.clj`
- [ ] Audit existing tests - add state assertions
- [ ] Document testing strategy in `/docs/testing-strategy.md`

---

### 6. Understand Before Changing

**Principle:** Map the system BEFORE making changes, not after breaking things.

**Problem We Had:** Spent 2 days fixing regressions because we didn't understand the dual cursor architecture before "fixing" it.

**Rules:**
- ✅ **For non-trivial changes, create design doc FIRST**
  - Map current state (what fields exist, who reads them)
  - Map proposed state (what will change, what impacts)
  - Get approval before coding
- ✅ **Read the code you're replacing**
  - Don't just delete - understand WHY it was written that way
  - Commit 9358d79 removed global cursor sync - we should have asked WHY
- ✅ **Document your understanding before fixing**
  - Create `/tmp/analysis.md` explaining root cause
  - Share with team/user for validation
  - Prevents fixing wrong problem
- ❌ **Never "fix later" - finish or revert**
  - Incomplete fixes create compound regressions
  - Better to revert and plan properly than ship broken code

**Process:**
1. Reproduce issue
2. Map current behavior (code + state)
3. Document root cause analysis
4. Design fix (with examples)
5. Get approval
6. Implement
7. Test thoroughly
8. Commit when stable

---

### 7. Conversion Functions as First-Class Citizens

**Principle:** Format conversions are critical infrastructure, not throwaway utilities.

**What We Got Right:** `line-col-to-linear-pos` and `linear-to-line-col` already existed and were well-tested.

**What We Got Wrong:** We didn't realize they existed and almost re-implemented them!

**Rules:**
- ✅ **Centralize ALL conversions in dedicated namespace**
  - `lexicon.position/linear->line-col`
  - `lexicon.position/line-col->linear`
  - `lexicon.position/normalize` (clamp to buffer bounds)
- ✅ **Test conversion functions extensively**
  - Round-trip tests: `pos == linear->line-col->linear(pos)`
  - Edge cases: Empty buffer, single line, trailing newline
  - Performance: These run on EVERY cursor move
- ✅ **Make conversions discoverable**
  - Clear naming: `position/linear->line-col` not `line-col-to-linear-pos`
  - Docstrings with examples
  - Reference from state ownership docs
- ✅ **Never inline conversion logic**
  - Always use the centralized functions
  - Easier to optimize later (memoization, caching)

**Action Items:**
- [ ] Create `lexicon.position` namespace
- [ ] Move conversion functions there
- [ ] Add comprehensive tests
- [ ] Document in `/docs/position-format.md`

---

### 8. Document Invariants Explicitly

**Principle:** State invariants should be documented in code and tested automatically.

**Problem We Had:** `db.cljs` line 31 said "synced with :ui :cursor-position" but no code enforced this!

**Rules:**
- ✅ **Document invariants as executable specs**
  ```clojure
  (s/def ::cursor-sync-invariant
    (fn [db]
      (let [linear (get-in db [:ui :cursor-position])
            window (get-active-window db)
            line-col (:cursor-position window)
            buffer-text (get-buffer-text db (:buffer-id window))]
        (= linear (line-col->linear line-col buffer-text)))))
  ```
- ✅ **Test invariants after EVERY state change**
  - Development mode: `(s/assert ::db db)` after every event
  - CI: Run invariant checks in integration tests
- ✅ **Write invariants in comments AND tests**
  - Comments explain WHAT must be true
  - Tests verify it IS true
- ✅ **Fail fast when invariants violated**
  - Throw exception in dev mode
  - Log error + stack trace in production

**Action Items:**
- [ ] Define core invariants in `lexicon.specs`
- [ ] Add invariant checking to event interceptors
- [ ] Write invariant tests for critical state

---

## Application to Current Architecture

### Cursor/Mark State (Issue #65)

**Current State (Flawed):**
```clojure
{:ui {:cursor-position 42}                    ; Linear - for tests/legacy
 :window-tree {:cursor-position {:line 5 :column 7}  ; Line-col - for UI
               :mark-position 30}}             ; Linear - inconsistent!
```

**Problems:**
1. ❌ Violates SSOT - two cursor positions
2. ❌ Violates format consistency - mixed linear/line-col
3. ❌ Documented invariant but no enforcement

**Short-Term Fix (DONE):**
- [x] `sync-window-and-global-cursor` helper maintains both atomically
- [x] Fixed 4 events to use helper

**Long-Term Fix (Proposed):**
```clojure
;; Single source of truth
{:window-tree {:cursor-position {:line 5 :column 7}
               :mark-position {:line 3 :column 10}}}  ; Consistent format!

;; Derived subscription for legacy code
(rf/reg-sub
 :cursor-position-linear
 :<- [:active-window-cursor]
 :<- [:active-buffer-text]
 (fn [[cursor text] _]
   (position/line-col->linear cursor text)))
```

**Benefits:**
- ✅ Single source of truth (window tree)
- ✅ Consistent format (line-col everywhere)
- ✅ Impossible to desync (computed, not stored)
- ✅ Clear ownership (ui.cljs owns window tree)

---

## Code Review Checklist

Before merging ANY PR that touches state:

- [ ] **SSOT:** Does this create duplicate state? If yes, is it justified and documented?
- [ ] **Format:** Are position/range formats consistent with existing code?
- [ ] **Ownership:** Does this event respect state ownership rules in `db.cljs`?
- [ ] **Atomicity:** Are related state changes in same event handler (not `:fx`)?
- [ ] **Testing:** Does test check BOTH state and UI output?
- [ ] **Invariants:** Are state invariants documented and tested?
- [ ] **Conversions:** Uses centralized conversion functions (not inline logic)?
- [ ] **Understanding:** Does PR description explain WHAT changed and WHY?

---

## Enforcement Strategy

### Phase 1: Documentation (This PR)
- [x] Create `/docs/design-principles.md`
- [ ] Update `/docs/state-model.md` with SSOT rules
- [ ] Add code review checklist to `/docs/contributing.md`

### Phase 2: Tooling (Next Sprint)
- [ ] Add `clojure.spec` definitions for core state
- [ ] Create development mode invariant checker
- [ ] Write state property tests (generative testing)

### Phase 3: Refactoring (Post-Stabilization)
- [ ] Eliminate `:ui :cursor-position` (Issue #66)
- [ ] Standardize position formats (Issue #67)
- [ ] Create `lexicon.position` namespace (Issue #68)

---

## Success Metrics

**Leading Indicators:**
- PRs rejected for violating ownership: Good! Checklist working.
- Spec errors caught in development: Good! Invariants preventing bugs.
- Time spent on root cause analysis before coding: Good! Understanding first.

**Lagging Indicators:**
- Test regressions per sprint: Should decrease
- Time spent fixing state bugs: Should decrease
- Confidence in refactoring: Should increase

---

## References

- Issue #65: Cursor state synchronization bug
- Issue #61: Test synchronization failures (async timing)
- `/tmp/root-cause-analysis.md`: Why we wasted 2 days
- `/tmp/state-architecture-map.md`: Current state model
- `db.cljs` lines 1-40: State ownership documentation

---

**Last Updated:** 2026-01-11
**Review Frequency:** After any major state architecture change
