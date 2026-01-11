# Development Guardrails - Lexicon Editor

**Date:** 2026-01-11
**Purpose:** Concrete processes and tools to prevent architectural mistakes

---

## Guardrail 0: Package Isolation from Re-frame

**Rule:** External packages MUST NOT use re-frame directly. Re-frame is a core implementation detail.

### What Qualifies as "External Package"?
- Any code in `packages/` that is NOT `editor-cljs`, `lexicon-engine`, or `backend-server`
- Examples: `vertico`, `orderless`, `consult`, `magit`, custom user packages
- Future completion ecosystem packages (Phase 8+)

### Required Pattern

**❌ VIOLATION:**
```clojure
(ns my-package.feature
  (:require [re-frame.core :as rf]))  ; ❌ Direct re-frame usage

(rf/dispatch [:some-event])  ; ❌ Tight coupling to core
(rf/subscribe [:some-sub])   ; ❌ Non-portable
```

**✅ CORRECT:**
```clojure
(ns my-package.feature
  (:require [lexicon.api.buffer :as buffer]
            [lexicon.api.cursor :as cursor]
            [lexicon.api.command :as command]))

(buffer/open "file.txt")     ; ✅ Pure API call
(cursor/move 100)            ; ✅ No re-frame knowledge
(command/execute :save-buffer) ; ✅ Portable
```

### Why This Matters
- **Portability** - Packages work even if core switches from re-frame
- **Stability** - `lexicon.api.*` is the contract, implementation can change
- **Separation** - Clear boundary between core and extensions
- **Maintainability** - Packages don't break on core refactors

### Enforcement
- Manual code review during package development
- Future: Linter rule to prevent `re-frame.core` requires in `packages/`
- Documented in architecture guides

### Exemptions
- Core packages (`editor-cljs`, `lexicon-engine`, `backend-server`) can use re-frame
- `evil-mode` gets temporary exemption as it's a core integration example

---

## Guardrail 1: Mandatory Design Doc for State Changes

**Rule:** ANY PR that modifies state structure requires a design document BEFORE coding.

### What Qualifies as "State Change"?
- Adding/removing fields from `db.cljs` default-db
- Changing format of existing state (linear → line-col)
- Adding new event that updates multiple state paths
- Changing ownership of state paths

### Design Doc Template

Create `/tmp/design-STATE_CHANGE_NAME.md` with:

```markdown
# Design: [Feature/Fix Name]

## Current State
- What fields exist today
- Who owns them (from db.cljs)
- What format they use
- Who reads them

## Proposed Change
- What will change
- Why (root cause, not symptom)
- New state structure (with example)

## Impact Analysis
- What code reads this state? (grep for the path)
- What subscriptions derive from it?
- What events update it?
- What tests check it?

## Migration Path
- Can old and new coexist? (for gradual migration)
- What breaks immediately?
- Rollback plan if tests fail

## Checklist
- [ ] Respects Single Source of Truth principle
- [ ] Uses consistent format (line-col for positions)
- [ ] Documents ownership in db.cljs
- [ ] Updates are atomic (not split across :fx)
- [ ] Tests verify state AND UI
- [ ] Invariants documented and tested
```

### Enforcement
- **Pre-coding:** Share design doc, get approval before writing code
- **Code review:** Reject PR if no design doc linked
- **Exception:** Bug fixes that restore previous behavior (no new state)

---

## Guardrail 2: State Invariant Tests (Automated)

**Rule:** Critical state relationships must have executable tests that run on EVERY event.

### Core Invariants to Test

**File:** `/packages/editor-cljs/test/lexicon/invariants_test.cljs`

```clojure
(ns lexicon.invariants-test
  (:require [clojure.test :refer [deftest testing is]]
            [lexicon.position :as position]))

(defn check-cursor-sync-invariant
  "Window cursor and global UI cursor must stay synchronized"
  [db]
  (let [active-window-id (:active-window-id db)
        window-tree (:window-tree db)
        active-window (db/find-window-in-tree window-tree active-window-id)

        ;; Read both cursor positions
        global-linear (get-in db [:ui :cursor-position] 0)
        window-line-col (:cursor-position active-window)

        ;; Get buffer text for conversion
        buffer-id (:buffer-id active-window)
        buffer-text (get-in db [:buffers buffer-id :cache :text] "")

        ;; Convert window cursor to linear
        expected-linear (position/line-col->linear window-line-col buffer-text)]

    (is (= global-linear expected-linear)
        (str "Cursor sync invariant violated!\n"
             "  Global cursor: " global-linear "\n"
             "  Window cursor: " window-line-col "\n"
             "  Expected: " expected-linear))))

(defn check-mark-consistency-invariant
  "If mark is active, mark position must be valid"
  [db]
  (let [active-window-id (:active-window-id db)
        window-tree (:window-tree db)
        active-window (db/find-window-in-tree window-tree active-window-id)
        mark-active? (get-in db [:ui :mark-active?] false)
        mark-position (:mark-position active-window)]

    (when mark-active?
      (is (some? mark-position)
          "Mark is active but mark-position is nil!"))))

(defn check-buffer-bounds-invariant
  "Cursor must never exceed buffer length"
  [db]
  (let [active-window-id (:active-window-id db)
        window-tree (:window-tree db)
        active-window (db/find-window-in-tree window-tree active-window-id)
        buffer-id (:buffer-id active-window)
        buffer-text (get-in db [:buffers buffer-id :cache :text] "")
        cursor-line-col (:cursor-position active-window)
        cursor-linear (position/line-col->linear cursor-line-col buffer-text)
        buffer-length (count buffer-text)]

    (is (<= cursor-linear buffer-length)
        (str "Cursor beyond buffer bounds!\n"
             "  Cursor: " cursor-linear "\n"
             "  Buffer length: " buffer-length))))

(deftest test-all-invariants
  (testing "State invariants hold after common operations"
    (let [db (-> default-db
                 (setup-test-buffer "test content\nline 2\nline 3"))]

      ;; Test after cursor movement
      (let [db' (-> db
                    (dispatch-sync [:update-cursor-position 5]))]
        (check-cursor-sync-invariant db')
        (check-buffer-bounds-invariant db'))

      ;; Test after mark set
      (let [db' (-> db
                    (dispatch-sync [:set-mark-command]))]
        (check-mark-consistency-invariant db'))

      ;; Test after text insertion
      (let [db' (-> db
                    (dispatch-sync [:insert-text "new text"]))]
        (check-cursor-sync-invariant db')
        (check-buffer-bounds-invariant db')))))
```

### Integration with Events (Development Mode)

Add interceptor that runs invariant checks after EVERY event:

**File:** `/packages/editor-cljs/src/lexicon/interceptors.cljs`

```clojure
(ns lexicon.interceptors
  (:require [re-frame.core :as rf]
            [lexicon.invariants :as invariants]))

(def check-invariants
  "Development-mode interceptor that validates state invariants"
  (rf/->interceptor
    :id :check-invariants
    :after (fn [context]
             (when goog.DEBUG  ; Only in development
               (let [db (get-in context [:effects :db])]
                 (when db
                   (try
                     (invariants/check-all db)
                     (catch js/Error e
                       (js/console.error "❌ INVARIANT VIOLATION:" (.-message e))
                       (js/console.error "Stack:" (.-stack e))
                       (throw e))))))
             context)))

;; Apply to all events
(rf/reg-global-interceptor check-invariants)
```

### CI Integration

Add to `.github/workflows/test.yml`:

```yaml
- name: Run Invariant Tests
  run: |
    npm run test:invariants
    if [ $? -ne 0 ]; then
      echo "❌ State invariant tests failed - PR blocked"
      exit 1
    fi
```

---

## Guardrail 3: Pre-Commit State Ownership Check

**Rule:** Git hook prevents commits that violate state ownership rules.

### Implementation

**File:** `.git/hooks/pre-commit` (or use `husky`)

```bash
#!/bin/bash

# Check for state ownership violations
echo "🔍 Checking state ownership rules..."

# Extract state ownership rules from db.cljs
OWNERSHIP_RULES=$(grep "^;; " packages/editor-cljs/src/lexicon/db.cljs | grep "→")

# Check staged files for violations
VIOLATIONS=0

for file in $(git diff --cached --name-only | grep "\.cljs$"); do
  # Check for direct :ui :cursor-position updates outside edit.cljs
  if [[ $file != *"edit.cljs" ]]; then
    if grep -q "assoc-in.*:ui.*:cursor-position" "$file"; then
      echo "❌ VIOLATION: $file updates :ui :cursor-position (owned by edit.cljs)"
      VIOLATIONS=$((VIOLATIONS + 1))
    fi
  fi

  # Check for direct :window-tree updates outside ui.cljs
  if [[ $file != *"ui.cljs" ]] && [[ $file != *"buffer.cljs" ]]; then
    if grep -q "assoc.*:window-tree" "$file"; then
      echo "⚠️  WARNING: $file updates :window-tree (owned by ui.cljs)"
      echo "   If using sync helper, this is OK. Otherwise, use :window/set-cursor event."
    fi
  fi
done

if [ $VIOLATIONS -gt 0 ]; then
  echo ""
  echo "❌ Found $VIOLATIONS state ownership violations"
  echo "   Review docs/design-principles.md - Principle #3"
  echo "   Use approved sync helpers or dispatch events to state owners"
  exit 1
fi

echo "✅ State ownership check passed"
```

### Install Hook

```bash
chmod +x .git/hooks/pre-commit
# Or use husky for team-wide enforcement
```

---

## Guardrail 4: Mandatory Test Coverage for State Changes

**Rule:** PRs that modify state MUST include tests for both state and UI.

### Test Template

For any event that updates state, include BOTH tests:

```clojure
(deftest test-query-replace-state-and-ui
  (testing "Query-replace updates cursor position"

    ;; STATE TEST: Check internal state
    (let [db (-> default-db
                 (setup-buffer "foo bar baz")
                 (dispatch-sync [:query-replace "foo" "qux"]))]

      ;; Check window cursor (line-col)
      (is (= {:line 0 :column 3}
             (get-in db [:window-tree :cursor-position])))

      ;; Check global cursor (linear) - CRITICAL!
      (is (= 3 (get-in db [:ui :cursor-position])))

      ;; Check mark position
      (is (= 0 (get-in db [:window-tree :mark-position]))))

    ;; UI TEST: Check what user sees
    (e2e/with-browser
      (e2e/execute-command "query-replace")
      (e2e/send-keys "foo" :enter "qux" :enter)

      ;; Check cursor rendered correctly
      (is (= {:row 0 :col 3} (e2e/get-cursor-position)))

      ;; Check selection highlighted
      (is (e2e/has-class? ".selection" "active")))))
```

### Code Review Checklist Item

```markdown
- [ ] **State Tests:** Does test verify internal state (not just UI)?
  - [ ] Checks cursor position in BOTH :window-tree AND :ui
  - [ ] Checks mark position if relevant
  - [ ] Checks buffer modification flag
  - [ ] Checks any derived state
```

---

## Guardrail 5: Regression Test Suite (Baseline)

**Rule:** Establish test baseline and NEVER allow regressions without explicit justification.

### Current Baseline (Post Issue #65 Fix)

**File:** `.github/test-baseline.txt`

```
Total Tests: 69
Errors: 3 (pre-existing, not regressions)
  - test-p2-04-buffer-modified-indicator (missing .status-bar)
  - test-p6b-03-status-bar-formatting (missing .status-bar)
  - test-p7-8-query-replace-regexp (unimplemented feature)
Failures: 5 (known issues, tracked)
Passes: 61

GOLDEN RULE: Do NOT merge if failures increase above 5.
```

### CI Enforcement

```yaml
- name: Check Test Regression
  run: |
    npm run test:rope > test-output.txt

    FAILURES=$(grep -oP '\d+ failures' test-output.txt | grep -oP '\d+')

    if [ $FAILURES -gt 5 ]; then
      echo "❌ TEST REGRESSION DETECTED"
      echo "   Baseline: 5 failures"
      echo "   Current: $FAILURES failures"
      echo "   Run 'npm run test:rope' locally and fix regressions"
      exit 1
    fi

    echo "✅ Test baseline maintained ($FAILURES failures)"
```

### Process for Intentional Changes

If you MUST increase baseline (rare):

1. Document in PR description:
   - Why baseline must increase
   - What functionality is temporarily broken
   - Issue number tracking the fix
2. Update `.github/test-baseline.txt`
3. Require 2 approvals (instead of 1)

---

## Guardrail 6: Root Cause Analysis Document

**Rule:** For any bug that takes >2 hours to debug, create RCA document BEFORE fixing.

### When to Write RCA

- Test regressions that weren't caught immediately
- Bugs that affect multiple modules
- Bugs caused by architectural issues
- Bugs that reveal missing documentation

### RCA Template

**File:** `/tmp/rca-ISSUE_NUMBER.md`

```markdown
# Root Cause Analysis: [Issue Title]

**Date:** YYYY-MM-DD
**Issue:** #123
**Time Spent:** X hours
**Impact:** [Critical/High/Medium/Low]

## Symptoms
- What broke
- When it was discovered
- What tests failed

## Root Cause
- WHY it broke (not just what)
- What architectural assumption was wrong
- What documentation was missing

## Why Wasn't This Caught Earlier?
- What tests should have caught this?
- What code review would have prevented it?
- What design principle was violated?

## Fix
- What we changed
- Why this fixes the root cause (not symptom)

## Prevention
- What guardrail would prevent this?
- What documentation needs updating?
- What process needs changing?

## Lessons Learned
- What design principle to add
- What tooling to build
- What knowledge to share
```

### Process

1. Create RCA document
2. Share with team/user
3. Get agreement on root cause
4. THEN implement fix
5. Update design principles if needed
6. Close RCA with link to fix PR

---

## Implementation Plan

### Phase 1: Documentation (This Week)
- [x] Create `/docs/design-principles.md`
- [x] Create `/docs/development-guardrails.md`
- [ ] Update `/docs/state-model.md` with ownership rules
- [ ] Create `.github/test-baseline.txt`
- [ ] Document labels in `.CLAUDE.md`

### Phase 2: Tooling (Next Week)
- [ ] Create `/packages/editor-cljs/test/lexicon/invariants_test.cljs`
- [ ] Add invariant checking interceptor (dev mode)
- [ ] Set up pre-commit hook for state ownership
- [ ] Add test baseline check to CI

### Phase 3: Process (Ongoing)
- [ ] Require design docs for state changes (code review)
- [ ] Write RCA for Issue #65 (cursor sync bug)
- [ ] Conduct monthly review of design principles
- [ ] Update guardrails based on new learnings

### Phase 4: Refactoring (Post-Stabilization)
- [ ] Issue #66: Eliminate duplicate cursor state
- [ ] Issue #67: Standardize position formats
- [ ] Issue #68: Create `lexicon.position` namespace

---

## Success Metrics

### Leading Indicators (Process)
- Design docs written before coding: >80% of state PRs
- Pre-commit hooks catching violations: >5 per month (good!)
- Code reviews citing design principles: >50% of PRs

### Lagging Indicators (Outcomes)
- Test regressions per sprint: <1 (down from current ~4)
- Hours debugging state bugs: <5 per sprint
- Confidence in refactoring: High (team survey)

---

## Emergency Brake

**If tests regress >10%:** STOP all feature work.

1. Revert last PR
2. Write comprehensive RCA
3. Review ALL recent state changes
4. Add missing invariant tests
5. Resume only when baseline restored

**Philosophy:** Better to ship slower than to ship broken.

---

## References

- `/docs/design-principles.md` - Core principles
- `/docs/state-model.md` - State architecture
- `.github/test-baseline.txt` - Test regression baseline
- `/tmp/root-cause-analysis.md` - Issue #65 RCA
