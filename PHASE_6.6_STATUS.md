# Phase 6.6 Testing Migration - Current Status

**Date:** 2025-12-28
**Phase:** 6.6 - Testing Infrastructure Migration (Playwright → Etaoin)
**Progress:** Phase 2 Complete (Setup) - Ready for Phase 3 (Test Conversion)

## What Was Completed

### ✅ Phase 1: Baseline Established
- Committed Playwright baseline (commit 2433cbc)
- All 7 Playwright E2E tests passing
- Documented in ROADMAP.md

### ✅ Phase 2: Etaoin Setup
1. **Removed Playwright**
   - Deleted from `packages/editor-cljs/package.json`
   - Removed test scripts (test:e2e, test:e2e:ui, test:e2e:debug)

2. **Created `deps.edn`** (root level)
   ```clojure
   {:deps
    {org.clojure/clojure {:mvn/version "1.11.1"}
     org.clojure/clojurescript {:mvn/version "1.11.132"}}

    :aliases
    {:e2e
     {:extra-deps {etaoin/etaoin {:mvn/version "1.0.40"}}
      :extra-paths ["e2e_tests"]}}}
   ```

3. **Updated `bb.edn`** with test tasks:
   - `bb test:unit` - Run ClojureScript unit tests
   - `bb test:unit:watch` - Watch unit tests (continuous feedback)
   - `bb test:e2e` - Run E2E tests with Etaoin
   - `bb test` - Run all tests (unit + E2E)

4. **Created Test Harness**
   - `e2e_tests/lexicon/basic_editing_test.clj`
   - Converted P0-01 test from Playwright to Etaoin
   - Helper functions: wait-for-editor-ready, get-editor-text, type-text, press-key

5. **Updated shell.nix**
   - Added Firefox (line 29)
   - Added geckodriver (line 30)

## Current File Structure

```
lexicon/
├── deps.edn                           # NEW - Clojure dependencies with :e2e alias
├── bb.edn                             # UPDATED - New test tasks
├── shell.nix                          # UPDATED - Firefox + geckodriver
├── e2e_tests/                         # NEW - E2E test directory
│   └── lexicon/
│       └── basic_editing_test.clj     # NEW - First Etaoin test (P0-01)
└── packages/editor-cljs/
    ├── package.json                   # UPDATED - Playwright removed
    ├── e2e-tests/                     # OLD - Playwright tests (to be deleted)
    │   └── basic-editing.spec.js      # OLD - 7 tests to be migrated
    └── playwright.config.js           # OLD - To be deleted
```

## Next Steps (Phase 3: E2E Test Migration)

### Immediate Action Required

1. **Start fresh nix-shell**
   ```bash
   exit  # Exit current Claude Code session
   # Restart Claude Code - nix-shell will reload with Firefox
   ```

2. **Start dev server**
   ```bash
   bb dev
   ```
   - Should start on http://localhost:8080
   - Wait for "Build completed" message

3. **Run first Etaoin test**
   ```bash
   bb test:e2e
   ```
   - Firefox will open (not headless - you can watch)
   - Browser navigates to localhost:8080
   - Types sentence: "The quick brown fox jumps over the lazy dog."
   - Verifies text appears
   - Test should PASS

### Remaining Work

1. **Convert remaining 6 Playwright tests** to Etaoin:
   - P0-02: Enter/Return key creates newline
   - P0-03: Backspace deletes character
   - P0-04: Delete key deletes character forward
   - P0-05: Arrow key navigation
   - P0-06: Mouse click positioning
   - REGRESSION: Typing after backspacing entire buffer

2. **Add remaining ManualTestingPlan.md tests** as E2E tests

3. **Delete Playwright files**:
   - `packages/editor-cljs/e2e-tests/`
   - `packages/editor-cljs/playwright.config.js`
   - `packages/editor-cljs/test-results/`
   - `packages/editor-cljs/playwright-report/`

4. **Update shadow-cljs for command-line unit tests** (Week 2)

## Test Commands Reference

```bash
# Unit tests (browser-based, port 8021)
bb test:unit              # Compile and run once
bb test:unit:watch        # Watch mode (auto-run on file changes)

# E2E tests (Etaoin + Firefox)
bb test:e2e               # Run all E2E tests

# All tests
bb test                   # Unit + E2E (+ Rust if exists)
```

## Key Files to Review

1. **Test Harness:** `e2e_tests/lexicon/basic_editing_test.clj`
2. **Babashka Tasks:** `bb.edn` (lines 193-242)
3. **Deps Config:** `deps.edn`
4. **Original Tests:** `packages/editor-cljs/e2e-tests/basic-editing.spec.js`

## Troubleshooting

### Firefox doesn't start
- Check: `firefox --version` and `geckodriver --version`
- If not found, you're not in nix-shell
- Solution: Exit and restart in nix-shell

### Port 8080 in use
- Check: `lsof -i :8080` or `ps aux | grep shadow-cljs`
- Kill zombie Java processes: `pkill -f shadow-cljs`

### Etaoin errors
- First run downloads dependencies (may take time)
- Geckodriver must be in PATH
- Firefox must be installed

## Commits Made

1. **2433cbc** - Fix backspace-at-position-0 bug and add Playwright E2E tests
2. **5f65340** - Add Phase 6.6: Testing Infrastructure Migration to roadmap
3. **2639798** - Update ROADMAP.md current focus to Phase 6.6

## What's NOT Committed Yet

- deps.edn
- bb.edn updates
- e2e_tests/ directory
- package.json changes

**These will be committed after verifying the first Etaoin test passes.**

---

## For Next Claude Session

**Context:** We're migrating E2E tests from Playwright (JavaScript) to Etaoin (ClojureScript) to maintain language consistency. Setup is complete. First test is ready to run.

**First Task:** Verify `bb test:e2e` runs successfully with Firefox.

**Expected Behavior:** Firefox opens, navigates to localhost:8080, types text, verifies it appears, test passes.

**Success Criteria:** One passing Etaoin test that matches Playwright baseline behavior.
