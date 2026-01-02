# Phase 7.2 Status - Events.cljs Refactoring

**Date:** 2026-01-02
**Status:** Planning Complete, Ready to Execute
**Time Estimate:** 8-12 hours of focused work

## What's Been Completed

### ✅ Phase 7.1 - Core Design Documents (DONE)
- Created 6 comprehensive specifications (146KB total)
- All committed to main branch
- Commits: `2f034b7`, `7a25719`

### ✅ Phase 7.2 Planning (DONE)
- **Analyzed events.cljs**: 3154 lines, 114 event handlers
- **Created refactoring plan**: 8 modules defined
- **Identified dependencies**: Extraction order determined
- **Documented process**: Step-by-step extraction guide

## Current Situation

### Files Created
1. `docs/refactoring/EVENTS_REFACTORING_PLAN.md` - Comprehensive plan (300+ lines)
2. `docs/refactoring/PHASE_7.2_STATUS.md` - This file

### Event Handler Analysis

**Total**: 114 event handlers across 8 planned modules:

1. **wasm-events.cljs** (19 handlers) - WASM, transactions, parser
2. **keymap-events.cljs** (3 handlers) - Key sequences
3. **mode-events.cljs** (9 handlers) - Modes and hooks
4. **buffer-events.cljs** (16 handlers) - Buffers and file I/O
5. **edit-events.cljs** (28 handlers) - Text editing, cursor, undo
6. **command-events.cljs** (18 handlers) - Command execution
7. **ui-events.cljs** (20 handlers) - UI, windows, minibuffer
8. **core-events.cljs** (11 handlers) - Init and coordinator

### Key Insight from Analysis

The existing E2E tests (7 tests, 10 assertions) serve as our **regression protection**. We don't need extensive new unit tests for each module during refactoring - we just need the E2E tests to stay green.

---

## Recommended Approach

### Strategy: Incremental with E2E Safety Net

1. **Extract one module at a time**
2. **Run E2E tests after each extraction**: `bb test:e2e`
3. **Commit immediately when tests pass**
4. **If tests fail**: `git reset --hard HEAD~1` and debug
5. **No new unit tests during refactoring** (pure code movement)

### Safest Extraction Order

Based on coupling analysis:

1. **Start simple**: Extract pure helper functions to `utils.cljs` first
2. **Then isolated handlers**: `keymap-events.cljs` (only 3 handlers)
3. **Build up complexity**: `mode-events.cljs`, `wasm-events.cljs`
4. **Finish with coupled modules**: `command-events.cljs`, `ui-events.cljs`
5. **Last**: `core-events.cljs` (coordinator)

---

## Next Steps - Execution Plan

### Option A: Continue with Full Refactoring (8-12 hours)

Execute the full plan:

```bash
# 1. Extract utils.cljs (helper functions) - 1 hour
# 2. Extract keymap-events.cljs - 1 hour
# 3. Extract mode-events.cljs - 1.5 hours
# 4. Extract wasm-events.cljs - 2 hours
# 5. Extract buffer-events.cljs - 1.5 hours
# 6. Extract edit-events.cljs - 2 hours
# 7. Extract command-events.cljs - 1.5 hours
# 8. Extract ui-events.cljs - 2 hours
# 9. Create core-events.cljs coordinator - 0.5 hours
```

**Expected outcome**:
- 8-9 new files
- events.cljs reduced to ~50 lines (just requires)
- 8-9 commits
- All E2E tests passing

### Option B: Start with One Module (Proof of Concept) - 1 hour

Extract just `keymap-events.cljs` to validate the approach:

1. Create `src/lexicon/events/keymap.cljs`
2. Move 3 event handlers:
   - `:handle-key-sequence`
   - `:set-prefix-key-state`
   - `:clear-prefix-key-state`
3. Update `events.cljs` to require it
4. Run `bb test:e2e`
5. Commit if green

**If successful**: Continue with remaining 7 modules
**If issues**: Refine approach before continuing

### Option C: Defer to Week 2 Focus Time

Save this for a dedicated focus session:
- Block 8-12 hours
- No interruptions
- Execute full plan start-to-finish
- Commit to ROADMAP.md

---

## Commands Reference

```bash
# Run E2E tests (regression check)
bb test:e2e

# Run unit tests (if needed)
bb test:unit

# Check current state
git status
git diff

# Rollback if needed
git reset --hard HEAD~1

# Commit after green tests
git add -A
git commit -m "refactor(events): extract <module> module"
```

---

## Risk Assessment

### Low Risk
- ✅ E2E tests provide good coverage (7 tests, 10 assertions)
- ✅ Plan is detailed and thought-through
- ✅ Incremental approach allows rollback at any step

### Medium Risk
- ⚠️ High coupling between modules (may require iteration)
- ⚠️ Large file size (easy to miss dependencies)
- ⚠️ Re-frame auto-registration patterns must be preserved

### Mitigation
- Test after EVERY extraction (not at the end)
- Use git commits as savepoints
- Don't hesitate to rollback and retry

---

## Files Modified (When Executed)

### New Files Created (8-9)
```
src/lexicon/events/
├── utils.cljs           (helper functions)
├── keymap.cljs          (3 handlers)
├── modes.cljs           (9 handlers)
├── wasm.cljs            (19 handlers)
├── buffer.cljs          (16 handlers)
├── edit.cljs            (28 handlers)
├── command.cljs         (18 handlers)
├── ui.cljs              (20 handlers)
└── core.cljs            (11 handlers + coordinator)
```

### Modified Files (1)
```
src/lexicon/events.cljs  (reduced from 3154 lines to ~50 lines)
```

---

## Success Criteria

- [ ] 8-9 new event modules created
- [ ] events.cljs reduced to ~50 lines
- [ ] All 114 event handlers still registered
- [ ] All E2E tests passing (7 tests, 10 assertions, 0 failures)
- [ ] 8-9 commits (1 per extraction)
- [ ] No behavior changes (pure refactoring)
- [ ] Updated ROADMAP.md to reflect Phase 7.2 completion

---

## Recommendation

**Start with Option B** (1-hour proof of concept):
- Extract just `keymap-events.cljs`
- Validate the approach works
- Gain confidence in the process
- Then decide whether to continue immediately or schedule dedicated time

This de-risks the full refactoring and provides a clear "go/no-go" decision point.

---

## Current Working State

```bash
# Branch: main
# Recent commits:
7a25719 docs(core): add Phase 7.1 core design specifications
2f034b7 docs(core): add Core API contract (Phase 7.1)
fa87faf docs: archive research docs and add comprehensive Phase 7-9 roadmap

# Untracked files:
docs/refactoring/EVENTS_REFACTORING_PLAN.md
docs/refactoring/PHASE_7.2_STATUS.md

# Next action:
git add docs/refactoring/*.md
git commit -m "docs(refactoring): add Phase 7.2 events.cljs refactoring plan"
```

---

**Status**: ✅ Ready to execute
**Blocking**: None
**Owner**: Ready for developer review and decision
