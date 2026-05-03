---
name: qa-check
description: Run tests, check lint, and validate quality. Use after implementation is complete to verify everything works before committing.
argument-hint: [scope e.g. "orderless", "completion", "full", or a test pattern]
allowed-tools: Read, Glob, Grep, Bash
---

# QA Check

Run verification steps and produce a pass/fail report. This is the last gate before work is declared done.

---

## Process

1. **Determine scope** from the argument
2. **Run lint** (`bb lint`)
3. **Run targeted tests** (`bb test:e2e <pattern>`)
4. **Run regression tests** for related subsystems
5. **Check for structural issues** in modified files
6. **Produce report**

---

## Verification Steps (in order)

### 1. Lint Check

```bash
bb lint
```

- Architecture boundary: hard fail
- E2E lint (no eval-lisp in UI tests): hard fail
- New clj-kondo warnings in modified files: hard fail
- Existing warnings in unmodified files: tolerated

### 2. Dev Server Check

```bash
curl -s -o /dev/null -w "%{http_code}" http://localhost:8080/index.html
```

If not running (returns 000), report it -- don't start the server yourself.

### 3. Targeted Tests

```bash
bb test:e2e <pattern> 2>&1 | tee /tmp/e2e-qa.log
```

### 4. Regression Tests

Run tests for related subsystems. Common groupings:

| Changed Area | Also Run |
|-------------|----------|
| `completion/styles.cljs` | `lisp.completion-test`, `lisp.vertico-core-test` |
| `lisp.cljs` | `lisp.completion-test`, any package tests |
| `events/edit.cljs` | `ui.editing.*` |
| `events/buffer.cljs` | `ui.buffers.*` |
| `events/ui.cljs` | `ui.minibuffer.*`, `ui.windows.*` |
| `lexpkgs/vertico/` | `lisp.vertico-core-test` |
| `lexpkgs/marginalia/` | `lisp.marginalia-core-test` |
| `lexpkgs/orderless/` | `lisp.orderless-test` |

### 5. Cross-Cutting Checks

```bash
# Check which files were modified
git diff --name-only

# Verify no core imports in package files
grep -rn "lexicon.core" lexpkgs/ || echo "OK: no core imports"

# Verify new API functions are in sci-namespace
# (manual check based on what was added to lisp.cljs)
```

---

## Output Format

```
## QA Report

### Scope: [what was checked]

### Lint
- Status: PASS / FAIL
- Architecture boundary: PASS / FAIL
- New warnings: [list or "none"]

### Tests: [pattern]
- Status: PASS / FAIL
- Passed: N
- Failed: N
- Failures:
  1. test-name: expected [X], got [Y]

### Regression: [pattern]
- Status: PASS / FAIL
- Passed: N
- Failed: N

### Cross-Cutting
- Core imports in packages: PASS / FAIL
- SCI registrations: PASS / FAIL

### Verdict: PASS / FAIL
[One sentence summary]
```

---

## Rules

- Run every check -- don't assume anything passes
- Capture test output to `/tmp/e2e-qa.log`
- Report failures precisely: test name, assertion, expected vs actual
- Verdict is PASS or FAIL, no "mostly passes"
- Do NOT fix problems -- only identify them
- Do NOT run the full test suite unless specifically asked (`full` scope)
