# Lexicon Development Guide

**Last Updated:** 2026-01-13

---

## Setup

### Prerequisites

- Nix (for reproducible environment)
- Firefox (for E2E tests)

### First Time Setup

```bash
# Clone repository
git clone https://github.com/eprapancha/lexicon.git
cd lexicon

# Enter nix shell (installs all dependencies)
nix-shell

# Build WASM
cd packages/lexicon-engine/wasm
wasm-pack build --target web

# Install npm dependencies
cd ../../../packages/editor-cljs
npm install

# Start development server
cd ../..
bb dev
```

Visit http://localhost:8080

---

## Daily Workflow

### Development

```bash
# Start dev server (hot reload enabled)
bb dev

# Make changes, save files
# Browser auto-reloads
```

### Before Commit

```bash
# Run tests
cargo test              # Rust tests
bb test:e2e             # E2E tests (requires app at :8080)

# Ensure clean build
npx shadow-cljs compile app  # Check for warnings
```

### Commit

```bash
git add -A
git commit -m "feat(scope): concise description

- Detailed bullet points
- Reference issue if applicable

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Testing

### Test Structure

**Rust Unit Tests:**
- Location: `packages/lexicon-engine/core/src/*.rs`
- Run: `cargo test`
- 44 tests covering gap buffer operations

**E2E Tests:**
- Location: `e2e_tests/lexicon/*.clj`
- Run: `bb test:e2e`
- Requires Firefox and app running at :8080

### Running Tests

```bash
# All tests
bb test

# Just Rust
cargo test

# Just E2E
bb test:e2e

# Watch mode (future)
bb test:unit:watch
```

### Writing Tests

**E2E Test Example:**
```clojure
(deftest test-basic-typing
  (testing "Can type text"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)
    (type-text "Hello World")
    (is (.contains (get-editor-text) "Hello World"))))
```

**Test Requirements:**
- ✅ Add tests for new features
- ✅ Add regression tests for bugs
- ✅ All tests must pass before commit

---

## Critical Rules

### NEVER Do

- ❌ Run compilation/dev servers yourself (user has watch running)
- ❌ Commit code that doesn't compile cleanly
- ❌ Ignore compilation warnings (zero tolerance)
- ❌ Violate state ownership (dispatch to owners)
- ❌ Create GitHub labels without documenting first
- ❌ Run tests in background where output is hidden

### ALWAYS Do

- ✅ Check GitHub issues FIRST before starting work
- ✅ Study Emacs source before implementing features
- ✅ Dispatch events to state owners (never mutate directly)
- ✅ Commit after each green test run (atomic commits)
- ✅ Reference issue numbers in commits
- ✅ Fix ALL warnings before commit

---

## Debugging

### Git for Regressions

```bash
# View recent changes
git diff HEAD~5

# Find breaking commit
git bisect start
git bisect bad                    # Current is broken
git bisect good <known-good>      # Last working
# Test each checkout
git bisect good/bad
git bisect reset

# File history
git log -p -- path/to/file.cljs
git blame path/to/file.cljs
```

### Browser DevTools

- Console for ClojureScript errors
- re-frame-10x for state inspection (if enabled)
- Network tab for WASM loading issues

### Rust Debugging

```bash
# Run with output
cargo test -- --nocapture

# Specific test
cargo test test_marker_creation -- --nocapture
```

---

## Code Standards

### File Size

- Max ~500 lines per namespace
- Split when exceeded
- Keep functions focused

### Naming

**Namespaces:**
```clojure
lexicon.events.buffer    ; Good: domain-specific
lexicon.helpers          ; Bad: too vague
```

**Functions:**
```clojure
(defn insert-at-point [text] ...)  ; Good: descriptive
(defn do-thing [x] ...)            ; Bad: vague
```

### Documentation

```clojure
(defn complex-function
  "Brief description.
  
  Args:
    arg1 - Description
    arg2 - Description
  
  Returns:
    Description"
  [arg1 arg2]
  ...)
```

### Code Style

```clojure
;; ✅ Use threading macros
(-> db
    (assoc :foo bar)
    (update :baz inc))

;; ✅ Destructure
(defn handler [{:keys [db]} [_ arg1 arg2]]
  ...)

;; ✅ Meaningful names
(let [active-buffer-id (:active-buffer-id db)]
  ...)

;; ❌ Deep nesting
(if x
  (if y
    (if z
      ...)))
```

---

## Build System

### Babashka Tasks

```bash
bb dev              # Start dev server
bb build            # Production build
bb test             # All tests
bb clean            # Clean artifacts
bb ci-test          # Simulate CI
```

### Build Files

- `bb.edn` - Babashka task definitions
- `packages/editor-cljs/shadow-cljs.edn` - ClojureScript build
- `packages/lexicon-engine/wasm/Cargo.toml` - Rust dependencies

---

## Troubleshooting

### WASM fails to compile

```bash
cd packages/lexicon-engine/wasm
cargo clean
cargo build
wasm-pack build --target web
```

### ClojureScript fails to compile

```bash
cd packages/editor-cljs
rm -rf .shadow-cljs resources/public/js
npx shadow-cljs compile app
```

### E2E tests fail

1. Ensure app running at http://localhost:8080
2. Ensure Firefox installed
3. Check test logs for specific failures

### Rust tests fail

```bash
cd packages/lexicon-engine/wasm
cargo test -- --nocapture
# Look for assertion failures
```

---

## GitHub Workflow

### Labels

**Required (every issue):**
- ONE priority: `priority-critical/high/medium/low`
- ONE type: `bug`, `enhancement`, `refactor`, `test`, `documentation`

**Optional:**
- Phase: `phase-6.5`, `phase-7`, etc.
- Component: `component-minibuffer`, `component-commands`, etc.
- Status: `status-blocked`, `status-in-progress`

**Label Creation:**
1. Update `.CLAUDE.md` FIRST
2. Get maintainer approval
3. Create in GitHub
4. Document why needed

### Issue Workflow

```bash
# List open issues
gh issue list

# View specific issue
gh issue view 76

# Create issue
gh issue create --title "..." --body "..."

# Comment on issue
gh issue comment 76 --body "..."

# Close issue
gh issue close 76 --comment "Completed in #PR"
```

---

## Performance

### Profiling

```javascript
// Browser console
console.time("operation");
// ... code to measure
console.timeEnd("operation");
```

### Optimization

- Profile BEFORE optimizing
- Gap buffer in Rust for speed
- Minimize re-renders (React.memo)
- Lazy load packages
- Cache expensive computations

---

## Quick Reference

### Key Commands

```bash
bb dev                   # Dev server
bb test                  # All tests
cargo test               # Rust tests
bb test:e2e              # E2E tests
git diff HEAD~3          # Recent changes
git bisect               # Find breaking commit
```

### Important Paths

```
packages/editor-cljs/src/lexicon/     # ClojureScript
packages/lexicon-engine/wasm/         # Rust WASM
e2e_tests/                            # E2E tests
docs/                                 # Documentation
```

### Emacs Source Reference

```
/tmp/emacs-source/lisp/simple.el      # universal-argument
/tmp/emacs-source/src/callint.c       # Interactive specs
/tmp/emacs-source/lisp/minibuffer.el  # Completion
/tmp/emacs-source/src/buffer.c        # Buffer-local vars
```

---

## Remember

1. ✅ **GitHub first** - Check issues before starting
2. ✅ **Emacs fidelity** - Study source code first
3. ✅ **State ownership** - Dispatch, never mutate
4. ✅ **Test first** - Write tests before implementing
5. ✅ **Commit frequently** - After each green test
6. ✅ **Zero warnings** - Fix all before commit
7. ✅ **Zero regressions** - All tests must pass

---

**For architecture details, see ARCHITECTURE.md. For project plan, see ROADMAP.md.**
