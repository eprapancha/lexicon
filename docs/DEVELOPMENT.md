# Lexicon Development Guide

## Quick Start

```bash
# Clone and enter nix shell
git clone https://github.com/anthropics/lexicon.git
cd lexicon
nix-shell

# Build WASM
cd packages/lexicon-engine/wasm && wasm-pack build --target web && cd ../../..

# Install deps and start dev server
cd packages/editor-cljs && npm install && cd ../..
bb dev
```

Visit http://localhost:8080

---

## Build System

### Babashka Tasks

| Command | Description |
|---------|-------------|
| `bb dev` | Start dev server (hot reload) |
| `bb build` | Full production build |
| `bb test` | Run all tests |
| `bb test:e2e` | E2E tests only |
| `bb test:e2e "namespace"` | Run specific test namespace |
| `bb clean` | Clean build artifacts |
| `bb lint` | Run all linters |
| `bb ci-test` | Simulate CI locally |

### Prerequisites

- Node.js >= 16
- Java >= 17
- Babashka >= 1.3.0
- Rust + wasm-pack
- Firefox (for E2E tests)

**NixOS users:** `nix-shell` provides everything.

**Others:** Install tools via your package manager.

---

## Testing

### Test Structure

```
e2e_tests/lexicon/
├── ui/           # Keyboard/UI tests (simulate user actions)
│   ├── editing/  # Core editing (undo, kill-ring, etc.)
│   ├── search/   # Search & replace
│   ├── buffers/  # Buffer management
│   └── ...
├── lisp/         # Lisp API tests (use evalLisp)
│   ├── buffer_test.clj
│   ├── editing_test.clj
│   └── ...
└── test_helpers.clj
```

### Running Tests

```bash
# All tests
bb test:e2e

# Specific namespace
bb test:e2e "ui.editing.undo-test"
bb test:e2e "lisp.buffer-test"

# Rust tests
cargo test
```

### Writing Tests

**UI tests** simulate keyboard actions:
```clojure
(deftest test-undo
  (testing "C-/ undoes insert"
    (h/setup-test*)
    (h/type-text "hello")
    (h/press-ctrl "/")
    (is (= "" (h/get-buffer-text*)))))
```

**Lisp tests** test API directly:
```clojure
(deftest test-save-excursion
  (testing "Point restored"
    (lisp/setup-test)
    (lisp/eval-lisp! "(goto-char 5)")
    (lisp/eval-lisp! "(save-excursion (goto-char 0))")
    (is (= 5 (lisp/eval-lisp! "(point)")))))
```

---

## Code Standards

### File Organization
- Max ~500 lines per namespace
- Use descriptive names: `lexicon.events.buffer` not `lexicon.helpers`

### Commit Messages
```
feat(scope): concise description

- Details if needed
- Reference issues: Fixes #123

Co-Authored-By: Claude <noreply@anthropic.com>
```

### Before Committing
1. All tests pass: `bb test:e2e`
2. No warnings: `npx shadow-cljs compile app`
3. Lint passes: `bb lint`

---

## Deployment

### GitHub Pages (Automatic)
Push to `main` → GitHub Actions builds and deploys.

### Manual Build
```bash
bb build
# Output in packages/editor-cljs/resources/public/
```

### Hosting Requirements
- MIME type: `.wasm` → `application/wasm`
- HTTPS required for WASM/Workers

---

## Troubleshooting

### WASM fails to compile
```bash
cd packages/lexicon-engine/wasm
cargo clean && wasm-pack build --target web
```

### ClojureScript fails to compile
```bash
cd packages/editor-cljs
rm -rf .shadow-cljs resources/public/js
npx shadow-cljs compile app
```

### E2E tests fail
1. App running at http://localhost:8080?
2. Firefox installed?
3. Check specific error in test output

---

## Key Paths

```
packages/editor-cljs/src/lexicon/  # ClojureScript source
packages/lexicon-engine/wasm/      # Rust WASM
e2e_tests/                         # E2E tests
docs/                              # Documentation
```

---

## CI Testing

### Quick Local Test
```bash
bb ci-test  # ~2-5 min, uses local env
```

### Full GitHub Simulation
```bash
bb install-act  # One-time setup
bb act-ci       # Runs actual workflow in Docker
```

**Workflow:** `bb ci-test` for iteration → `bb act-ci` before push.

---

## Rules

1. **GitHub first** - Check issues before starting work
2. **Emacs fidelity** - Study Emacs source before implementing
3. **Test first** - Write tests before implementing
4. **Zero warnings** - Fix all before commit
5. **Atomic commits** - Commit after each green test

See also: [ARCHITECTURE.md](./ARCHITECTURE.md), [CONTRIBUTING.md](../CONTRIBUTING.md)
