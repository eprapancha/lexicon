# Contributing to Lexicon

Thank you for your interest in contributing to Lexicon! This document provides guidelines and information for contributors.

## Project Overview

Lexicon is a faithful recreation of GNU Emacs in the browser, built with:
- **Rust/WASM**: Gap buffer text engine (`lexicon-engine`)
- **ClojureScript**: Emacs Lisp compatibility layer and UI
- **Re-frame**: Reactive state management

**Goal**: 100% Emacs keybinding compatibility and package ecosystem support.

## Getting Started

### Prerequisites

- Nix (for development environment)
- Node.js 18+ (provided via nix-shell)
- Rust/Cargo (provided via nix-shell)

### Setup

```bash
# Enter development environment
nix-shell

# Install dependencies
npm install

# Build WASM engine
cd packages/lexicon-engine
wasm-pack build --target web
cd ../..

# Start development server
npm run dev
```

Visit http://localhost:8020 to see Lexicon running.

## Development Workflow

### Running Tests

```bash
# Run all E2E tests
bb test:e2e

# Run specific test
bb test:e2e -- -i "test name"

# Watch mode (re-run on changes)
bb test:e2e:watch
```

### Making Changes

1. **Check existing issues** - Is there already an issue for this?
2. **Create/update issue** - Describe what you're working on
3. **Create branch** - `git checkout -b fix/issue-123` or `feat/new-feature`
4. **Write tests first** - Add failing test for bug/feature
5. **Implement fix** - Make tests pass
6. **Verify no regressions** - Run full test suite (`bb test:e2e`)
7. **Commit** - Use conventional commits (see below)
8. **Push and create PR** - Link to related issue

### Commit Message Format

We use [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `refactor`: Code refactoring
- `test`: Test improvements
- `docs`: Documentation changes
- `chore`: Build/tooling changes

**Examples:**
```
feat(minibuffer): implement isearch minibuffer integration

fix(buffer): correct cursor position after newline insertion

refactor(events): split events.cljs into focused modules

test(e2e): add test for query-replace-regexp

docs: update ROADMAP with Phase 7.8 status
```

### Code Style

**ClojureScript:**
- Follow [Clojure Style Guide](https://guide.clojure.style/)
- Use `cljfmt` for formatting
- Prefer pure functions over stateful code
- Document complex re-frame event handlers

**Rust:**
- Run `cargo fmt` before committing
- Run `cargo clippy` and fix warnings
- Keep gap buffer API minimal and well-tested

## Project Structure

```
lexicon/
├── packages/
│   ├── editor-cljs/          # ClojureScript UI and Emacs logic
│   │   ├── src/lexicon/
│   │   │   ├── events/       # Re-frame event handlers (by domain)
│   │   │   ├── subs.cljs     # Re-frame subscriptions
│   │   │   ├── views.cljs    # Reagent UI components
│   │   │   └── api/          # Emacs-compatible APIs
│   │   └── test/             # E2E tests
│   └── lexicon-engine/       # Rust gap buffer WASM
├── docs/                     # Documentation
│   ├── ROADMAP.md           # Development roadmap
│   └── SESSION_HANDOFF.md   # Current status
└── bb.edn                   # Babashka task definitions
```

## Testing Strategy

### E2E Tests (Primary)

Located in `packages/editor-cljs/test/e2e_tests/`

**Write E2E tests for:**
- User-facing workflows (typing, navigation, commands)
- Regression prevention (bugs that were fixed)
- Cross-cutting features (window management, buffer switching)

**Example:**
```clojure
(deftest test-isearch-forward
  (with-clean-editor driver
    (wait-visible driver {:css ".editor"})
    (type-keys driver "hello world")
    (send-keys driver "C-s")  ; Start isearch
    (wait-visible driver {:css ".minibuffer-input"})
    (type-keys driver "world")
    (is (= "world" (get-minibuffer-input driver)))))
```

### Test-Driven Development

1. Write failing test that demonstrates the bug/feature
2. Run test to verify it fails: `bb test:e2e -- -i "test name"`
3. Implement fix
4. Run test to verify it passes
5. Run full suite to check for regressions: `bb test:e2e`
6. Commit when all tests green

## Pull Request Guidelines

### Before Submitting

- [ ] All E2E tests pass (`bb test:e2e`)
- [ ] Added tests for new functionality
- [ ] Tested manually in browser
- [ ] No new warnings in console
- [ ] Code follows style guidelines
- [ ] Commit messages follow conventional format

### PR Description

Use the PR template (auto-populated) to provide:
- Clear description of changes
- Link to related issues (`Fixes #123`)
- Test results (paste output)
- Screenshots for UI changes
- Breaking changes noted

### Review Process

1. Automated tests run on PR
2. Maintainer reviews code and tests
3. Address feedback via new commits
4. Maintainer merges when approved

## Finding Work

### Good First Issues

Look for issues labeled `good-first-issue` - these are beginner-friendly tasks with clear scope.

### Current Priorities

Check the [GitHub Milestones](https://github.com/eprapancha/lexicon/milestones) to see current phase priorities:
- **Phase 7.8**: Emacs API compatibility (high priority)
- **Phase 8**: Completion ecosystem
- **Phase 9**: Evil-mode integration

### Issue Labels

- `bug`: Something isn't working
- `feature`: New functionality
- `refactor`: Code improvement
- `test`: Test coverage
- `phase-X`: Related to specific roadmap phase
- `component-*`: Affects specific subsystem
- `priority-critical/high/medium/low`: Urgency level

## Architecture Philosophy

### Core Principles

1. **Emacs Parity First** - Behavior should match GNU Emacs
2. **Test Coverage** - All features must have E2E tests
3. **Incremental Progress** - Small, tested commits over big rewrites
4. **Clean Abstractions** - Separate concerns (buffer, window, command, UI)
5. **Performance Matters** - Gap buffer for O(1) insertion, reactive subscriptions

### Key Abstractions

- **Buffer**: Text storage (WASM gap buffer) + metadata (mode, markers)
- **Window**: View into buffer (cursor, scroll position)
- **Command**: User-invocable function (registered, introspectable)
- **Keymap**: Key sequence → command binding (hierarchical, mode-specific)
- **Mode**: Buffer behavior (major mode + minor modes)
- **Event**: Re-frame event (pure state transition)

## Getting Help

- **Questions**: Open a [Discussion](https://github.com/eprapancha/lexicon/discussions)
- **Bugs**: Create an [Issue](https://github.com/eprapancha/lexicon/issues/new?template=bug_report.yml)
- **Features**: Create an [Issue](https://github.com/eprapancha/lexicon/issues/new?template=feature_request.yml)
- **Documentation**: Check [docs/](https://github.com/eprapancha/lexicon/tree/main/docs)

## License

By contributing to Lexicon, you agree that your contributions will be licensed under the same license as the project (check LICENSE file).

---

Thank you for contributing to Lexicon! 🎉
