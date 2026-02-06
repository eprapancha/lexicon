# Lexicon Development Roadmap

**Last Updated:** 2026-01-28
**Current Phase:** Phase 7 - E2E Testing & Quality Assurance

---

## Overview

This roadmap tracks Lexicon's evolution from a bare Emacs implementation to a full package ecosystem with Evil-mode, completion frameworks, and LSP integration.

**Strategy:** Clean refactor to establish solid foundation aligned with Emacs principles.
**Source of Truth:** [GitHub Issues](https://github.com/eprapancha/lexicon/issues) - see issue #94 for TDD roadmap

---

## Completed Phases

| Phase | Description | Status |
|-------|-------------|--------|
| **0** | Architecture Reset - Gap buffer, Evil-mode extraction | ✅ Dec 2025 |
| **1** | Basic Editing - Navigation, kill ring, undo | ✅ Dec 2025 |
| **2** | Buffers & Files - Multi-buffer, file I/O | ✅ Dec 2025 |
| **2.5** | Core Polish - Focus handling, echo area, prefix args | ✅ Dec 2025 |
| **3** | Windows & Frames - Splitting, navigation | ✅ Dec 2025 |
| **4** | Minibuffer & Completion - M-x, TAB completion, help system | ✅ Dec 2025 |
| **5** | Modes & Keymaps - Major/minor modes, hierarchical keymaps | ✅ Dec 2025 |
| **6A** | Package System - Package loading, namespacing | ✅ Dec 2025 |
| **6B** | Display & Theming - Faces, mode-line, themes | ✅ Dec 2025 |
| **6C** | Completion Framework - Metadata, annotations | ✅ Dec 2025 |
| **6D** | Missing Infrastructure - Various Emacs primitives | ✅ Dec 2025 |
| **6.5** | Initial Testing - Browser-based tests, manual test plan | ✅ Dec 2025 |
| **7.1** | Core API Design - Documentation for buffer/command/keymap APIs | ✅ Jan 2026 |
| **7.2** | events.cljs Refactor - Split 3000-line monolith into 8 modules | ✅ Jan 2026 |
| **7.3-7.5** | Hook System, Undo Architecture, Markers | ✅ Jan 2026 |
| **7.6-7.7** | Dynamic Context, Package Isolation | ✅ Jan 2026 |
| **7.8** | Query Replace, Minibuffer Redesign | ✅ Jan 2026 |

---

## Current Phase: E2E Testing & Quality Assurance

**Goal:** Comprehensive E2E test coverage using Etaoin/Firefox
**Tracking:** [Issue #94](https://github.com/eprapancha/lexicon/issues/94) - TDD Roadmap

### Test Structure

```
e2e_tests/lexicon/
├── ui/           # Keyboard simulation tests (user actions)
│   ├── editing/  # Core editing, undo, kill-ring
│   ├── search/   # Isearch, query-replace
│   ├── buffers/  # Buffer management
│   ├── windows/  # Window splitting, navigation
│   └── ...
└── lisp/         # Lisp API tests (evalLisp)
    ├── buffer_test.clj
    ├── editing_test.clj
    └── ...
```

### Active Work

- **UI Tests:** Keyboard simulation for all core functionality
- **Lisp Tests:** Direct API testing for SCI integration
- **Lint Rules:** Enforce proper test patterns (no evalLisp in UI tests)

### Running Tests

```bash
bb test:e2e                           # All tests
bb test:e2e "ui.editing.undo-test"    # Specific namespace
bb lint                               # Check test patterns
```

---

## Future Phases

### Phase 8: Completion Ecosystem (Vertico, Orderless, Consult)

**Goal:** Prove Elisp compatibility with high-value completion packages

- Package persistence (`~/.lexicon/packages.edn`)
- Startup package loading
- Orderless - Flexible matching
- Vertico - Vertical completion UI
- Consult - Enhanced commands

### Phase 9: Evil-mode Integration

**Goal:** Full Vim emulation as external package

- Modal editing (normal, insert, visual, operator-pending)
- Vim operators and motions
- Ex command line
- Integration with completion ecosystem

### Phase 10-13: Package Ecosystem

- Public package index
- Package developer experience
- Package observability & hygiene
- Ecosystem governance

### Phase 14: Syntax Highlighting (Tree-sitter)

**Goal:** Modern syntax highlighting with tree-sitter

- Tree-sitter WASM integration
- Language grammars
- Font-lock integration

### Phase 15: LSP Integration

**Goal:** Language Server Protocol support

- Bridge server WebSocket communication
- LSP client implementation
- Diagnostics, completion, navigation

---

## Development Workflow

1. **GitHub Issues First** - Check issues before starting work
2. **Test First** - Write failing tests before implementing
3. **Atomic Commits** - Commit after each green test
4. **Zero Warnings** - Fix all warnings before commit
5. **Emacs Fidelity** - Study Emacs source before implementing

---

## Key Resources

- **[DEVELOPMENT.md](./DEVELOPMENT.md)** - Build and test instructions
- **[ARCHITECTURE_BOUNDARY.md](./ARCHITECTURE_BOUNDARY.md)** - Core/package boundaries
- **[ARCHITECTURE.md](./ARCHITECTURE.md)** - Technical architecture
- **[GitHub Issues](https://github.com/eprapancha/lexicon/issues)** - Current work tracking

---

## Historical Documentation

Detailed implementation notes for completed phases are archived in `docs/archive/`.
