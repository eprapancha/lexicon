# Lexicon Development Roadmap

**Last Updated:** 2025-12-21
**Current Phase:** Phase 0 Complete! ✅ Ready for Phase 1

---

## Overview

This roadmap tracks Lexicon's evolution from the current state (architecture misalignment, broken basic input) to a working bare Emacs implementation, followed by package ecosystem development.

**Strategy:** Clean refactor to establish solid foundation aligned with Emacs principles. See [CORE_PRINCIPLES.md](./CORE_PRINCIPLES.md) for architectural guidelines.

---

## Phase 0: Architecture Reset & Foundation

**Status:** ✅ COMPLETE (Dec 21, 2025)
**Goal:** Separate core from packages, fix basic text input, establish clean architecture
**Timeline:** 2 days (Dec 20-21, 2025)

### Objectives

1. **Extract Evil-mode from Core** ✅ COMPLETE (Dec 20, 2025)
   - ✅ Move `fsm/` directory → `packages/evil-mode/src/lexicon/evil/fsm/`
   - ✅ Move `macros.clj` → `packages/evil-mode/src/lexicon/evil/macros.clj`
   - ✅ Move `command/` dispatcher → `packages/evil-mode/src/lexicon/evil/command/`
   - ✅ Move `keymaps/` → `packages/evil-mode/src/lexicon/evil/keymaps/`
   - ✅ Remove FSM state from core `events.cljs`
   - ✅ Core compiles cleanly with 0 errors
   - ✅ Created `evil-mode/README.md` documenting future integration

2. **Replace Piece Tree with Gap Buffer** ✅ COMPLETE (Dec 20, 2025)
   - ✅ Implemented gap buffer in Rust (350 lines, clean and simple)
   - ✅ WASM bindings created (`gap_buffer_wasm.rs`)
   - ✅ 15 comprehensive unit tests (all passing)
   - ✅ UTF-8 correctness validated (emoji support)
   - ✅ Fixed nix-shell TMPDIR issue for testing

3. **Fix Core Text Input** ✅ COMPLETE (Dec 21, 2025)
   - ✅ Integrated gap buffer into ClojureScript
   - ✅ Basic typing works (characters appear on screen)
   - ✅ Enter key inserts newline and advances cursor
   - ✅ Backspace deletes character before cursor
   - ✅ Delete key deletes character at cursor
   - ✅ Arrow keys move cursor (with C-f/C-b/C-n/C-p bindings)
   - ✅ Mouse click positions cursor
   - ✅ Multi-line editing works correctly
   - ✅ Fixed cursor position calculation for newlines
   - ✅ Fixed line rendering to show empty lines

4. **Restructure Directories**
   ```
   packages/
   ├── lexicon-core/
   │   ├── wasm/              # Rust gap buffer + bindings
   │   └── cljs/              # ClojureScript core
   │       └── src/lexicon/core/
   │           ├── buffer.cljs
   │           ├── command.cljs
   │           ├── keymap.cljs
   │           ├── mode.cljs
   │           └── ...
   └── evil-mode/             # First package
       └── src/lexicon/evil/
   ```

5. **Update Documentation** ✅ COMPLETE (Dec 20, 2025)
   - ✅ Created CORE_PRINCIPLES.md - Architectural foundation
   - ✅ Created ROADMAP.md - This document
   - ✅ Updated README.md - Reflects reality (broken features marked)
   - ✅ Updated architecture.md - Gap buffer design documented

### Success Criteria

- [x] **Can type text and see it on screen** ✅ Dec 21
- [x] **Enter key creates new line** ✅ Dec 21
- [x] **Backspace deletes characters** ✅ Dec 21
- [x] **Arrow keys move cursor** ✅ Dec 21
- [x] **Delete key works** ✅ Dec 21
- [x] **Mouse click positions cursor** ✅ Dec 21
- [x] **Core compiles without Evil-mode** ✅ Dec 20
- [x] **Evil-mode code completely moved to `packages/evil-mode/`** ✅ Dec 20
- [x] **Gap buffer passes all unit tests** ✅ Dec 20 (15/15 tests)
- [x] **README accurately describes current state** ✅ Dec 20

**Progress:** 10/10 complete (100%) ✅ PHASE 0 COMPLETE!

### Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Hidden Evil-mode dependencies in core | High | Grep entire codebase for `fsm`, `evil`, `operator-pending` |
| Gap buffer bugs with UTF-8 | High | Comprehensive unit tests with emoji, multi-byte chars |
| Transaction queue adds complexity | Medium | Simplify to direct synchronous calls for Phase 0, optimize later |

---

## Phase 1: Core Emacs - Basic Editing

**Status:** 🔲 Planned
**Goal:** Implement essential Emacs editing commands
**Timeline:** 2 weeks (Jan 10 - Jan 24, 2025)
**Prerequisites:** Phase 0 complete

### Core Commands to Implement

#### Navigation
- `forward-char` (C-f) / `backward-char` (C-b)
- `next-line` (C-n) / `previous-line` (C-p)
- `beginning-of-line` (C-a) / `end-of-line` (C-e)
- `beginning-of-buffer` (M-<) / `end-of-buffer` (M->)
- `forward-word` (M-f) / `backward-word` (M-b)

#### Editing
- `self-insert-command` - Insert printable characters
- `newline` (RET)
- `delete-backward-char` (DEL) / `delete-forward-char` (C-d)
- `kill-line` (C-k)
- `open-line` (C-o)

#### Selection & Kill Ring
- `set-mark-command` (C-SPC)
- `kill-region` (C-w)
- `copy-region-as-kill` (M-w)
- `yank` (C-y)
- `yank-pop` (M-y) - Cycle through kill ring

#### Undo
- `undo` (C-/) / (C-_)

### Success Criteria

- [ ] All navigation commands work correctly
- [ ] Can select text with mark and region
- [ ] Kill ring works (cut, copy, paste)
- [ ] Undo works for all operations
- [ ] Manual testing checklist passes

---

## Phase 2: Core Emacs - Buffers & Files

**Status:** 🔲 Planned
**Goal:** Multi-buffer editing with file I/O
**Timeline:** 2 weeks (Jan 24 - Feb 7, 2025)
**Prerequisites:** Phase 1 complete

### Features

#### Buffer Management
- `switch-to-buffer` (C-x b) - With minibuffer completion
- `kill-buffer` (C-x k)
- `list-buffers` (C-x C-b)
- `buffer-menu` - Interactive buffer selection
- Buffer list tracking

#### File Operations
- `find-file` (C-x C-f) - With file picker
- `save-buffer` (C-x C-s)
- `write-file` (C-x C-w) - Save as
- `insert-file` - Insert file contents at point
- Auto-detect file type for major mode

#### Buffer State
- Modified indicator (*scratch*)
- File association
- Major mode display

### Success Criteria

- [ ] Can create, switch, and kill buffers
- [ ] File open/save works reliably
- [ ] Modified buffers show indicator
- [ ] Switching buffers preserves state (cursor, content)
- [ ] Can have multiple buffers open simultaneously

---

## Phase 3: Core Emacs - Windows & Frames

**Status:** 🔲 Planned
**Goal:** Window splitting and management
**Timeline:** 1.5 weeks (Feb 7 - Feb 18, 2025)
**Prerequisites:** Phase 2 complete

### Features

#### Window Splitting
- `split-window-below` (C-x 2)
- `split-window-right` (C-x 3)
- `delete-window` (C-x 0)
- `delete-other-windows` (C-x 1)

#### Window Navigation
- `other-window` (C-x o)
- Window selection with mouse (future)

#### Window State
- Window tree structure (internal nodes for splits, leaves for buffers)
- Viewport tracking per window
- Independent cursor per window showing same buffer

### Success Criteria

- [ ] Can split windows horizontally and vertically
- [ ] Can navigate between windows
- [ ] Each window shows correct buffer
- [ ] Same buffer in multiple windows works correctly
- [ ] Deleting windows reclaims space properly

---

## Phase 4: Core Emacs - Minibuffer & Completion

**Status:** 🔲 Planned
**Goal:** Interactive command input and completion
**Timeline:** 2 weeks (Feb 18 - Mar 4, 2025)
**Prerequisites:** Phase 3 complete

### Features

#### Minibuffer Basics
- `minibuffer-mode` - Special mode for minibuffer
- Minibuffer activation/deactivation
- Prompt display
- Input reading

#### Reading Functions
- `read-string` - Simple string input
- `read-number` - Number input
- `read-buffer` - Buffer name with completion
- `read-file-name` - File name with completion
- `completing-read` - Generic completion

#### Completion
- Basic prefix completion (TAB)
- Completion candidates display
- Completion cycling
- Match highlighting

#### M-x Command
- `execute-extended-command` (M-x)
- Command name completion
- Show keybinding if available
- Recent commands tracking

### Success Criteria

- [ ] M-x works with completion
- [ ] File completion works in `find-file`
- [ ] Buffer completion works in `switch-to-buffer`
- [ ] TAB completes, RET confirms, C-g cancels
- [ ] Minibuffer shows prompt and input correctly

---

## Phase 5: Core Emacs - Modes & Keymaps

**Status:** 🔲 Planned
**Goal:** Robust major/minor mode system
**Timeline:** 2 weeks (Mar 4 - Mar 18, 2025)
**Prerequisites:** Phase 4 complete

### Features

#### Major Modes
- `fundamental-mode` - Base mode
- `text-mode` - Plain text editing
- `clojure-mode` - ClojureScript mode (basic)
- Mode inheritance (derive-mode concept)
- Syntax tables (basic)

#### Minor Modes
- `auto-fill-mode` - Automatic line wrapping
- `line-number-mode` - Show line numbers in mode line
- `column-number-mode` - Show column in mode line
- Minor mode toggling

#### Mode Hooks
- `after-change-hook`
- `before-save-hook`
- `after-save-hook`
- Mode-specific hooks (e.g., `text-mode-hook`)

#### Keymap Refinement
- Keymap inheritance
- Sparse vs full keymaps
- Prefix command setup
- Custom prefix keys

### Success Criteria

- [ ] Can switch major modes
- [ ] Major modes have distinct keymaps
- [ ] Minor modes can be toggled on/off
- [ ] Hooks fire at appropriate times
- [ ] Keymap precedence works correctly

---

## Phase 6: Package System & Evil-mode

**Status:** 🔲 Planned
**Goal:** External package loading, Evil-mode as first package
**Timeline:** 3 weeks (Mar 18 - Apr 8, 2025)
**Prerequisites:** Phase 5 complete

### Package System

#### Package Manager (Simple)
```clojure
(lexicon.packages/load-package! :evil-mode)
(lexicon.packages/package-loaded? :evil-mode)
(lexicon.packages/package-list)
```

#### Package Structure
```
packages/evil-mode/
├── package.edn        # Metadata
└── src/lexicon/evil/
    ├── core.cljs      # (initialize!) function
    ├── fsm.cljs
    ├── operators.cljs
    ├── motions.cljs
    └── keymaps.cljs
```

#### Package API
- Packages register commands via `lexicon.core.command/register-command!`
- Packages register keymaps via `lexicon.core.keymap/define-keymap`
- Packages define minor modes via `lexicon.core.mode/define-minor-mode`
- Packages add hooks via `lexicon.core.hooks/add-hook`

### Evil-mode Package

#### Features
- Normal, Insert, Visual modes (FSM state machine)
- Operators: `d`, `c`, `y`, `>`, `<`
- Motions: `w`, `b`, `e`, `$`, `0`, `gg`, `G`
- Text objects: `iw`, `aw`, `i"`, `a"`
- Operator-motion composition (`dw`, `ciw`, etc.)
- Evil-specific keymaps

#### Integration
- Toggle with `(evil-mode 1)` / `(evil-mode -1)`
- Coexists with Emacs keybindings
- Can be disabled without breaking core

### Success Criteria

- [ ] Package loading system works
- [ ] Evil-mode loads as external package
- [ ] Evil normal mode works (hjkl navigation)
- [ ] Evil insert mode works (i, a, o, O)
- [ ] Operators + motions work (dw, ciw, etc.)
- [ ] Disabling Evil-mode returns to pure Emacs
- [ ] No Evil code in `lexicon-core/` directory

---

## Phase 7: Advanced Editing Features

**Status:** 🔲 Planned
**Goal:** Polish core editing experience
**Timeline:** 2 weeks (Apr 8 - Apr 22, 2025)
**Prerequisites:** Phase 6 complete

### Features

#### Search & Replace
- `isearch-forward` (C-s)
- `isearch-backward` (C-r)
- `query-replace` (M-%)
- `occur` - List matching lines

#### Advanced Editing
- `transpose-chars` (C-t)
- `transpose-words` (M-t)
- `capitalize-word` (M-c)
- `upcase-word` (M-u)
- `downcase-word` (M-l)
- `fill-paragraph` (M-q)

#### Rectangle Commands
- `rectangle-mark-mode` (C-x SPC)
- `kill-rectangle` (C-x r k)
- `yank-rectangle` (C-x r y)
- `string-rectangle` (C-x r t)

#### Macros (Keyboard)
- `kmacro-start-macro` (F3 / C-x ()
- `kmacro-end-macro` (F4 / C-x ))
- `kmacro-call-macro` (F4 / C-x e)

### Success Criteria

- [ ] Incremental search works
- [ ] Query-replace works
- [ ] Rectangle operations work
- [ ] Keyboard macros record and replay
- [ ] Text transformation commands work

---

## Phase 8: Package Ecosystem

**Status:** 🔲 Planned
**Goal:** Essential packages for modern editing
**Timeline:** 4 weeks (Apr 22 - May 20, 2025)
**Prerequisites:** Phase 7 complete

### Packages to Implement

#### Vertico (Completion UI)
- Vertical completion interface
- Candidate preview
- Filtering as you type
- Integration with completing-read

#### Orderless (Completion Style)
- Flexible matching (space-separated terms)
- Out-of-order matching
- Component highlighting

#### Consult (Enhanced Commands)
- `consult-buffer` - Better buffer switching
- `consult-line` - Search current buffer
- `consult-ripgrep` - Project-wide search

#### Corfu (In-buffer Completion)
- Popup completion at point
- Integration with language servers (future)
- Company-mode alternative

### Success Criteria

- [ ] Vertico provides modern completion UI
- [ ] Orderless enables flexible matching
- [ ] Consult commands work as enhancements
- [ ] Corfu shows completions in buffer
- [ ] Packages can be loaded independently

---

## Phase 9: Syntax Highlighting & Tree-sitter

**Status:** 🔲 Planned
**Goal:** Modern syntax highlighting
**Timeline:** 3 weeks (May 20 - Jun 10, 2025)
**Prerequisites:** Phase 8 complete

### Features

#### Tree-sitter Integration
- Web Worker for parsing (keep UI responsive)
- Incremental parsing on edits
- Parse tree caching
- Query-based highlighting

#### Language Support
- JavaScript/TypeScript
- ClojureScript
- Python
- Rust
- Markdown

#### Highlighting
- Semantic token types
- Face (style) system
- Theme support (modus-operandi, modus-vivendi)

### Success Criteria

- [ ] Tree-sitter parses in background
- [ ] Syntax highlighting updates on edit
- [ ] No UI lag during typing
- [ ] At least 3 languages supported
- [ ] Themes can be switched

---

## Phase 10: LSP Integration

**Status:** 🔲 Planned
**Goal:** Language server protocol support
**Timeline:** 3 weeks (Jun 10 - Jul 1, 2025)
**Prerequisites:** Phase 9 complete, Bridge server working

### Features

#### LSP Client
- Initialize language server via bridge
- Document synchronization
- Diagnostics display (inline, gutter)
- Hover information
- Go-to-definition
- Find references
- Code actions

#### Supported Languages
- TypeScript (typescript-language-server)
- Rust (rust-analyzer)
- Clojure (clojure-lsp)
- Python (pylsp)

### Success Criteria

- [ ] LSP servers start via bridge
- [ ] Diagnostics show in editor
- [ ] Go-to-definition works (C-c g d)
- [ ] Hover shows type info
- [ ] Code actions available
- [ ] At least 2 languages work end-to-end

---

## Long-term Vision (Beyond Phase 10)

### Org-mode
- Outline editing
- TODO items
- Agenda
- Export to HTML/Markdown

### Magit (Git Interface)
- Status buffer
- Staging/unstaging
- Commits
- Branch management

### Collaboration
- Real-time collaborative editing
- Conflict resolution
- User presence indicators

### Cloud Integration
- Remote workspaces
- Devcontainer support
- Multi-user environments

---

## Metrics & Success Indicators

### Phase Completion Criteria
- All success criteria checkboxes ticked
- Demo video recorded showing features working
- Manual test pass (no automated tests yet)
- Documentation updated
- No known critical bugs

### Overall Project Health
- Can use Lexicon for editing its own source code
- Community interest (GitHub stars, issues, discussions)
- Package ecosystem growth
- Performance acceptable (< 100ms input latency)

---

## Current Focus

**Active Phase:** Phase 0 ✅ COMPLETE!
**Achievements:**
- Evil-mode extracted to packages/
- Gap buffer implemented and integrated
- All basic text editing operations working
- Core compiles cleanly with 0 errors

**Next Phase:** Phase 1 - Core Emacs Basic Editing
**Next Milestone:** Implement navigation commands (C-a, C-e, M-f, M-b, etc.)
**Blocking Issues:** None

**Last Updated:** 2025-12-21

---

*This roadmap is a living document. Update it as we progress, learn, and adapt.*
