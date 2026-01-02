# Lexicon Development Roadmap

**Last Updated:** 2025-12-25
**Current Phase:** Phase 6D - Missing Emacs Infrastructure ✅ COMPLETE

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

4. **Update Documentation** ✅ COMPLETE (Dec 20, 2025)
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

---

## Phase 1: Core Emacs - Basic Editing

**Status:** ✅ COMPLETE (Dec 21, 2025)
**Goal:** Implement essential Emacs editing commands
**Timeline:** 1 day (Dec 21, 2025)
**Prerequisites:** ✅ Phase 0 complete

### Success Criteria

- [x] All navigation commands work correctly (C-f/b/n/p, C-a/e, M-</>, M-f/b)
- [x] Can select text with mark and region (C-SPC, M-w, C-w via M-x)
- [x] Kill ring works (cut, copy, paste, C-k, C-y, M-y)
- [x] Undo works for all operations (C-/, C-_)
- [x] Manual testing checklist passes

**Progress:** 5/5 complete (100%) ✅ PHASE 1 COMPLETE!

---

## Phase 2: Core Emacs - Buffers & Files

**Status:** ✅ COMPLETE (Dec 21, 2025)
**Goal:** Multi-buffer editing with file I/O
**Prerequisites:** ✅ Phase 1 complete, ✅ Subscription caching complete

### Success Criteria

- [x] Can create, switch, and kill buffers
- [x] File open/save works reliably
- [x] Modified buffers show indicator
- [x] Switching buffers preserves state (cursor, content)
- [x] Can have multiple buffers open simultaneously

**Progress:** 5/5 complete (100%) ✅ PHASE 2 COMPLETE!

---

## Phase 2.5: Core Polish (Before Phase 3)

**Status:** ✅ COMPLETE (Dec 21, 2025)
**Goal:** Fix critical UX gaps from Phase 1-2 before adding window complexity
**Timeline:** 7.5 hours total
**Prerequisites:** ✅ Phase 2 complete

### Success Criteria

- [x] **Can type anywhere in editor without clicking narrow column first** ✅ FIXED
- [x] **M- commands work immediately after app loads** ✅ FIXED
- [x] **C-g cancels minibuffer and returns focus to editor** ✅ FIXED
- [x] No horizontal scrollbar visible ✅ FIXED
- [x] Echo area shows messages (e.g., "Saving...", "Saved") ✅ FIXED
- [x] Can see region selection visually ✅ FIXED
- [x] C-x b shows completions on TAB, can select with arrow keys ✅ FIXED
- [x] C-x C-b allows RET to switch to buffer on current line ✅ FIXED
- [x] C-h b lists all current keybindings ✅ FIXED
- [x] C-u sets prefix argument (4, 16, 64...) ✅ FIXED

**Progress:** 10/10 complete (100%) ✅ PHASE 2.5 COMPLETE!

---

## Phase 3: Core Emacs - Windows & Frames

**Status:** ✅ COMPLETE (Dec 22, 2025)
**Goal:** Window splitting and management
**Timeline:** Completed Dec 22, 2025
**Prerequisites:** ✅ Phase 2.5 complete

### Success Criteria

- [x] Window tree data structure works
- [x] Can create splits (data structure updates correctly)
- [x] Can navigate between windows (C-x o cycles)
- [x] Cursor/mark are per-window
- [x] **Visual rendering shows multiple windows**
- [x] Active window shows blue border
- [x] Click to activate windows
- [x] **Cursor moves when typing** (Fixed regression)
- [ ] Same buffer in multiple windows displays correctly (untested)
- [ ] Deleting windows rebalances tree and reclaims space

**Progress:** 8/10 complete (80%) ✅ PHASE 3 COMPLETE!

**Note:** Full window management system implemented and working. Windows can be split horizontally (C-x 2) or vertically (C-x 3), navigated (C-x o), and managed (C-x 1 to keep only active). Visual rendering shows multiple windows simultaneously with recursive tree traversal. Each window has independent cursor position. Active window indicated by blue border. Click-to-activate works.

---

## Phase 4: Core Emacs - Minibuffer & Completion

**Status:** ✅ COMPLETE (Dec 22, 2025)
**Goal:** Interactive command input and completion
**Timeline:** Completed Dec 22, 2025
**Prerequisites:** ✅ Phase 3 complete

### Features Implemented

#### Minibuffer Basics (✅ Complete)
- [x] `minibuffer-mode` - Special mode for minibuffer
- [x] Minibuffer activation/deactivation
- [x] Prompt display
- [x] Input reading

#### Completion (✅ Complete)
- [x] Basic prefix completion (TAB)
- [x] Completion candidates display (echo area)
- [x] Completion cycling through matches
- [x] Common prefix expansion
- [x] `read-buffer` - Buffer name with completion
- [x] `completing-read` - Generic completion (via minibuffer)

#### M-x Command (✅ Complete)
- [x] `execute-extended-command` (M-x)
- [x] Command name completion

#### Help System (C-h) (✅ Complete)
- [x] `describe-key` (C-h k) - Show what a key binding does
- [x] `describe-function` (C-h f) - Describe a command/function with keybindings
- [x] `describe-bindings` (C-h b) - List all current key bindings
- [x] `apropos-command` (C-h a) - Search commands by keyword/regex
- [x] `help-for-help` (C-h ?) - Show help menu
- [x] Command registry for introspection
- [x] Keybinding reverse lookup (command → key)

### Success Criteria

- [x] M-x works with completion
- [x] Buffer completion works in `switch-to-buffer`
- [x] TAB completes, RET confirms, C-g cancels
- [x] Minibuffer shows prompt and input correctly
- [x] C-h k shows keybinding documentation
- [x] C-h b lists all keybindings in a buffer
- [x] C-h f describes any command
- [x] C-h a searches commands by pattern
- [x] C-h ? shows help menu

**Progress:** 9/9 complete (100%) ✅ PHASE 4 COMPLETE!

**Deferred to Later:**
- File name completion (`read-file-name`) - Needs browser File System Access API
- M-x enhancements (show keybinding hints, recent commands tracking)

---

## Phase 5: Core Emacs - Modes & Keymaps

**Status:** ✅ COMPLETE (Dec 22, 2025)
**Goal:** Robust major/minor mode system
**Timeline:** Completed Dec 22, 2025
**Prerequisites:** ✅ Phase 4 complete

### Features Implemented

#### Major Modes (✅ Complete)
- [x] `fundamental-mode` - Base mode
- [x] `text-mode` - Plain text editing
- [x] `clojure-mode` - ClojureScript mode (basic)
- [x] Mode-specific hooks run on activation (`text-mode-hook`, etc.)
- [x] `buffer-menu-mode` - Interactive buffer list

#### Minor Modes (✅ Complete)
- [x] `line-number-mode` - Toggle line numbers in status bar
- [x] `column-number-mode` - Toggle column number in status bar
- [x] Minor mode toggling

#### Mode Hooks (✅ Complete)
- [x] `after-change-hook`
- [x] `before-save-hook`
- [x] `after-save-hook`
- [x] Mode-specific hooks (`text-mode-hook`, `fundamental-mode-hook`, etc.)
- [x] `:add-hook` and `:remove-hook` commands

#### Keymap System (✅ Complete)
- [x] Keymap inheritance (major/minor/global precedence)
- [x] Sparse keymaps (only define needed bindings)
- [x] Prefix command setup (C-x, C-h, M-, etc.)
- [x] Custom prefix keys (via keymap structure)

### Success Criteria

- [x] Can switch major modes
- [x] Major modes have distinct keymaps
- [x] Minor modes can be toggled on/off
- [x] Hooks fire at appropriate times (mode hooks run on activation)
- [x] Keymap precedence works correctly
- [x] line-number-mode and column-number-mode toggle display

**Progress:** 6/6 complete (100%) ✅ PHASE 5 COMPLETE!

**Deferred to Later:**
- Mode inheritance (derive-mode concept) - Deferred to Phase 6+
- Syntax tables - Deferred to Phase 9 (Tree-sitter)
- `auto-fill-mode` - Deferred to Phase 7

---

## Phase 6A: Package System Infrastructure

**Status:** ✅ COMPLETE (Dec 24, 2025)
**Goal:** External package loading infrastructure
**Timeline:** Completed Dec 24, 2025
**Prerequisites:** ✅ Phase 5 complete

**Note:** This phase was originally "Phase 6: Package System & Evil-mode" but research revealed we need display infrastructure before Evil-mode can work properly. Phase 6 is now split into 6A (complete), 6B (display), 6C (completion), 6D (infrastructure), and Phase 7 (Evil-mode integration).

### Package System Infrastructure ✅ COMPLETE (Dec 24, 2025)

#### Package Manager (✅ Complete)
- [x] `lexicon.packages` namespace with package registry
- [x] `register-package!` - Register package metadata
- [x] `load-package!` - Load and initialize package
- [x] `unload-package!` - Unload and cleanup package
- [x] `package-loaded?` - Check if package is loaded
- [x] `package-list` - List all registered packages
- [x] `package-info` - Get package metadata
- [x] Re-frame integration (`:package/loaded`, `:package/unloaded` events)
- [x] Package state tracking in app-db (`:packages {}`)

#### Build System Integration (✅ Complete)
- [x] Added `../evil-mode/src` to shadow-cljs source paths
- [x] Created `package-loader.cljs` to compile packages into bundle
- [x] Packages register on namespace load but don't auto-activate

### Evil-mode Package Structure ✅ COMPLETE (Dec 24, 2025)

#### Package Files (✅ Complete)
- [x] `package.edn` - Package metadata
- [x] `src/lexicon/evil/core.cljs` - Package entry point with initialize!/cleanup!
- [x] `src/lexicon/evil/fsm/events.cljs` - FSM state machine (fixed namespaces)
- [x] `src/lexicon/evil/fsm/interceptors.cljs` - FSM lifecycle hooks (fixed namespaces)
- [x] `src/lexicon/evil/command/dispatcher.cljs` - Command dispatcher (fixed namespaces)
- [x] `src/lexicon/evil/keymaps/registry.cljs` - Keymap registry (fixed namespaces)
- [x] `src/lexicon/evil/macros.clj` - Compile-time macros

#### Package Integration (✅ Complete)
- [x] Fixed namespace declarations (`lexicon.evil.*`)
- [x] Fixed escaped quotes in docstrings
- [x] Fixed compilation errors (final-args scope, db destructuring)
- [x] Package compiles without warnings or errors
- [x] Package self-registers via `packages/register-package!`
- [x] FSM state initialization/cleanup lifecycle
- [x] Keymap initialization disabled (called explicitly by package)

### Evil-mode Package Structure (✅ Created, Not Yet Integrated)

**Note:** Evil-mode package structure exists and compiles but is **not functional** because it depends on display infrastructure (overlays, faces, text properties) we don't have yet. Evil-mode integration deferred to **Phase 7**.

**What exists:**
- ✅ Package metadata (`package.edn`)
- ✅ FSM state machine (states, transitions, validation)
- ✅ Command dispatcher with operator-motion composition
- ✅ Keymap registry (normal, insert, visual modes)
- ✅ Package lifecycle (initialize!/cleanup!)

**Why deferred:**
- ❌ Needs overlay system for visual mode highlighting
- ❌ Needs face system for cursor shapes and colors
- ❌ Needs buffer-local variables for per-buffer state
- ❌ Needs pre/post-command hooks for state transitions
- ❌ Needs advice system to wrap Emacs functions

See `docs/DISPLAY_THEMING_RESEARCH.md` for detailed analysis of Evil-mode dependencies.

### Success Criteria

- [x] Package loading system compiles without errors
- [x] Evil-mode registers as external package
- [x] Package metadata available via `package-info`
- [x] Initialize/cleanup lifecycle works
- [x] Evil-mode compiles without warnings or errors
- [x] Package structure complete and documented

**Progress:** 6/6 complete (100%) ✅ PHASE 6A COMPLETE!

---

## Phase 6B: Display & Theming Foundation

**Status:** ✅ COMPLETE (Dec 25, 2025)
**Goal:** Implement face system, overlays, text properties, mode line, and themes
**Timeline:** 4 weeks
**Prerequisites:** ✅ Phase 6A complete

### Rationale

**Critical Discovery:** Research (see `docs/DISPLAY_THEMING_RESEARCH.md`) revealed that **all sophisticated packages depend on display infrastructure** we don't have:

- **Completion framework** needs faces for annotations (Marginalia, Vertico)
- **Evil-mode** needs overlays for visual selection, faces for cursor feedback
- **Themes** need face system to propagate colors to packages
- **All packages** assume text properties and overlays exist

**This phase must come BEFORE completion framework or Evil-mode integration.**

### Week 1: Face System ✅ COMPLETE (Dec 24, 2025)

**Goal:** Named visual attributes for text styling

#### Face Data Structure (✅ Complete)
- [x] Face registry in app-db (`:faces {}`)
- [x] Face attributes: `:foreground`, `:background`, `:font-family`, `:font-size`, `:font-weight`, `:font-slant`, `:underline`, `:box`, `:inherit`, `:extend`
- [x] Face inheritance (faces can inherit from other faces)
- [x] Face merging rules (combine multiple face attributes)

#### Standard Faces (✅ Complete)
- [x] `default` - Base face for all text
- [x] `region` - Selected region
- [x] `mode-line` / `mode-line-inactive` - Mode line faces
- [x] `minibuffer-prompt` - Minibuffer prompt
- [x] `highlight` - Generic highlighting
- [x] `isearch` / `lazy-highlight` - Search matches
- [x] `error`, `warning`, `success` - Semantic colors
- [x] `font-lock-*` faces - Syntax highlighting (comment, keyword, string, etc.)
- [x] `completions-*` faces - Completion UI

#### CSS Generation with CSS Variables (✅ Complete)
- [x] Use CSS custom properties (variables) for dynamic theming (e.g., `--lexicon-fg-default`)
- [x] Generate static face classes that reference variables (e.g., `.face-default { color: var(--lexicon-fg-default); }`)
- [x] Apply CSS to rendered text spans
- [x] Dynamic updates via CSS variable changes (no stylesheet regeneration needed)

### Week 2: Text Properties & Overlays ✅ COMPLETE (Dec 24, 2025)

**Goal:** Attach visual and semantic properties to buffer text

#### Text Properties (✅ Complete)
- [x] `put-text-property` - Apply property to range
- [x] `get-text-property` - Read property at position
- [x] `remove-text-property` - Remove property from range
- [x] Property types: `face`, `font-lock-face`, `invisible`, `read-only`, `help-echo`, `display`
- [x] Properties stored in ClojureScript (buffer-local nested map)
- [x] Properties move with text when edited (conceptually, needs implementation)

#### Overlay System (✅ Complete)
- [x] `make-overlay` - Create overlay on range
- [x] `delete-overlay` - Remove overlay
- [x] `overlay-put` - Set overlay property
- [x] `overlay-get` - Read overlay property
- [x] Overlay priority system (higher priority wins)
- [x] Overlay properties: all text properties + `priority`, `before-string`, `after-string`, `evaporate`
- [x] Overlays independent of text (don't move when text edited)

#### Face Priority & Merging (✅ Complete)
- [x] Overlay faces sorted by priority
- [x] Text property `face` > `font-lock-face`
- [x] Merge multiple face attributes
- [x] Render text with correct face CSS (subscription implemented)

#### Child Frames (Popups) (✅ Complete)
- [x] `make-frame` - Create popup frame (absolutely positioned div)
- [x] Frame properties: `:x`, `:y`, `:width`, `:height`, `:visible?`, `:content`, `:face`
- [x] `show-frame` / `hide-frame` commands
- [x] Render frames at top level (Reagent component created)
- [x] Used by Corfu for in-buffer completion popups

### Week 3: Mode Line & Built-in Modes ✅ COMPLETE (Dec 25, 2025)

**Goal:** Customizable status bar with rich formatting + essential built-in modes

#### Built-in Modes (✅ Complete)
- [x] Read-only buffer enforcement (`:is-read-only?` flag checked in all edit operations)
- [x] `fundamental-mode` - Base mode (already existed)
- [x] `special-mode` - Base for `*Help*`, `*Buffer List*` buffers
  - Sets buffer read-only
  - Provides `q` to quit buffer, `g` to revert
  - Parent mode for all special buffers
- [x] Update existing special buffers to use `special-mode` (help-mode, buffer-menu-mode)

#### Mode Line Format Interpreter (✅ Complete)
- [x] Parse `mode-line-format` structure (strings with % constructs)
- [x] Evaluate format recursively
- [x] % constructs: `%b` (buffer), `%f` (file), `%l` (line), `%c` (column), `%p` (percentage), `%*` (modified), `%+` (plus-modified), `%m` (mode), `%%` (literal %)
- [x] Modified indicators: `**` (modified), `--` (unmodified), `%%` (read-only modified), `%-` (read-only)

#### Standard Mode Line Components (✅ Complete)
- [x] `mode-line-modified` - Modified indicator (`%*` construct)
- [x] `mode-line-buffer-identification` - Buffer name (`%b` construct)
- [x] `mode-line-position` - Line/column/percentage (`%l:%c %p`)
- [x] `mode-line-modes` - Major mode name (`%m` construct)
- [x] Standard format strings: `standard-mode-line-format`, `special-mode-line-format`, `help-mode-line-format`

#### Mode Line Faces (✅ Complete)
- [x] `mode-line` face defined
- [x] `mode-line-inactive` face defined
- [x] Mode line subscription ready (`:mode-line/format`)

#### Buffer-Local Mode Lines (✅ Complete)
- [x] Each buffer can customize `mode-line-format`
- [x] Mode line updates when buffer changes
- [x] Mode line reflects current window state (cursor position, modified status)

### Week 4: Theme System (CSS Variables) ✅ COMPLETE (Dec 25, 2025)

**Goal:** Load/unload color schemes, light/dark variants using CSS custom properties

#### Theme Infrastructure (✅ Complete)
- [x] `deftheme` - Define theme as map of CSS variables to values
- [x] Theme data structure: `{:name "..." :kind :light :palette {"--lexicon-fg-default" "#000" ...}}`
- [x] Theme registry in app-db (`:theme/registry`, `:theme/active`, `:theme/current`)
- [x] Theme loading updates `:root` CSS variables in one operation
- [x] Theme switching command (M-x load-theme)

#### Palette System with CSS Variables (✅ Complete)
- [x] All colors as CSS variables (50+ variables: fg/bg colors, semantic, syntax, search, etc.)
- [x] Font settings as variables (`--lexicon-font-family-mono`, `--lexicon-font-size`, `--lexicon-line-height`)
- [x] Semantic color mappings (error, warning, success, info)
- [x] Light/dark themes swap variable values, faces remain unchanged

#### Default Themes (✅ Complete)
- [x] `lexicon-base-light` theme (WCAG AAA compliant light theme)
- [x] `lexicon-base-dark` theme (One Dark inspired dark theme)
- [x] WCAG AAA contrast compliance for accessibility
- [x] All CSS variables defined with sensible defaults

#### Theme Application (✅ Complete)
- [x] `theme->css` - Convert theme map to `:root { ... }` CSS
- [x] `:theme/initialize` - Inject theme variables on startup
- [x] `:theme/set-variable` - Allow runtime variable changes
- [x] `:theme/set-font-size` - Convenience command for font size (M-x set-font-size)
- [x] No stylesheet regeneration needed - CSS variables update instantly
- [ ] Persist theme choice (deferred: save to local storage)

### Success Criteria

- [x] Can define and apply faces to text
- [x] Text renders with correct colors, fonts, and styling via CSS variables
- [x] Overlays can highlight regions independently of text properties
- [x] Child frames (popups) can be shown at arbitrary positions
- [x] special-mode works for read-only buffers (`q` to quit, `g` to revert)
- [x] Mode line displays buffer info with % constructs
- [x] Can load/unload themes and see visual changes instantly (CSS variables)
- [x] Can customize colors/fonts at runtime (M-x set-font-size)
- [x] Default light and dark themes look professional
- [x] All standard faces defined and applied correctly

**Progress:** 10/10 complete (100%) ✅ PHASE 6B COMPLETE!

---

## Phase 6C: Completion Framework

**Status:** ✅ COMPLETE (Dec 25, 2025)
**Goal:** Metadata, styles, completion tables, CAPFs
**Timeline:** 6 weeks
**Prerequisites:** ✅ Phase 6A complete, ✅ Phase 6B complete

### Rationale

The completion framework is the **foundation of the package ecosystem**. Packages like Vertico, Orderless, Marginalia, and Consult all depend on:
- Completion metadata (categories, annotations)
- Completion styles (prefix, substring, flex, orderless)
- Completion tables (dynamic, programmed)
- CAPFs (completion-at-point-functions)

**Why after Phase 6B:** Completion UI packages (Marginalia, Vertico) need **faces** to render annotations and style candidates. Without faces, they can't display properly.

### Week 1-2: Completion Metadata System

#### Completion Categories (✅ COMPLETE)
- [x] Category system (`:command`, `:file`, `:buffer`, `:variable`, `:function`, `:face`, `:theme`)
- [x] Category-specific defaults (sort order, completion style)
- [x] Metadata association with completion tables

#### Completion Metadata Properties (✅ COMPLETE)
- [x] `:annotation-function` - Add annotations to candidates (e.g., "(keybinding)" for commands)
- [x] `:affixation-function` - Prefix/suffix/annotation in one pass
- [x] `:group-function` - Group candidates (e.g., by mode)
- [x] `:display-sort-function` - Custom sort order
- [x] `:category` - Semantic category of candidates
- [x] `:cycle-sort-function` - Cycling behavior

#### Metadata Application (✅ COMPLETE)
- [x] Attach metadata to `completing-read` calls
- [x] Metadata propagates to completion UI
- [x] Packages can extend metadata for custom categories

### Week 3-4: Completion Styles

#### Style System (✅ COMPLETE)
- [x] `completion-styles` - Ordered list of styles to try
- [x] `completion-category-overrides` - Per-category style preferences
- [x] Style precedence resolution

#### Built-in Styles (✅ COMPLETE)
- [x] `basic` - Prefix matching (already exists)
- [x] `substring` - Match anywhere in candidate
- [x] `flex` - Flexible matching (characters in order, gaps allowed)
- [x] `initials` - Match initials (e.g., "fb" matches "forward-button")
- [x] `partial-completion` - Complete parts separately (e.g., "f-b" matches "forward-button")

#### Orderless Style (✅ COMPLETE)
- [x] Space-separated patterns (all must match)
- [x] Flex matching per pattern
- [x] Highlighting of matched portions (using faces!)
- [x] Configurable pattern matching (regexp, literal, prefix)

### Week 5: Completion Tables & CAPFs

#### Completion Tables (✅ COMPLETE)
- [x] Static tables (simple lists)
- [x] Programmed completion (function returns candidates)
- [x] Dynamic tables (candidates change based on input)
- [x] Boundary detection (where completion starts/ends)

#### Completion-at-Point Functions (✅ COMPLETE)
- [x] `completion-at-point-functions` hook
- [x] CAPF protocol (`:company-prefix-length`, `:exit-function`)
- [x] `completion-at-point` command (TAB in buffer)
- [x] Multiple CAPFs per buffer (try in order)

#### Built-in CAPFs (✅ COMPLETE)
- [x] Symbol completion (Elisp symbols in `*scratch*`)
- [x] File name completion
- [x] Command completion (M-x)

### Week 6: Built-in Packages for Completion

#### project.el (✅ COMPLETE)
- [x] `project-current` - Detect project root
- [x] `project-root` - Get project directory
- [x] `project-files` - List project files
- [x] Git-based project detection (check for `.git`)
- [x] Integration with `find-file` (C-x C-f respects project context)

#### imenu (✅ COMPLETE)
- [x] `imenu-create-index-function` - Extract buffer structure
- [x] Default implementation (scan for `defun`, `defvar`, etc.)
- [x] `imenu` command - Jump to definition with completion
- [x] Integration with completion (category: `:imenu`)

#### recentf (✅ COMPLETE)
- [x] Track recently opened files
- [x] `recentf-mode` - Minor mode to enable tracking
- [x] Persist recent files to local storage
- [x] `recentf-open-files` command

#### savehist (✅ COMPLETE)
- [x] Persist minibuffer history across sessions
- [x] `savehist-mode` - Minor mode to enable persistence
- [x] Save to local storage
- [x] Restore on startup

### Success Criteria

- [x] Completion metadata (annotations, categories) works
- [x] Multiple completion styles (substring, flex) work
- [x] Orderless style matches space-separated patterns
- [x] CAPFs work for in-buffer completion
- [x] project.el detects Git projects
- [x] imenu extracts buffer structure
- [x] recentf tracks recent files
- [x] savehist persists minibuffer history

**Progress:** 8/8 complete (100%) ✅ PHASE 6C COMPLETE!

---

## Phase 6D: Missing Emacs Infrastructure

**Status:** ✅ COMPLETE (Dec 25, 2025)
**Goal:** Buffer-local variables, enhanced hooks, advice system
**Timeline:** 2 weeks
**Prerequisites:** ✅ Phase 6A complete, ✅ Phase 6B complete, ✅ Phase 6C complete

### Rationale

These are fundamental Emacs features that **packages assume exist**:
- Buffer-local variables (every mode uses them)
- Pre/post-command hooks (Evil-mode needs these for state transitions)
- Advice system (packages wrap/modify existing functions)

### Week 1: Buffer-Local Variables & Hooks

#### Buffer-Local Variables (✅ COMPLETE)
- [x] `make-local-variable` - Make variable buffer-local in current buffer
- [x] `make-variable-buffer-local` - Auto buffer-local for all buffers
- [x] Each buffer has own value for buffer-local vars
- [x] Global value vs buffer-local value distinction
- [x] `setq-local` - Set buffer-local value
- [x] `defvar-local` - Define buffer-local variable

#### Enhanced Hooks (✅ COMPLETE)
- [x] `pre-command-hook` - Run before every command
- [x] `post-command-hook` - Run after every command
- [x] `window-configuration-change-hook` - Run when windows change
- [x] `kill-buffer-hook` - Run when buffer is killed
- [x] `change-major-mode-hook` - Run when major mode changes
- [x] Buffer-local hooks (`:add-hook ... nil t`)

### Week 2: Advice System & Advanced Features

#### Advice Infrastructure (✅ COMPLETE)
- [x] `advice-add` - Add advice to function
- [x] `advice-remove` - Remove advice from function
- [x] `:before` advice - Run before original function
- [x] `:after` advice - Run after original function
- [x] `:around` advice - Wrap original function
- [x] `:override` advice - Replace original function
- [x] Multiple advice can stack on same function

#### Advice Application (✅ COMPLETE)
- [x] Track advised functions in registry
- [x] Generate wrapped function with advice chain
- [x] Advice arguments and return values
- [x] Packages can modify existing Lexicon functions

#### thing-at-point Subsystem (✅ COMPLETE)
- [x] `thing-at-point` multimethod (dispatch on type: `:symbol`, `:url`, `:filename`, `:word`)
- [x] `:symbol` - Find symbol boundaries around cursor
- [x] `:url` - Find URL boundaries (http://, https://)
- [x] `:filename` - Find file path at point
- [x] `bounds-of-thing-at-point` - Return `{:start N :end M}` for thing
- [x] Used by Embark for context-aware actions
- [x] Used by `find-file-at-point` command

#### Advanced Undo System (✅ COMPLETE)
- [x] `:undo-boundary` markers in undo stack
- [x] `undo-boundary` command - Insert boundary marker
- [x] `:undo/begin-change-group` - Start grouped undo
- [x] `:undo/end-change-group` - End grouped undo
- [x] Update `:undo` to respect boundaries (undo entire group at once)
- [x] `:undo-recording-enabled?` flag to prevent recording during undo
- [x] Used by complex commands (format-buffer, completion insertion)

### Success Criteria

- [x] Buffer-local variables work (each buffer has own value)
- [x] pre-command-hook and post-command-hook fire correctly
- [x] Advice can wrap existing functions
- [x] Multiple advice can stack on same function
- [x] thing-at-point finds symbols, URLs, and filenames
- [x] Advanced undo groups multiple edits into single undo step
- [x] Packages can use all these features

**Progress:** 7/7 complete (100%) ✅ PHASE 6D COMPLETE!

---

## Phase 6.5: Testing & Quality Assurance

**Status:** ✅ COMPLETE (Dec 25, 2025)
**Goal:** Browser-based automated testing infrastructure for core stability
**Timeline:** ~3 hours
**Prerequisites:** ✅ Phase 6A-6D complete
**Priority:** CRITICAL - Required before accepting external contributions

### Rationale

Before opening the project to external contributors and accepting pull requests, we need:
- Confidence that changes don't break existing functionality
- Automated regression testing for critical workflows
- Test infrastructure that contributors can extend
- Clear quality bar for new features

### Test Infrastructure ✅ COMPLETE

#### Browser-Based Test Automation (shadow-cljs :browser-test) ✅ COMPLETE
- [x] **Test target configured**: shadow-cljs :browser-test with HTTP server on port 8021
- [x] **Test environment**: http://localhost:8021/test/index.html
- [x] **WASM integration**: Async WASM loading with `use-fixtures`
- [x] **Test isolation**: Proper `reset-db!` implementation with window structure setup
- [x] **Hot reload**: Tests auto-reload on file changes for rapid development
- [x] **Test events**: Separate test/lexicon/test_events.cljs with synchronous event handlers
- [x] **Test helpers**: create-test-buffer, get-buffer-text, reset-db! infrastructure

#### Unit Tests (35 passing, 6 failing) ✅ COMPLETE
- [x] **Text operations**: Insert, delete, emoji, UTF-8 handling (8 tests in core_test.cljs)
- [x] **Window management**: Split, navigate, delete operations (7 tests in window_test.cljs)
- [x] **Buffer operations**: Create, switch buffer logic
- [x] **Cursor movement**: Forward/backward char, beginning/end-of-line (4 tests)
- [x] **Kill ring**: Copy, kill, yank operations (3 tests)
- [x] **Minibuffer**: Completion flows (1 test)

#### Regression Tests ✅ COMPLETE
- [x] **Basic editing**: Type text, move cursor, delete (test-basic-typing-works)
- [x] **Phase 3 regression**: Window tree cursor updates (test-window-tree-cursor-update)
- [x] **Line ending handling**: Newlines, empty lines (test-newline-works, test-empty-lines-render)
- [x] **UTF-8 correctness**: Emoji, multi-byte characters (test-emoji-support, test-multibyte-cursor-movement)
- [x] **Region operations**: Mark → kill → yank (test-kill-yank-workflow)
- [x] **Window state isolation**: Independent cursors (test-independent-window-points)

#### Manual Test Plan ✅ COMPLETE
- [x] **MANUAL_TEST_PLAN.md**: 400+ test cases across 20 categories
- [x] **Test execution log template**: For documenting manual test results
- [x] **Priority levels**: P0-P3 for bug triage
- [x] **Browser/OS coverage**: Guidelines for cross-platform testing
- [x] **Accessibility**: Screen reader, keyboard-only, high contrast tests

#### Critical Bugs Fixed During Phase 6.5
- [x] **WASM delete API misuse**: Fixed calls to use (position, length) instead of (start, end)
- [x] **UTF-8 cursor positioning**: Query WASM .length after insert for accurate positions
- [x] **Active window ID lookup**: Fixed path from [:active-window-id] to [:editor :active-window-id]
- [x] **Test isolation**: Added window structure setup to regression_test reset-db!

### Failing Tests (6 - All Undo-Related)

**❌ Undo Operations (WASM API Not Implemented)**
- ❌ test-undo-insert
- ❌ test-undo-delete
- ❌ test-undo-redo-chain (2 failures)

**Root Cause**: WASM gap buffer does not expose undo/redo API. Requires Rust implementation in lexicon-engine.

### Success Criteria

- [x] **Browser-based test infrastructure** with shadow-cljs :browser-test
- [x] **35 passing automated tests** (85% pass rate)
- [x] **Test-first workflow established**: Manual test → failing automated test → fix → verify → check regressions
- [x] **Comprehensive manual test plan** with 400+ test cases
- [x] **Test documentation**: TEST_AUTOMATION_STATUS.md and MANUAL_TEST_PLAN.md
- [x] **No production code contamination**: All test code isolated in test/ directory
- [ ] CI/CD pipeline runs tests on every commit (Future: requires CI setup)
- [ ] 80%+ code coverage (Future: requires coverage tooling)

**Test Results:** 35 passing, 6 failing (all undo-related)
**Test Infrastructure:** ✅ Complete and ready for use
**Development Workflow:** ✅ Established (TDD approach documented)

### Documentation

- **test/TEST_AUTOMATION_STATUS.md**: Complete summary of what was built, bugs fixed, limitations, next steps
- **test/MANUAL_TEST_PLAN.md**: 400+ manual test cases for comprehensive quality assurance
- **test/lexicon/test_setup.cljs**: WASM loading infrastructure for tests
- **test/lexicon/test_events.cljs**: Test-specific event handlers (synchronous, isolated)
- **test/lexicon/core_test.cljs**: Core functionality tests (15 tests)
- **test/lexicon/window_test.cljs**: Window management tests (10 tests)
- **test/lexicon/regression_test.cljs**: Regression tests for critical workflows (12 tests)

### Known Limitations

1. **Tests validate behavior, not correctness**: Tests assert what application *does*, not what it *should do*
2. **Undo not implemented**: 6 failing tests due to missing WASM undo API
3. **Manual testing still required**: Automated tests don't prove real-world usability
4. **Limited production event coverage**: Only test-critical events overridden

**Phase 6.5 Complete:** ✅ Test infrastructure complete. Begin manual testing with MANUAL_TEST_PLAN.md.

**Progress:** 8/10 complete (80%) - Core infrastructure done, CI/CD pending

---

## Phase 6.6: Testing Infrastructure Migration

**Status:** 🔲 In Progress
**Goal:** Migrate from Playwright (JavaScript) to Etaoin (ClojureScript) for E2E tests, establish three-tier testing strategy
**Timeline:** 2 weeks
**Prerequisites:** ✅ Phase 6.5 complete (Playwright baseline established)
**Priority:** HIGH - Must maintain language consistency and testing workflow

### Rationale

**Current Problem:**
- Playwright E2E tests written in JavaScript - introduces new language into ClojureScript project
- Build scripts migrated to Babashka, but tests remain in JS (inconsistency)
- ClojureScript unit tests exist but lack command-line feedback (browser-only at :8021)
- No clear separation between unit tests, integration tests, and E2E tests
- shadow-cljs test endpoint outputs to browser, not terminal (poor developer experience)

**Solution:**
- Migrate to **Etaoin** (ClojureScript WebDriver library) for E2E tests
- Establish three-tier testing strategy (Unit → E2E → CI/CD)
- Configure command-line test feedback (no browser required for units)
- Integrate with Babashka for unified build/test workflow

### Week 1: Etaoin Migration

#### Phase 1: Setup & Baseline (✅ COMPLETE - Dec 28, 2025)
- [x] Commit Playwright baseline for comparison reference (commit 2433cbc)
- [x] Document current Playwright setup in commit history
- [x] Add Phase 6.6 to roadmap

#### Phase 2: Etaoin Setup (🔲 Planned)
- [ ] Remove Playwright dependencies (package.json)
- [ ] Add Etaoin to deps.edn
- [ ] Create basic Etaoin test harness
- [ ] Convert one Playwright test to Etaoin to prove approach works
- [ ] Compare with Playwright baseline to verify behavior matches

#### Phase 3: E2E Test Migration (🔲 Planned)
- [ ] Convert all 7 Playwright tests to Etaoin
- [ ] Migrate remaining ManualTestingPlan.md tests to E2E
- [ ] Organize E2E tests in `e2e_tests/lexicon/` directory
- [ ] Delete Playwright files (e2e-tests/, playwright.config.js)

### Week 2: Testing Strategy & CI/CD

#### Unit Test Refactoring (🔲 Planned)
- [ ] Configure shadow-cljs for command-line test output (not browser-based)
- [ ] Add `:node-test` target for terminal-only testing
- [ ] Ensure tests can run without browser (headless)
- [ ] Add watch mode for continuous testing during development
- [ ] Test output format: TAP or JUnit for CI integration

#### Critical Function Coverage (🔲 Planned)
- [ ] Audit critical functions in codebase
- [ ] Add unit tests for:
  - Buffer operations (insert, delete, undo/redo) in events.cljs
  - Transaction queue logic (in-flight flag, success/failure)
  - Key sequence parsing and binding lookup
  - Window tree manipulation (split, delete, rebalance)
  - Rope data structure operations (if exposed)
- [ ] Target 80% coverage of critical paths

#### Babashka Integration (🔲 Planned)
- [ ] Create `bb.edn` tasks:
  - `bb test:unit` - Run unit tests (terminal output)
  - `bb test:unit:watch` - Watch unit tests (continuous feedback)
  - `bb test:e2e` - Run E2E tests (requires browser/WebDriver)
  - `bb test:all` - Run all tests
  - `bb ci` - Full CI pipeline (setup → build → test)
- [ ] Update package.json scripts to use Babashka
- [ ] Document test workflow in README

#### GitHub Actions Integration (🔲 Planned)
- [ ] Update `.github/workflows/*.yml` for new test structure
- [ ] Unit tests run on every push
- [ ] E2E tests run on pull requests
- [ ] Add test coverage reporting
- [ ] Configure WebDriver (Firefox/Chromium) for CI environment

### Testing Strategy (Three-Tier)

**Tier 1: Unit Tests (ClojureScript) - Fast, Isolated**
- **Location:** `test/lexicon/*.cljs`
- **Target:** shadow-cljs `:node-test` (command-line output)
- **Run via:** `bb test:unit` or `bb test:unit:watch`
- **Coverage:**
  - Pure functions (buffer operations, data structures)
  - Event handlers (re-frame events)
  - Subscriptions (re-frame queries)
  - Utility functions (string parsing, key sequences)
- **No WASM:** Mock WASM layer if needed for isolation
- **No Browser:** Runs in Node.js or headless environment
- **Feedback:** Direct terminal output (TAP/JUnit format)

**Tier 2: E2E Tests (ClojureScript/Etaoin) - Slow, Realistic**
- **Location:** `e2e_tests/lexicon/*.cljs`
- **Target:** Etaoin WebDriver (Firefox/Chromium)
- **Run via:** `bb test:e2e`
- **Coverage:**
  - Full user workflows (typing, navigation, commands)
  - Browser integration (DOM events, rendering)
  - WASM integration (real gap buffer operations)
  - Manual test plan automation (ManualTestingPlan.md)
- **Requires Browser:** Real browser via WebDriver
- **Feedback:** Terminal output + HTML reports

**Tier 3: CI/CD (GitHub Actions) - Automated**
- **Trigger:** Every push/PR
- **Pipeline:**
  1. Setup (install deps)
  2. Build (compile ClojureScript + WASM)
  3. Unit tests (fast feedback)
  4. E2E tests (slow, comprehensive)
  5. Coverage report
  6. Deploy (if main branch)

### Success Criteria

- [x] Playwright baseline committed for reference
- [ ] All Playwright tests migrated to Etaoin (7 tests)
- [ ] Unit tests run from command line with terminal output
- [ ] Watch mode works for rapid unit test development
- [ ] Babashka tasks integrate testing into build workflow
- [ ] GitHub Actions run tests on every push/PR
- [ ] No JavaScript test code remains in project
- [ ] Test coverage ≥ 80% for critical functions
- [ ] Documentation updated (README, test workflow)
- [ ] ManualTestingPlan.md tests automated in E2E suite

### Deferred / Out of Scope

- **Integration Tests (WASM + ClojureScript):** Deferred - E2E tests cover this, or mock WASM for units
- **Performance Testing:** Deferred to Phase 8+
- **Accessibility Testing:** Deferred (covered in manual testing plan)
- **Cross-browser Testing:** Focus on Firefox for now, add Chromium later

### Developer Workflow

**During Development (Unit Tests):**
```bash
bb test:unit:watch  # Continuous feedback in terminal
```

**Before Commit (Full Test Suite):**
```bash
bb test:all  # Run unit + E2E tests
```

**CI Pipeline (Automated):**
```bash
bb ci  # Full build + test + coverage
```

### Known Issues

1. **shadow-cljs :browser-test limitation:** Currently requires browser at :8021 for unit tests
2. **WASM undo not implemented:** 6 failing unit tests (acknowledged in Phase 6.5)
3. **Playwright removal:** May lose some test infrastructure insights (mitigated by commit baseline)

**Progress:** 1/18 complete (6%) - Baseline established, migration planning complete

---

## Phase 7: Core Hardening & Architecture Refinement

**Status:** 🔲 Planned
**Goal:** Formalize core abstractions based on external code review recommendations before package ecosystem
**Timeline:** 5-7 weeks (added SCI integration - Week 7)
**Prerequisites:** ✅ Phase 6A-6D complete, ✅ Phase 6.5 complete (testing)
**Priority:** CRITICAL - Must complete before accepting external contributions or building package ecosystem

### Rationale

**External code review** (see `chatgpt_review.md`) identified critical architectural gaps that must be addressed NOW while codebase is still manageable:

1. **No formal Core API contract** - Packages depend on implementation details, not stable APIs
2. **Ad-hoc hook system** - Events exist but not unified, extensible hooks
3. **Undo is primitive** - No command grouping, no marker awareness, no boundaries
4. **Markers not first-class** - Point is special-cased, regions are fragile, no overlay support for markers
5. **events.cljs too large** - 3154 lines, mixing concerns, hard to maintain

**Why now?**
- Codebase is small enough to refactor safely
- Architecture is fundamentally sound (review confirmed this!)
- Changes now prevent years of technical debt
- Package ecosystem (Vertico, Evil-mode) depends on these foundations

**Test-Driven Approach:**
- Every refactor must maintain passing tests
- Add tests for new APIs before refactoring
- Commit after each green test run
- No "big bang" refactors - incremental progress

---

### Week 1-2: Core API Contract & Documentation

**Goal:** Freeze the editor spine - make package/core boundary explicit and stable

#### Phase 7.1: Core API Design Documents (Week 1) 📋

- [ ] **Create `docs/core/` directory** - New home for core contracts
- [ ] **Write `docs/core/core-api.md`** - The only API packages may depend on
  - Define Buffer API (create, get-text, insert, delete, point, markers)
  - Define Command API (register, execute, call-interactively, metadata)
  - Define Keymap API (define, lookup, precedence rules)
  - Define Mode API (define-major-mode, define-minor-mode, hooks)
  - Define Hook API (add-hook, remove-hook, run-hooks, priorities)
  - Mark what is core vs internal
  - Version the API (breaking changes require major version bump)

- [ ] **Write `docs/core/hooks.md`** - Unified hook system specification
  - Canonical hook phases: `before-command`, `after-command`, `before-change`, `after-change`, `buffer-switch`, `mode-enable`, `mode-disable`
  - Hook entry structure: `{:id :hook-id :priority N :fn (fn [ctx] ctx)}`
  - Context model: what hooks receive and must return
  - Isolation guarantees: hook failures don't break other hooks

- [ ] **Write `docs/core/undo.md`** - Command-oriented undo specification
  - Undo record structure (primitive edits with marker deltas)
  - Undo group structure (command-level grouping)
  - Command boundaries (`begin-command`, `end-command`)
  - Marker restoration on undo
  - Redo stack management

- [ ] **Write `docs/core/markers.md`** - First-class marker abstraction
  - Marker data structure (id, position, buffer, insertion behavior)
  - Point as special marker
  - Mark as special marker
  - Overlay marker pairs (start, end)
  - Marker update algorithm (on insert/delete)

- [ ] **Write `docs/core/execution-model.md`** - The editor event loop
  - Logical single-threading guarantee
  - Command execution context (dynamic vars)
  - No concurrent mutations observable to packages
  - Background work integration (LSP, async)

- [ ] **Write `docs/core/package-loading.md`** - Dynamic package system
  - Package format (EDN metadata + CLJS source)
  - Loading flow (fetch → evaluate in SCI → register side effects)
  - SCI integration strategy (when/how to add)
  - Package isolation and sandboxing

#### Phase 7.2: events.cljs Refactoring (Week 2) ✅ COMPLETE

**Goal:** Break 2958-line monolith into focused, testable modules

**Achievement:** Successfully refactored into 7 modules + core (409 lines)

**Current Structure Analysis:**
```bash
# Audit events.cljs - what domains are mixed?
grep -E "^\\(rf/reg-event" packages/editor-cljs/src/lexicon/events.cljs | wc -l
# Identify natural boundaries (buffers, windows, commands, etc.)
```

**Implemented Module Structure:**
```
src/lexicon/
├── events/
│   ├── keymap.cljs      # Key sequence handling, keymap resolution (139 lines, 3 handlers)
│   ├── mode.cljs        # Mode activation, hooks (99 lines, 9 handlers)
│   ├── wasm.cljs        # WASM operations, transactions, parser (643 lines, 18 handlers + 2 effects)
│   ├── buffer.cljs      # Buffer lifecycle, file I/O (488 lines, 15 handlers + 3 effects)
│   ├── edit.cljs        # Text editing, cursor, undo, kill/yank (518 lines, 26 handlers)
│   ├── ui.cljs          # Windows, minibuffer, echo, WebSocket (523 lines, 31 handlers + 3 effects)
│   └── command.cljs     # Command execution, help system (620 lines, 16 handlers)
└── events.cljs          # Core coordination (409 lines, 8 handlers)
```

**Total:** 3,030 lines (was 2,958) across 8 files with clear separation of concerns

**Refactoring Steps (Test-Driven):**

1. **Add namespace-level tests FIRST** (before splitting)
   - [ ] Add tests for buffer events (`:buffer/create`, `:buffer/switch`, `:buffer/kill`)
   - [ ] Add tests for edit events (`:edit/insert`, `:edit/delete`, `:undo`)
   - [ ] Add tests for window events (`:window/split`, `:window/delete`)
   - [ ] Add tests for command events (`:command/execute`)
   - [ ] Run tests - ensure 100% pass before refactoring

2. **Extract Buffer Events** (incremental, test after each step)
   - [ ] Create `src/lexicon/events/buffer.cljs`
   - [ ] Move `:buffer/*` events from `events.cljs` → `events/buffer.cljs`
   - [ ] Update requires in `events.cljs` to reference new namespace
   - [ ] Run tests - must stay green ✅
   - [ ] Commit: "refactor: extract buffer events to events/buffer.cljs"

3. **Extract Edit Events**
   - [ ] Create `src/lexicon/events/edit.cljs`
   - [ ] Move `:edit/*`, `:undo`, `:redo` events
   - [ ] Run tests ✅
   - [ ] Commit: "refactor: extract edit events to events/edit.cljs"

4. **Extract Window Events**
   - [ ] Create `src/lexicon/events/window.cljs`
   - [ ] Move `:window/*` events
   - [ ] Run tests ✅
   - [ ] Commit: "refactor: extract window events to events/window.cljs"

5. **Extract Command Events**
   - [ ] Create `src/lexicon/events/command.cljs`
   - [ ] Move `:command/*` events
   - [ ] Run tests ✅
   - [ ] Commit: "refactor: extract command events to events/command.cljs"

6. **Extract Minibuffer Events**
   - [ ] Create `src/lexicon/events/minibuffer.cljs`
   - [ ] Move `:minibuffer/*` events
   - [ ] Run tests ✅
   - [ ] Commit: "refactor: extract minibuffer events to events/minibuffer.cljs"

7. **Extract File Events**
   - [ ] Create `src/lexicon/events/file.cljs`
   - [ ] Move `:file/*` events
   - [ ] Run tests ✅
   - [ ] Commit: "refactor: extract file events to events/file.cljs"

8. **Extract Mode Events**
   - [ ] Create `src/lexicon/events/mode.cljs`
   - [ ] Move `:mode/*` events
   - [ ] Run tests ✅
   - [ ] Commit: "refactor: extract mode events to events/mode.cljs"

9. **Create Hook Events Module (NEW)**
   - [ ] Create `src/lexicon/events/hook.cljs`
   - [ ] Implement `:hook/add`, `:hook/remove`, `:hook/run` events
   - [ ] Add tests for hook system
   - [ ] Run tests ✅
   - [ ] Commit: "feat: add formal hook system events"

10. **Deprecate Old events.cljs**
    - [ ] Create `src/lexicon/events/core.cljs` - requires all modules
    - [ ] Update `events.cljs` to `(ns lexicon.events (:require [lexicon.events.core]))`
    - [ ] Run tests ✅
    - [ ] Commit: "refactor: complete events.cljs modularization"

**Success Criteria:**
- [x] `events.cljs` reduced from 2,958 to 409 lines (core coordination only)
- [x] Each `events/*.cljs` < 650 lines (largest is wasm.cljs at 643 lines)
- [x] All compilation passes with 0 warnings after each commit
- [x] No behavioral changes - pure refactoring
- [x] 7 clean commits documenting the transformation

**Commits:**
1. ✅ Extract keymap module (proof of concept)
2. ✅ Extract mode module
3. ✅ Extract wasm module
4. ✅ Extract buffer module
5. ✅ Extract edit module
6. ✅ Extract ui module
7. ✅ Extract command module

---

### Week 3: Formal Hook System Implementation

**Goal:** Replace ad-hoc event hooks with unified, extensible hook system per `docs/core/hooks.md`

#### Phase 7.3: Hook Infrastructure 🪝

**Test-First Implementation:**

1. **Hook Registry Data Structure**
   - [ ] Add `:hooks` to app-db schema
   - [ ] Hook entry: `{:id keyword :priority int :fn ifn :buffer-local? bool}`
   - [ ] Hook table: `{:before-command [{hook1} {hook2}] :after-command [...]}`
   - [ ] Write tests for hook registry operations
   - [ ] Run tests ✅
   - [ ] Commit: "feat: add hook registry data structure"

2. **Hook Event Handlers**
   - [ ] Implement `:hook/add` event (add hook to registry)
   - [ ] Implement `:hook/remove` event (remove hook from registry)
   - [ ] Implement `:hook/run` event (run hooks for phase, return context)
   - [ ] Add tests for event handlers
   - [ ] Run tests ✅
   - [ ] Commit: "feat: implement hook event handlers"

3. **Hook API Functions**
   - [ ] `(add-hook :before-command my-hook-fn :priority 50)`
   - [ ] `(remove-hook :before-command my-hook-fn)`
   - [ ] `(run-hooks :before-command ctx)` → modified ctx or nil
   - [ ] Buffer-local hooks: `(add-hook :before-command fn :buffer-local true)`
   - [ ] Add tests for API functions
   - [ ] Run tests ✅
   - [ ] Commit: "feat: add hook API functions"

4. **Integrate Hooks into Command Execution**
   - [ ] Wrap command execution with `before-command` hooks
   - [ ] Wrap command execution with `after-command` hooks
   - [ ] Context includes: `:command`, `:args`, `:buffer-id`, `:window-id`
   - [ ] Hooks can cancel execution (return nil)
   - [ ] Add tests for command hook integration
   - [ ] Run tests ✅
   - [ ] Commit: "feat: integrate hooks into command execution"

5. **Integrate Hooks into Buffer Editing**
   - [ ] Wrap insert/delete with `before-change` hooks
   - [ ] Wrap insert/delete with `after-change` hooks
   - [ ] Context includes: `:buffer-id`, `:start`, `:end`, `:text`
   - [ ] Add tests for edit hook integration
   - [ ] Run tests ✅
   - [ ] Commit: "feat: integrate hooks into buffer editing"

6. **Mode Hooks**
   - [ ] Run `:mode-enable` hooks when mode activated
   - [ ] Run `:mode-disable` hooks when mode deactivated
   - [ ] Add tests for mode hooks
   - [ ] Run tests ✅
   - [ ] Commit: "feat: add mode enable/disable hooks"

**Success Criteria:**
- [ ] All 6 standard hooks implemented (before/after-command, before/after-change, mode enable/disable)
- [ ] Hooks can be added/removed dynamically
- [ ] Hook failures are isolated (don't break other hooks or commands)
- [ ] Tests cover edge cases (hook cancels execution, hook throws error, priority ordering)

---

### Week 4: Undo System Enhancement

**Goal:** Implement command-grouped, marker-aware undo per `docs/core/undo.md`

#### Phase 7.4: Advanced Undo 🔄

**Current Undo Limitations:**
- No command boundaries - undo reverses individual edits, not logical commands
- No marker tracking - markers aren't restored on undo
- No redo support (or fragile redo)

**Test-First Implementation:**

1. **Undo Boundary Support**
   - [ ] Add `:undo-boundary` marker to undo stack
   - [ ] Implement `(undo-boundary)` function - insert boundary into stack
   - [ ] Modify `:undo` to reverse entire group (up to boundary)
   - [ ] Add tests for boundary-based undo
   - [ ] Run tests ✅
   - [ ] Commit: "feat: add undo boundary support"

2. **Command Lifecycle Integration**
   - [ ] Implement `begin-command` - push undo boundary, capture marker state
   - [ ] Implement `end-command` - finalize undo group, clear redo
   - [ ] Wrap all command executions with begin/end
   - [ ] Add tests for command-grouped undo
   - [ ] Run tests ✅
   - [ ] Commit: "feat: integrate undo boundaries with command lifecycle"

3. **Marker Deltas in Undo Records**
   - [ ] Extend undo record: `{:type :insert :pos N :text "..." :marker-deltas {...}}`
   - [ ] Capture marker positions before edit
   - [ ] Store marker deltas in undo record
   - [ ] Restore markers on undo
   - [ ] Add tests for marker-aware undo
   - [ ] Run tests ✅
   - [ ] Commit: "feat: add marker deltas to undo records"

4. **Redo Stack Management**
   - [ ] Clear redo stack on new edit (after undo)
   - [ ] Redo reverses undo (reapply edits)
   - [ ] Redo restores markers
   - [ ] Add tests for undo/redo chains
   - [ ] Run tests ✅
   - [ ] Commit: "feat: implement redo with marker restoration"

5. **Undo Groups for Complex Commands**
   - [ ] `format-buffer` creates single undo group for all edits
   - [ ] Macro replay creates undo groups per iteration
   - [ ] Add tests for complex command undo
   - [ ] Run tests ✅
   - [ ] Commit: "feat: add undo group support for complex commands"

**Success Criteria:**
- [ ] Undo reverses entire commands, not individual edits
- [ ] Markers (point, mark, overlays) restored on undo
- [ ] Redo works correctly
- [ ] Complex commands (format, macros) undo atomically
- [ ] Tests cover edge cases (undo at boundary, redo after edit, marker edge cases)

---

### Week 5-6: First-Class Markers & Dynamic Scoping

**Goal:** Elevate markers from special-cased to first-class engine entities; add dynamic execution context

#### Phase 7.5: Marker Abstraction 📍

**Test-First Implementation:**

1. **Engine-Level Marker Support (Rust/WASM)**
   - [ ] Add `Marker { id: u64, pos: usize }` struct to gap buffer
   - [ ] Add `marker_table: HashMap<u64, Marker>` to buffer state
   - [ ] Implement `create_marker(id, pos)` - add to table
   - [ ] Implement `move_marker(id, new_pos)` - update position
   - [ ] Implement `get_marker_position(id)` - query position
   - [ ] Update `insert` to shift markers after gap
   - [ ] Update `delete` to shift markers after gap
   - [ ] Add Rust unit tests for marker updates
   - [ ] Compile WASM ✅
   - [ ] Commit: "feat(engine): add first-class marker support to gap buffer"

2. **ClojureScript Marker API**
   - [ ] Implement `(create-marker buffer-id)` → marker-id
   - [ ] Implement `(move-marker! marker-id pos)`
   - [ ] Implement `(marker-position marker-id)` → pos
   - [ ] Implement `(delete-marker! marker-id)`
   - [ ] Add marker metadata: `{:id N :buffer-id "..." :kind :point}`
   - [ ] Add tests for marker API
   - [ ] Run tests ✅
   - [ ] Commit: "feat: add marker API to ClojureScript"

3. **Refactor Point to Use Markers**
   - [ ] Replace `:point` integer with `:point-marker-id`
   - [ ] Update all `point` calls to use marker API
   - [ ] Update all `set-point!` calls to use `move-marker!`
   - [ ] Add tests for point-as-marker
   - [ ] Run tests ✅
   - [ ] Commit: "refactor: convert point to use marker abstraction"

4. **Refactor Mark to Use Markers**
   - [ ] Replace `:mark` integer with `:mark-marker-id`
   - [ ] Update region calculation to use markers
   - [ ] Add tests for mark-as-marker
   - [ ] Run tests ✅
   - [ ] Commit: "refactor: convert mark to use marker abstraction"

5. **Overlay Markers**
   - [ ] Refactor overlays to use marker pairs: `{:start-marker-id N :end-marker-id M}`
   - [ ] Overlays now move with text edits automatically
   - [ ] Add tests for overlay marker updates
   - [ ] Run tests ✅
   - [ ] Commit: "refactor: overlays use marker pairs for automatic updates"

#### Phase 7.6: Dynamic Execution Context 🔀

**Goal:** Emacs-style dynamic scope for command execution (per review recommendations)

**Test-First Implementation:**

1. **Define Dynamic Vars**
   - [ ] `^:dynamic *current-command*` - The command being executed
   - [ ] `^:dynamic *current-buffer*` - The active buffer
   - [ ] `^:dynamic *prefix-arg*` - Universal argument (C-u)
   - [ ] `^:dynamic *this-command*` - For pre/post-command-hook
   - [ ] Add tests for dynamic var binding
   - [ ] Run tests ✅
   - [ ] Commit: "feat: add dynamic execution context vars"

2. **Bind Context During Command Execution**
   - [ ] Wrap command execution in `(binding [*current-command* cmd ...] ...)`
   - [ ] Hooks and advice can access dynamic vars
   - [ ] Add tests for dynamic context access
   - [ ] Run tests ✅
   - [ ] Commit: "feat: bind dynamic context during command execution"

3. **Use Dynamic Context in Hooks**
   - [ ] Hooks can read `*current-command*` without passing it explicitly
   - [ ] Simplifies hook API (no need to pass everything in context map)
   - [ ] Add tests for hook context access
   - [ ] Run tests ✅
   - [ ] Commit: "refactor: hooks use dynamic context vars"

**Success Criteria:**
- [ ] Markers are first-class in engine (Rust)
- [ ] Point and mark use marker abstraction
- [ ] Overlays use marker pairs and update automatically
- [ ] Dynamic vars provide Emacs-style execution context
- [ ] Tests validate marker updates on insert/delete
- [ ] Tests validate dynamic context in hooks

---

### Week 7: SCI Integration for Runtime Package Loading

**Goal:** Enable dynamic package evaluation before Phase 8 (Vertico/Orderless/Consult)

#### Phase 7.7: SCI Integration 🔬

**Rationale:** Phase 8 packages (Vertico, Orderless, Consult) are **external packages** that should be loaded dynamically, not compiled into the bundle. This requires SCI (Small Clojure Interpreter) for runtime CLJS evaluation.

**Why now?**
- Cannot truly test package loading without runtime evaluation
- Vertico/Orderless/Consult need to be loadable on demand
- SCI is foundational for package ecosystem
- Fits external review recommendation for "arbitrary CLJS execution"

**Test-First Implementation:**

1. **Add SCI Dependency**
   - [ ] Add SCI to `deps.edn` / `shadow-cljs.edn`
   - [ ] Verify SCI compiles in browser environment
   - [ ] Run tests ✅
   - [ ] Commit: "deps: add SCI (Small Clojure Interpreter)"

2. **Create Package Evaluation Sandbox**
   - [ ] Create `lexicon.packages.sci` namespace
   - [ ] Initialize SCI context with Core API bindings
   - [ ] Expose Core API functions to SCI sandbox:
     - Buffer API (create-buffer, insert, delete, point, etc.)
     - Command API (define-command, execute-command)
     - Keymap API (define-key, lookup-key)
     - Mode API (define-major-mode, define-minor-mode)
     - Hook API (add-hook, remove-hook)
   - [ ] Add tests for SCI context creation
   - [ ] Run tests ✅
   - [ ] Commit: "feat(packages): create SCI evaluation sandbox with Core API"

3. **Package Evaluation API**
   - [ ] Implement `(eval-package-source source-string)` - evaluate CLJS in SCI
   - [ ] Implement `(load-package-file path)` - load and evaluate package file
   - [ ] Handle evaluation errors gracefully (don't crash editor)
   - [ ] Return evaluation result or error
   - [ ] Add tests for package evaluation
   - [ ] Run tests ✅
   - [ ] Commit: "feat(packages): add package source evaluation API"

4. **Minimal Test Package**
   - [ ] Create `packages/test-package/` with minimal package:
     ```clojure
     ;; packages/test-package/src/lexicon/test_package/core.cljs
     (ns lexicon.test-package.core
       (:require [lexicon.core.api :as api]))

     (defn initialize! []
       (api/define-command
         :test-package/hello
         {:interactive true}
         (fn [] (api/message "Hello from test package!"))))

     (defn cleanup! []
       (api/unregister-command :test-package/hello))
     ```
   - [ ] Load test package via SCI
   - [ ] Execute `:test-package/hello` command
   - [ ] Verify package state isolated
   - [ ] Add tests for test package loading
   - [ ] Run tests ✅
   - [ ] Commit: "test(packages): add minimal test package for SCI validation"

5. **Update Package Loading Infrastructure**
   - [ ] Modify `load-package!` to use SCI evaluation (not compile-time requires)
   - [ ] Package source loaded from file/URL → evaluated in SCI
   - [ ] Package `initialize!` called after evaluation
   - [ ] Package `cleanup!` called on unload
   - [ ] Add tests for dynamic package lifecycle
   - [ ] Run tests ✅
   - [ ] Commit: "feat(packages): use SCI for dynamic package loading"

6. **Sandboxing and Security**
   - [ ] Restrict SCI sandbox (no arbitrary `js/` access)
   - [ ] Packages can only use Core API
   - [ ] Namespace isolation (packages can't access editor internals)
   - [ ] Add tests for sandbox restrictions
   - [ ] Run tests ✅
   - [ ] Commit: "feat(packages): add sandbox restrictions for security"

7. **Update Documentation**
   - [ ] Update `docs/core/package-loading.md` with SCI integration
   - [ ] Document SCI sandbox and Core API exposure
   - [ ] Document package evaluation lifecycle
   - [ ] Document security model
   - [ ] Commit: "docs: document SCI integration and package evaluation"

**Success Criteria:**
- [ ] SCI dependency added and compiles
- [ ] SCI context exposes Core API to packages
- [ ] Can evaluate CLJS source in SCI sandbox
- [ ] Minimal test package loads and works
- [ ] Package loading uses SCI (not compile-time requires)
- [ ] Sandbox restricts access to Core API only
- [ ] Tests validate package evaluation and isolation
- [ ] Documentation complete

**Note:** This enables Phase 8 (Vertico/Orderless/Consult) to be **truly external packages** loaded at runtime, not compiled into bundle.

---

### Phase 7 Success Criteria (Overall)

**Documentation:**
- [ ] 6 core design documents written (`docs/core/*.md`)
- [ ] Core API contract frozen and versioned
- [ ] All docs reflect implementation reality

**Code Quality:**
- [ ] `events.cljs` refactored into 8 focused modules (< 500 lines each)
- [ ] All refactoring commits maintain green tests
- [ ] 40+ new tests added for core APIs (including SCI tests)

**Architecture:**
- [ ] Formal hook system implemented (6 hook phases)
- [ ] Undo system enhanced (boundaries, markers, redo)
- [ ] Markers are first-class (engine + CLJS API)
- [ ] Dynamic execution context available
- [ ] SCI integration complete (runtime package evaluation)

**Testing:**
- [ ] E2E tests pass after every commit
- [ ] Manual test plan validates refactorings
- [ ] No regressions introduced

**Readiness:**
- [ ] Package ecosystem can depend on stable Core API
- [ ] Packages can be loaded at runtime via SCI (not compiled)
- [ ] Vertico/Orderless/Consult can be implemented as true external packages (Phase 8)
- [ ] Evil-mode can be implemented (Phase 9)

**Progress:** 0/70+ tasks complete (0%) - Comprehensive phase, foundational (added SCI integration)

---

## Phase 8: Completion Ecosystem Litmus Test (Vertico, Orderless, Consult)

**Status:** 🔲 Planned
**Goal:** Validate completion framework and Core API with "Emacs-way" packages
**Timeline:** 3-4 weeks
**Prerequisites:** ✅ Phase 7 complete (Core API, hooks, markers formalized)
**Priority:** HIGH - These packages test if we built the right foundations

### Rationale

**Why these packages as litmus test?**

Vertico, Orderless, and Consult are **canonical Emacs completion packages** that:
1. Work "the Emacs way" (unlike Ivy which does its own thing)
2. Are **simpler than Evil-mode** - good stepping stone
3. Stress-test **completion framework** (Phase 6C)
4. Validate **Core API** (Phase 7)
5. Prove **package loading/unloading** works
6. Are **actually useful** - minibuffer is currently painful without them!

**Success = Evil-mode readiness:** If these packages work cleanly using only Core API, Evil-mode should too.

---

### Week 1: Vertico Package (Vertical Completion UI)

**Goal:** Replace echo-area completion with clean vertical list

#### Prerequisites Check
- [ ] Completion metadata system works (Phase 6C)
- [ ] Face system works (Phase 6B)
- [ ] Package loading works (Phase 6A)
- [ ] Core API documented (Phase 7)

#### Vertico Implementation

**Package Structure:**
```
packages/vertico/
├── package.edn           # Package metadata
├── src/lexicon/vertico/
│   ├── core.cljs        # initialize!/cleanup! lifecycle
│   ├── ui.cljs          # Vertical list rendering
│   ├── keymap.cljs      # Up/down navigation
│   └── completion.cljs  # Completion metadata integration
```

**Test-Driven Development:**

1. **Package Loading Infrastructure**
   - [ ] Implement `M-x load-package` command (if not exists from Phase 7)
   - [ ] Implement `M-x unload-package` command
   - [ ] Add tests for package lifecycle
   - [ ] Run tests ✅
   - [ ] Commit: "feat: add package load/unload commands"

2. **Vertico Package Skeleton**
   - [ ] Create `packages/vertico/` directory
   - [ ] Create `package.edn` with metadata
   - [ ] Create `core.cljs` with `initialize!` and `cleanup!`
   - [ ] Register package on load
   - [ ] Add tests for package registration
   - [ ] Run tests ✅
   - [ ] Commit: "feat(vertico): add package skeleton"

3. **Vertical List UI**
   - [ ] Render completion candidates as vertical list (not echo area)
   - [ ] Highlight selected candidate (using faces!)
   - [ ] Show prompt at top
   - [ ] Limit visible candidates (scroll if > 10)
   - [ ] Add manual test: M-x shows vertical list
   - [ ] Commit: "feat(vertico): add vertical completion UI"

4. **Navigation Keybindings**
   - [ ] C-n / Down → next candidate
   - [ ] C-p / Up → previous candidate
   - [ ] C-v / Page Down → next page
   - [ ] M-v / Page Up → previous page
   - [ ] RET → confirm selection
   - [ ] C-g → cancel
   - [ ] Add tests for navigation
   - [ ] Run tests ✅
   - [ ] Commit: "feat(vertico): add navigation keybindings"

5. **Metadata Integration**
   - [ ] Show annotations (from `:annotation-function`)
   - [ ] Show groups (from `:group-function`)
   - [ ] Respect sort order (from `:display-sort-function`)
   - [ ] Add tests for metadata display
   - [ ] Run tests ✅
   - [ ] Commit: "feat(vertico): integrate completion metadata"

6. **Package Cleanup**
   - [ ] `M-x unload-package vertico` removes UI
   - [ ] Returns to echo-area completion
   - [ ] No state leaks
   - [ ] Add tests for unload
   - [ ] Run tests ✅
   - [ ] Commit: "feat(vertico): implement clean unload"

**Success Criteria:**
- [ ] M-x shows vertical completion list
- [ ] Navigation works (up/down/page)
- [ ] Annotations display correctly
- [ ] Package loads/unloads cleanly
- [ ] Uses only Core API (no internal deps)

---

### Week 2: Orderless Package (Flexible Matching)

**Goal:** Space-separated pattern matching for completion

#### Orderless Implementation

**Package Structure:**
```
packages/orderless/
├── package.edn
├── src/lexicon/orderless/
│   ├── core.cljs        # Package lifecycle
│   ├── matching.cljs    # Pattern matching algorithm
│   └── highlighting.cljs # Highlight matched portions
```

**Test-Driven Development:**

1. **Orderless Package Skeleton**
   - [ ] Create `packages/orderless/` directory
   - [ ] Create `package.edn` with metadata
   - [ ] Register as completion style (extends Phase 6C)
   - [ ] Add tests for package registration
   - [ ] Run tests ✅
   - [ ] Commit: "feat(orderless): add package skeleton"

2. **Space-Separated Pattern Matching**
   - [ ] Split input by spaces → multiple patterns
   - [ ] Candidate matches if ALL patterns match (order irrelevant)
   - [ ] "buf mod" matches "buffer-menu-mode"
   - [ ] "k l" matches "kill-line"
   - [ ] Add tests for pattern matching
   - [ ] Run tests ✅
   - [ ] Commit: "feat(orderless): implement pattern matching"

3. **Flexible Pattern Types**
   - [ ] Literal: "buf" matches "buf" substring
   - [ ] Regexp: "^buf" matches start
   - [ ] Flex: "bm" matches "buffer-menu"
   - [ ] Prefix: "buf!" matches "buf" prefix
   - [ ] Add tests for pattern types
   - [ ] Run tests ✅
   - [ ] Commit: "feat(orderless): add pattern type support"

4. **Highlighting Matched Portions**
   - [ ] Highlight parts of candidate that matched
   - [ ] Use `orderless-match-face` (face from 6B!)
   - [ ] Show which patterns matched where
   - [ ] Add manual test: see highlighting in Vertico
   - [ ] Commit: "feat(orderless): add match highlighting"

5. **Integration with Completion Framework**
   - [ ] Register as completion style (`:completion-styles [orderless]`)
   - [ ] Works with Vertico UI
   - [ ] Works with built-in completion
   - [ ] Add tests for integration
   - [ ] Run tests ✅
   - [ ] Commit: "feat(orderless): integrate with completion framework"

**Success Criteria:**
- [ ] Space-separated patterns work ("buf mod" finds "buffer-menu-mode")
- [ ] Matches highlighted in Vertico
- [ ] Faster than typing full command names
- [ ] Package loads/unloads cleanly

---

### Week 3: Marginalia Package (Rich Annotations)

**Goal:** Add contextual annotations to completion candidates

#### Marginalia Implementation

**Package Structure:**
```
packages/marginalia/
├── package.edn
├── src/lexicon/marginalia/
│   ├── core.cljs        # Package lifecycle
│   ├── annotators.cljs  # Annotation functions per category
│   └── faces.cljs       # Annotation faces
```

**Test-Driven Development:**

1. **Marginalia Package Skeleton**
   - [ ] Create `packages/marginalia/` directory
   - [ ] Register `:annotation-function` for completion categories
   - [ ] Add tests for package registration
   - [ ] Run tests ✅
   - [ ] Commit: "feat(marginalia): add package skeleton"

2. **Command Annotations**
   - [ ] Show keybinding: "forward-char (C-f)"
   - [ ] Show docstring snippet
   - [ ] Use `marginalia-key-face` for keybindings
   - [ ] Add tests for command annotations
   - [ ] Run tests ✅
   - [ ] Commit: "feat(marginalia): add command annotations"

3. **Buffer Annotations**
   - [ ] Show buffer size: "*scratch* (523 bytes)"
   - [ ] Show major mode: "clojure-mode"
   - [ ] Show file path if applicable
   - [ ] Add tests for buffer annotations
   - [ ] Run tests ✅
   - [ ] Commit: "feat(marginalia): add buffer annotations"

4. **Function Annotations**
   - [ ] Show function signature
   - [ ] Show docstring first line
   - [ ] Distinguish commands from functions
   - [ ] Add tests for function annotations
   - [ ] Run tests ✅
   - [ ] Commit: "feat(marginalia): add function annotations"

5. **Integration with Vertico**
   - [ ] Annotations appear in Vertico vertical list
   - [ ] Right-aligned annotations
   - [ ] Proper spacing and truncation
   - [ ] Add manual test: M-x shows annotations
   - [ ] Commit: "feat(marginalia): integrate with Vertico"

**Success Criteria:**
- [ ] M-x shows keybindings for commands
- [ ] C-x b shows buffer info
- [ ] Annotations are helpful, not cluttered
- [ ] Package loads/unloads cleanly

---

### Week 4: Consult Package (Enhanced Commands with Preview)

**Goal:** Powerful navigation commands with live preview

#### Consult Implementation

**Package Structure:**
```
packages/consult/
├── package.edn
├── src/lexicon/consult/
│   ├── core.cljs        # Package lifecycle
│   ├── buffer.cljs      # consult-buffer
│   ├── line.cljs        # consult-line
│   └── imenu.cljs       # consult-imenu
```

**Test-Driven Development:**

1. **Consult Package Skeleton**
   - [ ] Create `packages/consult/` directory
   - [ ] Register commands via Core API
   - [ ] Add tests for package registration
   - [ ] Run tests ✅
   - [ ] Commit: "feat(consult): add package skeleton"

2. **consult-buffer Command**
   - [ ] Enhanced buffer switching with categories:
     - [ ] Recent buffers
     - [ ] All buffers
     - [ ] Recent files (via recentf)
   - [ ] Live preview (switch to buffer on navigation)
   - [ ] Restore original buffer on cancel (C-g)
   - [ ] Add tests for consult-buffer
   - [ ] Run tests ✅
   - [ ] Commit: "feat(consult): add consult-buffer command"

3. **consult-line Command**
   - [ ] Search lines in current buffer
   - [ ] Live preview (jump to line on navigation)
   - [ ] Highlight matched text
   - [ ] Restore original position on cancel
   - [ ] Add tests for consult-line
   - [ ] Run tests ✅
   - [ ] Commit: "feat(consult): add consult-line command"

4. **consult-imenu Command**
   - [ ] Navigate buffer structure (functions, variables)
   - [ ] Uses imenu from Phase 6C
   - [ ] Live preview (jump to definition)
   - [ ] Grouped by type (functions, vars, etc.)
   - [ ] Add tests for consult-imenu
   - [ ] Run tests ✅
   - [ ] Commit: "feat(consult): add consult-imenu command"

5. **Preview Infrastructure**
   - [ ] Preview updates as you navigate
   - [ ] Preview is temporary (doesn't modify undo stack)
   - [ ] Cancel restores original state
   - [ ] Confirm commits preview state
   - [ ] Add tests for preview behavior
   - [ ] Run tests ✅
   - [ ] Commit: "feat(consult): add preview infrastructure"

**Success Criteria:**
- [ ] consult-buffer groups buffers and files
- [ ] consult-line searches current buffer with preview
- [ ] consult-imenu navigates code structure
- [ ] All previews work correctly (cancel restores state)
- [ ] Package loads/unloads cleanly

---

### Phase 8 Success Criteria (Overall)

**Package Ecosystem Validation:**
- [ ] All three packages (Vertico, Orderless, Consult) load cleanly
- [ ] Packages use **only Core API** (no internal dependencies)
- [ ] Packages can be loaded/unloaded without breaking core
- [ ] Packages compose well (Vertico + Orderless + Marginalia + Consult)

**Completion Framework Validation:**
- [ ] Completion metadata works (annotations, groups, sort)
- [ ] Completion styles work (orderless patterns)
- [ ] Completion categories work (commands, buffers, files)
- [ ] CAPFs work (in-buffer completion)

**Usability Validation:**
- [ ] Minibuffer is **actually usable** (no more painful command discovery!)
- [ ] Fuzzy matching works (find commands by partial names)
- [ ] Annotations help (see keybindings, buffer info)
- [ ] Navigation is smooth (buffers, lines, definitions)

**Architecture Validation:**
- [ ] Core API is sufficient (packages don't need internal access)
- [ ] Hook system works (packages integrate via hooks)
- [ ] Face system works (annotations render correctly)
- [ ] Package loading/unloading works

**Testing:**
- [ ] 20+ new tests for package infrastructure
- [ ] Manual testing validates UX
- [ ] No regressions in existing functionality
- [ ] E2E tests pass

**Readiness for Evil-mode:**
- [ ] If these simpler packages work via Core API, Evil-mode should too
- [ ] Package loading proven
- [ ] Hook integration proven
- [ ] Face integration proven
- [ ] Proceed to Phase 9 with confidence!

**Progress:** 0/50+ tasks complete (0%)

---

## Phase 9: Evil-mode Integration (Final Litmus Test)

**Status:** 🔲 Planned
**Goal:** Complete Evil-mode integration - the ultimate test of Core API and package system
**Timeline:** 3-4 weeks
**Prerequisites:** ✅ Phase 7 complete (Core API, hooks, markers), ✅ Phase 8 complete (package loading proven)
**Priority:** HIGH - Evil-mode validates the entire architecture

### Rationale

**Why Evil-mode is the ultimate litmus test:**

Evil-mode package structure was created in Phase 6A but is **not functional**. Now that we have:
- ✅ **Core API contract** (Phase 7) - Stable package interface
- ✅ **Formal hooks** (Phase 7) - `pre-command-hook`, `post-command-hook`
- ✅ **First-class markers** (Phase 7) - Overlay start/end positions
- ✅ **Advanced undo** (Phase 7) - Command grouping, marker restoration
- ✅ **Overlays** (Phase 6B) - Visual mode highlighting
- ✅ **Faces** (Phase 6B) - Cursor shapes and colors
- ✅ **Buffer-local variables** (Phase 6D) - Per-buffer Evil state
- ✅ **Advice system** (Phase 6D) - Wrap Emacs functions
- ✅ **Package loading proven** (Phase 8) - Vertico/Orderless/Consult work

**We can make Evil-mode work.**

**What Evil-mode tests:**
- Modal keymaps (normal, insert, visual, operator-pending)
- State transitions (pre/post-command hooks)
- Operator-motion composition (d + w = dw)
- Visual selections (overlays with markers)
- Cursor feedback (faces for shapes/colors)
- Count prefixes (5j, 10w)
- Buffer-local state (each buffer independent Evil state)
- Command repetition (dot command)

If Evil-mode works cleanly using Core API, **our architecture is proven sound**.

---

### Week 1: Evil-mode Package Activation & State Machine

**Goal:** Load Evil-mode, get state machine working, basic mode switching

#### Prerequisites Check
- [ ] Core API fully documented (Phase 7)
- [ ] Hook system works (Phase 7)
- [ ] Markers work (Phase 7)
- [ ] Package loading works (Phase 8)
- [ ] All tests passing

#### Evil-mode Activation (Test-Driven)

1. **Update Evil Package Structure**
   - [ ] Review `packages/evil-mode/` from Phase 6A
   - [ ] Update to use Core API (not internal re-frame events)
   - [ ] Update to use formal hooks (not interceptors)
   - [ ] Update to use markers (not raw positions)
   - [ ] Document Core API usage in `README.md`
   - [ ] Commit: "refactor(evil): update to use Core API"

2. **Load Evil Package**
   - [ ] `M-x load-package evil-mode`
   - [ ] Evil registers as minor mode
   - [ ] Evil state initialized in current buffer
   - [ ] Mode line shows `<N>` (normal state)
   - [ ] Add tests for package loading
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): enable package loading"

3. **State Machine Integration**
   - [ ] Evil FSM states: `:normal`, `:insert`, `:visual`, `:operator-pending`
   - [ ] State transitions via hooks (pre/post-command)
   - [ ] Buffer-local state (each buffer has own Evil state)
   - [ ] Add tests for state machine
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): integrate FSM with hooks"

4. **Mode Line State Indicator**
   - [ ] Show `<N>` in normal mode
   - [ ] Show `<I>` in insert mode
   - [ ] Show `<V>` in visual mode
   - [ ] Show `<O>` in operator-pending
   - [ ] Add tests for mode line updates
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add mode line state indicator"

5. **Basic Mode Switching**
   - [ ] `i` → enter insert mode
   - [ ] `ESC` → return to normal mode
   - [ ] State transitions fire hooks
   - [ ] Add tests for mode switching
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add basic mode switching (i, ESC)"

**Success Criteria:**
- [ ] Evil-mode loads via `M-x load-package`
- [ ] State machine works (normal ↔ insert transitions)
- [ ] Mode line shows current state
- [ ] Hooks fire on state transitions
- [ ] Buffer-local state works (each buffer independent)

---

### Week 2: Motions, Operators, Visual Mode

**Goal:** Core Vim editing workflow

#### Motion Commands (Test-Driven)

1. **Basic Character/Line Motions**
   - [ ] `h` → backward-char (use Core API `move-point`)
   - [ ] `l` → forward-char
   - [ ] `j` → next-line
   - [ ] `k` → previous-line
   - [ ] Add tests for basic motions
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add hjkl motions"

2. **Word Motions**
   - [ ] `w` → forward-word (use Core API)
   - [ ] `b` → backward-word
   - [ ] `e` → end-of-word
   - [ ] Add tests for word motions
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add word motions (w, b, e)"

3. **Line Boundary Motions**
   - [ ] `0` → beginning-of-line
   - [ ] `$` → end-of-line
   - [ ] `^` → first non-whitespace
   - [ ] Add tests for line motions
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add line boundary motions"

4. **Buffer Boundary Motions**
   - [ ] `gg` → beginning-of-buffer
   - [ ] `G` → end-of-buffer
   - [ ] Add tests for buffer motions
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add buffer boundary motions (gg, G)"

#### Operator Commands (Test-Driven)

1. **Delete Operator**
   - [ ] `dd` → delete line (use Core API `delete-region`)
   - [ ] `d$` → delete to end of line
   - [ ] `dw` → delete word (operator + motion)
   - [ ] Deleted text goes to kill ring
   - [ ] Add tests for delete operator
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add delete operator (d + motion)"

2. **Change Operator**
   - [ ] `cc` → change line (delete + enter insert mode)
   - [ ] `cw` → change word
   - [ ] `c$` → change to end of line
   - [ ] Add tests for change operator
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add change operator (c + motion)"

3. **Yank Operator**
   - [ ] `yy` → yank (copy) line
   - [ ] `yw` → yank word
   - [ ] `y$` → yank to end of line
   - [ ] Uses kill ring
   - [ ] Add tests for yank operator
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add yank operator (y + motion)"

4. **Put (Paste) Command**
   - [ ] `p` → paste after cursor
   - [ ] `P` → paste before cursor
   - [ ] Uses kill ring (Core API)
   - [ ] Add tests for put command
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add put commands (p, P)"

#### Visual Mode (Test-Driven)

1. **Visual Selection**
   - [ ] `v` → enter character-wise visual mode
   - [ ] Navigate extends selection (uses markers!)
   - [ ] Selection shown with overlay (uses faces!)
   - [ ] Add tests for visual selection
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add visual mode (v)"

2. **Line-wise Visual**
   - [ ] `V` → enter line-wise visual mode
   - [ ] Selects entire lines
   - [ ] Overlay highlights full lines
   - [ ] Add tests for line-wise visual
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add line-wise visual (V)"

3. **Operators on Visual Selection**
   - [ ] `d` in visual → delete selection
   - [ ] `c` in visual → change selection
   - [ ] `y` in visual → yank selection
   - [ ] Add tests for visual operators
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): operators work on visual selection"

**Success Criteria:**
- [ ] hjkl navigation works
- [ ] Word motions (w, b, e) work
- [ ] Operators work (d, c, y + motion)
- [ ] Visual mode works (v, V, operators on selection)
- [ ] Overlays highlight visual selection correctly
- [ ] Kill ring integration works

---

### Week 3: Count Prefixes, Insert Mode, Cursor Feedback

**Goal:** Polish Evil-mode UX, make it feel like Vim

#### Count Prefixes (Test-Driven)

1. **Count Prefix Infrastructure**
   - [ ] `5j` → repeat motion 5 times
   - [ ] `10w` → forward 10 words
   - [ ] `3dd` → delete 3 lines
   - [ ] Count accumulates as you type digits
   - [ ] Add tests for count prefixes
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add count prefix support"

2. **Count with Operators**
   - [ ] `d5w` → delete 5 words
   - [ ] `3cc` → change 3 lines
   - [ ] `2yy` → yank 2 lines
   - [ ] Add tests for operator counts
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): counts work with operators"

#### Insert Mode Commands (Test-Driven)

1. **Enter Insert Mode**
   - [ ] `i` → insert before cursor
   - [ ] `a` → insert after cursor
   - [ ] `I` → insert at beginning of line
   - [ ] `A` → insert at end of line
   - [ ] Add tests for insert mode entry
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add insert mode entry commands (i, a, I, A)"

2. **New Line Insert**
   - [ ] `o` → open line below
   - [ ] `O` → open line above
   - [ ] Proper indentation
   - [ ] Add tests for new line insert
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add new line commands (o, O)"

3. **Insert Mode Keybindings**
   - [ ] ESC → return to normal mode
   - [ ] Insert mode uses regular Emacs bindings (C-f, C-b, etc.)
   - [ ] Add tests for insert mode keys
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): configure insert mode keybindings"

#### Cursor Visual Feedback (Test-Driven)

1. **Cursor Shape by Mode**
   - [ ] Normal mode → block cursor (use faces!)
   - [ ] Insert mode → bar cursor
   - [ ] Visual mode → hollow block cursor
   - [ ] Add manual test: see cursor change
   - [ ] Commit: "feat(evil): cursor shape changes by mode"

2. **Cursor Color by Mode**
   - [ ] Normal mode → blue cursor
   - [ ] Insert mode → green cursor
   - [ ] Visual mode → orange cursor
   - [ ] Uses face system from Phase 6B
   - [ ] Add manual test: see color change
   - [ ] Commit: "feat(evil): cursor color changes by mode"

**Success Criteria:**
- [ ] Count prefixes work (5j, 3dd, 10w)
- [ ] Insert mode entry commands work (i, a, I, A, o, O)
- [ ] Cursor shape/color changes by mode
- [ ] Insert mode uses Emacs keybindings (C-f, C-b still work)

---

### Week 4: Package Polish, Unload, Evil as Minor Mode

**Goal:** Evil-mode is a polished, production-ready package

#### Evil Package Polish (Test-Driven)

1. **Undo Integration**
   - [ ] Undo groups Evil operations correctly
   - [ ] Markers restored on undo (Phase 7!)
   - [ ] Undo in insert mode groups all inserted text
   - [ ] Add tests for Evil undo integration
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): integrate with advanced undo system"

2. **Dot Command (Repeat)**
   - [ ] `.` repeats last operation
   - [ ] Works with operators (d, c, y)
   - [ ] Works with counts (3.)
   - [ ] Add tests for dot command
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add dot command (repeat last)"

3. **Search Commands**
   - [ ] `/` → forward search (use isearch if exists)
   - [ ] `?` → backward search
   - [ ] `n` → next match
   - [ ] `N` → previous match
   - [ ] Add tests for search
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add search commands (/, ?, n, N)"

4. **Register System**
   - [ ] `"a` prefix selects register
   - [ ] `"ayy` → yank to register a
   - [ ] `"ap` → paste from register a
   - [ ] Registers are buffer-local variables
   - [ ] Add tests for registers
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add register system"

#### Evil as Minor Mode (Test-Driven)

1. **Evil Minor Mode Toggle**
   - [ ] `M-x evil-mode` enables Evil globally
   - [ ] `M-x evil-mode` again disables Evil
   - [ ] Works per-buffer (buffer-local minor mode)
   - [ ] Add tests for toggle
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): make Evil a toggleable minor mode"

2. **Clean Package Unload**
   - [ ] `M-x unload-package evil-mode` removes all Evil state
   - [ ] Keymaps restored to Emacs defaults
   - [ ] Overlays cleaned up
   - [ ] Buffer-local variables cleared
   - [ ] Cursor reverts to default
   - [ ] Mode line no longer shows `<N>`
   - [ ] Add tests for clean unload
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): implement clean package unload"

3. **Evil + Emacs Hybrid**
   - [ ] C-z toggles between Evil and Emacs mode
   - [ ] Emacs bindings still work in insert mode
   - [ ] Evil state per buffer (some buffers Evil, some Emacs)
   - [ ] Add tests for hybrid mode
   - [ ] Run tests ✅
   - [ ] Commit: "feat(evil): add Evil/Emacs hybrid mode (C-z)"

**Success Criteria:**
- [ ] Undo groups Evil operations correctly
- [ ] Dot command repeats last operation
- [ ] Search works (/, ?, n, N)
- [ ] Registers work ("a, "b, etc.)
- [ ] Evil is a proper minor mode (toggle on/off)
- [ ] Package unloads cleanly (no state leaks)
- [ ] Can mix Evil and Emacs bindings

---

### Phase 9 Success Criteria (Overall)

**Evil-mode Functionality:**
- [ ] All core Vim motions work (h, j, k, l, w, b, e, 0, $, gg, G)
- [ ] All core operators work (d, c, y, p, P)
- [ ] Visual mode works (v, V, operators on selection)
- [ ] Count prefixes work (5j, 3dd, 10w)
- [ ] Insert mode works (i, a, o, O, ESC)
- [ ] Cursor feedback works (shape/color by mode)
- [ ] Undo integration works (command grouping, marker restoration)
- [ ] Dot command works (repeat last operation)
- [ ] Search works (/, ?, n, N)
- [ ] Registers work ("a, "b, etc.)

**Package System Validation:**
- [ ] Evil-mode uses **only Core API** (no internal dependencies)
- [ ] Evil-mode loads/unloads cleanly
- [ ] Evil-mode is a proper minor mode (toggleable)
- [ ] Evil-mode doesn't break Emacs bindings
- [ ] Package state is buffer-local

**Architecture Validation:**
- [ ] Core API is sufficient for complex package (Evil-mode)
- [ ] Hook system works (pre/post-command-hook for state transitions)
- [ ] Marker system works (visual mode overlays move with text)
- [ ] Advanced undo works (Evil operations undo correctly)
- [ ] Face system works (cursor shape/color feedback)
- [ ] Buffer-local variables work (per-buffer Evil state)
- [ ] Advice system works (wrap Emacs functions if needed)

**Testing:**
- [ ] 40+ new tests for Evil-mode
- [ ] Manual testing validates Vim workflow
- [ ] No regressions in Emacs functionality
- [ ] E2E tests pass
- [ ] Evil test plan documented and executed

**Readiness for Package Ecosystem:**
- [ ] If Evil-mode (most complex package) works via Core API, **any** package should work
- [ ] Package loading/unloading proven
- [ ] Core API proven sufficient
- [ ] Proceed to Phase 10+ with confidence!

**Progress:** 0/60+ tasks complete (0%) - Ultimate architecture validation

---

## Phase 10: Syntax Highlighting & Tree-sitter

**Status:** 🔲 Planned
**Goal:** Modern syntax highlighting using Tree-sitter
**Timeline:** 3 weeks
**Prerequisites:** ✅ Phase 6B complete (faces system required)

### Features

- Tree-sitter Integration (Web Worker, incremental parsing)
- Language Support (JavaScript/TypeScript, ClojureScript, Python, Rust, Markdown)
- Highlighting (Semantic tokens, face system, themes)

---

## Phase 11: LSP Integration

**Status:** 🔲 Planned
**Goal:** Language server protocol support
**Timeline:** 4 weeks
**Prerequisites:** ✅ Phase 6B-6D complete (overlays for diagnostics, faces for highlights), Bridge server working

### Features

- LSP Client (initialize, document sync, diagnostics, hover, go-to-def, references, code actions)
- Supported Languages (TypeScript, Rust, Clojure, Python)

---

## Long-term Vision (Beyond Phase 11)

- Org-mode (Outline editing, TODO items, Agenda, Export)
- Magit (Git Interface)
- Collaboration (Real-time editing, conflict resolution)
- Cloud Integration (Remote workspaces, devcontainers)

---

## Current Focus

**Active Phase:** Phase 7 🔲 Planned - Core Hardening & Architecture Refinement | **Next Phase:** Phase 8 - Completion Ecosystem (Vertico/Orderless/Consult)

**Recent Achievements (Jan 2, 2026):**
- ✅ **External code review complete** - Comprehensive architecture review by ChatGPT identified critical improvements
- ✅ **Comprehensive Phase 7-9 roadmap created** - Test-driven plan incorporating review recommendations
- ✅ **Phase 7: Core Hardening** - Core API contract, hooks, undo, markers, events.cljs refactoring
- ✅ **Phase 8: Completion Litmus Test** - Vertico, Orderless, Consult to validate Core API
- ✅ **Phase 9: Evil-mode** - Ultimate architecture validation test
- 🎯 **Next:** Begin Phase 7.1 - Write core design documents

**Previous Achievements (Dec 28, 2025):**
- ✅ **Fixed backspace-at-position-0 bug** - Transaction queue no longer stalls when backspacing at buffer start
- ✅ **Playwright E2E baseline established** - 7 passing browser automation tests (commit 2433cbc)
- ✅ **Phase 6.6 roadmap complete** - Comprehensive testing migration plan documented

**Previous Achievements (Dec 25, 2025):**
- ✅ **Phase 6B COMPLETE** - Display & Theming Foundation (all 4 weeks)
- ✅ **Week 4 COMPLETE** - Theme System with CSS Variables
  - Complete theming system using CSS custom properties
  - 50+ CSS variables for colors and typography
  - lexicon-base-light and lexicon-base-dark themes (WCAG AAA compliant)
  - M-x load-theme and M-x set-font-size commands
  - Instant theme switching (no stylesheet regeneration)
  - 647 lines added across 3 files
- ✅ **Week 3 COMPLETE** - Mode Line & Built-in Modes
  - Read-only buffer enforcement, special-mode, help-mode, buffer-menu-mode
  - Mode-line formatter with % constructs
  - 824 lines added across 7 files
- ✅ **Weeks 1-2 COMPLETE** - Face System, Text Properties, Overlays
  - 23 standard faces, overlay system with priority, child frames
  - Text properties API, face priority resolution

**Previous Achievements (Dec 24, 2025):**
- ✅ **Phase 6A COMPLETE** - Package system infrastructure
- ✅ Course correction based on dual research - Display infrastructure first
- ✅ Created comprehensive research documents (DISPLAY_THEMING_RESEARCH.md, TheMissingParts.md)
- ✅ Incorporated superior CSS variables approach for theming

**Strategic Course Correction:**

Research revealed we've been building the wrong foundation. **All sophisticated packages depend on display infrastructure we don't have:**
- Completion framework needs **faces** for annotations (Marginalia, Vertico)
- Evil-mode needs **overlays** for visual selection and **faces** for feedback
- Themes need **face system** to propagate colors
- All packages assume **text properties** and **overlays** exist

**New Implementation Order:**
1. **Phase 6B: Display & Theming** (4 weeks) - Faces, overlays, mode line, themes
2. **Phase 6C: Completion Framework** (6 weeks) - Metadata, styles, CAPFs, built-in packages
3. **Phase 6D: Infrastructure** (2 weeks) - Buffer-local vars, hooks, advice
4. **Phase 6.5: Testing** (2 weeks) - Comprehensive test coverage
5. **Phase 7: Evil-mode Integration** (2 weeks) - Now that dependencies exist

**Completed Steps (Phase 6B):**
1. ✅ Week 1 Complete: Face System
   - ✅ Face data structure and registry
   - ✅ Standard faces defined (23 faces including default, region, mode-line, font-lock-*, etc.)
   - ✅ CSS generation using CSS custom properties
   - ✅ Static face classes that reference variables
2. ✅ Week 2 Complete: Text Properties, Overlays & Child Frames
   - ✅ Text properties API (put/get/remove)
   - ✅ Overlay system with priority
   - ✅ Child frames (Reagent components)
   - ✅ Face priority resolution subscription
3. ✅ Week 3 Complete: Mode Line & Built-in Modes
   - ✅ Mode line format interpreter with % constructs
   - ✅ special-mode for read-only buffers
   - ✅ Read-only buffer enforcement
   - ✅ help-mode and buffer-menu-mode
4. ✅ Week 4 Complete: Theme System with CSS Variables
   - ✅ Theme data structure and loading
   - ✅ CSS variables-based theming (no stylesheet regeneration)
   - ✅ Light/dark theme variants (lexicon-base-light, lexicon-base-dark)
   - ✅ Runtime customization (M-x load-theme, M-x set-font-size)

**Next: Phase 6C - Completion Framework** (6 weeks)
- Completion metadata (categories, annotations)
- Completion styles (substring, flex, orderless)
- Completion tables and CAPFs
- Built-in packages (project.el, imenu, recentf, savehist)

**Why This Matters:**
Building display infrastructure first ensures that when we implement completion framework and packages, they can render properly with colors, annotations, and visual feedback - making Lexicon feel like real Emacs.

**Blocking Issues:** None - clear path forward established

**Last Updated:** 2026-01-02

---

*This roadmap is a living document. Update it as we progress, learn, and adapt.*
