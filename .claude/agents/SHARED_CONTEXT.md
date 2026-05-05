# Shared Agent Context -- Lexicon Project

This file is referenced by all agent configs. It contains project-wide knowledge that every agent needs.

## Build System: Babashka (bb)

All build, test, and lint commands use [Babashka](https://babashka.org/) task runner. Tasks are defined in `bb.edn` at the project root.

### Key Commands

| Command | Purpose |
|---------|---------|
| `bb dev` | Start shadow-cljs watch + dev server at http://localhost:8080 |
| `bb lint` | Run ALL linters (architecture boundary, clj-kondo, e2e checks) |
| `bb lint:arch` | Check architecture boundary only |
| `bb lint:kondo` | Run clj-kondo static analysis only |
| `bb test:e2e` | Run all E2E tests (headless Firefox via Etaoin) |
| `bb test:e2e <pattern>` | Run specific E2E tests matching namespace pattern |
| `bb test:unit` | Run ClojureScript unit tests |
| `bb test` | Run all tests (unit + E2E + Rust) |
| `bb clean` | Clean all build artifacts |
| `bb build` | Full production build (WASM + ClojureScript) |
| `bb build-wasm` | Build Rust WASM module (release) |
| `bb build-cljs-dev` | Build ClojureScript frontend (dev mode) |

### E2E Tests Are Self-Contained and Headless

- E2E tests use **Etaoin** with **headless Firefox** (geckodriver)
- They are fully self-contained -- they run in CI (GitHub Actions) without manual setup
- **Prerequisite:** The app must be served at `http://localhost:8080`
  - If `bb dev` (shadow-cljs watch) is already running, tests can run immediately
  - If not, start it with `bb dev` in background first
- Tests run via Kaocha test runner: `clojure -M:e2e -m kaocha.runner`
- Typical test run: `bb test:e2e ui.minibuffer.completion-sorting-test`
- Capture output: `bb test:e2e <pattern> 2>&1 | tee /tmp/e2e-<feature>.log`

### What NOT to Run

- **NEVER** run `npm run build`, `shadow-cljs compile`, or `npx shadow-cljs ...` during development
- The user typically has `bb dev` (shadow-cljs watch) running -- it recompiles on file save
- Only `bb lint` and `bb test:e2e` are safe to run alongside the dev server

## Project Structure

```
lexicon/
  bb.edn                    # Babashka task definitions
  CLAUDE.md                 # Project memory and engineering standards
  deps.edn                  # Clojure dependencies
  tests.edn                 # Kaocha E2E test configuration
  docs/                     # Architecture docs (org-mode)
    ARCHITECTURE.org
    ARCHITECTURE_BOUNDARY.org
    EMACS_COMPATIBILITY_CONTRACT.org
    VISION.org
    ROADMAP.org
  e2e_tests/                # E2E tests (Clojure + Etaoin + headless Firefox)
    lexicon/
      test_helpers.clj      # Shared test utilities
      ui/                   # Keyboard-only UI tests
        editing/
        buffers/
        windows/
        minibuffer/
        modes/
        files/
        search/
      lisp/                 # Lisp API tests (eval-lisp allowed)
  packages/
    editor-cljs/             # Main ClojureScript editor
      src/lexicon/
        core/                # Internal core modules
          events/            # Re-frame event handlers (buffer, edit, command, ui, etc.)
          completion/        # Completion system (metadata, styles, tables)
          modes/             # Major/minor mode definitions
          ui/                # UI subsystems (faces, frames, overlays)
          api/               # Internal APIs (test.cljs)
          fs/                # File system access
          db.cljs            # App-db schema
          minibuffer.cljs    # Minibuffer stack operations
          views.cljs         # Reagent view components
          init.cljs          # Initialization
          main.cljs          # Entry point (requires all modules)
          log.cljs           # Logging
        lisp.cljs            # PUBLIC API -- the core/package boundary
        packages/            # Package implementations
          icomplete.cljs     # Icomplete package
          dired.cljs         # Dired package
          flymake.cljs       # Flymake package
          etc.
    lexicon-engine/          # Rust WASM gap buffer engine
      wasm/src/              # Rust source
    evil-mode/               # Evil mode (vim emulation) package
    backend-server/          # Backend server
    lexicon-bridge/          # Bridge between core and packages
    language-grammars/       # Tree-sitter grammar files
```

## State Ownership (Critical Rule)

Each state variable is owned by exactly ONE module. Others MUST dispatch events to the owner.

| State Key | Owner | Events |
|-----------|-------|--------|
| `:minibuffer` | `ui.cljs` | `:minibuffer/activate`, `:minibuffer/deactivate` |
| `:echo-area` | `ui.cljs` | `:echo/message`, `:echo/clear` |
| `:mark-position` | `edit.cljs` | `:set-mark`, `:deactivate-mark` |
| `:window-tree` | `ui.cljs` | `:window/set-buffer`, `:window/set-mark` |
| `:buffers` | `buffer.cljs` | `:buffer/set-mode`, `:buffer/update-version` |
| `:kill-ring` | `edit.cljs` | (kill/yank commands only) |

## Core/Package Boundary

- Packages import ONLY `lexicon.lisp` (the public API)
- Packages NEVER import `lexicon.core.*` or `re-frame`
- `bb lint:arch` enforces this -- violations are hard failures

## External Package Architecture

### Design Decision: SCI + Multi-File Bundling

External packages (like Vertico, evil-mode) are:
- **Separate git repos** named `lexicon-<name>` (e.g., `lexicon-vertico`)
- **Written as ClojureScript source** (`.cljs` files), NOT compiled JavaScript
- **Interpreted at runtime by SCI** (Small Clojure Interpreter, v0.8.42)
- **Only able to call `lexicon.lisp` functions** -- the SCI sandbox denies access to `lexicon.core.*`, `re-frame`, `js/eval`, `js/fetch`, etc.

This mirrors Emacs: Elisp packages are source files loaded at runtime by the Lisp interpreter. Our packages are ClojureScript source files loaded at runtime by SCI.

### SCI Infrastructure (Already Built)

| Component | File | Status |
|-----------|------|--------|
| SCI evaluation engine | `core/eval.cljs` | Working |
| Package SCI sandbox | `core/packages/sci.cljs` | Working |
| Package loader | `core/packages/loader.cljs` | Working |
| Lisp API → SCI bindings | `lisp.cljs` `sci-namespace` (~250+ functions) | Working |
| Trust levels | `core/packages/sci.cljs` | Working |
| Test package | `packages/lexicon-test-package/` | Working |
| Package registry (`lexpa`) | `/home/nixos/projects/lexpa/` | Working |

### Trust Levels

- **`:core`** -- Built-in packages (full access)
- **`:local`** -- User-installed from filesystem (full access)
- **`:external`** -- Third-party from internet (SCI sandbox, Core API only)

### Package Metadata (`package.edn`)

```clojure
{:name "lexicon-vertico"
 :version "0.1.0"
 :description "Vertical completion UI"
 :entry lexicon.vertico.core
 :lexicon-version ">=0.1.0"
 :dependencies []}
```

### What Packages CAN Call (via SCI)

All ~250+ functions in `lexicon.lisp/sci-namespace`: buffer ops, point/mark, insert/delete, commands, keymaps, modes, hooks, minibuffer, completion, windows, text properties, overlays, filesystem, variables, messages.

### What Packages CANNOT Call (denied by SCI sandbox)

`js/eval`, `js/Function`, `js/fetch`, `js/XMLHttpRequest`, `re-frame.core/dispatch`, `re-frame.core/subscribe`, `lexicon.db/*`, `lexicon.events/*`.

### When a Package Needs a Missing Primitive

If an external package needs functionality not in `lexicon.lisp`:
1. **Do NOT work around it** -- no reaching into internals
2. **Foundation-builder adds the function** to `lexicon.lisp`
3. **Foundation-builder registers it** in `lisp.cljs` `sci-namespace` map
4. **Then the package can use it** via SCI

### `lexpa` (Lexicon Package Archive)

A recipe-based package registry (like MELPA/straight.el):
- Located at `/home/nixos/projects/lexpa/`
- Recipes in `recipes/*.edn` map package names to GitHub repo + path
- Local dev via `local.edn` (maps package names to filesystem paths)
- Server (`server.js`) serves packages at `http://localhost:3100`
- Client fetches `.cljs` source and `package.edn` via HTTP
- Multi-file packages are concatenated in dependency order
- Evaluated in SCI at install/load time
- Install from SCI: `(install-package "vertico")` or `(install-package "http://url")`

### External Package Monorepo (`lexpkgs`)

All first-party external packages live in a shared monorepo at `/home/nixos/projects/lexpkgs/`:
```
lexpkgs/
  vertico/          # Vertical completion UI
  marginalia/       # Rich annotations for completions
  orderless/        # Orderless completion matching style
```

Each subdirectory contains `package.edn` + `src/lexicon/<name>/core.cljs`.

### Customization System (`defcustom` / `defgroup`)

Packages can declare user-configurable variables:
```clojure
(defcustom orderless-smart-case true
  :type :boolean
  :group :orderless
  :set (fn [val] (orderless-set-smart-case val)))
```

- `defcustom` registers a variable with a standard value, type, and optional setter
- `defgroup` organizes related custom variables
- `setopt` sets a custom variable (triggers its `:set` function)
- Values persist via `custom-set-variables`

## Emacs Source Reference

Emacs 29.4 source is at `~/projects/emacs-source/`:
- `lisp/simple.el` -- universal-argument, basic commands
- `src/callint.c` -- interactive specs
- `lisp/minibuffer.el` -- completion system
- `src/buffer.c` -- buffer-local variables
- `lisp/icomplete.el` -- icomplete-mode
