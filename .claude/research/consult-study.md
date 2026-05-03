# Consult Package Deep Dive

**Date:** 2026-05-03
**Source:** minad/consult, ~/projects/consult/

---

I now have a thorough understanding of the entire consult package source code. Let me compile the comprehensive research document.

---

# Comprehensive Study: The `consult` Package (minad/consult)

## Package Overview

Consult (by Daniel Mendler) is a GNU ELPA package providing enhanced search and navigation commands built on top of Emacs's `completing-read` API. It is part of a broader completion ecosystem alongside Vertico (vertical completion UI), Marginalia (rich annotations), Orderless (flexible matching), and Embark (contextual actions).

The package consists of 10 source files:
- `consult.el` -- Main file (~5720 lines), contains the core infrastructure and most commands
- `consult-compile.el` -- Compilation error navigation (124 lines)
- `consult-flymake.el` -- Flymake diagnostic navigation (115 lines)
- `consult-imenu.el` -- Imenu navigation (258 lines)
- `consult-info.el` -- Info manual full-text search (218 lines)
- `consult-kmacro.el` -- Keyboard macro selection (84 lines)
- `consult-org.el` -- Org-mode heading navigation (145 lines)
- `consult-register.el` -- Register management (329 lines)
- `consult-xref.el` -- Xref integration (121 lines)

All extension files are split out to enable lazy loading of heavy dependencies (compile.el, flymake.el, imenu.el, info.el, kmacro.el, org.el, xref.el).

---

## 1. Complete Command Inventory

### 1.1 Buffer/File Navigation Commands

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-buffer` | Enhanced `switch-to-buffer` with virtual buffers (recent files, bookmarks, project files, registers) | Multi-source | None | `completing-read`, `buffer-list`, `recentf-list`, `bookmark-alist`, `register-alist`, `project-current` |
| `consult-buffer-other-window` | Same as above, opens in other window | Multi-source | None | `switch-to-buffer-other-window` |
| `consult-buffer-other-frame` | Same as above, opens in other frame | Multi-source | None | `switch-to-buffer-other-frame` |
| `consult-buffer-other-tab` | Same as above, opens in other tab | Multi-source | None | `switch-to-buffer-other-tab` |
| `consult-project-buffer` | Buffer switching restricted to current project | Multi-source (project) | None | `project-current`, `consult-project-function` |
| `consult-recent-file` | Select from recently opened files | All recent files | None | `recentf-list`, `find-file` |
| `consult-bookmark` | Select or create bookmark | All bookmarks | None | `bookmark-alist`, `bookmark-jump`, `bookmark-set` |

### 1.2 In-Buffer Search/Navigation Commands

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-line` | Search for matching line in current buffer | Current buffer | None | Buffer text scanning, `line-number-at-pos`, markers, overlays for preview |
| `consult-line-multi` | Search for matching line across multiple buffers | Multiple buffers | None | Dynamic candidate computation via `consult--dynamic-collection`, buffer traversal |
| `consult-goto-line` | Jump to line number with preview | Current buffer | None | `goto-char`, `point-min`, `forward-line`, `display-line-numbers` |
| `consult-outline` | Jump to outline heading | Current buffer | None | `outline-regexp`, `outline-level`, `outline-search-function`, `re-search-forward` |
| `consult-mark` | Jump to marker in buffer-local `mark-ring` | Current buffer | None | `mark-ring`, `mark-marker`, markers |
| `consult-global-mark` | Jump to marker in `global-mark-ring` | All buffers | None | `global-mark-ring`, markers, `with-current-buffer` |
| `consult-focus-lines` | Show/hide lines matching regexp (like `occur` but in-place) | Current buffer | None | Overlays (`make-overlay`, `overlay-put`), `re-search-forward` |
| `consult-keep-lines` | Delete lines NOT matching regexp (like `flush-lines`) | Current buffer | None | `re-search-forward`, `delete-region`, `fontify-region` |
| `consult-isearch-history` | Browse and select from isearch history | History | None | `isearch-mode`, `with-isearch-suspended`, `search-ring`, `regexp-search-ring` |

### 1.3 External Search Commands (Async/Process-Based)

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-grep` | Async grep search in directory | File system | `grep` (GNU grep) | `make-process`, process filters/sentinels, `completing-read` |
| `consult-git-grep` | Async git grep search | Git repository | `git grep` | `make-process`, process filters/sentinels |
| `consult-ripgrep` | Async ripgrep search | File system | `rg` (ripgrep) | `make-process`, process filters/sentinels |
| `consult-find` | Async find files by name | File system | `find` (GNU find) | `make-process`, `find-file` |
| `consult-fd` | Async find files using fd | File system | `fd` / `fdfind` | `make-process`, `find-file` |
| `consult-locate` | Async locate files | System-wide | `locate` | `make-process`, `find-file` |
| `consult-man` | Async man page search | System man pages | `man -k` (apropos) | `make-process`, `man` (elisp), `Man-prefer-synchronous-call` |

### 1.4 Error/Diagnostic Navigation

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-compile-error` | Jump to compilation error | Compilation buffers | None | `compilation-message`, `compilation-next-single-property-change`, `compilation-next-error-function` |
| `consult-grep-match` | Jump to grep matches (thin wrapper around `consult-compile-error`) | Grep buffers | None | `grep-mode` buffer parsing |
| `consult-flymake` | Jump to Flymake diagnostic | Current buffer or project | None | `flymake-diagnostics`, `flymake--project-diagnostics`, `flymake-diagnostic-buffer`, `flymake-diagnostic-type` |

### 1.5 Code Structure Navigation

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-imenu` | Flat imenu with preview and narrowing | Current buffer | None | `imenu-create-index-function`, `imenu--make-index-alist`, `imenu--truncate-items` |
| `consult-imenu-multi` | Imenu across multiple buffers of same mode | Multiple buffers (project) | None | Same as imenu, plus `consult--buffer-query` |

### 1.6 Editing/Kill Ring Commands

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-yank-from-kill-ring` | Select from kill-ring and insert with preview | Kill ring | None | `kill-ring`, `kill-ring-yank-pointer`, `insert-for-yank`, `push-mark` |
| `consult-yank-pop` | DWIM yank-pop: cycle if last was yank, otherwise select from kill-ring | Kill ring | None | `yank-pop`, `last-command` |
| `consult-yank-replace` | Replace last yank with selection from kill-ring | Kill ring | None | `yank-undo-function`, `delete-region`, `insert-for-yank` |
| `consult-completion-in-region` | Use minibuffer completion for `completion-at-point` | Current buffer | None | `completion-in-region`, `completion-all-completions`, `completing-read` |

### 1.7 History/Command Commands

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-history` | Insert string from mode-specific or minibuffer history | History variable | None | `minibuffer-history-variable`, `comint-input-ring`, `eshell-history-ring`, `ring-elements` |
| `consult-complex-command` | Select from command history and re-evaluate | Command history | None | `command-history`, `prin1-to-string`, `funcall-interactively` |

### 1.8 Mode/Theme/Misc Commands

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-theme` | Select and preview themes | Custom themes | None | `custom-available-themes`, `enable-theme`, `disable-theme`, `load-theme` |
| `consult-minor-mode-menu` | Toggle minor modes | Minor modes | None | `minor-mode-list`, `minor-mode-alist`, `local-variable-if-set-p` |
| `consult-mode-command` | Run command from active major/minor modes | Mode commands | None | `load-history`, `commandp`, `command-execute` |

### 1.9 Info Manual Search

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-info` | Full-text search through info manuals | Info manuals | None | `Info-mode`, `Info-find-node`, `Info-read-subfile`, `Info-select-node`, `re-search-forward` |
| `consult-info-define` | Macro/function to define named info search commands | Info manuals | None | Same as `consult-info` |

### 1.10 Keyboard Macro Commands

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-kmacro` | Select and execute keyboard macro | Macro ring | None | `last-kbd-macro`, `kmacro-ring`, `kmacro-ring-head`, `format-kbd-macro` |

### 1.11 Register Commands

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-register` | Browse and use registers with preview | All registers | None | `register-alist`, `get-register`, `jump-to-register`, `insert-register` |
| `consult-register-load` | DWIM register load (jump or insert) | Single register | None | `jump-to-register`, `insert-register` |
| `consult-register-store` | Context-aware register store with action menu | Current context | None | `copy-to-register`, `point-to-register`, `window-configuration-to-register`, `frameset-to-register`, `kmacro-to-register` |
| `consult-register-window` | Enhanced register preview window | All registers | None | `register-preview`, `display-buffer-at-bottom` |
| `consult-register-format` | Format register for display | Single register | None | `register-val-describe`, `cl-defgeneric`/`cl-defmethod` for polymorphic formatting |

### 1.12 Org-Mode Commands

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-org-heading` | Jump to Org heading | Current buffer or scope | None | `org-map-entries`, `org-get-outline-path`, `org-heading-components`, `org-get-tags` |
| `consult-org-agenda` | Jump to Org agenda heading | Agenda files | None | `org-agenda-files`, `org-map-entries` with `'agenda` scope |

### 1.13 Xref Integration

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-xref` | Show xrefs with preview (drop-in for `xref-show-xrefs-function`) | Xref results | None (uses whatever xref backend provides) | `xref-item-location`, `xref-location-group`, `xref-location-line`, `xref-pop-to-location` |

### 1.14 Isearch Integration

| Command | Description | Scope | External Tools | Key Emacs Primitives |
|---------|-------------|-------|----------------|---------------------|
| `consult-isearch-forward` | Continue isearch forward from consult-isearch-history | Search | None | `isearch-new-forward`, `exit-minibuffer` |
| `consult-isearch-backward` | Continue isearch backward | Search | None | Same |

---

## 2. External Tool Dependencies

### 2.1 grep (GNU grep)

**Used by:** `consult-grep`

**Default arguments:**
```
grep --null --line-buffered --color=never --ignore-case --with-filename --line-number -I -r
```
Plus dynamically computed exclude arguments from `grep-find-ignored-files` and `grep-find-ignored-directories` (producing `--exclude=PATTERN` and `--exclude-dir=PATTERN`).

**Regex type:** Extended (`-E`) or PCRE (`-P`) -- consult probes for PCRE support via lookahead test. Falls back to extended.

**Input format:** Emacs regexp from user input is compiled to POSIX extended regex or PCRE. Multiple words are ANDed using lookahead (PCRE) or run as separate patterns.

**Output format:** Line-based, NUL-separated fields: `filename\0linenumber:content` or `filename\0linenumber-content` (for context lines). Parsed by `consult--grep-match-regexp`.

**Fallback:** No explicit fallback. If grep is not available, the command will error.

### 2.2 git grep

**Used by:** `consult-git-grep`

**Default arguments:**
```
git --no-pager grep --null --color=never --ignore-case --extended-regexp --line-number -I
```

**Regex type:** Extended regex only. Multiple words compiled to `--and -e PATTERN1 --and -e PATTERN2` chains.

**Output format:** Same NUL-separated format as grep.

**Fallback:** None. Requires git repository.

### 2.3 rg (ripgrep)

**Used by:** `consult-ripgrep`

**Default arguments:**
```
rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip
```

**Regex type:** PCRE (`-P`) if supported, else extended regex. Consult probes for PCRE support.

**Smart case:** Enabled by default. Case insensitive if no uppercase letters in query. Explicit `-s`/`--case-sensitive` overrides.

**Output format:** Same NUL-separated format as grep.

**Fallback:** None.

### 2.4 find (GNU find)

**Used by:** `consult-find`

**Default arguments:**
```
find . -not ( -path */.[A-Za-z]* -prune )
```

**Regex type:** Probes for emacs regex type support (`-regextype emacs`). Falls back to basic. Uses `-iregex` for case-insensitive matching. Multiple words ANDed with `-and -iregex PATTERN`.

**Output format:** One filename per line. `./` prefix stripped.

**Fallback:** None.

### 2.5 fd / fdfind

**Used by:** `consult-fd`

**Default arguments:**
```
fd --full-path --color=never
```
(Or `fdfind` if that is the executable name, common on Debian.)

**Regex type:** PCRE. Multiple words ANDed with `--and PATTERN`. Also supports `--fixed-strings` and `--glob` modes.

**Smart case:** Same logic as ripgrep.

**Output format:** One filename per line.

**Fallback:** None.

### 2.6 locate

**Used by:** `consult-locate`

**Default arguments:**
```
locate --ignore-case
```

**Input handling:** Input is split into escaped words and passed literally (not as regex) so locate can use its database index.

**Output format:** One filename per line.

**Fallback:** None.

### 2.7 man -k (apropos)

**Used by:** `consult-man`

**Default arguments:**
```
man -k
```

**Input handling:** Extended regex compiled from user input.

**Output format:** Line-based. Parsed by `consult--man-format` which extracts `names (section) - description` format. The parsed section and name are stored as `consult-man` text property for use when calling `man PAGE`.

**Fallback:** None.

---

## 3. Emacs Infrastructure Dependencies

### 3.1 completing-read and the Completion Framework

This is the **foundational** dependency. Every consult command ultimately calls `completing-read` through the `consult--read` wrapper. Key completion infrastructure used:

- `completing-read` -- The standard Emacs completion entry point
- `completion-metadata` / `metadata` response in completion tables -- Consult generates metadata including `category`, `group-function`, `affixation-function`, `display-sort-function`, `cycle-sort-function`
- `complete-with-action` -- Used to build dynamic completion tables
- `completion-all-sorted-completions` / `completion-all-completions` -- For candidate retrieval
- `minibuffer-completion-table` / `minibuffer-completion-predicate` -- Accessed during completion
- `test-completion` -- Used by default completion candidate function
- Completion styles integration via `consult--split-setup` which sets up completion-style filtering
- `minibuffer-contents-no-properties` -- Constantly read during preview

### 3.2 Process Management

The entire async pipeline (used by grep, ripgrep, find, fd, locate, man) relies on:

- `make-process` -- Creates async subprocesses with `:connection-type pipe`, `:filter` for stdout, `:sentinel` for completion, `:stderr` for error buffer, `:noquery t`
- Process filters (`:filter`) -- Parse line-based output by splitting on `[\r\n]+`, accumulate partial lines in `rest` variable
- Process sentinels (`:sentinel`) -- Detect finished/killed/failed states, flush remaining partial output
- `delete-process` -- Cancel running processes when input changes
- `process-file-shell-command` -- Used to probe for grep PCRE support and find regex type support
- `process-adaptive-read-buffering` -- Set to nil for real-time output
- `read-process-output-max` -- Increased to `consult--process-chunk` (1MB) during async operations
- `accept-process-output` -- Called during dynamic computation to allow timers to trigger and refresh the UI

### 3.3 Buffer Management

- `buffer-list` / `consult--frame-buffer-list` -- List all buffers or frame-local buffers
- `buffer-local-value` -- Read buffer-local variables (major-mode, default-directory) without switching
- `with-current-buffer` -- Extensively used for cross-buffer operations
- `get-buffer` / `get-buffer-window` / `buffer-live-p` -- Buffer existence and visibility checks
- `switch-to-buffer` / `switch-to-buffer-other-window` / `switch-to-buffer-other-frame` -- Buffer display
- `buffer-name` / `buffer-file-name` -- Buffer identity
- `buffer-modified-p` -- For modified buffer source
- `frame-parameter nil 'buffer-list'` / `'buried-buffer-list'` -- Frame/tab buffer management
- `generate-new-buffer` / `kill-buffer` -- Temporary preview buffer management
- `find-file-noselect` / `find-file` -- File opening
- `buffer-substring-no-properties` / `buffer-substring` -- Text extraction
- `set-visited-file-name` -- Used to disassociate preview buffers from files

### 3.4 Markers and Overlays

**Markers:**
- Used extensively for position tracking across buffer modifications
- `make-marker` / `set-marker` / `copy-marker` -- Create position markers
- `marker-buffer` / `marker-position` -- Read marker state
- `set-marker-insertion-type` -- Control marker growth behavior
- "Cheap markers" -- `(buffer . position)` cons cells used instead of real markers for performance; upgraded to real markers on window selection change

**Overlays:**
- `make-overlay` / `overlay-put` / `delete-overlay` -- Core overlay operations
- Used for: preview line highlighting (`consult-preview-line`), preview match highlighting (`consult-preview-match`), insertion preview, focus-lines (hiding non-matching lines), async indicator
- `overlays-in` / `overlays-at` -- Query existing overlays
- `overlay-get` with `'isearch-open-invisible` -- Temporarily reveal hidden content during preview

### 3.5 Text Properties

Text properties are the primary metadata transport mechanism in consult:

- `consult-location` -- Stores (marker . line-number) for location candidates
- `consult--type` -- Stores narrowing type character
- `consult--candidate` -- Stores the actual candidate object (separate from display string)
- `consult--prefix-group` -- Stores group name for file-grouped results
- `consult--info` -- Stores (node bol buffer) for info search results
- `consult-man` -- Stores "section name" for man page results
- `consult-xref` -- Stores xref item objects
- `consult-org--heading` -- Stores (level todo priority . buffer)
- `multi-category` -- Stores (category . item) for multi-source candidates
- `consult-strip` -- Marks prefixes to be stripped
- `consult--force` -- Forces async input processing regardless of min-input
- `consult--split` -- Marks split points in async input
- `face` properties -- Extensively used for candidate highlighting
- `invisible` -- Used by focus-lines overlays
- `yank-handler` -- Preserved from kill-ring entries

### 3.6 Hooks

- `minibuffer-setup-hook` (via `minibuffer-with-setup-hook`) -- Core setup mechanism for preview, keymaps, async initialization
- `minibuffer-exit-hook` -- Used to clean up preview state
- `after-change-functions` -- Used to trigger async re-computation on input change, and to hide tofu characters
- `post-command-hook` -- Used to trigger preview after each command
- `window-selection-change-functions` -- Used to upgrade cheap markers to real markers, and to upgrade preview buffers to fully initialized buffers
- `pre-command-hook` -- Used to disassociate preview buffers from files
- `find-file-hook` -- Filtered during preview to only allow whitelisted hooks
- `change-major-mode-hook` -- Used by global mode activation during preview
- `consult-after-jump-hook` -- Custom hook run after jumping (default: `recenter`)
- `consult--completion-candidate-hook` -- Integration point for completion UIs (Vertico, Icomplete)
- `consult--completion-refresh-hook` -- Integration point for refreshing completion UIs
- `imenu-after-jump-hook` -- Run after imenu jump
- `completion-list-mode-hook` -- Used for default *Completions* preview setup

### 3.7 Timers

Timers are **critical** infrastructure for the async pipeline:

- `timer-create` / `timer-set-function` / `timer-set-time` / `timer-activate` / `cancel-timer` -- Low-level timer API used throughout
- `consult-async-input-debounce` (default 0.2s) -- Debounce delay before starting async process
- `consult-async-input-throttle` (default 0.5s) -- Minimum interval between async process starts
- `consult-async-refresh-delay` (default 0.2s) -- Delay before refreshing the completion UI with new results
- Preview debouncing -- `consult--preview-key-debounce` supports per-key debounce delays
- `timer-relative-time` -- Used for relative time calculations
- `run-at-time` -- Used for register preview delay

### 3.8 Xref Subsystem

- `consult-xref` is designed as a drop-in `xref-show-xrefs-function`
- Uses: `xref-item-location`, `xref-item-summary`, `xref-location-group`, `xref-location-line`, `xref-location-marker`, `xref-file-location-column`, `xref-pop-to-location`
- Supports preview of `xref-buffer-location`, `xref-file-location`, `xref-etags-location`

### 3.9 Imenu Subsystem

- `imenu-create-index-function` -- The mode-specific index builder
- `imenu--make-index-alist` / `imenu--truncate-items` -- Index generation
- `imenu--subalist-p` -- Detect nested menus
- `imenu-default-goto-function` -- Jump function
- `imenu-use-markers` -- Set to t to get marker positions
- Caching via `buffer-modified-tick` comparison

### 3.10 Outline Subsystem

- `outline-regexp` -- Heading pattern
- `outline-level` -- Level computation function
- `outline-heading-alist` -- Heading-to-level mapping
- `outline-search-function` -- Custom search function (newer Emacs)

### 3.11 Bookmark Subsystem

- `bookmark-alist` -- All bookmarks
- `bookmark-maybe-load-default-file` -- Ensure bookmarks are loaded
- `bookmark-jump` / `bookmark-set` -- Navigate/create bookmarks
- `bookmark-get-handler` / `bookmark-get-filename` / `bookmark-get-position` -- Extract bookmark data
- `bookmark-all-names` -- List all bookmark names
- `bookmark-make-record` -- Create record for defaults
- `bookmark-default-handler` -- Used to filter previewable bookmarks

### 3.12 Register System

- `register-alist` -- All registers
- `get-register` / `set-register` -- Read/write registers
- `jump-to-register` / `insert-register` -- Use registers
- `register-read-with-preview` / `register-preview` -- Built-in preview
- `register-val-describe` -- Format register values
- `point-to-register` / `copy-to-register` / `append-to-register` / `prepend-to-register` -- Store operations
- `number-to-register` / `increment-register` -- Numeric registers
- `window-configuration-to-register` / `frameset-to-register` / `kmacro-to-register` -- Special registers
- `cl-defgeneric` / `cl-defmethod` -- Polymorphic register description via CLOS-style generics

### 3.13 Mark Ring

- `mark-ring` -- Buffer-local mark ring
- `global-mark-ring` -- Global mark ring
- `mark-marker` / `mark-ring-max` -- Ring access
- `push-mark` -- Add to mark ring before jumping

### 3.14 Info Mode

- `Info-mode` / `Info-find-node` / `Info-select-node` -- Info buffer management
- `Info-read-subfile` -- Handle multi-file info manuals
- `Info-current-file` / `Info-current-subfile` -- Current info state
- `Info-history` / `Info-history-list` / `Info-history-forward` -- Suppressed during preview
- `info-initialize` / `info--manual-names` -- List available manuals
- Manual text is searched with regex across nodes using `\^_\n` as node separators

### 3.15 Flymake

- `flymake-diagnostics` / `flymake--project-diagnostics` -- Get diagnostics
- `flymake-diagnostic-buffer` / `flymake-diagnostic-beg` / `flymake-diagnostic-end` / `flymake-diagnostic-type` / `flymake-diagnostic-text` -- Diagnostic fields
- `flymake--lookup-type-property` -- Get category/severity information
- `flymake--severity` -- For sorting
- `flymake-running-backends` / `flymake-reporting-backends` -- Status reporting

### 3.16 Compilation Mode

- `compilation-message` text property -- Found via `compilation-next-single-property-change`
- `compilation--message->loc` / `compilation--message->type` -- Extract location and severity
- `compilation-next-error-function` -- Jump to error location
- `compilation-current-error` / `overlay-arrow-position` -- Track current error
- `grep-mode` / `grep-edit-mode` -- Distinguish grep from compile buffers
- `compilation-locs` -- Check if buffer has compilation data

### 3.17 Org Mode

- `org-map-entries` -- Iterate over headings (with MATCH, SCOPE, SKIP args)
- `org-heading-components` -- Parse heading data
- `org-get-outline-path` -- Get full path with caching
- `org-format-outline-path` -- Format path for display
- `org-get-tags` -- Get inherited tags
- `org-get-todo-face` -- Face for TODO keywords
- `org-todo-keywords` -- Available TODO states
- `org-highest-priority` / `org-lowest-priority` -- Priority bounds
- `org-use-tag-inheritance` -- Tag inheritance flag
- `org-outline-path-cache` -- Performance cache
- `org-agenda-files` -- Agenda scope

### 3.18 Isearch Integration

- `isearch-mode` / `isearch-string` / `isearch-regexp` / `isearch-regexp-function` -- Core isearch state
- `with-isearch-suspended` -- Critical macro for suspending isearch and running completion
- `isearch-update` / `isearch-update-from-string-properties` -- Resume isearch with new string
- `search-ring` / `regexp-search-ring` -- Search histories
- `isearch-done` -- End isearch (used when consult-line starts from isearch)

### 3.19 Recentf

- `recentf-mode` -- Must be enabled for recent file sources
- `recentf-list` -- The list of recent files
- `recentf-filename-handlers` -- File name processing

### 3.20 Project.el Integration

- `project-current` -- Get current project via `consult-project-function`
- Default implementation uses `project-current` and `project-root`
- `project-known-project-roots` -- All known projects

---

## 4. Async Architecture

The async architecture is one of consult's most sophisticated subsystems. It uses a **functional pipeline** pattern where async functions are curried: each takes a `sink` function and returns a handler function.

### 4.1 Pipeline Model

```
User Input (string)
    |
    v
[consult--async-split]     -- Split input into async part and filter part
    |
    v
[consult--async-options]   -- Highlight command-line options in input
    |
    v
[consult--async-min-input] -- Enforce minimum input length (default: 3 chars)
    |
    v
[consult--async-throttle]  -- Debounce + throttle input changes
    |
    v
[consult--async-process]   -- Launch external process, parse output
    or
[consult--async-dynamic]   -- Compute candidates dynamically in Elisp
    |
    v
[transform]                -- Format/transform candidates
    |
    v
[consult--async-highlight] -- Highlight matching portions of candidates
    |
    v
[consult--async-indicator] -- Show running/finished/failed status in prompt
    |
    v
[consult--async-refresh]   -- Debounced refresh of completion UI
    |
    v
[consult--async-sink]      -- Terminal sink, accumulates candidate list
```

### 4.2 Actions (Message Protocol)

The pipeline communicates via a set of action messages:

| Action | Direction | Meaning |
|--------|-----------|---------|
| `'setup` | Top-down | Initialize state, guaranteed from minibuffer context |
| `'destroy` | Top-down | Clean up state |
| `'flush` | Top-down | Clear accumulated candidates |
| `'refresh` | Bottom-up | Request UI refresh |
| `'cancel` | Top-down | Cancel running process |
| `nil` | Query | Return current candidate list |
| `(list ...)` | Bottom-up | Append new candidates |
| `"string"` | Top-down | New user input |
| `[indicator STATE]` | Bottom-up | Update indicator (running/finished/killed/failed) |

### 4.3 Process Launch and Management (`consult--async-process`)

When user input changes:

1. The builder function is called with the input string to produce command-line arguments
2. If args differ from the previous invocation:
   - Previous process is killed (`delete-process`)
   - Previous stderr buffer is killed
   - A new process is created via `make-process` with `:connection-type pipe`
3. The **process filter** splits stdout on `[\r\n]+`, accumulates partial lines in a `rest` variable, and sends complete lines downstream as lists
4. The **process sentinel** handles process completion:
   - Flushes any remaining partial line
   - Sets indicator to `finished`, `killed`, or `failed`
   - Logs stderr to `*consult-async-log*` buffer
5. `process-adaptive-read-buffering` is set to nil for real-time streaming

### 4.4 Input Debouncing and Throttling (`consult--async-throttle`)

Two mechanisms prevent excessive process restarts:

- **Debounce** (default 0.2s): After the last keystroke, wait this long before starting the process. If initial input is present, debounce is skipped for immediate results.
- **Throttle** (default 0.5s): Minimum time between consecutive process starts, preventing rapid restart cycles.

Implementation: A timer is set with delay = max(debounce, throttle_remaining). Previous input causes `'cancel` to be sent downstream, killing any running process.

### 4.5 Input Splitting (`consult--async-split`)

Consult uses a split model where the input string is divided into two parts:

1. **Async part** (sent to the external process)
2. **Filter part** (used for local completion-style filtering)

Default split style is "perl" with `#` separator:
- `#async-input#filter-input` -- Everything before the first `#` is the async query, everything after is local filter
- `#async-input --grep-opts#filter-input` -- Command-line options can be passed after `--`

The split function also installs a custom completion style to handle the split correctly.

### 4.6 Incremental Result Handling

Results stream in incrementally:

1. First batch of results triggers `'flush` (clear old candidates) then appends new ones
2. Subsequent batches are appended to the existing list using linked-list tail tracking for O(1) append
3. A refresh timer (default 0.2s delay) batches UI refreshes to avoid excessive redisplay
4. `accept-process-output` is called during dynamic computation to allow timers and refresh

### 4.7 Dynamic (Non-Process) Async (`consult--async-dynamic`)

For Elisp-computed async candidates (e.g., `consult-line-multi`, `consult-info`):

1. The FUN receives input and a callback function
2. Computation runs inside `while-no-input` to be interruptible
3. If interrupted (by user input), computation is restarted after a delay
4. The callback can be called incrementally during computation

### 4.8 Merged Async (`consult--async-merge`)

For multi-source async (used by `consult--multi` with async sources):

- Multiple async pipelines run in parallel
- Each has its own sink that merges results into a shared ordered list
- An indicator vector tracks the state of each sub-pipeline
- Overall state is the "worst" of all sub-states

### 4.9 Prebuilt Pipelines

Two convenience constructors:

- `consult--process-collection` -- For external process commands (grep, find, etc.): split + options + min-input + throttle + process + transform + highlight
- `consult--dynamic-collection` -- For Elisp-computed candidates: min-input + throttle + dynamic + transform + highlight

---

## 5. Source System (`consult--multi`)

### 5.1 Source Protocol

A source is a plist with these fields:

**Required (one of):**
- `:items` -- List of candidate strings, or function returning same. Candidates can be `(display-string . actual-value)` pairs.
- `:async` -- An async function (curried) for asynchronous candidate generation

**Optional:**
- `:name` -- String name, used for group titles and annotations
- `:narrow` -- Narrowing key: a character, `(char . string)` pair, or list of pairs for multi-key narrowing
- `:category` -- Completion category symbol (e.g., `buffer`, `file`, `bookmark`)
- `:enabled` -- Predicate function; source is used only if this returns non-nil
- `:hidden` -- When t, candidates are hidden by default (only shown when narrowed to)
- `:face` -- Face applied to all candidates from this source
- `:annotate` -- Per-candidate annotation function
- `:history` -- History variable to record selected candidate
- `:default` -- When t, first item is the default selection
- `:action` -- Function called with selected candidate on RET
- `:new` -- Function called with new (non-matching) candidate name
- `:state` -- State constructor for preview; must return a state function
- `:preview-key` -- Per-source preview key override

### 5.2 How Sources Compose (`consult-buffer` example)

`consult-buffer` uses `consult-buffer-sources`, which defaults to:

```elisp
'(consult-source-buffer           ; ?b - Regular buffers
  consult-source-hidden-buffer    ; SPC - Hidden (space-prefixed) buffers
  consult-source-modified-buffer  ; ?* - Modified file buffers
  consult-source-other-buffer     ; ?o - Buffers from other frames/tabs
  consult-source-recent-file      ; ?f - Recent files
  consult-source-buffer-register  ; ?r - Buffer registers
  consult-source-file-register    ; ?r - File registers
  consult-source-bookmark         ; ?m - Bookmarks
  consult-source-project-buffer-hidden   ; ?p/?B - Project buffers
  consult-source-project-recent-file-hidden  ; ?p/?F - Project files
  consult-source-project-root-hidden)    ; ?p/?R - Project roots
```

Each source variable is a plist. Example:

```elisp
(defvar consult-source-buffer
  `( :name     "Buffer"
     :narrow   ?b
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :default  t
     :items    ,(lambda () (consult--buffer-query :sort 'visibility
                                                   :as #'consult--buffer-pair))))
```

### 5.3 Narrowing Mechanism

1. Each source has a `:narrow` key (character or list)
2. `consult--multi-narrow` collects all narrow keys into a flat alist
3. When `consult-narrow-key` is pressed followed by a narrowing character, `consult--narrow` is set
4. The predicate `consult--multi-predicate` checks if a candidate's source is visible given the current narrow state
5. Hidden sources (`:hidden t`) are only shown when their narrow key is active
6. Multi-key narrowing allows `?p` to show all project sources, while `?B`, `?F`, `?R` narrow to specific project sources

### 5.4 Item Preparation

`consult--multi-items` processes raw items from each source:

1. Gets items from `:items` function
2. Appends a "tofu" character (Unicode private-use area U+E0000-U+E00FF) encoding the source index
3. Sets `multi-category` text property for Embark integration
4. Applies `:face` from source

The tofu suffix allows looking up which source a candidate came from without extra data structures.

---

## 6. Candidate Annotation and Grouping

### 6.1 Group Functions

Consult uses the standard Emacs `group-function` completion metadata:

- **Signature:** `(group-function CAND TRANSFORM)` -- If TRANSFORM is nil, return group title; if non-nil, return transformed candidate
- **`consult--type-group`:** Groups by `consult--type` text property (narrowing character to name mapping)
- **`consult--prefix-group`:** Groups by `consult--prefix-group` text property (used for grep results grouped by file)
- **`consult-org--group`:** Groups org headings by buffer name
- **`consult-info--group`:** Groups info results by node name
- **`consult--multi-group`:** Groups multi-source candidates by source `:name`

### 6.2 Annotation Functions

Annotations are provided via `affixation-function` in completion metadata:

- `consult--read-affixate` wraps annotation functions to produce `(candidate prefix suffix)` triples
- `consult--annotate-align` computes alignment width across candidates for consistent formatting
- Per-source `:annotate` functions in `consult--multi`
- Custom annotations for specific commands (e.g., `consult-org--annotate` adds TODO and priority)

### 6.3 History Integration

- Commands specify `:history` as either a symbol (standard history variable) or `(:input VAR)` for input-only history
- `:history t` disables history for commands where it doesn't make sense (e.g., kill-ring selection)
- `consult--add-history` populates future history (M-n) with defaults, custom items, and (for non-async) all completions

---

## 7. Preview System

### 7.1 State Function Protocol

The state function follows a strict lifecycle:

```
1. 'setup nil         -- After minibuffer initialization
2. 'preview CAND/nil  -- Preview candidate or reset (repeated)
3. 'preview nil       -- Final reset before exit
4. 'exit nil          -- Before minibuffer exit (still alive)
5. 'return CAND/nil   -- After minibuffer exit, perform action
```

The state function is ALWAYS called with the original window selected (via `consult--original-window`).

### 7.2 Preview Types

**Jump Preview (`consult--jump-preview`):**
- Saves current position (point, narrowing via min/max markers)
- Moves point to candidate position
- Creates overlays: line highlight (`consult-preview-line`) and match highlights (`consult-preview-match`)
- Temporarily opens invisible overlays (e.g., folded org sections)
- Restores everything on cancel
- Sets `cursor-in-non-selected-windows` to `'box` for visible cursor

**Buffer Preview (`consult--buffer-preview`):**
- Switches to candidate buffer in the preview window
- Saves and restores `window-prev-buffers`, `window-next-buffers`, and frame buffer lists
- Prevents preview in tabs (hard to restore)
- Respects `consult-preview-excluded-buffers`

**File Preview (`consult--file-preview`):**
- Combines `consult--temporary-files` with `consult--buffer-preview`
- Opens files temporarily with minimal initialization
- Filters `find-file-hook` to only allow whitelisted hooks
- Binds `consult-preview-variables` (inhibit-message, disable dir-locals, etc.)
- Supports partial preview for large files (only reads first `consult-preview-partial-chunk` bytes)
- Detects and refuses binary files
- Limits number of open preview buffers to `consult-preview-max-count` (default 10)
- Upgrades preview buffers to fully initialized on window selection change

**Insertion Preview (`consult--insertion-preview`):**
- Used by yank/history/completion-in-region
- Creates invisible overlay over the region to be replaced
- Shows candidate text as `before-string` with `consult-preview-insertion` face

**Man Preview (`consult--man-preview`):**
- Opens man page synchronously in background
- Caches opened buffers
- Kills preview buffers on exit

### 7.3 Preview Triggering

- `consult-preview-key` controls when preview runs:
  - `'any` -- Preview on every keystroke/navigation (default)
  - `nil` -- No preview
  - A key string -- Only preview when that key is pressed
  - `(:debounce SECONDS any)` -- Debounced preview on every action
- Per-command customization via `consult-customize`
- Per-source customization via `:preview-key` in source plists

### 7.4 Preview Debouncing

- Preview timer with configurable delay per key
- Compares candidates with `equal-including-properties` to avoid redundant previews
- Only previews when a completion window is selected and the preview window is alive

### 7.5 Temporary File Management

`consult--temporary-files` returns a closure that:
- Opens files for preview with `consult--find-file-temporarily`
- Caches already-previewed files
- Checks against existing fully-initialized buffers first
- Limits preview count to `consult-preview-max-count`
- Disassociates preview buffers from files (sets `buffer-file-name` to nil) to allow re-opening via Embark
- On window selection change (leaving minibuffer), fully initializes any live preview buffers

---

## 8. Key Internal Utilities

### 8.1 Tofu System

Consult uses Unicode private-use-area characters (U+E0000 range, called "tofu") as invisible suffixes on candidate strings to:
- Encode source index in multi-source candidates
- Disambiguate duplicate candidates (e.g., same text from different isearch types)
- Made invisible in minibuffer via `after-change-functions` hook
- Stripped via `kill-transform-function` to prevent tofu in kill ring

### 8.2 Regexp Compilation

`consult--compile-regexp` transforms user input into search patterns:
- Splits input into space-separated words
- Each word must match independently (AND semantics)
- Supports PCRE (lookahead for AND), extended (separate -e flags), emacs, and basic regex types
- Returns `(regexps . highlight-function)` pair
- Handles case-sensitivity detection

### 8.3 Lookup Functions

Various lookup strategies for resolving selected candidate string to actual value:
- `consult--lookup-member` -- Find exact member in candidates list
- `consult--lookup-candidate` -- Get `consult--candidate` text property
- `consult--lookup-location` -- Get `consult-location` text property
- `consult--lookup-cons` -- Return `(car . cdr)` of matching cons
- `consult--lookup-prop` -- Get arbitrary text property

---

## 9. Implications for Lexicon Implementation

### 9.1 What Can Run in WASM/Browser Without External Tools

The following commands only need Emacs-internal infrastructure:
- `consult-buffer` and variants (buffer/file/bookmark sources)
- `consult-line` / `consult-line-multi`
- `consult-outline`
- `consult-mark` / `consult-global-mark`
- `consult-goto-line`
- `consult-yank-*` (kill ring)
- `consult-history`
- `consult-theme`
- `consult-minor-mode-menu`
- `consult-mode-command`
- `consult-bookmark`
- `consult-imenu` / `consult-imenu-multi`
- `consult-register*`
- `consult-kmacro`
- `consult-completion-in-region`
- `consult-keep-lines` / `consult-focus-lines`
- `consult-isearch-history`
- `consult-complex-command`
- `consult-org-heading` / `consult-org-agenda`
- `consult-compile-error` / `consult-flymake`

### 9.2 What Requires External Process Support

These commands are fundamentally process-based and would need a backend/server:
- `consult-grep` / `consult-git-grep` / `consult-ripgrep`
- `consult-find` / `consult-fd`
- `consult-locate`
- `consult-man`

### 9.3 Critical Infrastructure Needed

For a Lexicon implementation of consult, the following subsystems are essential:

1. **`completing-read` with metadata support** -- category, group-function, affixation-function, sort control
2. **Minibuffer hooks** -- setup, exit, after-change-functions, post-command-hook
3. **Timer system** -- For debouncing, throttling, and async refresh
4. **Marker system** -- For position tracking across edits
5. **Overlay system** -- For preview highlighting
6. **Text property system** -- For candidate metadata transport
7. **Buffer-local variables** -- Extensively used
8. **Mark ring** (buffer-local and global)
9. **Kill ring** with `insert-for-yank`
10. **Narrowing/widening** -- `point-min`/`point-max` restrictions
11. **Async process communication** (for grep/find commands) -- Would need WebSocket/Worker bridge to backend

### 9.4 Complexity Assessment

The async pipeline (~660 lines) is self-contained and well-architected. The multi-source system (~250 lines) is clean. The preview system (~520 lines) is the most complex part due to buffer/file management concerns that may not apply in a browser context. The individual commands are relatively simple once the infrastructure is in place.