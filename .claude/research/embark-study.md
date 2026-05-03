# Embark Package Deep Dive

**Date:** 2026-05-03
**Source:** oantolin/embark, ~/projects/embark/

---

Here is the comprehensive study of the Embark package.

---

# Comprehensive Study: Embark (oantolin/embark)

**Package:** embark.el v1.2
**Author:** Omar Antolin Camarena
**License:** GPL-3.0 (part of GNU Emacs)
**Requires:** Emacs 29.1, compat 31
**Repository:** https://github.com/oantolin/embark
**Source files studied:** `embark.el` (4698 lines), `embark-consult.el` (full), `embark-org.el` (summary), `README.org`

---

## 1. Core Concept

### What Problem Does Embark Solve?

Embark provides a "right-click contextual menu" for Emacs. In traditional Emacs, when you are in a minibuffer completing a file name, you can only do one thing with the result: pass it to the command that opened the minibuffer. Embark breaks this limitation by letting you perform *any* action on the current completion candidate (or any "thing at point" in a regular buffer) without leaving the completion session.

The core insight is: **targets and actions should be decoupled from the command that started the completion session.**

### How It Differs from Just Selecting a Completion Candidate

Without Embark, the workflow is linear:
1. Run `find-file`
2. Type partial filename
3. Select candidate
4. The file is opened (the only possible outcome)

With Embark, at step 3 you can instead press `embark-act` and then:
- Copy the file (`c`)
- Delete the file (`d`)
- Rename the file (`r`)
- Open it in another window (`o`)
- Run a shell command on it (`!`)
- Insert it into the previous buffer (`i`)
- Copy the candidate string to the kill ring (`w`)
- Export all candidates to a Dired buffer (`E`)
- ...and any other command you want

The minibuffer does not necessarily quit after the action -- you can continue selecting and acting on other candidates.

### The "Act on Thing at Point" Model

Embark works both inside and outside the minibuffer:

- **In the minibuffer:** The target is the current top completion candidate (as determined by the completion UI -- Vertico, Icomplete, etc.).
- **In the *Completions* buffer:** The target is the completion at point.
- **In regular buffers:** The target is determined by a chain of "target finders" that examine what is at point -- the active region, a file path, a URL, a symbol, an expression, a heading, etc.

Critically, in regular buffers there can be *multiple* targets at the same point (e.g., the text "foo.el" could be both a symbol and a file). Embark supports **target cycling** to let you choose which interpretation to act on.

---

## 2. Complete Command Inventory

### Primary User Commands

| Command | Description |
|---|---|
| `embark-act` | Prompt for and perform an action on the current target. Uses `embark-prompter` (default: keymap-based). Supports prefix arg to toggle quit behavior (in minibuffer) or to skip/rotate targets (outside minibuffer). |
| `embark-dwim` | "Do What I Mean" -- immediately run the *default* action on the current target without prompting. The default action is the command that opened the minibuffer, or the RET binding in the target's action keymap. |
| `embark-act-all` | Prompt for an action, then apply it to *all* current candidates (not just the current one). Supports confirmation via `embark-confirm-act-all`. |
| `embark-collect` | Snapshot all current candidates into a dedicated `embark-collect-mode` buffer (tabulated-list-mode). |
| `embark-live` | Like `embark-collect` but live-updating: the buffer refreshes as you type in the minibuffer. Uses idle timers (0.05s) via `after-change-functions`. |
| `embark-export` | Export candidates to an *appropriate major mode* buffer (Dired for files, Ibuffer for buffers, Package Menu for packages, etc.). |
| `embark-select` | Toggle selection of the current target for later bulk action with `embark-act-all`. Bound to SPC in action keymaps. Uses overlays to highlight selected items. |
| `embark-become` | While in a minibuffer, switch to a *different* command, keeping the current input. E.g., switch from `switch-to-buffer` to `find-file` mid-input. |
| `embark-cycle` | Cycle to the next target at point (when multiple targets overlap). Not meant to be called directly; activated via `embark-act`. |
| `embark-done` | Terminate a sequence of repeated actions. |
| `embark-toggle-quit` | Toggle whether the next `embark-act` quits the minibuffer. |

### Exploration Commands

| Command | Description |
|---|---|
| `embark-bindings` | Explore current command key bindings with completing-read. Optionally includes global bindings with prefix arg. |
| `embark-bindings-in-keymap` | Explore a specific keymap's bindings interactively. |
| `embark-bindings-at-point` | Explore key bindings from text-property keymaps at point. |
| `embark-prefix-help-command` | Intended for `prefix-help-command`: shows bindings under the current prefix. |

### Collect/Export Management

| Command | Description |
|---|---|
| `embark-rerun-collect-or-export` | Revert/rerun the embark-collect or embark-export that created the current buffer. |
| `embark-collect-direct-action-minor-mode` | Minor mode that binds type-specific actions directly on collect buffer entries (without needing `embark-act`). |

### Integration with Eldoc

| Command | Description |
|---|---|
| `embark-eldoc-first-target` | Eldoc function reporting the first Embark target at point. |
| `embark-eldoc-target-types` | Eldoc function reporting all target types at point. |

### Context Menu Integration

| Command | Description |
|---|---|
| `embark-context-menu` | Adds Embark items to the Emacs context menu (right-click). |

---

## 3. Target System

### What Is a "Target"?

A target is a structured representation of "the thing the user wants to act on." It is a plist with the following keys:

```
:type          - Symbol indicating the category (file, buffer, symbol, url, etc.)
:target        - String value of the target (possibly transformed)
:orig-type     - Original type before transformation
:orig-target   - Original target string before transformation
:bounds        - Cons cell (START . END) of buffer positions bounding the target
```

### How Targets Are Identified

Targets are discovered by running functions in the hook `embark-target-finders`. Each function returns either nil or a target specification.

**Critical distinction:**
- **In the minibuffer:** Only the *first* target finder returning non-nil is used (since the minibuffer has a single unambiguous target: the completion candidate).
- **In regular buffers:** *All* target finders are run, producing a list of potentially overlapping targets that can be cycled through.

### Target Finder Return Format

A target finder returns one of:
1. A cons `(TYPE . TARGET)` -- type symbol and target string
2. A dotted list `(TYPE TARGET START . END)` -- with buffer bounds
3. A list of such items (multiple targets from one finder)
4. `nil` -- no target found

### Default Target Finders (in priority order)

```elisp
embark-target-finders:
  embark-target-completion-list-candidate  ;; *Completions* buffer
  embark-target-top-minibuffer-candidate   ;; minibuffer top candidate
  embark-target-active-region              ;; active region -> type 'region
  embark-target-collect-candidate          ;; embark-collect buffer
  embark-target-text-heading-at-point      ;; outline heading (text modes)
  embark-target-bug-reference-at-point     ;; bug-reference overlay
  embark-target-flymake-at-point           ;; flymake diagnostic overlay
  embark-target-smerge-at-point            ;; smerge conflict overlay
  embark-target-package-at-point           ;; package in package-menu
  embark-target-email-at-point             ;; email address
  embark-target-url-at-point               ;; URL (including shr-url text properties)
  embark-target-file-at-point              ;; file (dired, ffap-aware)
  embark-target-buffer-at-point            ;; buffer in ibuffer/buffer-menu
  embark-target-custom-variable-at-point   ;; customize widget variable
  embark-target-identifier-at-point        ;; identifier/symbol (with Lisp promotion)
  embark-target-guess-file-at-point        ;; ffap file guess
  embark-target-expression-at-point        ;; sexp at point
  embark-target-sentence-at-point          ;; sentence (text modes only)
  embark-target-paragraph-at-point         ;; paragraph (text modes only)
  embark-target-defun-at-point             ;; defun at point
  embark-target-prog-heading-at-point      ;; outline heading (prog modes)
```

### Target Type Hierarchy

The identifier target finder (`embark-target-identifier-at-point`) is particularly interesting. In Emacs Lisp buffers, it "promotes" identifiers to more specific types by introspecting the symbol:

```
identifier -> command (if commandp)
identifier -> variable (if boundp and not keyword)
identifier -> function (if fboundp and not commandp)
identifier -> face (if facep)
identifier -> library (if ffap-el-mode finds it)
identifier -> package (if package-desc exists)
identifier -> symbol (fallback in Lisp modes)
```

A single identifier can return *multiple* targets of different types, all of which are added to the target list for cycling.

### Target Transformers

After targets are found, they are optionally transformed via `embark-transformer-alist`:

```elisp
embark-transformer-alist:
  (minor-mode . embark--lookup-lighter-minor-mode)  ;; lighter -> minor mode name
  (embark-keybinding . embark--keybinding-command)   ;; keybinding -> command
  (project-file . embark--project-file-full-path)    ;; relative -> absolute path
  (package . embark--remove-package-version)          ;; remove version suffix
  (multi-category . embark--refine-multi-category)   ;; resolve multi-category
  (buffer . embark--uniquify-orig-buffer)             ;; resolve uniquify name
  (file . embark--simplify-path)                      ;; substitute-in-file-name
```

Each transformer receives `(type target)` and returns `(new-type . new-target)`.

### Target Cycling Mechanism

When `embark-act` is called outside the minibuffer, the list of targets found by `embark--targets` is presented in order. The user can cycle through them using `embark-cycle` (bound to the same key as `embark-act` by default, or a separate key via `embark-cycle-key`).

The cycle key is dynamically bound into the action keymap. Pressing it rotates the targets list and updates the indicators.

For **repeated actions** (actions in `embark-repeat-actions`), after the action executes, `embark-act` does not exit. Instead, it re-queries for targets and attempts to start the cycle at the same target type that was just acted upon (or a specified type, e.g., `region` after a marking command).

### Macros for Defining Target Finders

Embark provides three macros for concisely defining target finders:

1. **`embark-define-overlay-target`** -- for targets identified by overlays with a specific property
2. **`embark-define-thingatpt-target`** -- for targets using `thing-at-point`, optionally restricted to specific major modes
3. **`embark-define-regexp-target`** -- for targets matching a regexp around point

---

## 4. Action System

### What Is an Action?

An action is simply any Emacs command. Embark treats every command as a potential action. The target is "injected" into the command's first minibuffer prompt by inserting it and scheduling `exit-minibuffer` on `post-command-hook`.

### Action Keymaps per Target Type

The association between target types and action keymaps is defined in `embark-keymap-alist`:

| Target Type | Keymap(s) |
|---|---|
| `file` | `embark-file-map` |
| `library` | `embark-library-map` |
| `environment-variables` | `embark-file-map` |
| `url` | `embark-url-map` |
| `email` | `embark-email-map` |
| `buffer` | `embark-buffer-map` |
| `tab` | `embark-tab-map` |
| `expression` | `embark-expression-map` |
| `identifier` | `embark-identifier-map` |
| `defun` | `embark-defun-map` (parent: `embark-expression-map`) |
| `symbol` | `embark-symbol-map` (parent: `embark-identifier-map`) |
| `face` | `embark-face-map` (parent: `embark-symbol-map`) |
| `command` | `embark-command-map` (parent: `embark-function-map`) |
| `variable` | `embark-variable-map` (parent: `embark-symbol-map`) |
| `function` | `embark-function-map` (parent: `embark-symbol-map`) |
| `minor-mode` | `embark-command-map` |
| `unicode-name` | `embark-unicode-name-map` |
| `package` | `embark-package-map` |
| `bookmark` | `embark-bookmark-map` |
| `region` | `embark-region-map` |
| `sentence` | `embark-sentence-map` (parent: `embark-prose-map`) |
| `paragraph` | `embark-paragraph-map` (parent: `embark-prose-map`) |
| `kill-ring` | `embark-kill-ring-map` |
| `heading` | `embark-heading-map` |
| `flymake` | `embark-flymake-map` |
| `smerge` | `smerge-basic-map` + `embark-general-map` |
| `t` (default) | `embark-general-map` |

Multiple keymaps per type are supported; they are composed into one via `make-composed-keymap`.

### The embark-general-map (Available for All Types)

All action keymaps inherit from `embark-general-map` (via `:parent`), which provides these universally available actions:

```
i   embark-insert         -- Insert target into previous buffer
w   embark-copy-as-kill   -- Copy target to kill ring
q   embark-toggle-quit    -- Toggle quit behavior
E   embark-export         -- Export all candidates
S   embark-collect        -- Snapshot all candidates
L   embark-live           -- Live-updating collect
B   embark-become         -- Switch to different command
A   embark-act-all        -- Act on all candidates
C-s embark-isearch-forward
C-r embark-isearch-backward
C-SPC mark
DEL delete-region
SPC embark-select         -- Toggle selection
```

### Detailed Action Keymaps

**embark-file-map** (key actions for files):
```
RET/f  find-file           d  delete-file
F      find-file-literally  D  delete-directory
o      find-file-other-window   r  rename-file
c      copy-file            s  make-symbolic-link
!      shell-command        &  async-shell-command
x      embark-open-externally   j  embark-dired-jump
$      eshell               <  insert-file
m      chmod                =  ediff-files
+      make-directory       \  embark-recentf-remove
I      embark-insert-relative-path
W      embark-save-relative-path
e      eww-open-file        l  load-file
b      byte-compile-file    R  byte-recompile-directory
v      embark-vc-file-map (sub-keymap: d=vc-delete, r=vc-rename, i=vc-ignore)
```

**embark-buffer-map**:
```
RET/b  switch-to-buffer     k  kill-buffer
o      switch-to-buffer-other-window
z      embark-bury-buffer   K  embark-kill-buffer-and-window
r      embark-rename-buffer =  ediff-buffers
|      embark-shell-command-on-buffer
<      insert-buffer        x  embark-open-externally
j      embark-dired-jump    $  eshell
```

**embark-symbol-map** (for Emacs Lisp symbols):
```
RET/d  embark-find-definition   h  describe-symbol
s      embark-info-lookup-symbol   e  pp-eval-expression
a      apropos              \  embark-history-remove
(inherits from embark-identifier-map: xref-find-definitions, xref-find-references, etc.)
```

**embark-region-map** (for active regions, with sub-keymaps):
```
u  upcase-region     l  downcase-region   c  capitalize-region
|  shell-command-on-region   e  eval-region   <  embark-eval-replace
a  align             A  align-regexp
TAB indent-region    f  fill-region       $  ispell-region
=  count-words-region   ;  comment-or-uncomment-region
s  embark-sort-map (sub-keymap: l=sort-lines, n=sort-numeric-fields, etc.)
>  embark-encode-map (sub-keymap: r=rot13, m=md5, b=base64-encode, u=url-encode, etc.)
```

### Default Action Resolution

The default action (bound to RET / `[13]`) is determined by `embark--default-action`:

1. Check `embark-default-action-overrides` for a `(type . command)` pair-specific override
2. Check `embark-default-action-overrides` for a type-specific override
3. Check `embark-default-action-overrides` for a `t` (universal) override
4. Use `embark--command` (the command that opened the minibuffer)
5. Look up the RET binding in the raw action keymap for the type

### Action Execution: The Hook Pipeline

When an action is executed via `embark--act`, it goes through a sophisticated hook pipeline:

1. **`embark-target-injection-hooks`** -- Run after the target is injected into the minibuffer. Used to prepare the target (e.g., `embark--shell-prep` adds quoting for shell commands, `embark--eval-prep` wraps functions in parens, `embark--allow-edit` removes auto-exit to let user edit, `embark--ignore-target` removes the injected target).

2. **`embark-pre-action-hooks`** -- Run before the action. Used for positioning (`embark--beginning-of-target`, `embark--end-of-target`), confirmation (`embark--confirm`), unmarking (`embark--unmark-target`), marker stacking (`embark--xref-push-marker`).

3. **`embark-around-action-hooks`** -- Wrap the action execution. Used for `save-mark-and-excursion` around region commands (`embark--mark-target`), changing `default-directory` (`embark--cd`), and the selection mechanism (`embark--select`). These are composed as nested function wrappers via `seq-reduce`.

4. **`embark-post-action-hooks`** -- Run after the action. Primarily used for `embark--restart` (restarting the minibuffer after destructive operations like `delete-file`, `kill-buffer`, etc.).

Each hook alist supports three special keys:
- A command symbol -- hooks for that specific command
- `t` -- default hooks (used when no command-specific hooks exist)
- `:always` -- hooks that always run regardless

### embark-act-all (Bulk Actions)

`embark-act-all` collects all candidates via `embark-candidate-collectors` and runs the chosen action on each one. For commands in `embark-multitarget-actions` (like `embark-insert` and `embark-copy-as-kill`), the action receives the full list as a single argument rather than being called once per candidate.

Confirmation is requested via `y-or-n-p` if `embark-confirm-act-all` is non-nil.

The `embark--confirm` and `embark--restart` hooks are suppressed during bulk actions (via `cl-letf` overriding them with `#'ignore`).

### Multitarget Actions

Commands listed in `embark-multitarget-actions` receive a list of targets as a single argument:
```elisp
(defcustom embark-multitarget-actions '(embark-insert embark-copy-as-kill) ...)
```

For `embark-act` on a single target, these are called with a one-element list. For `embark-act-all`, they receive the complete candidate list.

---

## 5. Integration Points

### How Embark Integrates with completing-read

Embark does not modify the completion framework itself. Instead:

1. It records the command that opened the minibuffer via `embark--record-this-command` (on `minibuffer-setup-hook`).
2. It queries completion metadata to determine the category of candidates (via `completion-metadata-get` for `'category`).
3. It retrieves candidates via `completion-all-completions` or `completion-all-sorted-completions`.
4. The category becomes the target *type*, determining which action keymap to use.

This means Embark works with **any** completion framework that uses `completing-read` and provides proper metadata.

### Vertico Integration (built into embark.el)

Embark includes built-in Vertico integration, activated via `with-eval-after-load 'vertico`:

- **`embark--vertico-selected`**: Target finder that gets the currently selected Vertico candidate via `vertico--candidate`.
- **`embark--vertico-candidates`**: Candidate collector that returns `vertico--candidates`.
- **`embark--vertico-indicator`**: Highlights the current Vertico candidate with `embark-target` face via `face-remapping-alist`.
- **Selection display**: Uses `cl-defmethod vertico--format-candidate :around` to apply `embark-selected` face to selected candidates.

These are added to the appropriate hooks automatically when Vertico is loaded.

### Marginalia Integration (built into embark.el)

Minimal but important:
```elisp
(with-eval-after-load 'marginalia
  (push 'marginalia--cache-reset (alist-get :always embark-post-action-hooks)))
```

This ensures Marginalia's annotation cache is reset after every action, so that (for example) `embark-toggle-variable` immediately shows the updated value in the annotations.

Embark also uses Marginalia's affixation/annotation functions when formatting collect buffers, via `embark--get-affixator`.

### Ivy Integration (built into embark.el)

Similar to Vertico: provides `embark--ivy-selected` and `embark--ivy-candidates` target finder/collectors, detected by checking for `ivy--queue-exhibit` on `post-command-hook`.

### The Prompter System

Embark uses a pluggable prompter system (`embark-prompter`):

1. **`embark-keymap-prompter`** (default): Activates the action keymap via `overriding-terminal-local-map` and reads a key sequence. This is the "which-key-like" experience. Handles universal argument, scrolling, digit arguments, and falls back to `execute-extended-command` for M-x.

2. **`embark-completing-read-prompter`**: Shows all available actions via `completing-read`, with formatted key bindings and documentation. Supports switching to the keymap prompter via `@` key.

You can switch between them: from the keymap prompter, pressing C-h switches to completing-read; from completing-read, pressing `@` switches to the keymap prompter.

### Indicator System

Embark uses an indicator protocol to display information about the current action context. Multiple indicators can be active simultaneously. The protocol:

- **Called without arguments**: Returns a closure (the indicator instance).
- **Closure called with `(keymap targets prefix)`**: Update the display.
- **Closure called with no arguments**: Cleanup (remove overlays, close windows, etc.).

Built-in indicators:

1. **`embark-minimal-indicator`**: Shows target type and value in echo area or minibuffer prompt overlay.
2. **`embark-verbose-indicator`**: Pops up a buffer (`" *Embark Actions*"`) with a formatted table of key bindings, docstrings, target info, and cycle information. Sections are configurable via `embark-verbose-indicator-buffer-sections`.
3. **`embark-mixed-indicator`**: Combines minimal + verbose with a delay (`embark-mixed-indicator-delay`, default 0.5s). Shows minimal first, then verbose after the delay.
4. **`embark-highlight-indicator`**: Highlights the target at point using an overlay with `embark-target` face.
5. **`embark-isearch-highlight-indicator`**: For identifier/symbol targets, lazily highlights all occurrences using isearch's lazy highlight.

Default configuration:
```elisp
(defcustom embark-indicators
  '(embark-mixed-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator) ...)
```

---

## 6. Emacs Infrastructure Dependencies

### Critical Emacs Primitives Used

**Keymaps:**
- `make-composed-keymap` -- composing multiple keymaps (action keymaps inherit from general-map)
- `make-sparse-keymap`, `define-key`, `keymap-set`
- `overriding-terminal-local-map` -- used by the keymap prompter to intercept all input
- `key-binding` -- look up bindings considering overriding maps
- `map-keymap` -- iterate over keymap entries
- `keymap-canonicalize` -- normalize keymap for iteration
- `where-is-internal` -- find keys bound to a command

**Minibuffer:**
- `minibuffer-setup-hook` -- to record the command
- `minibuffer-completion-table`, `minibuffer-completion-predicate` -- to access completion state
- `completion-metadata`, `completion-metadata-get` -- to read category and other metadata
- `completion-all-completions`, `completion-all-sorted-completions` -- to get candidates
- `completion-boundaries` -- used by `embark-become`
- `minibuffer-with-setup-hook` -- to inject targets into action minibuffers
- `enable-recursive-minibuffers` -- actions may open their own minibuffers
- `minibuffer-quit-recursive-edit` -- for quitting
- `minibuffer-selected-window` -- to find the "previous" window

**Text Properties:**
- `multi-category` -- used by `embark-select` and Consult's multi-source buffers
- `embark-command` -- on keybinding strings, to associate commands
- `embark--initial-input` -- stored on injected text to support `embark--ignore-target`
- `embark--location` -- markers for collect buffer entries back to source positions
- `consult-strip`, `consult-xref`, `consult-man` -- Consult-specific properties

**Overlays:**
- Target highlighting (`embark-target-overlay` category, priority 1001)
- Selection highlighting (`embark-selected-overlay` category, priority 1001)
- Minimal indicator overlay in minibuffer prompt

**Buffer Management:**
- `tabulated-list-mode` -- base for `embark-collect-mode`
- `generate-new-buffer`, `rename-buffer`
- `display-buffer` with configurable `display-buffer-alist` entries
- `set-window-dedicated-p` -- collect buffers use dedicated windows
- Button types (`define-button-type`) for interactive collect entries

**Completion Metadata:**
- `category` -- determines target type
- `affixation-function` / `annotation-function` -- for collect buffer formatting
- `group-function` -- for grouped display in collect buffers
- `display-sort-function`, `cycle-sort-function` -- set to `identity` to preserve order

**Hooks:**
- `post-command-hook` -- for deferred execution after quitting minibuffer
- `after-change-functions` -- for live collect updating
- `completion-setup-hook` -- to cache info in *Completions* buffer
- `change-major-mode-hook` -- to stop live collect

**Timers:**
- `run-at-time` / `run-with-idle-timer` -- for mixed indicator delay and live collect debouncing

### How embark-export Decides What Mode to Use

The `embark-exporters-alist` maps completion categories to exporter functions:

```elisp
(defcustom embark-exporters-alist
  '((buffer . embark-export-ibuffer)          ;; -> ibuffer
    (file . embark-export-dired)              ;; -> dired
    (package . embark-export-list-packages)   ;; -> package-menu-mode
    (bookmark . embark-export-bookmarks)      ;; -> bookmark-bmenu-mode
    (variable . embark-export-customize-variable) ;; -> Custom-mode
    (face . embark-export-customize-face)     ;; -> Custom-mode
    (symbol . embark-export-apropos)          ;; -> apropos-mode
    (minor-mode . embark-export-apropos)
    (function . embark-export-apropos)
    (command . embark-export-apropos)
    (t . embark-collect))                     ;; -> embark-collect-mode (fallback)
  ...)
```

The `t` key provides a fallback: if no specific exporter exists, `embark-collect` is used. This alist is extensible -- `embark-consult.el` adds exporters for `consult-location` and `consult-grep`.

Each exporter function receives a list of candidate strings and must create and display an appropriate buffer.

---

## 7. The embark-consult Integration

`embark-consult.el` is a separate file (also by Omar Antolin) that bridges Embark and Consult. It is automatically loaded when both packages are present:

```elisp
(with-eval-after-load 'consult
  (unless (require 'embark-consult nil 'noerror)
    (warn "...")))
```

### Specific Integrations

#### consult-location Support

Consult commands like `consult-line`, `consult-outline`, `consult-mark`, and `consult-global-mark` produce candidates with category `consult-location`. These are markers+line-numbers embedded in text properties.

**Target transformer:** `embark-consult--target-strip` -- strips invisible unicode characters that Consult uses for internal purposes (via the `consult-strip` text property).

**Default action override:** `embark-consult-goto-location` -- jumps to the location and pulses the line.

**Exporters:**
- `embark-consult-export-location-occur` -- Creates an `occur-mode` buffer. This is the default. Uses proper `occur-prefix`, `occur-target`, `occur-match` text properties so that `occur-edit-mode` works for in-place editing.
- `embark-consult-export-location-grep` -- Creates a `grep-mode` buffer. Only works for file-backed buffers. Warns about non-file buffers.

**Marker upgrade:** `embark-consult--upgrade-markers` on `embark-collect-mode-hook` converts Consult's cheap markers to real markers when collecting `consult-location` candidates.

#### consult-grep Support

Candidates from `consult-grep`, `consult-ripgrep`, `consult-git-grep` have category `consult-grep`.

**Default action override:** `embark-consult-goto-grep` -- jumps to the grep match location and pulses the line.

**Exporter:** `embark-consult-export-grep` -- Creates a `grep-mode` buffer with proper compilation-message properties. Supports `wgrep-setup` for editable grep results (and the newer `grep-edit-mode` on Emacs 31). Includes:
- `embark-consult-rerun-map` with `g` bound to `embark-rerun-collect-or-export`
- Match highlighting via `consult-highlight-match` face -> `match` face conversion
- Proper `compilation--ensure-parse` for next-error navigation

The shared infrastructure function `embark-consult--export-grep` handles the common logic for both grep and location-grep exporters, accepting keyword arguments `:header`, `:lines`, `:insert`, and `:footer`.

#### consult-xref Support

**Exporter:** `embark-consult-export-xref` -- Creates an xref buffer from `consult-xref` candidates using `xref--show-xref-buffer`.

**Default action:** `embark-consult-xref` -- jumps to the xref location via `xref-pop-to-location`.

#### consult-find/locate/fd Support

For file-finding commands, the default action is overridden to `find-file`:
```elisp
(dolist (cmd '(consult-find consult-locate consult-fd))
  (setf (alist-get `(file . ,cmd) embark-default-action-overrides ...) #'find-file))
```

#### consult-man and consult-info Support

- `embark-consult-man` -- opens man page using the `consult-man` text property
- `embark-consult-info` -- opens info node via `consult-info--action`
- Both have target strippers and default action overrides

#### consult-isearch-history Support

Target stripper applied to `consult-isearch-history` to clean up display strings.

#### embark-consult-search-map

A key part of the integration: Consult search commands are made available as Embark actions:

**Sync search commands** (`embark-consult-sync-search-map`):
```
o  consult-outline
i  consult-imenu
I  consult-imenu-multi
l  consult-line
L  consult-line-multi
```

**Async search commands** (`embark-consult-async-search-map`):
```
g  consult-grep
r  consult-ripgrep
G  consult-git-grep
f  consult-find
d  consult-fd
F  consult-locate
```

These are combined into `embark-consult-search-map` and bound under `C` in `embark-general-map`, meaning from any target you can press `C g` to grep, `C l` to search lines, etc.

The sync search map is also added to `embark-become-match-map` under `C`, enabling "becoming" a Consult search command from another search.

**DWIM behavior for async searches:** `embark-consult--async-search-dwim` is an around-action hook for async search commands. When acting on files/buffers/libraries/bookmarks, it searches *within* those files (setting `consult-project-function` to nil). For other target types, it uses the target as initial input.

#### Candidate Collectors

- `embark-consult-outline-candidates` -- collects outline headings as `consult-location`
- `embark-consult-imenu-candidates` -- collects imenu items
- `embark-consult-imenu-or-outline-candidates` -- picks imenu for prog-mode, outline otherwise

These are added to `embark-candidate-collectors`, enabling `embark-act-all` to work on outline headings and imenu items in regular buffers.

#### Live Preview

```elisp
(add-hook 'embark-collect-mode-hook
          'consult--default-completion-list-preview-setup)
```

This enables Consult's preview mechanism in `embark-live` buffers.

---

## 8. Architecture Summary for Lexicon Implementation

### Key Architectural Patterns

1. **Target-Action Decoupling**: Targets are discovered independently of actions. The type system (via `embark-keymap-alist`) connects them. This is a clean separation of concerns.

2. **Keymap-Driven Actions**: Actions are organized as Emacs keymaps with inheritance. This leverages Emacs's existing keymap composition infrastructure. Each type-specific map inherits from `embark-general-map`.

3. **Hook Pipeline**: The four hook types (injection, pre, around, post) provide extreme flexibility without modifying action commands themselves. Actions are just normal Emacs commands -- Embark wraps them.

4. **Pluggable Everything**: Target finders, transformers, prompters, indicators, candidate collectors, and exporters are all hook-based or alist-based, making the system open for extension without modification.

5. **State Caching**: Information about the completion context (command, type, target buffer, target window, default-directory) is cached in buffer-local variables in collect/completions buffers.

6. **Deferred Execution**: `embark--quit-and-run` uses `post-command-hook` and backup timers to execute actions after the minibuffer has been properly cleaned up.

### What Lexicon Would Need

To implement Embark-like functionality, Lexicon would need:

- **Target finder framework**: A hook/chain of functions that inspect the current context (minibuffer candidate, thing at point) and return typed targets.
- **Action keymaps per type**: Hierarchical keymaps associated with target types.
- **Target injection mechanism**: The ability to insert a target string into a new minibuffer prompt and auto-accept it.
- **Candidate collection**: Access to all current completion candidates (not just the selected one).
- **Export infrastructure**: The ability to create mode-specific buffers (Dired-like for files, etc.) from a list of candidates.
- **Collect/tabulated-list infrastructure**: A table display mode for arbitrary candidates with annotations.
- **Indicator display**: Some mechanism to show the user what target is selected and what actions are available (overlay-based for minimal, buffer-based for verbose).
- **Selection mechanism**: Buffer-local selection state with overlay highlighting.
- **Completion metadata**: The category system from completing-read metadata is essential for determining target types from minibuffer context.