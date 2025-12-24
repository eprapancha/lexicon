# Display, Theming, and Visual Infrastructure Research

**Date:** 2025-12-24
**Context:** Strategic research to understand Emacs's visual presentation systems before implementing package ecosystem

---

## Executive Summary

This research reveals that **we've been building the wrong foundation**. We focused on package system and Evil-mode while missing the fundamental display infrastructure that makes Emacs actually *Emacs*.

**Critical Finding:** The completion framework, Evil-mode, and all sophisticated packages **depend on display infrastructure we don't have**:
- Face system (colors, fonts, styling)
- Overlay system (temporary visual annotations)
- Text properties (persistent text styling)
- Mode line customization
- Theme system

**Recommendation:** **Pause all package work**. Implement display/theming foundation first (4 weeks), then completion framework (6 weeks), then Evil-mode integration.

---

## 1. The Face System: Foundation of Visual Presentation

### What is a Face?

A **face** in Emacs is a named collection of visual attributes that can be applied to text. Think of faces as CSS classes for buffer text.

```elisp
;; Face definition
(defface my-warning-face
  '((t (:foreground "red" :weight bold)))
  "Face for warnings")

;; Apply to text
(put-text-property start end 'face 'my-warning-face)
```

### Face Attributes

**Core attributes we need:**

| Attribute | Values | Purpose |
|-----------|--------|---------|
| `:foreground` | Color (hex, rgb, named) | Text color |
| `:background` | Color | Background color |
| `:font-family` | String ("monospace", "serif") | Font family |
| `:font-size` | Number (points or pixels) | Font size |
| `:font-weight` | `normal`, `bold`, `light` | Weight/boldness |
| `:font-slant` | `normal`, `italic`, `oblique` | Slant/style |
| `:underline` | Boolean, color, or `(:color "red" :style wave)` | Underline |
| `:overline` | Boolean or color | Line above text |
| `:strike-through` | Boolean or color | Strikethrough |
| `:box` | Boolean or `(:line-width 1 :color "#000")` | Border box |
| `:inverse-video` | Boolean | Swap fg/bg |
| `:extend` | Boolean | Extend to window edge (for region) |
| `:inherit` | Face name or list | Inherit from other faces |

### Standard Faces Required

These faces are **expected to exist** by all Emacs packages:

**Text editing:**
- `default` - Base face for all text
- `region` - Selected region (our current blue highlight)
- `highlight` - Generic temporary highlighting
- `shadow` - De-emphasized text (comments, etc.)

**UI elements:**
- `mode-line` - Active window mode line
- `mode-line-inactive` - Inactive window mode line
- `mode-line-highlight` - Mouse hover in mode line
- `minibuffer-prompt` - Minibuffer prompt
- `echo-area` - Echo area messages

**Search:**
- `isearch` - Current search match
- `lazy-highlight` - Other matches
- `match` - Matching in completion

**Semantic:**
- `error` - Error messages (red)
- `warning` - Warnings (orange/yellow)
- `success` - Success messages (green)

**Syntax highlighting (`font-lock-*`):**
- `font-lock-comment-face` - Comments
- `font-lock-string-face` - Strings
- `font-lock-keyword-face` - Keywords (def, let, if)
- `font-lock-function-name-face` - Function names
- `font-lock-variable-name-face` - Variables
- `font-lock-type-face` - Types/classes
- `font-lock-constant-face` - Constants
- `font-lock-builtin-face` - Built-ins
- `font-lock-warning-face` - Warnings in code

**Completion UI:**
- `completions-annotations` - Completion annotations (Marginalia uses this)
- `completions-common-part` - Common prefix in completions
- `completions-first-difference` - First char that differs

### Face Inheritance

Faces can inherit from other faces:

```clojure
:faces {
  :default {:foreground "#000000" :background "#ffffff"}
  :error {:foreground "#ff0000" :inherit :default}  ; Red text, inherits other attrs
  :my-special-error {:underline true :inherit :error}  ; Red, underlined
}
```

### Face Application Priority

When multiple sources specify faces, priority order is:

1. **Overlay faces** (sorted by overlay priority, highest wins)
2. **Text property `face`**
3. **Text property `font-lock-face`** (lower priority, used by syntax highlighting)
4. **Default face**

---

## 2. Text Properties vs Overlays

### Text Properties

**What:** Attributes attached directly to buffer text characters.

**Characteristics:**
- Part of the buffer text itself
- Move with text when copied/pasted
- Persistent across buffer modifications
- Used by: font-lock (syntax highlighting), read-only regions

**Key properties:**
- `face` - Apply face to text
- `font-lock-face` - Lower priority face (syntax highlighting)
- `display` - Replace text with image/string/space (visual substitution)
- `invisible` - Hide text (folding, etc.)
- `read-only` - Prevent modification
- `help-echo` - Tooltip on hover
- `cursor` - Cursor behavior at position

**API:**
```clojure
(defn put-text-property [start end prop value]
  ;; Attach property to text range
  ...)

(defn get-text-property [pos prop]
  ;; Get property at position
  ...)

(defn remove-text-properties [start end props]
  ;; Remove properties from range
  ...)
```

### Overlays

**What:** Temporary annotations independent of buffer text.

**Characteristics:**
- **Not** part of buffer text
- Do **not** move when text is copied
- Have priority system (higher priority overlays win)
- Can be deleted without affecting text
- Used by: completion UI, visual selection, temporary highlights

**All text properties, plus:**
- `priority` - Stacking order (default 0, higher wins)
- `before-string` - Insert string before overlay
- `after-string` - Insert string after overlay
- `line-prefix` - Prefix for wrapped lines
- `wrap-prefix` - Prefix for continuation lines
- `evaporate` - Auto-delete when empty
- `modification-hooks` - Called when overlayed text modified
- `insert-in-front-hooks` - Called when inserting before overlay

**API:**
```clojure
(defn make-overlay [start end &optional buffer]
  ;; Create overlay
  {:start start :end end :properties {} :priority 0})

(defn overlay-put [overlay prop value]
  ;; Set overlay property
  ...)

(defn delete-overlay [overlay]
  ;; Remove overlay
  ...)

(defn overlays-in [start end]
  ;; Get all overlays in range
  ...)

(defn overlays-at [pos]
  ;; Get all overlays at position
  ...)
```

### When to Use What

| Use Case | Text Property | Overlay |
|----------|---------------|---------|
| Syntax highlighting | ✅ (font-lock-face) | ❌ |
| Completion candidates | ❌ | ✅ |
| Visual selection (Evil visual mode) | ❌ | ✅ |
| Region highlighting | ❌ | ✅ |
| Read-only regions | ✅ | ✅ (both work) |
| Folded/hidden text | ✅ (invisible prop) | ✅ |
| Temporary highlights | ❌ | ✅ |
| Persistent styling | ✅ | ❌ |

**Rule of thumb:** If it should copy with text → text property. If it's temporary UI → overlay.

---

## 3. Mode Line System

### Architecture

The mode line is **not** just a string. It's a **format specification** that's dynamically evaluated to produce the displayed text.

```clojure
:mode-line-format [
  " "  ; Literal string
  :mode-line-modified  ; Variable (evaluate and insert value)
  " "
  :mode-line-buffer-identification
  " "
  :mode-line-position
  " "
  [:eval (if (> (buffer-size) 1000000) "LARGE" "")]  ; Lisp form
  " "
  :mode-line-modes
]
```

### Standard Mode Line Variables

These variables should exist and be customizable:

**Buffer state:**
- `mode-line-modified` - `"**"` if modified, `"--"` if not, `"%%"` if read-only
- `mode-line-buffer-identification` - Buffer name (often with `%b`)
- `mode-line-remote` - `"@"` if remote buffer (TRAMP)

**Position:**
- `mode-line-position` - Line/column/percentage
  - Default: `" L%l C%c %p%%"` (Line 10 C5 25%)

**Modes:**
- `mode-line-modes` - Major mode + minor mode list
  - Format: `"Clojure[line-number column-number]"`

**Process:**
- `mode-line-process` - Background process indicator
  - Example: `":run"` for compilation buffer

**Misc:**
- `mode-line-misc-info` - Additional information (time, battery, etc.)
- `mode-line-front-space` - Space for alignment
- `mode-line-end-spaces` - Padding at end

### % Constructs

Special codes in mode line format strings:

| Code | Meaning |
|------|---------|
| `%b` | Buffer name |
| `%f` | File name visited in buffer |
| `%F` | Frame name |
| `%l` | Line number |
| `%c` | Column number |
| `%p` | Percentage through buffer |
| `%P` | Percentage above top of window |
| `%I` | Buffer size |
| `%*` | `*` if modified, `%` if read-only, `-` otherwise |
| `%+` | `*` if modified, `%` if read-only, `+` if narrowed, `-` otherwise |
| `%-` | Fill with dashes to window width |

### Mode Line Faces

- `mode-line` - Active window mode line
- `mode-line-inactive` - Inactive window mode line
- `mode-line-highlight` - Mouse hover
- `mode-line-buffer-id` - Buffer identification
- `mode-line-emphasis` - Emphasized parts

### Buffer-Local Mode Line

Each buffer can have its own mode-line-format:

```clojure
;; In *Help* buffer
:mode-line-format ["Help: " :mode-line-buffer-identification]

;; In normal buffer
:mode-line-format [standard-mode-line-format]
```

### How doom-modeline Works

doom-modeline **replaces** `mode-line-format` with its own sophisticated system:
- Modular segments (each is a function)
- Conditional display based on context
- Icon support (requires icon fonts)
- Color-coded based on state (git status, LSP, etc.)
- Custom faces for each segment

**Key insight:** Mode line is just a format spec. Packages can completely replace it.

---

## 4. Theme System

### How Themes Work

A theme is a **collection of face definitions** and **variable settings**.

```elisp
(deftheme modus-operandi
  "Light theme with high contrast")

(custom-theme-set-faces 'modus-operandi
  '(default ((t (:foreground "#000000" :background "#ffffff"))))
  '(region ((t (:background "#b3d7ff" :extend t))))
  '(mode-line ((t (:foreground "#ffffff" :background "#333333"))))
  ;; ... hundreds more faces
  )

(provide-theme 'modus-operandi)
```

### Modus Themes Architecture

**Key insights from studying modus-themes:**

1. **Palette System:**
   - Named colors, not just hex values
   - Semantic names: `bg-main`, `fg-main`, `blue-warmer`, `blue-cooler`
   - Variants share structure but have different palettes

2. **Face Customization:**
   - Extensive `defcustom` variables
   - Users can customize without editing theme
   - Options for bold, italic, colors, etc.

3. **Accessibility:**
   - WCAG AAA contrast compliance
   - Color-deficiency variants (deuteranopia, tritanopia)
   - Every color pairing tested for contrast

4. **Coverage:**
   - Defines ~500 faces
   - Covers built-in Emacs faces
   - Popular third-party packages (Vertico, Magit, Org, etc.)

### Theme Activation

```clojure
;; Load theme
(load-theme 'modus-operandi t)

;; Disable theme
(disable-theme 'modus-operandi)

;; Multiple themes can be active (faces merge, later wins)
```

### What We Need for Themes

1. **Face definition system** - register faces with default values
2. **Theme definition system** - bundle of face customizations
3. **Theme loading** - apply theme face specs to actual faces
4. **Theme disabling** - revert faces to default or previous theme
5. **Palette system** - named colors that can be referenced
6. **Default theme** - basic professional light theme

---

## 5. Evil Mode Dependencies Deep Dive

### State Machine Requirements

**What Evil needs:**

1. **Buffer-local state** ❌ MISSING
   - Each buffer must have its own Evil state (normal, insert, visual, etc.)
   - `make-local-variable` and buffer-local variable system
   - Currently our state is global!

2. **Visual feedback** ❌ MISSING (needs faces)
   - Cursor shape changes (box in normal, bar in insert)
   - Cursor color changes
   - Mode line tag ("<N>", "<I>", "<V>")
   - All require face/cursor customization

3. **Overlay system for visual mode** ❌ MISSING
   - Visual mode needs to highlight selected region
   - Block mode (C-v) needs column highlighting
   - Overlays with `face` property

4. **Hooks** ❌ MISSING
   - `pre-command-hook` - runs before every command
   - `post-command-hook` - runs after every command
   - Evil uses these to manage state transitions

5. **Advice system** ❌ MISSING
   - Evil wraps many Emacs functions to make them Vim-like
   - `advice-add` infrastructure
   - Example: wrapping `yank` to work with Evil registers

### Operator-Motion Composition

How `dw` (delete word) works:

```
1. User presses 'd'
   → Evil enters operator-pending state
   → Stores 'delete' operator
   → Shows "<O>" in mode line (requires face system)

2. User presses 'w'
   → Execute 'forward-word' motion, get range (start, end, type)
   → Apply 'delete' operator to range
   → Type determines behavior:
      - 'char: delete characters
      - 'line: delete whole lines
      - 'block: delete rectangle
   → Return to normal state

3. Record for repeat
   → Store {:operator :delete :motion :forward-word :count 1}
   → Next '.' press replays this
```

**Range types:**
- `char` - Character-wise (exclusive or inclusive)
- `line` - Line-wise (whole lines)
- `block` - Rectangle/column selection

**What we need:**
- Range calculation (already have point/mark)
- Type system for ranges
- Operator registry
- Motion registry
- Repeat mechanism

### Visual Mode Implementation

**Visual mode needs:**
1. Overlays to highlight selection ❌
2. Three selection types:
   - Character-wise (v)
   - Line-wise (V)
   - Block-wise (C-v) - hardest, needs column calculation
3. Cursor at one end, mark at other
4. Operators work on visual selection

**Block mode specifically:**
- Select rectangular region
- Needs special rendering (overlay per line)
- Operators apply to each line's column range

---

## 6. Other Missing Fundamental Systems

### Buffer-Local Variables

**Critical for:**
- Evil state per buffer
- Major mode configuration
- Minor modes
- Completion state

**API needed:**
```clojure
(defn make-local-variable [symbol]
  ;; Make variable buffer-local for current buffer only
  ...)

(defn make-variable-buffer-local [symbol]
  ;; Make variable buffer-local for ALL buffers
  ...)

(defn buffer-local-value [variable buffer]
  ;; Get buffer's local value
  ...)

(defn set-buffer-local-value [variable buffer value]
  ;; Set buffer's local value
  ...)
```

**Implementation:**
```clojure
;; In buffer state
:buffers {
  1 {
    :id 1
    :name "foo.txt"
    :local-variables {
      :evil-state :normal
      :major-mode :text-mode
      :my-custom-var 42
    }
  }
}
```

### Pre/Post Command Hooks

**What they are:**
- `pre-command-hook` - runs before every command
- `post-command-hook` - runs after every command

**Used by:**
- Evil (state management, mode line updates)
- Completion UI (update candidates)
- Syntax highlighting (re-fontify changed region)
- Any package that needs to react to user input

**Why critical:**
Evil's state transitions **depend** on post-command-hook:

```elisp
(add-hook 'post-command-hook 'evil-refresh-cursor)
(add-hook 'post-command-hook 'evil-update-mode-line)
(add-hook 'post-command-hook 'evil-clear-operator-pending)
```

### Advice System

**What it is:**
Modify existing functions without redefining them.

```elisp
;; Add advice
(advice-add 'yank :around #'evil-yank-advice)

;; Remove advice
(advice-remove 'yank #'evil-yank-advice)
```

**Why Evil needs it:**
- Wrap `yank` to use Evil registers
- Wrap `kill-region` to use Vim deletion semantics
- Wrap `undo` to group undos by insert session

**Advice types:**
- `:before` - run before function
- `:after` - run after function
- `:around` - wrap function (can modify args/return value)
- `:override` - completely replace function
- `:filter-args` - transform arguments
- `:filter-return` - transform return value

---

## 7. Implementation Priority Recommendations

### Revised Roadmap

Based on this research, here's the recommended order:

### **Phase 6B: Display & Theming Foundation (4 weeks)**

This must come **before** completion framework because:
- Completion UI needs faces for annotations
- Themes make Lexicon look professional
- Visual feedback is core to Emacs experience

#### Week 1: Face System
**Priority:** CRITICAL
**Estimated:** 40 hours

1. **Face data structure** (4h)
   ```clojure
   :faces {
     :default {:foreground "#000" :background "#fff" ...}
     :region {:background "#b3d7ff" :extend true}
     ;; ...
   }
   ```

2. **Face registry** (4h)
   - `defface` equivalent - register face with default attrs
   - Face lookup by name
   - Face merging/inheritance

3. **Standard faces** (8h)
   - Define all standard faces
   - default, region, mode-line, minibuffer-prompt, etc.
   - font-lock faces
   - completion faces

4. **Face application to text** (12h)
   - Modify rendering to apply faces
   - CSS class generation from face attributes
   - Handle multiple faces on same text (priority)

5. **Face inheritance** (4h)
   - `:inherit` attribute
   - Resolve inheritance chains
   - Merge attributes from multiple parents

6. **Tests** (8h)
   - Face definition works
   - Inheritance works
   - Rendering applies correct styles

**Success criteria:**
- Can define faces
- Can apply faces to text (via text properties initially)
- Text renders with correct colors/fonts/styles

#### Week 2: Text Properties & Overlays
**Priority:** CRITICAL
**Estimated:** 40 hours

1. **Text property system** (12h)
   - Store properties per character in buffer
   - `put-text-property`, `get-text-property`, `remove-text-properties`
   - Properties: face, font-lock-face, display, invisible, read-only, help-echo

2. **Overlay system** (16h)
   - Overlay data structure
   - `make-overlay`, `delete-overlay`, `move-overlay`
   - `overlay-put`, `overlay-get`
   - `overlays-at`, `overlays-in`
   - Priority system (sort by priority, higher wins)

3. **Face priority/merging** (8h)
   - Overlay faces > text property face > font-lock-face > default
   - Within overlays, sort by priority
   - Merge face attributes correctly

4. **Rendering integration** (4h)
   - Update views to check overlays + text properties
   - Apply combined face to rendered text
   - Handle `invisible` property

**Success criteria:**
- Can create overlays
- Can attach text properties
- Rendering respects both overlays and text properties
- Priority system works correctly

#### Week 3: Mode Line System
**Priority:** HIGH
**Estimated:** 32 hours

1. **mode-line-format interpreter** (12h)
   - Parse format spec (strings, symbols, lists, :eval)
   - Evaluate symbols (look up variable value)
   - Evaluate `:eval` forms
   - Handle conditionals `(when condition value)`

2. **% constructs** (8h)
   - `%b`, `%f`, `%l`, `%c`, `%p`, `%*`, etc.
   - Substitute with actual values
   - Handle alignment (`%-`)

3. **Standard mode line variables** (8h)
   - `mode-line-modified`, `mode-line-buffer-identification`, etc.
   - Default values
   - Customizable by user

4. **Mode line faces** (4h)
   - Apply mode-line face to rendered mode line
   - mode-line-inactive for inactive windows
   - mode-line-highlight for mouse hover

**Success criteria:**
- Mode line displays buffer name, position, mode
- Can customize mode-line-format
- % constructs work
- Faces apply correctly

#### Week 4: Theme System
**Priority:** HIGH
**Estimated:** 32 hours

1. **Theme definition infrastructure** (8h)
   - `deftheme` equivalent - register theme
   - Theme data structure (name, faces, variables)
   - Theme registry

2. **Theme loading** (8h)
   - `load-theme` - apply theme's face specs
   - Merge with existing faces
   - Handle theme priority (later themes override earlier)

3. **Theme disabling** (4h)
   - `disable-theme` - revert faces
   - Track which theme set which face

4. **Palette system** (4h)
   - Named colors in theme
   - Reference palette in face definitions
   - Makes customization easier

5. **Default light theme** (8h)
   - Professional-looking default theme
   - Good contrast, readable
   - Defines all standard faces
   - Use as reference for other themes

**Success criteria:**
- Can load/disable themes
- Default theme looks good
- Face definitions use palette
- Multiple themes can coexist (later overrides earlier)

**Total Phase 6B:** 144 hours (4 weeks @ 36 hours/week)

---

### **Phase 6C: Essential Infrastructure (2 weeks)**

#### Week 5: Buffer-Local Variables & Hooks
**Priority:** CRITICAL
**Estimated:** 32 hours

1. **Buffer-local variables** (16h)
   - Store `:local-variables` map in buffer
   - `make-local-variable`, `make-variable-buffer-local`
   - Get/set buffer-local values
   - Global vs local value resolution

2. **Pre/post-command hooks** (12h)
   - `:pre-command-hook` global variable
   - `:post-command-hook` global variable
   - Run hooks before/after every command
   - Buffer-local hooks

3. **Hook utilities** (4h)
   - `add-hook`, `remove-hook` with buffer-local support
   - Run hooks with error handling

#### Week 6: Advice System (Basic)
**Priority:** MEDIUM-HIGH
**Estimated:** 24 hours

1. **Advice registry** (8h)
   - Store advice per function
   - Advice types: :before, :after, :around

2. **Advice application** (12h)
   - Wrap functions with advice
   - Call advice in correct order
   - Handle advice return values

3. **advice-add/remove API** (4h)
   - Register/unregister advice
   - Named advice (can remove by name)

**Total Phase 6C:** 56 hours (2 weeks @ 28 hours/week)

---

### **Phase 6D: Completion Framework (6 weeks)**

Now proceed with completion metadata, styles, CAPFs, etc. as previously planned.

With faces + overlays + text properties in place:
- Completion annotations can use faces (Marginalia)
- Completion candidates can be highlighted (Vertico)
- In-buffer completion can use overlays (Corfu)

---

### **Phase 6E: Evil Mode Integration (2 weeks)**

With all infrastructure in place:
- Buffer-local Evil state
- Visual feedback via faces/cursor
- Visual mode via overlays
- State transitions via hooks
- Function wrapping via advice

---

## 8. What We Got Right vs Wrong

### ✅ What We Got Right

1. **Gap buffer** - Correct Emacs-aligned choice
2. **Keymap system** - Working well
3. **Command registry** - Good foundation
4. **Kill ring** - Proper implementation
5. **Window tree** - Solid architecture
6. **Mode system** - Major/minor modes work
7. **Package system** - Infrastructure is sound

### ❌ What We Missed

1. **Face system** - Critical omission, affects everything visual
2. **Overlay system** - Needed for all UI packages
3. **Text properties** - Needed for syntax highlighting
4. **Buffer-local variables** - Needed for per-buffer state
5. **Pre/post-command hooks** - Needed for reactive packages
6. **Advice system** - Needed for extensibility
7. **Mode line format** - Currently too simplistic
8. **Theme system** - No theming support at all

### Why This Matters

**Without display infrastructure:**
- Completion annotations don't work (Marginalia needs faces)
- Syntax highlighting primitive (needs text properties + font-lock faces)
- Evil visual mode can't work (needs overlays)
- Themes impossible (no face system)
- Professional appearance impossible
- Third-party packages can't integrate

**With display infrastructure:**
- Rich completion UI possible
- Proper syntax highlighting
- Visual mode works
- Themeable like real Emacs
- Professional appearance
- Package ecosystem flourishes

---

## 9. Strategic Recommendation

### Current State

We have:
- Solid editor core (gap buffer, navigation, editing)
- Package system infrastructure (compiles, loads)
- Evil-mode package structure (not integrated)

We're missing:
- Display layer (faces, overlays, text properties)
- Visual feedback infrastructure
- Theme system
- Some fundamental infrastructure (buffer-local vars, hooks, advice)

### Recommended Path Forward

**Option A: Display First (RECOMMENDED)**
1. Phase 6B: Display & Theming (4 weeks)
2. Phase 6C: Infrastructure (2 weeks)
3. Phase 6D: Completion Framework (6 weeks)
4. Phase 6E: Evil Mode Integration (2 weeks)

**Total:** 14 weeks to full package ecosystem + Evil mode

**Why this order:**
- Display is foundational - everything depends on it
- Completion framework needs faces for annotations
- Evil mode needs overlays + faces + hooks + advice
- Professional appearance from day 1
- Each phase builds on previous

**Option B: Completion First (NOT RECOMMENDED)**
1. Implement completion framework without proper visual feedback
2. Realize we need faces for Marginalia
3. Implement faces
4. Redo completion UI to use faces properly
5. Total time wasted: ~2 weeks of rework

### Measuring Success

**After Phase 6B (Display):**
- ✅ Text can have different colors/fonts/styles
- ✅ Region highlights properly
- ✅ Mode line looks professional
- ✅ Can load themes
- ✅ Overlays work for temporary highlights

**After Phase 6C (Infrastructure):**
- ✅ Buffer-local variables work
- ✅ Pre/post-command hooks run
- ✅ Advice system functional
- ✅ Evil state can be per-buffer

**After Phase 6D (Completion):**
- ✅ Vertico works
- ✅ Orderless works
- ✅ Marginalia shows rich annotations with colors
- ✅ Corfu works (in-buffer completion with popup)
- ✅ Package ecosystem viable

**After Phase 6E (Evil):**
- ✅ Evil normal mode works
- ✅ Evil insert mode works
- ✅ Evil visual mode works (with highlighted selection)
- ✅ Operators + motions work (dw, ciw, etc.)
- ✅ Mode line shows state
- ✅ Cursor changes shape/color

---

## 10. Conclusion

**We've been building a house without walls** - solid foundation (gap buffer, keymaps, commands) but no paint, no windows, no curb appeal.

**The face/overlay/theme system isn't cosmetic - it's infrastructural.** Every sophisticated Emacs package assumes it exists. Without it:
- Completion packages can't annotate (Marginalia)
- Syntax highlighting is primitive
- Visual feedback is limited
- Evil mode can't show visual selection
- Professional appearance impossible
- Third-party integration blocked

**Recommendation:** Implement display infrastructure first (4 weeks), then completion framework (6 weeks), then integrate Evil mode (2 weeks).

**Total time to full ecosystem:** 14 weeks
**Benefit:** Professional Emacs clone with working package ecosystem, themes, and Evil mode

**The question isn't** "Should we implement display infrastructure?"
**The question is** "How did we get this far without it?"

---

*This research document should guide Phase 6 development priorities.*
