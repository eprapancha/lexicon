---
name: chrome-test
description: Generate a test prompt for the Claude Chrome Extension to manually test the running Lexicon app. Use after making changes that need browser verification.
argument-hint: [focus-area e.g. "minibuffer", "eval", "keybindings", "rendering", "packages"]
allowed-tools: Read, Glob, Grep
---

# Chrome Extension Test Prompt Generator

Generate a structured test prompt that the user will paste into the Claude Chrome Extension to manually test the running Lexicon application.

---

## Process

1. **Determine focus area** from the argument (or ask)
2. **Check recent changes** to understand what needs testing
3. **Generate a prompt** following the template below
4. **Output the prompt** in a code block the user can copy-paste

---

## Prompt Template

Every generated prompt MUST follow this structure:

```
You are testing a web-based Emacs clone called Lexicon at localhost:8080.
[Setup instructions: refresh, wait, clear console, etc.]

## FIRST: Compiler check (GATE)

After refreshing, look for a shadow-cljs compiler warning/error overlay.
This appears as a dark overlay covering the page with orange/red text.
If present: STOP all testing, copy the full overlay text, and report using
the ABORTED format below. Do NOT proceed with other tests.

## Tests to perform:

1. **Console check**: [what to look for]
2-6. **[Test name]**:
   - [Specific action steps with browser key names]
   - [Expected outcome]
   - Report: [What to note]

## Report format:

If compiler overlay is present, respond with:

\```
LEXICON TEST REPORT (ABORTED - COMPILER ERROR)
===============================================
Compiler overlay detected. Full text:
<the overlay text>
\```

Otherwise, respond with:

\```
LEXICON TEST REPORT ([focus area])
====================================
[Test name]: [PASS/FAIL - description]
Console errors: [NONE / list]
\```
```

---

## Key Mappings (ALWAYS use browser names in prompts)

| Emacs | In prompt write |
|-------|-----------------|
| C-x | Ctrl+X |
| C-c | Ctrl+C |
| C-g | Ctrl+G |
| M-x | Alt+X |
| M-: | Alt+Shift+; |
| C-x C-f | Ctrl+X, then Ctrl+F |
| C-x b | Ctrl+X, then b |
| C-x C-s | Ctrl+X, then Ctrl+S |
| RET | Enter |
| TAB | Tab |
| DEL | Backspace |
| SPC | Space |
| C-SPC | Ctrl+Space |

---

## Focus Area Test Banks

### minibuffer
- C-x b: prompt appears, all letters type correctly, Enter confirms
- M-: eval: prompt appears, can type parens, Enter evaluates
- M-x: prompt appears, command completes, Enter executes
- TAB: completion works without errors
- C-g: cancels minibuffer

### keybindings
- C-f/C-b: cursor moves forward/backward
- C-a/C-e: beginning/end of line
- C-n/C-p: next/previous line
- C-k: kill line
- C-y: yank
- C-x C-f: opens find-file prompt
- Multi-key sequences work

### eval
- M-: (message "test") → echo area shows "test"
- M-: (+ 1 2) → echo area shows "3"
- M-: (buffer-name) → shows current buffer name
- M-: (point) → shows cursor position number
- Error handling: M-: (undefined-fn) → shows error

### rendering
- Mode line shows buffer name, line, column
- Echo area shows/clears messages
- Cursor visible and positioned correctly
- Buffer content scrolls
- Multiple windows display correctly

### packages
- (install-package "http://localhost:3100/packages/vertico") works
- Echo area shows success/error message
- No console errors during package load

---

## Rules

- Always include a console error check at start AND end
- Always ask for hard refresh (Ctrl+Shift+R) first
- Always include a "wait 2-3 seconds for app to load" instruction
- Keep to 4-6 tests per prompt (not overwhelming)
- Always specify the exact report format
- Use browser key names, never Emacs notation in the action steps
- Include both positive tests (thing works) and negative checks (no errors)
- Always include a compiler warning/error gate (see below)
- Type slowly with 100-200ms pauses between keystrokes for reliability

---

## Compiler Warning/Error Gate (MANDATORY)

Every generated prompt MUST include this as the FIRST check after refresh:

```
IMPORTANT: After refreshing, check the page for a shadow-cljs compiler
warning/error overlay. This appears as a dark overlay with orange/red
text covering the page. If you see this:
- STOP all testing immediately
- Copy the FULL text of the warning/error message
- Report it using this format:

LEXICON TEST REPORT (ABORTED - COMPILER ERROR)
===============================================
Compiler overlay detected. Full text:
<paste the compiler warning/error text here>

Do NOT proceed with any other tests if the compiler overlay is present.
```

This gate ensures we catch compilation issues before wasting time on tests
that will inevitably fail due to broken code.
