# Lexicon Editor - Comprehensive Manual Test Plan

**Purpose**: This document provides a comprehensive list of manual tests to validate the Lexicon editor's functionality. Tests should be run manually in the browser to identify bugs before implementing automated tests.

**Workflow**:
1. Run manual test and observe failure
2. Create failing automated test for the scenario
3. Implement fix
4. Verify both manual and automated tests pass
5. Check for regressions in existing tests

---

## 1. Basic Text Input & Editing

### 1.1 Character Input
- [ ] Type ASCII characters (a-z, A-Z, 0-9)
- [ ] Type special characters (!@#$%^&*()_+-=[]{}|;:'",.<>?/)
- [ ] Type space characters
- [ ] Type tab characters
- [ ] Rapid typing (test for input lag)
- [ ] Very slow typing (one char per second)

### 1.2 Newlines
- [ ] Press Enter to create newline at end of line
- [ ] Press Enter in middle of line (should split line)
- [ ] Press Enter at beginning of line
- [ ] Press Enter multiple times to create blank lines
- [ ] Press Enter in empty buffer

### 1.3 Backspace/Delete
- [ ] Backspace at end of line
- [ ] Backspace in middle of line
- [ ] Backspace at beginning of non-empty line (should merge with previous)
- [ ] Backspace in empty buffer (should do nothing)
- [ ] Backspace to delete emoji
- [ ] Delete key (forward delete) - all above scenarios

### 1.4 Multi-byte Characters (Unicode)
- [ ] Type emoji: 👍 🎉 😀 ❤️
- [ ] Type accented characters: é è ê ë à ù ô
- [ ] Type CJK characters: 你好 世界 こんにちは 안녕하세요
- [ ] Type right-to-left text: مرحبا العالم
- [ ] Mix ASCII and Unicode on same line
- [ ] Cursor position after inserting emoji
- [ ] Backspace over emoji (should delete entire emoji)
- [ ] Select and copy emoji

---

## 2. Cursor Movement

### 2.1 Arrow Keys
- [ ] Left arrow at beginning of line (should move to previous line end)
- [ ] Left arrow at beginning of buffer (should do nothing)
- [ ] Right arrow at end of line (should move to next line start)
- [ ] Right arrow at end of buffer (should do nothing)
- [ ] Up arrow on first line (should do nothing)
- [ ] Down arrow on last line (should do nothing)
- [ ] Up/Down arrow maintains column position when possible
- [ ] Arrow keys with emoji (correct character boundary navigation)

### 2.2 Word Movement (C-left, C-right / M-b, M-f)
- [ ] Forward-word at end of word
- [ ] Forward-word in middle of word
- [ ] Forward-word over punctuation
- [ ] Backward-word at beginning of word
- [ ] Backward-word over multiple spaces
- [ ] Word boundaries with CJK characters
- [ ] Word boundaries with underscores (programming context)

### 2.3 Line Movement (C-a, C-e / Home, End)
- [ ] Beginning-of-line from middle of line
- [ ] Beginning-of-line when already at beginning
- [ ] End-of-line from middle of line
- [ ] End-of-line when already at end
- [ ] Beginning/end of line with very long lines (>1000 chars)
- [ ] Beginning/end of line with only whitespace

### 2.4 Page Movement (Page Up, Page Down)
- [ ] Page down on short document (< 1 page)
- [ ] Page down on long document
- [ ] Page up from middle of document
- [ ] Page up from top of document
- [ ] Continuous page down to end of buffer
- [ ] Page navigation maintains horizontal position

### 2.5 Buffer Movement (C-home, C-end / M-<, M->)
- [ ] Beginning-of-buffer from middle
- [ ] Beginning-of-buffer when already at beginning
- [ ] End-of-buffer from middle
- [ ] End-of-buffer when already at end
- [ ] Goto-line with valid line number
- [ ] Goto-line with out-of-range line number
- [ ] Goto-line with negative number
- [ ] Goto-line with non-numeric input

---

## 3. Selection & Regions

### 3.1 Mouse Selection
- [ ] Click to position cursor
- [ ] Click-drag to select text
- [ ] Double-click to select word
- [ ] Triple-click to select line
- [ ] Click-drag beyond viewport (should scroll)
- [ ] Click-drag from end to beginning (reverse selection)
- [ ] Click in empty buffer
- [ ] Click outside text area (should position at nearest valid point)

### 3.2 Keyboard Selection (Shift+arrows)
- [ ] Shift-right to select forward
- [ ] Shift-left to select backward
- [ ] Shift-down to select multiple lines
- [ ] Shift-up to select multiple lines backward
- [ ] Shift-End to select to end of line
- [ ] Shift-Home to select to beginning of line
- [ ] Shift-PageDown to select page
- [ ] Shift-C-End to select to end of buffer

### 3.3 Mark & Region (C-Space, C-x C-x)
- [ ] Set mark (C-Space) and navigate to create region
- [ ] Exchange mark and point (C-x C-x)
- [ ] Set mark, deactivate, then reactivate
- [ ] Region highlighting visibility
- [ ] Mark with emoji text
- [ ] Multiple mark operations in sequence

### 3.4 Select All (C-a)
- [ ] Select all in non-empty buffer
- [ ] Select all in empty buffer
- [ ] Select all, then type (should replace all)
- [ ] Select all in very large buffer (10k+ lines)

---

## 4. Kill, Yank, & Clipboard (Cut/Copy/Paste)

### 4.1 Kill Region (C-w, Cut)
- [ ] Kill selected region
- [ ] Kill empty region (should do nothing)
- [ ] Kill region with emoji
- [ ] Kill region spanning multiple lines
- [ ] Kill region at beginning of buffer
- [ ] Kill region at end of buffer
- [ ] Multiple kills in sequence (kill ring accumulation)

### 4.2 Copy Region (M-w, Copy)
- [ ] Copy selected region
- [ ] Copy empty region
- [ ] Copy and verify original text unchanged
- [ ] Copy region with newlines
- [ ] Copy very large region (>100KB)

### 4.3 Yank (C-y, Paste)
- [ ] Yank after kill
- [ ] Yank after copy
- [ ] Yank at beginning of buffer
- [ ] Yank at end of buffer
- [ ] Yank in middle of line
- [ ] Multiple yanks in sequence
- [ ] Yank with empty kill ring (should do nothing)

### 4.4 System Clipboard Integration
- [ ] Copy in Lexicon, paste in external app
- [ ] Copy in external app, paste in Lexicon
- [ ] Cut in Lexicon, paste in external app
- [ ] Preserve formatting (if applicable)
- [ ] Clipboard with emoji
- [ ] Very large clipboard content (>1MB)

### 4.5 Kill Ring Navigation
- [ ] Yank-pop (M-y) to cycle through kill ring
- [ ] Kill multiple items, yank, then yank-pop
- [ ] Kill ring limit (if exists)
- [ ] Kill ring persistence across page refresh

---

## 5. Undo & Redo

### 5.1 Basic Undo (C-/, C-_)
- [ ] Undo single character insert
- [ ] Undo character delete
- [ ] Undo multiple operations in sequence
- [ ] Undo to beginning of buffer history
- [ ] Undo with empty undo stack (should do nothing)
- [ ] Undo after save (undo stack preserved?)

### 5.2 Basic Redo (C-?, M-_)
- [ ] Redo after undo
- [ ] Redo multiple operations
- [ ] Redo to end of redo stack
- [ ] Redo with empty redo stack

### 5.3 Undo Boundaries
- [ ] Undo after typing multiple characters (single undo boundary?)
- [ ] Undo after paste operation
- [ ] Undo after kill operation
- [ ] Undo coalescing (multiple similar edits = one undo)
- [ ] Undo non-coalescing (different edit types = separate undos)

### 5.4 Complex Undo Scenarios
- [ ] Type, undo, type again (redo stack cleared?)
- [ ] Undo across save boundaries
- [ ] Undo very old operations (memory limits?)
- [ ] Undo with multiple buffers open

---

## 6. Search & Replace

### 6.1 Incremental Search (C-s)
- [ ] Search for simple text
- [ ] Search wraps at end of buffer
- [ ] Search with multiple matches (C-s to next match)
- [ ] Search with no matches
- [ ] Search backward (C-r)
- [ ] Search case-sensitive vs case-insensitive
- [ ] Search for emoji
- [ ] Search for regex patterns (if supported)
- [ ] Cancel search (ESC or C-g)

### 6.2 Search & Replace (M-%)
- [ ] Replace single instance
- [ ] Replace all instances
- [ ] Replace with prompt (y/n for each)
- [ ] Replace wraps at end of buffer
- [ ] Replace with empty string (delete)
- [ ] Replace empty string with text (insert everywhere)
- [ ] Undo replace operation

---

## 7. Buffer Management

### 7.1 Buffer Creation
- [ ] Create new empty buffer (C-x b new-name)
- [ ] Switch to existing buffer
- [ ] Switch to *scratch* buffer
- [ ] Create multiple buffers (test limit if any)
- [ ] Buffer names with special characters
- [ ] Buffer names with unicode

### 7.2 Buffer Switching
- [ ] Switch between 2 buffers (C-x b)
- [ ] Switch among many buffers (>10)
- [ ] Switch to non-existent buffer (creates new?)
- [ ] Buffer list/menu (if available)
- [ ] Most-recent buffer switching (C-x C-b?)

### 7.3 Buffer Killing
- [ ] Kill current buffer (C-x k)
- [ ] Kill modified buffer (should prompt?)
- [ ] Kill last remaining buffer
- [ ] Kill *scratch* buffer
- [ ] Kill and verify content not leaked to other buffers

---

## 8. Window Management (Splits)

### 8.1 Window Splitting
- [ ] Split horizontally (C-x 2)
- [ ] Split vertically (C-x 3)
- [ ] Split multiple times (3+ windows)
- [ ] Split with very small window sizes
- [ ] Split when window too small (should fail gracefully?)

### 8.2 Window Navigation
- [ ] Other-window (C-x o) to cycle through windows
- [ ] Navigate with many windows open
- [ ] Navigate wraps to first window
- [ ] Click to focus window

### 8.3 Window Deletion
- [ ] Delete current window (C-x 0)
- [ ] Delete other windows (C-x 1)
- [ ] Delete when only one window (should do nothing)
- [ ] Delete and verify buffer not killed

### 8.4 Window Buffers
- [ ] Display same buffer in multiple windows
- [ ] Edit in one window, verify other window updates
- [ ] Cursor position independent per window
- [ ] Scroll position independent per window
- [ ] Switch buffer in one window, other window unchanged

### 8.5 Window Resizing
- [ ] Resize window with mouse
- [ ] Resize window with keyboard (if supported)
- [ ] Resize to minimum size
- [ ] Resize and verify content reflows correctly

---

## 9. File Operations

### 9.1 File Opening
- [ ] Open existing text file
- [ ] Open non-existent file (creates new?)
- [ ] Open very large file (>10MB)
- [ ] Open file with long lines (>10k chars per line)
- [ ] Open binary file (should handle gracefully?)
- [ ] Open file with different encodings (UTF-8, UTF-16, etc.)
- [ ] Open file with mixed line endings (LF, CRLF, CR)
- [ ] Open file with BOM (byte order mark)

### 9.2 File Saving
- [ ] Save new file (C-x C-s)
- [ ] Save modifications to existing file
- [ ] Save as (C-x C-w)
- [ ] Save without modifications (should be no-op?)
- [ ] Save very large file
- [ ] Save and verify file permissions preserved
- [ ] Save with disk full (error handling?)
- [ ] Save to read-only location (error handling?)

### 9.3 File Reloading
- [ ] Revert buffer from disk (M-x revert-buffer?)
- [ ] File modified externally, detect and prompt?
- [ ] Reload after external deletion
- [ ] Reload after external modification with unsaved changes

### 9.4 File Handle (File System Access API)
- [ ] Save using file handle
- [ ] Open using file handle
- [ ] File handle persistence across page refresh
- [ ] File handle with multiple tabs open

---

## 10. Syntax Highlighting & Language Modes

### 10.1 Language Detection
- [ ] Auto-detect language from file extension (.js, .py, .clj, etc.)
- [ ] Auto-detect from shebang (#! /usr/bin/env python)
- [ ] Manual language selection
- [ ] Switch language mode on existing buffer

### 10.2 Syntax Highlighting
- [ ] Highlighting applies correctly for JavaScript
- [ ] Highlighting applies correctly for Python
- [ ] Highlighting applies correctly for ClojureScript
- [ ] Highlighting updates as you type
- [ ] Highlighting with very large files (performance?)
- [ ] Highlighting with deeply nested structures

### 10.3 Indentation
- [ ] Auto-indent on new line
- [ ] Tab key behavior (insert tab vs spaces)
- [ ] Indent region (C-M-\)
- [ ] Language-specific indentation rules
- [ ] Indentation with mixed tabs and spaces

---

## 11. Minibuffer & Command Input

### 11.1 Minibuffer Activation
- [ ] M-x to activate command input
- [ ] Type command name and execute
- [ ] Cancel minibuffer (C-g)
- [ ] Minibuffer history (up/down arrows)

### 11.2 Completion
- [ ] Tab completion for commands
- [ ] Tab completion for file names
- [ ] Tab completion for buffer names
- [ ] Completion with multiple matches (show list)
- [ ] Completion with no matches
- [ ] Partial completion (progressive narrowing)

### 11.3 Command Arguments
- [ ] Commands with numeric prefix (C-u)
- [ ] Commands with string arguments
- [ ] Commands with multiple arguments
- [ ] Invalid arguments (error handling)

---

## 12. Key Bindings & Keymaps

### 12.1 Emacs Keybindings
- [ ] All basic Emacs keys work (C-a, C-e, C-n, C-p, etc.)
- [ ] Meta keys work (M-b, M-f, M-<, M->, etc.)
- [ ] Control-X prefix keys (C-x C-s, C-x C-f, etc.)
- [ ] Escape-based meta (ESC-b instead of M-b)
- [ ] Key sequence timeout handling

### 12.2 Custom Keybindings
- [ ] Define custom keybinding
- [ ] Override default keybinding
- [ ] Keybinding conflicts (precedence rules)
- [ ] Remove/unbind key
- [ ] Keybindings persisted across sessions

### 12.3 Evil Mode (Vim Emulation)
- [ ] Normal mode navigation (hjkl)
- [ ] Insert mode (i, a, o, O)
- [ ] Visual mode (v, V, C-v)
- [ ] Delete operators (d, dd, dw)
- [ ] Yank operators (y, yy, yw)
- [ ] Paste (p, P)
- [ ] Undo/redo (u, C-r)
- [ ] Search (/, ?, n, N)
- [ ] Mode indicator visible and correct
- [ ] ESC returns to normal mode

---

## 13. Performance & Stress Tests

### 13.1 Large Files
- [ ] Open 1MB file
- [ ] Open 10MB file
- [ ] Open 100MB file (should it be supported?)
- [ ] Scroll through very large file
- [ ] Search in very large file
- [ ] Edit near end of very large file

### 13.2 Long Lines
- [ ] Line with 1k characters
- [ ] Line with 10k characters
- [ ] Line with 100k characters
- [ ] Cursor movement on very long line
- [ ] Edit in middle of very long line
- [ ] Horizontal scrolling on long line

### 13.3 Many Lines
- [ ] File with 10k lines
- [ ] File with 100k lines
- [ ] File with 1M lines (if feasible)
- [ ] Jump to end of very long file
- [ ] Line number display performance

### 13.4 Rapid Operations
- [ ] Hold down key to repeat (e.g., hold 'a' for 1000 chars)
- [ ] Rapid undo/redo cycling
- [ ] Rapid buffer switching (switch 100 times quickly)
- [ ] Rapid window cycling
- [ ] Copy/paste large content repeatedly

### 13.5 Memory & Resource Usage
- [ ] Memory usage with single small buffer
- [ ] Memory usage with 10 large buffers
- [ ] Memory leaks (observe over extended use)
- [ ] CPU usage during idle
- [ ] CPU usage during syntax highlighting
- [ ] Battery impact on laptop

---

## 14. Edge Cases & Error Handling

### 14.1 Empty States
- [ ] Empty buffer operations (save, search, select all)
- [ ] Empty selection operations
- [ ] Empty clipboard operations
- [ ] Empty search query

### 14.2 Boundary Conditions
- [ ] Cursor at position 0
- [ ] Cursor at end of buffer
- [ ] Selection from 0 to end
- [ ] Undo stack empty
- [ ] Redo stack empty
- [ ] Kill ring empty

### 14.3 Invalid Operations
- [ ] Undo when nothing to undo
- [ ] Redo when nothing to redo
- [ ] Paste when clipboard empty
- [ ] Go to line 0
- [ ] Go to negative line
- [ ] Go to line beyond end

### 14.4 Browser-Specific
- [ ] Browser back/forward buttons (should not navigate away)
- [ ] Browser refresh (F5) - state preserved?
- [ ] Browser close tab - prompt if unsaved?
- [ ] Multiple tabs with same editor
- [ ] Browser zoom in/out
- [ ] Browser dev tools open/close
- [ ] Resize browser window

### 14.5 Network & Offline
- [ ] Work offline (no network)
- [ ] Network disconnect during operation
- [ ] ServiceWorker caching (if applicable)

---

## 15. Accessibility

### 15.1 Keyboard-Only Navigation
- [ ] Tab key behavior (focus management)
- [ ] Shift-Tab to navigate backward
- [ ] All features accessible without mouse
- [ ] Focus indicators visible
- [ ] Skip to content links

### 15.2 Screen Reader Support
- [ ] ARIA labels present
- [ ] Cursor position announced
- [ ] Edit operations announced
- [ ] Modal dialogs accessible
- [ ] Error messages announced

### 15.3 Visual Accessibility
- [ ] High contrast mode
- [ ] Font size scaling (browser zoom)
- [ ] Colorblind-friendly highlighting
- [ ] Cursor visibility with different themes

---

## 16. Integration & Cross-Feature Tests

### 16.1 Multi-Step Workflows
- [ ] Open file, edit, save, close
- [ ] Open file, search, replace, save
- [ ] Create buffer, split window, edit both, save both
- [ ] Copy from one buffer, paste to another
- [ ] Kill in one window, yank in another window of same buffer

### 16.2 State Persistence
- [ ] Open files list persisted across refresh
- [ ] Cursor positions persisted
- [ ] Undo history persisted
- [ ] Window layout persisted
- [ ] Keybindings persisted

### 16.3 Configuration
- [ ] Change theme
- [ ] Change font family
- [ ] Change font size
- [ ] Enable/disable line numbers
- [ ] Enable/disable syntax highlighting
- [ ] Configuration saved and reloaded

---

## 17. WASM-Specific Tests

### 17.1 WASM Initialization
- [ ] Editor loads when WASM loads successfully
- [ ] Editor shows error when WASM fails to load
- [ ] WASM load timeout handling
- [ ] Multiple WASM instances for multiple buffers

### 17.2 WASM Memory
- [ ] WASM memory grows for large buffers
- [ ] WASM memory released when buffer killed
- [ ] WASM memory limit handling

### 17.3 WASM Error Handling
- [ ] Rust panic handling (should not crash editor)
- [ ] Invalid UTF-8 handling
- [ ] Out-of-bounds access prevention

---

## 18. Tree-sitter Parser Tests

### 18.1 Parser Initialization
- [ ] Parser loads for supported languages
- [ ] Parser fallback for unsupported languages
- [ ] Parser web worker communication

### 18.2 Parse Results
- [ ] AST generated correctly for valid code
- [ ] Error recovery for invalid code
- [ ] Incremental parsing on edit
- [ ] Parse performance on large files

---

## 19. LSP Integration Tests (if implemented)

### 19.1 Connection
- [ ] Connect to language server
- [ ] Disconnect handling
- [ ] Reconnect after disconnect
- [ ] Multiple language servers

### 19.2 Features
- [ ] Autocomplete
- [ ] Go to definition
- [ ] Find references
- [ ] Hover documentation
- [ ] Diagnostics (errors/warnings)
- [ ] Code actions (quick fixes)

---

## 20. Regression Tests (From Known Bugs)

### 20.1 Cursor Position Bugs
- [ ] Cursor position after emoji insert
- [ ] Cursor position after paste at end
- [ ] Cursor jumps after undo
- [ ] Cursor lost after window switch

### 20.2 Text Corruption Bugs
- [ ] Invalid UTF-8 from delete operations
- [ ] Text duplication on paste
- [ ] Text disappears after certain operations
- [ ] Newline handling on different platforms

### 20.3 UI Rendering Bugs
- [ ] Blank lines not rendering
- [ ] Cursor not visible
- [ ] Selection highlighting incorrect
- [ ] Scrollbar incorrect size
- [ ] Window splits misaligned

---

## Test Execution Log Template

```markdown
## Test Session: [Date/Time]
**Tester**: [Name]
**Browser**: [Chrome/Firefox/Safari] [Version]
**OS**: [Windows/Mac/Linux]

### Test: [Test Name from above]
- **Status**: ✅ Pass / ❌ Fail / ⚠️ Partial
- **Steps**: [What you did]
- **Expected**: [What should happen]
- **Actual**: [What actually happened]
- **Screenshot**: [If applicable]
- **Notes**: [Any additional observations]

### Bugs Found:
1. [Bug description]
   - Severity: Critical / High / Medium / Low
   - Reproducible: Always / Sometimes / Once
   - Steps to reproduce: [List steps]
```

---

## Priority Levels

**P0 - Critical (Fix Immediately)**
- Data loss bugs
- Editor crashes
- Complete feature failure
- Security vulnerabilities

**P1 - High (Fix Soon)**
- Major features broken
- Severe usability issues
- Performance problems affecting core usage

**P2 - Medium (Fix When Possible)**
- Minor features broken
- Edge cases
- Polish issues

**P3 - Low (Nice to Have)**
- Cosmetic issues
- Rare edge cases
- Minor enhancements

---

## Notes

- Run tests in all supported browsers (Chrome, Firefox, Safari, Edge)
- Test on different OS platforms (Windows, Mac, Linux)
- Test on mobile browsers (if supported)
- Document all failures with screenshots and steps to reproduce
- When a bug is found, create automated test BEFORE fixing
- Re-run related automated tests after each fix to catch regressions
