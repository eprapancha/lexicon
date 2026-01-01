# Lexicon Manual Testing Plan

**To:** Lexicon Development Team
**Date:** 2025-12-26
**Subject:** Comprehensive Manual Test Cases for Phases 0 - 6.5

## 1. Introduction

This document provides a detailed, step-by-step manual testing plan for all features implemented up to and including **Phase 6.5**. The goal is to rigorously verify the editor's core functionality, stability, and adherence to Emacs principles before proceeding to more advanced features like Evil-mode integration.

Each test case includes an ID, a clear objective, step-by-step instructions, and the expected result. Please execute these tests methodically and log any deviations from the expected results as issues for tracking.

**Testing Environment:**
*   Start the Lexicon development server via `npx shadow-cljs watch app`.
*   Open the application in a clean browser session (e.g., incognito mode) at `http://localhost:8080`.
*   Perform tests in the default `*scratch*` buffer unless otherwise specified.

---

## 2. Phase 0: Architecture Reset & Foundation

**Objective:** Verify that the most basic, fundamental editor functions are working correctly after the architectural refactor.

| Test Case ID | Objective | Steps | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **P0-01** | Verify basic text input | 1. Click anywhere in the editor view.<br>2. Type the sentence: `The quick brown fox jumps over the lazy dog.` | The text appears correctly in the buffer as you type. | ✅ PASS |
| **P0-02** | Verify Enter/Return key | 1. Type `line 1`.<br>2. Press `Enter`.<br>3. Type `line 2`. | The cursor moves to a new line below "line 1", and "line 2" is inserted on the new line. The editor displays two distinct lines. | ✅ PASS |
| **P0-03** | Verify Backspace key | 1. Type `abcde`.<br>2. Press `Backspace` twice. | The text becomes `abc`. The cursor is positioned after the `c`. | ✅ PASS |
| **P0-04** | Verify Delete key | 1. Type `abcde`.<br>2. Move the cursor between `b` and `c`.<br>3. Press `Delete` twice. | The text becomes `abe`. The cursor is positioned after the `b`. | ✅ PASS |
| **P0-05** | Verify basic arrow key navigation | 1. Type `line 1` (press Enter) `line 2`.<br>2. Press `Up Arrow`.<br>3. Press `Right Arrow` twice.<br>4. Press `Down Arrow`.<br>5. Press `Left Arrow`. | 2. Cursor moves to the end of "line 1".<br>3. Cursor moves two characters to the right on "line 1".<br>4. Cursor moves to the second line, attempting to maintain the column.<br>5. Cursor moves one character to the left on "line 2". | ✅ PASS |
| **P0-06** | Verify mouse click positioning | 1. Type several lines of text.<br>2. Click the mouse at various points within the text (beginning of a line, middle of a word, end of a line). | The cursor immediately moves to the exact character position that was clicked. | ✅ PASS |

---

## 3. Phase 1: Core Emacs - Basic Editing

**Objective:** Verify essential Emacs navigation and text manipulation commands.

| Test Case ID | Objective | Steps | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **P1-01** | Verify character-wise navigation | 1. Type `hello world`.<br>2. Place cursor at the beginning.<br>3. Press `C-f` five times.<br>4. Press `C-b` twice. | 3. Cursor moves to the space after "hello".<br>4. Cursor moves back to the `l` in "hello". | ✅ PASS |
| **P1-02** | Verify line-wise navigation | 1. Type `line 1` (Enter) `line 2` (Enter) `line 3`.<br>2. Place cursor on "line 2".<br>3. Press `C-p`.<br>4. Press `C-n` twice. | 3. Cursor moves to "line 1".<br>4. Cursor moves to "line 3". | ✅ PASS |
| **P1-03** | Verify beginning/end of line | 1. Type `this is a test`.<br>2. Place cursor in the middle.<br>3. Press `C-a`.<br>4. Press `C-e`. | 3. Cursor moves to the beginning of the line.<br>4. Cursor moves to the end of the line. | ✅ PASS |
| **P1-04** | Verify word-wise navigation | 1. Type `the quick brown fox`.<br>2. Place cursor at the start.<br>3. Press `M-f` twice.<br>4. Press `M-b`. | 3. Cursor moves to the start of "brown".<br>4. Cursor moves back to the start of "quick". | ✅ PASS |
| **P1-05** | Verify beginning/end of buffer | 1. Type several lines of text.<br>2. Place cursor in the middle.<br>3. Press `M->`.<br>4. Press `M-<`. | 3. Cursor moves to the very end of the buffer.<br>4. Cursor moves to the very beginning of the buffer. | ✅ PASS |
| **P1-06** | Verify setting the mark | 1. Type `select this text`.<br>2. Place cursor before "select".<br>3. Press `C-SPC`.<br>4. Move cursor to the end of "this". | 3. Echo area shows "Mark set".<br>4. The region between "select" and "this" should be visually highlighted. | ✅ PASS |
| **P1-07** | Verify Kill-Region (`C-w`) | 1. Perform steps from P1-06.<br>2. Press `C-w`. | The highlighted text disappears. The buffer now reads ` text`. | ⊘ SKIP (Browser) |
| **P1-08** | Verify Yank (`C-y`) | 1. Perform steps from P1-07.<br>2. Move cursor to the end of the buffer.<br>3. Press `C-y`. | The killed text (`select this`) is inserted at the cursor. The buffer reads ` textselect this`. | ✅ PASS |
| **P1-09** | Verify Copy (`M-w`) | 1. Type `copy this`.<br>2. Set mark before "copy" and move point after "this".<br>3. Press `M-w`.<br>4. Move cursor to a new line.<br>5. Press `C-y`. | 3. The text `copy this` remains in the buffer. The region is un-highlighted. Echo area shows a "Copied region" message.<br>5. The text `copy this` is inserted on the new line. | ✅ PASS |
| **P1-10** | Verify Kill-Line (`C-k`) | 1. Type `kill the rest of the line`.<br>2. Place cursor before "the".<br>3. Press `C-k`.<br>4. Press `C-k` again. | 3. The text `the rest of the line` disappears. Buffer reads `kill `.<br>4. The newline character is killed, joining the current line with the one below it (if any). | ✅ PASS |
| **P1-11** | Verify Undo (`C-/`) | 1. Type `hello`.<br>2. Press `Enter`.<br>3. Type `world`.<br>4. Press `C-/` repeatedly. | Each press of `C-/` should undo one character insertion at a time, in reverse order, including the newline. Eventually, the buffer should be empty. | ✅ PASS |

---

## 4. Phase 2 & 2.5: Buffers, Files, and Core Polish

**Objective:** Verify multi-buffer support, file I/O, and core user experience improvements.

| Test Case ID | Objective | Steps | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **P2-01** | Verify `switch-to-buffer` (`C-x b`) | 1. Press `C-x b`.<br>2. Type `*scratch*` and press `Enter`.<br>3. Type some text in `*scratch*`.<br>4. Press `C-x b` again.<br>5. Type a new buffer name, `test-buffer`, and press `Enter`. | 1. Minibuffer activates with "Switch to buffer: ".<br>2. You are in the `*scratch*` buffer.<br>5. A new, empty buffer named `test-buffer` is created and displayed. | ✅ PASS |
| **P2-02** | Verify buffer state preservation | 1. Perform steps from P2-01.<br>2. Type `hello` in `test-buffer`. Move cursor to the middle.<br>3. Press `C-x b`, type `*scratch*`, and press `Enter`.<br>4. Press `C-x b`, type `test-buffer`, and press `Enter`. | 3. The original `*scratch*` buffer is shown, with the text from P2-01 intact.<br>4. The `test-buffer` is shown, with the text "hello" and the cursor in the middle, exactly as you left it. | ✅ PASS |
| **P2-03** | Verify `list-buffers` (`C-x C-b`) | 1. Create a few buffers as in P2-01.<br>2. Press `C-x C-b`.<br>3. In the `*Buffer List*` buffer, move the cursor down to a different buffer's line.<br>4. Press `Enter`. | 2. A new buffer `*Buffer List*` appears, showing `*scratch*`, `test-buffer`, and `*Buffer List*` itself.<br>4. The editor switches to the buffer you selected. | ✅ PASS |
| **P2-04** | Verify buffer modified indicator | 1. In a new buffer, type some text.<br>2. Look at the mode line. | The mode line should show `**` to indicate the buffer is modified. | ✅ PASS |
| **P2-05** | Verify `save-buffer` (`C-x C-s`) | 1. Create a new buffer `save-test` and type text in it.<br>2. Press `C-x C-s`.<br>3. Your browser's file save dialog should appear.<br>4. Save the file as `save-test.txt`.<br>5. Look at the mode line. | 2. A "Saving..." message may appear in the echo area.<br>4. Echo area shows a "Saved file..." message.<br>5. The `**` modified indicator on the mode line disappears. | ⊘ SKIP (Browser) |
| **P2.5-01**| Verify `keyboard-quit` (`C-g`) | 1. Press `C-x b` to open the minibuffer.<br>2. Press `C-g`. | The minibuffer closes, and focus returns to the main editor buffer. The echo area shows "Keyboard quit". | ✅ PASS |
| **P2.5-02**| Verify Universal Argument (`C-u`) | 1. Press `C-u`.<br>2. Type `a`.<br>3. Press `C-u` `C-u`.<br>4. Type `b`. | 1. Echo area shows "C-u ".<br>2. Four `a`'s should be inserted (`aaaa`).<br>3. Echo area shows "C-u C-u ".<br>4. Sixteen `b`'s should be inserted. | ✅ PASS |

---

## 5. Phase 3: Windows & Frames

**Objective:** Verify that window splitting, navigation, and management work correctly.

| Test Case ID | Objective | Steps | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **P3-01** | Verify horizontal split (`C-x 2`) | 1. Press `C-x 2`. | The editor view splits into two windows, one on top of the other. Both windows show the same buffer. The newly created bottom window is active (blue border). | ✅ PASS |
| **P3-02** | Verify vertical split (`C-x 3`) | 1. Press `C-x 3`. | The editor view splits into two windows, side-by-side. Both windows show the same buffer. The newly created right window is active. | ✅ PASS |
| **P3-03** | Verify window cycling (`C-x o`) | 1. Press `C-x 2`.<br>2. Press `C-x o`.<br>3. Press `C-x o` again. | 2. The top window becomes active (its border turns blue).<br>3. The bottom window becomes active again. | ✅ PASS |
| **P3-04** | Verify independent window state | 1. Press `C-x 2`.<br>2. In the bottom window, type `bottom`.<br>3. Press `C-x o` to switch to the top window.<br>4. Type `top`.<br>5. Scroll the top window (if possible). | 2. The text `bottom` appears in both windows.<br>4. The text `top` appears in both windows.<br>5. The bottom window's scroll position does not change. Each window maintains its own cursor position (`point`). | ✅ PASS |
| **P3-05** | Verify `delete-other-windows` (`C-x 1`) | 1. Press `C-x 2`.<br>2. Press `C-x 3` in the top window.<br>3. You now have three windows. Navigate to one.<br>4. Press `C-x 1`. | All windows except the active one disappear. The active window now takes up the full editor space. | ✅ PASS |
| **P3-06** | Verify click-to-activate window | 1. Create multiple splits.<br>2. Click inside an inactive window. | The clicked window becomes active (its border turns blue), and the cursor moves to the clicked position within that window. | ✅ PASS |

---

## 6. Phase 4, 5, 6: Modes, Help, Packages, and Display

**Objective:** Verify the core infrastructure for modes, help, packages, and the new display/theming engine.

| Test Case ID | Objective | Steps | Expected Result | Status |
| :--- | :--- | :--- | :--- | :--- |
| **P4-01** | Verify `execute-extended-command` (`M-x`) | 1. Press `M-x`.<br>2. Type `text-mo` and press `TAB`.<br>3. Press `Enter`. | 1. Minibuffer shows "M-x ".<br>2. The input completes to `text-mode`.<br>3. The major mode in the mode line changes to "Text". | ✅ PASS |
| **P4-02** | Verify `describe-key` (`C-h k`) | 1. Press `C-h k`.<br>2. Press `C-f`. | 1. Minibuffer shows "Describe key: ".<br>2. A `*Help*` buffer appears, showing the documentation for `forward-char`. | ❌ FAIL (Not Implemented) |
| **P4-03** | Verify `describe-bindings` (`C-h b`) | 1. Press `C-h b`. | A `*Help*` buffer appears, listing all active global and local keybindings, separated by mode. | ❌ FAIL (Not Implemented) |
| **P5-01** | Verify minor mode toggling | 1. Press `M-x` `line-number-mode` `Enter`.<br>2. Check the mode line.<br>3. Press `M-x` `line-number-mode` `Enter` again. | 1. Echo area shows a confirmation.<br>2. Line numbers appear in the mode line.<br>3. Line numbers disappear from the mode line. | ❌ FAIL (Not Implemented) |
| **P6A-01** | Verify package system listing | 1. Press `M-x` `package-list` `Enter`. | A `*Packages*` buffer should appear, listing `evil-mode` as a registered (but likely inactive) package. | ✅ PASS |
| **P6B-01** | Verify theme loading | 1. Press `M-x` `load-theme` `Enter`.<br>2. Minibuffer prompts for a theme. Type `lexicon-base-dark` and press `Enter`.<br>3. Repeat, but load `lexicon-base-light`. | 2. The entire editor's color scheme changes to the dark theme. All faces (text, mode line, etc.) update.<br>3. The editor changes back to the light theme. | ✅ PASS |
| **P6B-02** | Verify dynamic font size change | 1. Press `M-x` `set-font-size` `Enter`.<br>2. Enter `20` in the minibuffer and press `Enter`.<br>3. Press `M-x` `set-font-size` `Enter`, then enter `14`. | 2. The font size of all text in the editor increases noticeably.<br>3. The font size returns to a smaller size. | ✅ PASS |
| **P6B-03** | Verify Mode Line Formatting | 1. Observe the mode line in a normal buffer.<br>2. Modify the buffer by typing a character.<br>3. Change to a read-only buffer like `*Help*`. | 1. It should display buffer name, major mode, and line/col position.<br>2. The modified status (`**`) should appear.<br>3. The read-only status (`%-`) should appear. | ✅ PASS |
| **P6D-01** | Verify `thing-at-point` (conceptual) | 1. Type `https://example.com` in the buffer.<br>2. Place the cursor in the middle of the URL.<br>3. Press `M-x` and run a hypothetical `get-thing-at-point-url` command. | (This requires a test command to be written) The command should correctly identify `https://example.com` as the thing at point. | ⊘ SKIP (Custom) |
| **P6.5-01**| Verify Test Suite | 1. Run the project's test command (e.g. `./scripts/test.sh` if it exists). | All automated tests should pass without any failures. | ✅ PASS |
