# Phase 7.8 Manual Test Plan

Test all 16 commands side-by-side with Emacs to ensure **zero deviation**.

## Setup

1. Open Emacs in one window
2. Open Lexicon (http://localhost:8080) in another window
3. For each test, perform the EXACT same steps in both editors
4. Document any deviations

---

## Batch 1: Quick Wins (6 commands)

### Test 1.1: scroll-up-command (C-v)

**Emacs steps:**
1. Open scratch buffer
2. Type enough text to fill multiple screens (50+ lines)
3. Go to beginning (M-<)
4. Press C-v
5. Note cursor position after scroll

**Lexicon steps:**
1. Same as Emacs

**Expected:** Cursor advances ~20 lines, viewport scrolls forward one screen

**Deviations:** _______________

---

### Test 1.2: scroll-down-command (M-v)

**Emacs steps:**
1. Continue from Test 1.1
2. Press M-v
3. Note cursor position after scroll

**Lexicon steps:**
1. Same as Emacs

**Expected:** Cursor moves back ~20 lines, viewport scrolls backward one screen

**Deviations:** _______________

---

### Test 1.3: kill-word (M-d)

**Emacs steps:**
1. New buffer
2. Type: "The quick brown fox jumps"
3. Go to beginning (C-a)
4. Press M-d
5. Press C-y to see what was killed

**Lexicon steps:**
1. Same as Emacs

**Expected:** "The" is deleted, kill ring contains "The", yank restores "The"

**Deviations:** _______________

---

### Test 1.4: backward-kill-word (M-DEL)

**Emacs steps:**
1. Type: "The quick brown fox"
2. Go to end (C-e)
3. Press M-DEL
4. Press C-y to see what was killed

**Lexicon steps:**
1. Same as Emacs

**Expected:** "fox" is deleted backward, kill ring contains "fox", yank restores "fox"

**Deviations:** _______________

---

### Test 1.5: exchange-point-and-mark (C-x C-x)

**Emacs steps:**
1. Type: "Hello world"
2. Go to beginning (C-a)
3. Set mark (C-SPC)
4. Move forward 5 chars (C-f C-f C-f C-f C-f)
5. Press C-x C-x
6. Note cursor position

**Lexicon steps:**
1. Same as Emacs

**Expected:** Cursor jumps back to mark (beginning), region stays highlighted, mark is now at position 5

**Deviations:** _______________

---

### Test 1.6: describe-variable (C-h v)

**Emacs steps:**
1. Press C-h v
2. Type a variable name (e.g., "fill-column")
3. Press RET

**Lexicon steps:**
1. Press C-h v
2. Type a variable name
3. Press RET

**Expected:** Help buffer shows variable value and documentation

**Deviations:** _______________

---

## Batch 2: File/Buffer Commands (4 commands)

### Test 2.1: insert-file (C-x i)

**Emacs steps:**
1. Create a test file `/tmp/test-insert.txt` with content "INSERTED TEXT"
2. Open new buffer
3. Type: "Before"
4. Press C-x i
5. Enter: `/tmp/test-insert.txt`
6. Verify content

**Lexicon steps:**
1. Same as Emacs

**Expected:** File contents inserted at cursor, buffer shows "BeforeINSERTED TEXT"

**Deviations:** _______________

---

### Test 2.2: save-some-buffers (C-x s)

**Emacs steps:**
1. Open buffer1, type "content1", don't save
2. Open buffer2, type "content2", don't save
3. Press C-x s
4. Respond to prompts

**Lexicon steps:**
1. Same as Emacs

**Expected:** Prompts to save each modified buffer

**Deviations:** _______________

---

### Test 2.3: find-alternate-file (C-x C-v)

**Emacs steps:**
1. Open a file (C-x C-f /tmp/file1.txt)
2. Press C-x C-v
3. Enter different file: /tmp/file2.txt

**Lexicon steps:**
1. Same as Emacs

**Expected:** Current buffer is killed, new file opens in same window

**Deviations:** _______________

---

### Test 2.4: revert-buffer (M-x revert-buffer)

**Emacs steps:**
1. Open a file
2. Make edits
3. Save the file externally (simulate external change)
4. Run M-x revert-buffer
5. Confirm

**Lexicon steps:**
1. Same as Emacs

**Expected:** Buffer contents reload from disk, discarding unsaved changes (with confirmation)

**Deviations:** _______________

---

## Batch 3 & 4: Replace Commands (4 commands)

### Test 3.1: replace-string (M-x replace-string)

**Emacs steps:**
1. Type: "foo bar foo baz foo"
2. Go to beginning (M-<)
3. M-x replace-string RET
4. Search: "foo" RET
5. Replace: "FOO" RET

**Lexicon steps:**
1. Same as Emacs

**Expected:** All "foo" from cursor forward become "FOO": "FOO bar FOO baz FOO"

**Deviations:** _______________

---

### Test 3.2: replace-regexp (M-x replace-regexp)

**Emacs steps:**
1. Type: "test123 foo456 bar789"
2. Go to beginning (M-<)
3. M-x replace-regexp RET
4. Regexp: "[0-9]+" RET
5. Replace: "NUM" RET

**Lexicon steps:**
1. Same as Emacs

**Expected:** All numbers replaced: "testNUM fooNUM barNUM"

**Deviations:** _______________

---

### Test 3.3: query-replace (M-%)

**Emacs steps:**
1. Type: "one two one two one"
2. Go to beginning (M-<)
3. Press M-%
4. Search: "one" RET
5. Replace: "ONE" RET
6. Press: y, n, y (replace 1st, skip 2nd, replace 3rd)

**Lexicon steps:**
1. Same as Emacs

**Expected:** Result: "ONE two one two ONE", message shows "Replaced 2 occurrences"

**Deviations:** _______________

---

### Test 3.4: query-replace with ! (replace all)

**Emacs steps:**
1. Type: "test test test test"
2. Go to beginning (M-<)
3. Press M-%
4. Search: "test" RET
5. Replace: "TEST" RET
6. Press: n, ! (skip first, replace all remaining)

**Lexicon steps:**
1. Same as Emacs

**Expected:** Result: "test TEST TEST TEST"

**Deviations:** _______________

---

## Batch 5: Isearch (2 commands)

### Test 5.1: isearch-forward (C-s) - Basic

**Emacs steps:**
1. Type: "apple banana apple cherry apple"
2. Go to beginning (M-<)
3. Press C-s
4. Type: "apple"
5. Press RET to exit

**Lexicon steps:**
1. Same as Emacs

**Expected:** Cursor at end of first "apple", match highlighted, echo shows "I-search: apple"

**Deviations:** _______________

---

### Test 5.2: isearch-forward - Repeat search (C-s C-s)

**Emacs steps:**
1. From Test 5.1, press C-s again (repeat)
2. Press C-s again
3. Press C-s again (should wrap)

**Lexicon steps:**
1. Same as Emacs

**Expected:** Cursor jumps to each successive "apple", wraps around at end

**Deviations:** _______________

---

### Test 5.3: isearch-backward (C-r)

**Emacs steps:**
1. Type: "alpha beta alpha gamma alpha"
2. Go to end (M->)
3. Press C-r
4. Type: "alpha"
5. Press C-r to find previous
6. Press RET to exit

**Lexicon steps:**
1. Same as Emacs

**Expected:** Search backward, finds "alpha" instances in reverse order

**Deviations:** _______________

---

### Test 5.4: isearch - Case folding

**Emacs steps:**
1. Type: "Test test TEST"
2. Go to beginning (M-<)
3. Press C-s
4. Type: "test" (lowercase)

**Lexicon steps:**
1. Same as Emacs

**Expected:** Case-insensitive search finds all three (Test, test, TEST)

---

### Test 5.5: isearch - Case sensitive

**Emacs steps:**
1. Type: "Test test TEST"
2. Go to beginning (M-<)
3. Press C-s
4. Type: "Test" (with capital T)

**Lexicon steps:**
1. Same as Emacs

**Expected:** Case-sensitive search finds only "Test"

**Deviations:** _______________

---

### Test 5.6: isearch - DEL (backspace)

**Emacs steps:**
1. Type: "foobar foobaz"
2. Go to beginning (M-<)
3. Press C-s
4. Type: "foobaz"
5. Press DEL DEL DEL (delete "baz")
6. Should now search for "foo"

**Lexicon steps:**
1. Same as Emacs

**Expected:** Backspace removes chars from search string, re-searches with shorter string

**Deviations:** _______________

---

### Test 5.7: isearch - C-g (abort)

**Emacs steps:**
1. Type: "hello world"
2. Go to beginning (M-<)
3. Press C-s
4. Type: "world"
5. Press C-g

**Lexicon steps:**
1. Same as Emacs

**Expected:** Cursor returns to starting position, isearch exits

**Deviations:** _______________

---

## Summary

**Total tests:** 23
**Passed:** ___
**Failed:** ___

**Critical deviations:** (list any behavior that differs from Emacs)

1.
2.
3.

**Notes:**
