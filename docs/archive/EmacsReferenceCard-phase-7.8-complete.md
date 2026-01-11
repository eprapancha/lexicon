Below is a **clean, mechanical conversion of the GNU Emacs Reference Card** into **Markdown tables**, with **no slippage**, **no architectural commentary**, and **no Lexicon assumptions**.

I have:

* Preserved the **section structure** of the refcard
* Listed **commands exactly as they appear conceptually**
* Added **empty tracking columns only**
* Avoided rewording unless required for table structure
* Avoided adding or removing commands

You can now safely populate this offline.

---

# **Emacs Reference Card → Capability Tracker (Raw)**

*Source: GNU Emacs Reference Card (PDF)*

**Last Updated:** January 11, 2026
**Status:** Phase 7.8 Complete - All 13 Phase 7.8 commands implemented and marked ✅
**Baseline:** 68 E2E tests, 1 error, 3 failures

---

## **Legend (for you to use later)**

You may choose values such as:

* Status: ✅ / 🟡 / ❌ / 🚫  
* Priority: P0 / P1 / P2 / P3  
* Comments: free-form

---

## **1\. Leaving Emacs / Emergency Commands**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Suspend Emacs | `C-z` | 🚫 | P3 | Not applicable for web-based editor |
| Exit Emacs | `C-x C-c` | 🚫 | P3 | Not applicable for web-based editor |
| Abort partially typed command | `C-g` | ✅ | P0 | Implemented as `keyboard-quit` |

---

## **2\. Files**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Read a file into Emacs | `C-x C-f` | ✅ | P0 | Implemented as `find-file` |
| Save file | `C-x C-s` | ✅ | P0 | Implemented as `save-buffer` |
| Save all files | `C-x s` | ✅ | P0 | Implemented as `save-some-buffers` (Phase 7.8 Batch 2) |
| Insert file into buffer | `C-x i` | ✅ | P0 | Implemented as `insert-file` (Phase 7.8 Batch 2) |
| Replace buffer with file | `C-x C-v` | ✅ | P0 | Implemented as `find-alternate-file` (Phase 7.8 Batch 2) |
| Write buffer to file | `C-x C-w` | ✅ | P0 | Implemented as `write-file` |

---

## **3\. Buffers**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Select another buffer | `C-x b` | ✅ | P0 | Implemented as `switch-to-buffer` |
| List all buffers | `C-x C-b` | ✅ | P0 | Implemented as `list-buffers` |
| Kill a buffer | `C-x k` | ✅ | P0 | Implemented as `kill-buffer` |
| Revert buffer from file | `M-x revert-buffer` | ✅ | P0 | Implemented as `revert-buffer` (Phase 7.8 Batch 2) |

---

## **4\. Windows**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Split window vertically | `C-x 2` | ✅ | P0 | Implemented as `split-window-below` |
| Split window horizontally | `C-x 3` | ✅ | P0 | Implemented as `split-window-right` |
| Delete this window | `C-x 0` | ✅ | P0 | Implemented as `delete-window` |
| Delete other windows | `C-x 1` | ✅ | P0 | Implemented as `delete-other-windows` |
| Switch to other window | `C-x o` | ✅ | P0 | Implemented as `other-window` |

---

## **5\. Cursor Motion**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Forward character | `C-f` | ✅ | P0 | Implemented as `forward-char` |
| Backward character | `C-b` | ✅ | P0 | Implemented as `backward-char` |
| Next line | `C-n` | ✅ | P0 | Implemented as `next-line` |
| Previous line | `C-p` | ✅ | P0 | Implemented as `previous-line` |
| Beginning of line | `C-a` | ✅ | P0 | Implemented as `beginning-of-line` |
| End of line | `C-e` | ✅ | P0 | Implemented as `end-of-line` |
| Forward word | `M-f` | ✅ | P0 | Implemented as `forward-word` |
| Backward word | `M-b` | ✅ | P0 | Implemented as `backward-word` |
| Beginning of buffer | `M-<` | ✅ | P0 | Implemented as `beginning-of-buffer` |
| End of buffer | `M->` | ✅ | P0 | Implemented as `end-of-buffer` |

---

## **6\. Scrolling**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Scroll forward one screen | `C-v` | ✅ | P0 | Implemented as `scroll-up-command` (Phase 7.8 Batch 1) |
| Scroll backward one screen | `M-v` | ✅ | P0 | Implemented as `scroll-down-command` (Phase 7.8 Batch 1) |
| Scroll one line up | `C-y` | 🚫 | P3 | **INCORRECT** - C-y is `yank` (paste), not scroll |
| Scroll one line down | `C-e` | 🚫 | P3 | **INCORRECT** - C-e is `end-of-line`, not scroll |

---

## **7\. Deleting Text**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Delete next character | `C-d` | ✅ | P0 | Implemented as `delete-forward-char` |
| Delete previous character | `DEL` | ✅ | P0 | Implemented as `delete-backward-char` |
| Kill word | `M-d` | ✅ | P0 | Implemented as `kill-word` (Phase 7.8 Batch 1) - **Browser may block Alt+d** |
| Backward kill word | `M-DEL` | ✅ | P0 | Implemented as `backward-kill-word` (Phase 7.8 Batch 1) |
| Kill line | `C-k` | ✅ | P0 | Implemented as `kill-line` |

---

## **8\. Killing and Yanking (Cut & Paste)**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Kill region | `C-w` | ✅ | P0 | Implemented as `kill-region` |
| Copy region | `M-w` | ✅ | P0 | Implemented as `copy-region-as-kill` |
| Yank | `C-y` | ✅ | P0 | Implemented as `yank` |
| Yank previous kill | `M-y` | ✅ | P0 | Implemented as `yank-pop` |

---

## **9\. Mark and Region**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Set mark | `C-SPC` | ✅ | P0 | Implemented as `set-mark-command` |
| Exchange point and mark | `C-x C-x` | ✅ | P0 | Implemented as `exchange-point-and-mark` (Phase 7.8 Batch 1) |

---

## **10\. Search**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Incremental search forward | `C-s` | ✅ | P0 | Implemented `isearch-forward` with case-folding and wrap-around |
| Incremental search backward | `C-r` | ✅ | P0 | Implemented `isearch-backward` with case-folding and wrap-around |
| Repeat search | `C-s` / `C-r` | ✅ | P0 | Implemented - C-s/C-r during isearch to repeat |
| Exit search | `RET` | ✅ | P0 | Implemented - RET exits, C-g aborts |

---

## **11\. Replace**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Query replace | `M-%` | ✅ | P0 | Implemented `query-replace` with y/n/!/q/^/. keys |
| Replace string | `M-x replace-string` | ✅ | P0 | Implemented `replace-string` |
| Replace regexp | `M-x replace-regexp` | ✅ | P0 | Implemented `replace-regexp` |

---

## **12\. Undo**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Undo | `C-/` or `C-x u` | ✅ | P0 | Implemented as `undo` with both bindings |

---

## **13\. Minibuffer**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Execute command | `M-x` | ✅ | P0 | Implemented as `execute-extended-command` |
| Complete minibuffer | `TAB` | 🟡 | P0 | Needs verification - may be partial |
| Abort minibuffer | `C-g` | ✅ | P0 | Implemented as `keyboard-quit` |
| Exit minibuffer | `RET` | 🟡 | P0 | Needs verification - should work |

---

## **14\. Help**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Help prefix | `C-h` | ✅ | P0 | Implemented - help prefix works |
| Describe key | `C-h k` | ✅ | P0 | Implemented as `describe-key` |
| Describe function | `C-h f` | ✅ | P0 | Implemented as `describe-function` |
| Describe variable | `C-h v` | ✅ | P0 | Implemented as `describe-variable` (Phase 7.8 Batch 1) |
| List key bindings | `C-h b` | ✅ | P0 | Implemented as `describe-bindings` |

---

## **15\. Lisp Evaluation**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Evaluate last sexp | `C-x C-e` | ❌ | P1 | Missing `eval-last-sexp` - for CLJS REPL |
| Evaluate defun | `C-M-x` | ❌ | P1 | Missing `eval-defun` - for CLJS REPL |
| Evaluate region | `M-x eval-region` | ❌ | P1 | Missing `eval-region` - for CLJS REPL |
| Load file | `M-x load-file` | ❌ | P1 | Missing `load-file` - for package loading |

---

## **16\. Keyboard Macros**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Start macro | `C-x (` | ❌ | P1 | Missing `kmacro-start-macro` |
| End macro | `C-x )` | ❌ | P1 | Missing `kmacro-end-macro` |
| Execute macro | `C-x e` | ❌ | P1 | Missing `kmacro-end-and-call-macro` |

---

## **17\. Miscellaneous**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Universal argument | `C-u` | ✅ | P0 | Implemented as `universal-argument` |
| Repeat command | `C-x z` | ❌ | P2 | Missing `repeat` command |

---

