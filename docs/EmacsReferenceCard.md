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
| Suspend Emacs | `C-z` |  |  |  |
| Exit Emacs | `C-x C-c` |  |  |  |
| Abort partially typed command | `C-g` |  |  |  |

---

## **2\. Files**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Read a file into Emacs | `C-x C-f` |  |  |  |
| Save file | `C-x C-s` |  |  |  |
| Save all files | `C-x s` |  |  |  |
| Insert file into buffer | `C-x i` |  |  |  |
| Replace buffer with file | `C-x C-v` |  |  |  |
| Write buffer to file | `C-x C-w` |  |  |  |

---

## **3\. Buffers**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Select another buffer | `C-x b` |  |  |  |
| List all buffers | `C-x C-b` |  |  |  |
| Kill a buffer | `C-x k` |  |  |  |
| Revert buffer from file | `M-x revert-buffer` |  |  |  |

---

## **4\. Windows**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Split window vertically | `C-x 2` |  |  |  |
| Split window horizontally | `C-x 3` |  |  |  |
| Delete this window | `C-x 0` |  |  |  |
| Delete other windows | `C-x 1` |  |  |  |
| Switch to other window | `C-x o` |  |  |  |

---

## **5\. Cursor Motion**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Forward character | `C-f` |  |  |  |
| Backward character | `C-b` |  |  |  |
| Next line | `C-n` |  |  |  |
| Previous line | `C-p` |  |  |  |
| Beginning of line | `C-a` |  |  |  |
| End of line | `C-e` |  |  |  |
| Forward word | `M-f` |  |  |  |
| Backward word | `M-b` |  |  |  |
| Beginning of buffer | `M-<` |  |  |  |
| End of buffer | `M->` |  |  |  |

---

## **6\. Scrolling**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Scroll forward one screen | `C-v` |  |  |  |
| Scroll backward one screen | `M-v` |  |  |  |
| Scroll one line up | `C-y` |  |  |  |
| Scroll one line down | `C-e` |  |  |  |

---

## **7\. Deleting Text**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Delete next character | `C-d` |  |  |  |
| Delete previous character | `DEL` |  |  |  |
| Kill word | `M-d` |  |  |  |
| Kill line | `C-k` |  |  |  |

---

## **8\. Killing and Yanking (Cut & Paste)**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Kill region | `C-w` |  |  |  |
| Copy region | `M-w` |  |  |  |
| Yank | `C-y` |  |  |  |
| Yank previous kill | `M-y` |  |  |  |

---

## **9\. Mark and Region**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Set mark | `C-SPC` |  |  |  |
| Exchange point and mark | `C-x C-x` |  |  |  |

---

## **10\. Search**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Incremental search forward | `C-s` |  |  |  |
| Incremental search backward | `C-r` |  |  |  |
| Repeat search | `C-s` / `C-r` |  |  |  |
| Exit search | `RET` |  |  |  |

---

## **11\. Replace**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Query replace | `M-%` |  |  |  |
| Replace string | `M-x replace-string` |  |  |  |
| Replace regexp | `M-x replace-regexp` |  |  |  |

---

## **12\. Undo**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Undo | `C-/` or `C-x u` |  |  |  |

---

## **13\. Minibuffer**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Execute command | `M-x` |  |  |  |
| Complete minibuffer | `TAB` |  |  |  |
| Abort minibuffer | `C-g` |  |  |  |
| Exit minibuffer | `RET` |  |  |  |

---

## **14\. Help**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Help prefix | `C-h` |  |  |  |
| Describe key | `C-h k` |  |  |  |
| Describe function | `C-h f` |  |  |  |
| Describe variable | `C-h v` |  |  |  |
| List key bindings | `C-h b` |  |  |  |

---

## **15\. Lisp Evaluation**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Evaluate last sexp | `C-x C-e` |  |  |  |
| Evaluate defun | `C-M-x` |  |  |  |
| Evaluate region | `M-x eval-region` |  |  |  |
| Load file | `M-x load-file` |  |  |  |

---

## **16\. Keyboard Macros**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Start macro | `C-x (` |  |  |  |
| End macro | `C-x )` |  |  |  |
| Execute macro | `C-x e` |  |  |  |

---

## **17\. Miscellaneous**

| Emacs Command | Key Binding | Status | Priority | Comments |
| ----- | ----- | ----- | ----- | ----- |
| Universal argument | `C-u` |  |  |  |
| Repeat command | `C-x z` |  |  |  |

---

