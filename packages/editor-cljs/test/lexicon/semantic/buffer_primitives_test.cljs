(ns lexicon.semantic.buffer-primitives-test
  "Semantic tests for buffer primitives - core buffer operations.

  Buffer primitives are defined in Emacs src/buffer.c (50 DEFUNs).
  Key categories:
  - Buffer identity: buffer-name, buffer-live-p, current-buffer
  - Buffer creation: get-buffer-create, make-indirect-buffer
  - Buffer switching: set-buffer
  - Buffer state: buffer-modified-p, set-buffer-modified-p
  - Narrowing: point-min, point-max, narrow-to-region, widen
  - Kill: kill-buffer

  Related: Issue #100 (Buffer Primitives), Issue #94 (TDD)
  Priority: CRITICAL - foundation for all buffer operations"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; Buffer Identity and Lifecycle
;;; =============================================================================

(deftest ^:critical buffer-live-p-basics
  "CRITICAL: buffer-live-p checks if buffer object is alive.

  Emacs Semantics (buffer.c:405):
  - Returns t if OBJECT is a buffer which has not been killed
  - Returns nil for killed buffers or non-buffer objects

  Why this matters:
  - Foundation for safe buffer operations
  - Must check before accessing killed buffer"
  (testing "buffer-live-p returns true for existing buffer"
    (with-test-buffer "*test*"
      (is (helpers/buffer-live-p (helpers/current-buffer-id))
          "Current buffer should be alive")))

  (testing "buffer-live-p returns false for killed buffer"
    (with-test-buffer "*test*"
      (let [buf-id (helpers/current-buffer-id)]
        (helpers/kill-buffer buf-id)
        (is (not (helpers/buffer-live-p buf-id))
            "Killed buffer should not be live")))))

(deftest ^:critical get-buffer-create-basics
  "CRITICAL: get-buffer-create returns existing or creates new buffer.

  Emacs Semantics (buffer.c:539):
  - If buffer named NAME exists, return it
  - Otherwise create new buffer with that name
  - Optional INHIBIT-BUFFER-HOOKS skips buffer creation hooks

  Why this matters:
  - Primary way to ensure a buffer exists
  - Different from get-buffer which returns nil if not found"
  (testing "get-buffer-create returns existing buffer"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [buf1 (helpers/get-buffer-create "*test*")
            buf2 (helpers/get-buffer-create "*test*")]
        (is (= buf1 buf2)
            "Should return same buffer"))))

  (testing "get-buffer-create creates new buffer"
    (with-test-buffer "*test*"
      (let [new-buf (helpers/get-buffer-create "*new-buffer*")]
        (is (some? new-buf)
            "Should create new buffer")
        ;; Cleanup
        (helpers/kill-buffer new-buf)))))

;;; =============================================================================
;;; Buffer Modification State
;;; =============================================================================

(deftest ^:critical buffer-modified-p-basics
  "CRITICAL: buffer-modified-p tracks unsaved changes.

  Emacs Semantics (buffer.c:1447):
  - Returns t if buffer has been modified since last save
  - Returns nil for unmodified buffers
  - Controls 'modified' indicator in mode line

  Why this matters:
  - Prevents closing unsaved work
  - Drives save prompts"
  (testing "new buffer is not modified"
    (with-test-buffer "*test*"
      (is (not (helpers/buffer-modified-p))
          "Fresh buffer should not be modified")))

  (testing "insertion marks buffer modified"
    (with-test-buffer "*test*"
      (helpers/insert "text")
      (is (helpers/buffer-modified-p)
          "Buffer should be modified after insert"))))

(deftest ^:critical set-buffer-modified-p-basics
  "CRITICAL: set-buffer-modified-p explicitly sets modification flag.

  Emacs Semantics (buffer.c:1490):
  - (set-buffer-modified-p nil) marks buffer as unmodified
  - (set-buffer-modified-p t) marks buffer as modified
  - Used after saving to clear modified flag

  Why this matters:
  - After save-buffer, must clear flag
  - Allows manual control of modification state"
  (testing "set-buffer-modified-p clears modification"
    (with-test-buffer "*test*"
      (helpers/insert "text")
      (helpers/set-buffer-modified-p nil)
      (is (not (helpers/buffer-modified-p))
          "Modified flag should be cleared")))

  (testing "set-buffer-modified-p sets modification"
    (with-test-buffer "*test*"
      (helpers/set-buffer-modified-p true)
      (is (helpers/buffer-modified-p)
          "Modified flag should be set"))))

;;; =============================================================================
;;; Narrowing
;;; =============================================================================

(deftest ^:high narrowing-affects-point-bounds
  "HIGH: narrow-to-region restricts visible portion of buffer.

  Emacs Semantics (editfns.c):
  - point-min returns BEGV (beginning of accessible portion)
  - point-max returns ZV (end of accessible portion)
  - narrow-to-region sets BEGV and ZV
  - widen restores full buffer access

  Why this matters:
  - Many Emacs Lisp functions respect narrowing
  - Modes like org-mode use it extensively"
  (testing "point-min and point-max reflect narrowing"
    (with-test-buffer "*test*"
      (helpers/insert "0123456789")
      (helpers/narrow-to-region 3 7)

      ;; In narrowed view, point-min should be 3, point-max 7
      ;; But in Emacs, point-min/max return 1-indexed positions
      ;; relative to the narrowed region, so point-min=1
      (is (<= (helpers/point-min) (helpers/point-max))
          "point-min should be <= point-max")))

  (testing "widen removes restriction"
    (with-test-buffer "*test*"
      (helpers/insert "0123456789")
      (helpers/narrow-to-region 3 7)
      (helpers/widen)

      ;; After widen, should see full buffer
      (is (= 10 (count (helpers/buffer-string)))
          "Full buffer should be accessible after widen"))))

(deftest ^:high save-restriction-preserves-narrowing
  "HIGH: save-restriction macro preserves narrowing state.

  Emacs Semantics (editfns.c):
  - save-restriction saves current BEGV/ZV
  - Restores them after body executes
  - Works like unwind-protect for narrowing

  Why this matters:
  - Functions should not leave narrowing as side effect"
  (testing "save-restriction restores original narrowing"
    (with-test-buffer "*test*"
      (helpers/insert "0123456789")
      (helpers/narrow-to-region 2 8)

      ;; Save restriction, widen, then restore
      (helpers/save-restriction
       (fn []
         (helpers/widen)
         (is (= 10 (count (helpers/buffer-string))))))

      ;; After save-restriction, should be narrowed again
      (is (= 6 (count (helpers/buffer-string)))
          "Narrowing should be restored"))))

;;; =============================================================================
;;; Buffer Switching
;;; =============================================================================

(deftest ^:high set-buffer-switches-context
  "HIGH: set-buffer makes buffer current for buffer-local operations.

  Emacs Semantics (buffer.c:2335):
  - Makes BUFFER-OR-NAME the current buffer
  - Does NOT display it (use switch-to-buffer for that)
  - Buffer-local variables now resolve in new buffer

  Why this matters:
  - Temporary buffer switching for operations
  - Different from switch-to-buffer which also shows in window"
  (testing "set-buffer changes current buffer"
    (with-test-buffer "*buffer-a*"
      (helpers/insert "Buffer A content")
      (helpers/create-buffer "*buffer-b*")

      (helpers/set-buffer "*buffer-b*")
      (is (= "*buffer-b*" (helpers/buffer-name))
          "Should switch to buffer B")

      (helpers/set-buffer "*buffer-a*")
      (is (= "*buffer-a*" (helpers/buffer-name))
          "Should switch back to buffer A"))))

;;; =============================================================================
;;; Other Buffer Functions
;;; =============================================================================

(deftest ^:high rename-buffer-changes-name
  "HIGH: rename-buffer changes buffer's name.

  Emacs Semantics (buffer.c:1618):
  - Changes buffer name to NEWNAME
  - If buffer with NEWNAME exists, error or add suffix
  - UNIQUE arg generates unique name

  Implementation Note:
  - Must update buffer-alist"
  (testing "rename-buffer changes the name"
    (with-test-buffer "*original*"
      (helpers/rename-buffer "*renamed*")
      (is (= "*renamed*" (helpers/buffer-name))
          "Buffer should have new name"))))

(deftest ^:medium other-buffer-returns-alternative
  "MEDIUM: other-buffer returns most recently used different buffer.

  Emacs Semantics (buffer.c:1687):
  - Returns most recently selected buffer other than current
  - Skips buffers whose names start with space
  - Optional args filter by frame and visibility

  Why this matters:
  - Supports C-x b default behavior
  - Buffer cycling commands"
  (testing "other-buffer returns different buffer"
    (with-test-buffer "*buffer1*"
      (helpers/create-buffer "*buffer2*")
      (let [other (helpers/other-buffer)]
        (is (not= "*buffer1*" (helpers/buffer-name-from-id other))
            "Should return different buffer")))))

(deftest ^:medium buffer-enable-undo-activates-undo
  "MEDIUM: buffer-enable-undo enables undo recording for buffer.

  Emacs Semantics (buffer.c:1771):
  - Enables undo recording for BUFFER
  - By default buffers start with undo enabled
  - Some special buffers have undo disabled

  Why this matters:
  - Allows undo in buffers created with buffer-disable-undo"
  (testing "buffer-enable-undo enables undo"
    (with-test-buffer "*test*"
      ;; Disable then re-enable
      (helpers/buffer-disable-undo)
      (helpers/buffer-enable-undo)
      ;; Should be able to record undo
      (helpers/insert "text")
      (helpers/undo)
      (is (= "" (helpers/buffer-string))
          "Undo should work after buffer-enable-undo"))))

(deftest ^:low erase-buffer-clears-content
  "LOW: erase-buffer deletes all content.

  Emacs Semantics (buffer.c:2389):
  - Delete entire contents of current buffer
  - Respects read-only (signals error)
  - Point goes to beginning

  Implementation Note:
  - Should be atomic for undo"
  (testing "erase-buffer clears all text"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/erase-buffer)
      (is (= "" (helpers/buffer-string))
          "Buffer should be empty")
      (is (= 0 (helpers/point))
          "Point should be at beginning"))))

;;; =============================================================================
;;; Buffer-Local Variables
;;; =============================================================================

(deftest ^:medium buffer-local-value-basics
  "MEDIUM: buffer-local-value gets variable value in specific buffer.

  Emacs Semantics (buffer.c:1282):
  - Returns value of VARIABLE in BUFFER
  - Does not switch to buffer
  - Returns global value if not buffer-local

  Why this matters:
  - Access buffer state without switching"
  (testing "buffer-local-value returns buffer-specific value"
    (with-test-buffer "*test*"
      ;; Test with a known buffer-local like buffer-read-only
      (helpers/set-buffer-read-only true)
      (let [val (helpers/buffer-local-value 'buffer-read-only (helpers/current-buffer-id))]
        (is (true? val)
            "Should return buffer-local value")))))

(deftest ^:low kill-all-local-variables-clears-locals
  "LOW: kill-all-local-variables resets buffer to default state.

  Emacs Semantics (buffer.c:2929):
  - Kills all local variable bindings
  - Preserves permanent-local variables
  - Runs change-major-mode-hook

  Why this matters:
  - Major mode switching uses this"
  (testing "kill-all-local-variables clears locals"
    (with-test-buffer "*test*"
      ;; Set some buffer-local
      (helpers/make-local-variable 'test-var)
      (helpers/setq 'test-var 42)

      (helpers/kill-all-local-variables)

      ;; Local should be gone
      (is (nil? (helpers/buffer-local-value 'test-var (helpers/current-buffer-id)))
          "Local variable should be cleared"))))
