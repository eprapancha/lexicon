(ns lexicon.semantic.kill-ring-test
  "Semantic tests for kill ring - core editing primitive.

  Kill ring is a circular list storing killed/copied text for yanking.
  Emacs implementation: lisp/simple.el (kill-new, kill-append, current-kill, yank)

  Key variables:
  - kill-ring: The list of killed strings
  - kill-ring-max: Maximum size (default 120 in Emacs)
  - kill-ring-yank-pointer: Points to next item for yank-pop

  Related: Issue #97 (Kill ring not functioning), Issue #104 (TDD)
  Priority: CRITICAL - required for basic editing workflow"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; Kill Ring Core Operations
;;; =============================================================================

(deftest ^:critical kill-region-adds-to-kill-ring
  "CRITICAL: C-w (kill-region) saves text to kill ring before deleting.

  Emacs Semantics (simple.el:5757):
  - Kills text between point and mark
  - Text is saved to kill-ring via kill-new
  - If mark not set, signals error

  Why this matters:
  - Foundation of copy/paste workflow
  - Must work for C-w, M-w, C-k to function"
  (testing "kill-region saves text and deletes"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/set-mark 0)
      (helpers/goto-char 5)

      ;; Kill region should save "Hello" and delete it
      (helpers/kill-region 0 5)

      (is (= " World" (helpers/buffer-string))
          "Text should be deleted")
      (is (= "Hello" (helpers/current-kill 0))
          "Killed text should be on kill ring"))))

(deftest ^:critical yank-inserts-from-kill-ring
  "CRITICAL: C-y (yank) inserts most recent kill.

  Emacs Semantics (simple.el:6214):
  - Inserts most recent kill at point
  - Sets mark at beginning, point at end
  - With prefix arg N, inserts Nth most recent kill

  Why this matters:
  - This is the 'paste' operation
  - Must work for basic editing"
  (testing "yank inserts last killed text"
    (with-test-buffer "*test*"
      (helpers/insert "World")

      ;; Add something to kill ring
      (helpers/kill-new "Hello ")

      ;; Yank at beginning
      (helpers/goto-char 0)
      (helpers/yank)

      (is (= "Hello World" (helpers/buffer-string))
          "Yanked text should be inserted"))))

(deftest ^:critical yank-pop-rotates-kill-ring
  "CRITICAL: M-y (yank-pop) replaces yank with previous kill.

  Emacs Semantics (simple.el:6166):
  - Only works immediately after yank or yank-pop
  - Replaces last yank with next item in kill ring
  - Rotates through the ring

  Why this matters:
  - Access to kill ring history
  - Essential for multi-step paste operations"
  (testing "yank-pop rotates through kills"
    (with-test-buffer "*test*"
      ;; Add multiple items to kill ring
      (helpers/kill-new "First")
      (helpers/kill-new "Second")
      (helpers/kill-new "Third")

      ;; Yank most recent
      (helpers/yank)
      (is (= "Third" (helpers/buffer-string)))

      ;; Pop to previous
      (helpers/yank-pop)
      (is (= "Second" (helpers/buffer-string)))

      ;; Pop again
      (helpers/yank-pop)
      (is (= "First" (helpers/buffer-string))))))

(deftest ^:high kill-new-adds-to-front
  "HIGH: kill-new adds string to front of kill ring.

  Emacs Semantics (simple.el:5604):
  - New kill becomes car of kill-ring
  - kill-ring-yank-pointer points to it
  - Optional REPLACE arg replaces instead of adding

  Implementation Note:
  - kill-ring is a list, newest first"
  (testing "kill-new prepends to kill ring"
    (with-test-buffer "*test*"
      (helpers/kill-new "First")
      (helpers/kill-new "Second")

      (is (= "Second" (helpers/current-kill 0))
          "Most recent kill at front")
      (is (= "First" (helpers/current-kill 1))
          "Previous kill accessible"))))

(deftest ^:high kill-append-concatenates
  "HIGH: kill-append adds to existing kill instead of new entry.

  Emacs Semantics (simple.el:5672):
  - Appends (or prepends with BEFORE-P) to car of kill-ring
  - Used by C-k when killing multiple lines
  - Does NOT create new kill ring entry

  Why this matters:
  - Killing multiple lines should create one kill entry
  - append-next-kill (C-M-w) uses this"
  (testing "kill-append concatenates with last kill"
    (with-test-buffer "*test*"
      (helpers/kill-new "Hello")
      (helpers/kill-append " World" false)

      (is (= "Hello World" (helpers/current-kill 0))
          "Text appended to last kill")
      (is (= 1 (helpers/kill-ring-length))
          "No new kill ring entry created")))

  (testing "kill-append with before-p prepends"
    (with-test-buffer "*test*"
      (helpers/kill-new "World")
      (helpers/kill-append "Hello " true)

      (is (= "Hello World" (helpers/current-kill 0))
          "Text prepended to last kill"))))

(deftest ^:high current-kill-rotates
  "HIGH: current-kill returns Nth kill and optionally rotates.

  Emacs Semantics (simple.el:5703):
  - (current-kill 0) returns most recent
  - (current-kill N) returns Nth and rotates pointer
  - With DO-NOT-MOVE, doesn't rotate

  Implementation Note:
  - Circular access via modulo"
  (testing "current-kill with rotation"
    (with-test-buffer "*test*"
      (helpers/kill-new "First")
      (helpers/kill-new "Second")
      (helpers/kill-new "Third")

      ;; Rotate to second
      (let [kill (helpers/current-kill 1)]
        (is (= "Second" kill)))

      ;; Now yank should get second (pointer rotated)
      (helpers/yank)
      (is (= "Second" (helpers/buffer-string))))))

(deftest ^:medium kill-ring-max-size
  "MEDIUM: Kill ring respects maximum size.

  Emacs Semantics:
  - kill-ring-max defaults to 120
  - Oldest kills dropped when max exceeded

  Implementation Note:
  - KILL_RING_MAX_SIZE in constants"
  (testing "Kill ring truncates at max size"
    (with-test-buffer "*test*"
      ;; Add many kills
      (dotimes [i 130]
        (helpers/kill-new (str "kill-" i)))

      ;; Should have at most kill-ring-max items
      (is (<= (helpers/kill-ring-length) 120)
          "Kill ring respects max size"))))

(deftest ^:medium kill-region-with-read-only
  "MEDIUM: kill-region in read-only buffer copies instead of killing.

  Emacs Semantics (simple.el:5787):
  - With kill-read-only-ok non-nil, copies text
  - Without it, signals buffer-read-only error

  Implementation Note:
  - Check read-only before delete, after copy to kill ring"
  (testing "kill-region in read-only copies (if allowed)"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/set-buffer-read-only true)

      ;; Attempt to kill
      (helpers/kill-region 0 5)

      ;; Text should still be there (read-only)
      (is (= "Hello World" (helpers/buffer-string)))
      ;; But should be on kill ring (copied)
      (is (= "Hello" (helpers/current-kill 0))))))

;;; =============================================================================
;;; Kill Commands
;;; =============================================================================

(deftest ^:critical kill-line-basic
  "CRITICAL: C-k (kill-line) kills to end of line.

  Emacs Semantics:
  - With no prefix: kill to end of line (not including newline)
  - If at end of line: kill the newline
  - With prefix N: kill N lines

  Why this matters:
  - Most common kill command"
  (testing "kill-line kills to end of line"
    (with-test-buffer "*test*"
      (helpers/insert "Hello\nWorld")
      (helpers/goto-char 0)

      (helpers/kill-line)

      (is (= "\nWorld" (helpers/buffer-string))
          "Killed to end of first line")
      (is (= "Hello" (helpers/current-kill 0))
          "Killed text on ring")))

  (testing "kill-line at end of line kills newline"
    (with-test-buffer "*test*"
      (helpers/insert "Hello\nWorld")
      (helpers/goto-char 5)  ; After "Hello", before newline

      (helpers/kill-line)

      (is (= "HelloWorld" (helpers/buffer-string))
          "Newline was killed"))))

(deftest ^:high kill-word-basic
  "HIGH: M-d (kill-word) kills word forward.

  Emacs Semantics:
  - Kills from point to end of current/next word
  - With prefix N, kills N words

  Implementation Note:
  - Uses forward-word to find boundary"
  (testing "kill-word kills one word forward"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World Goodbye")
      (helpers/goto-char 0)

      (helpers/kill-word)

      (is (= " World Goodbye" (helpers/buffer-string)))
      (is (= "Hello" (helpers/current-kill 0))))))

(deftest ^:high backward-kill-word-basic
  "HIGH: M-DEL (backward-kill-word) kills word backward.

  Emacs Semantics:
  - Kills from point to beginning of current/previous word
  - With prefix N, kills N words backward

  Implementation Note:
  - Uses backward-word to find boundary"
  (testing "backward-kill-word kills one word backward"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World Goodbye")
      (helpers/goto-char 11)  ; After "World"

      (helpers/backward-kill-word)

      (is (= "Hello  Goodbye" (helpers/buffer-string)))
      (is (= "World" (helpers/current-kill 0))))))

;;; =============================================================================
;;; Copy (Non-destructive Kill)
;;; =============================================================================

(deftest ^:critical copy-region-as-kill
  "CRITICAL: M-w (kill-ring-save) copies without deleting.

  Emacs Semantics (simple.el:5827):
  - Saves region to kill ring
  - Does NOT delete text
  - Deactivates mark

  Why this matters:
  - This is 'copy' operation"
  (testing "copy-region-as-kill saves without deleting"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/set-mark 0)
      (helpers/goto-char 5)

      (helpers/copy-region-as-kill 0 5)

      (is (= "Hello World" (helpers/buffer-string))
          "Text should NOT be deleted")
      (is (= "Hello" (helpers/current-kill 0))
          "Text should be on kill ring"))))

;;; =============================================================================
;;; Kill Ring with Text Properties
;;; =============================================================================

(deftest ^:medium yank-preserves-text-properties
  "MEDIUM: Yank preserves text properties from killed text.

  Emacs Semantics:
  - Text properties saved with kill
  - Restored on yank (subject to yank-excluded-properties)

  Implementation Note:
  - Store properties in kill ring entry
  - Apply on insert"
  (testing "Yanked text retains properties"
    (with-test-buffer "*test*"
      ;; Insert text with property
      (helpers/insert "Hello")
      (helpers/put-text-property 0 5 'face 'bold)

      ;; Kill it
      (helpers/kill-region 0 5)

      ;; Yank elsewhere
      (helpers/yank)

      ;; Check property preserved
      (is (= 'bold (helpers/get-text-property 0 'face))
          "Face property preserved after yank"))))

;;; =============================================================================
;;; Edge Cases
;;; =============================================================================

(deftest ^:low yank-empty-kill-ring
  "LOW: Yank with empty kill ring signals error.

  Emacs Semantics:
  - (yank) with empty kill-ring signals error
  - Message: 'Kill ring is empty'

  Implementation Note:
  - Check kill-ring before attempting yank"
  (testing "Yank signals error when kill ring empty"
    (with-test-buffer "*test*"
      ;; Ensure kill ring is empty
      (helpers/clear-kill-ring)

      ;; Attempt yank should fail gracefully
      (let [result (helpers/yank)]
        (is (nil? result) "Yank returns nil on empty ring")))))

(deftest ^:low kill-ring-yank-pointer-reset
  "LOW: kill-ring-yank-pointer resets after kill-new.

  Emacs Semantics:
  - kill-new sets kill-ring-yank-pointer to kill-ring
  - Ensures next yank gets newest kill

  Implementation Note:
  - Reset :kill-ring-index on new kill"
  (testing "Pointer resets after new kill"
    (with-test-buffer "*test*"
      (helpers/kill-new "First")
      (helpers/kill-new "Second")

      ;; Rotate to first
      (let [_ (helpers/current-kill 1)])

      ;; Add new kill
      (helpers/kill-new "Third")

      ;; Yank should get newest, not where pointer was
      (helpers/yank)
      (is (= "Third" (helpers/buffer-string))))))
