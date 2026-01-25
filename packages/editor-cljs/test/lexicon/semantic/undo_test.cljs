(ns lexicon.semantic.undo-test
  "Semantic tests for undo system - core editing primitive.

  Undo system components:
  - C level: undo.c has record_insert, record_delete, undo-boundary
  - Lisp level: simple.el has undo, primitive-undo, undo-start, undo-more

  Key concepts:
  - buffer-undo-list: List of undo entries (nil boundaries separate commands)
  - pending-undo-list: Pointer into undo list during undo sequence
  - undo-equiv-table: Tracks redo equivalences
  - Entries: (BEG . END) for insertion, (TEXT . POS) for deletion

  Related: Issue #103 (Undo System), Issue #94 (TDD)
  Priority: CRITICAL - required for safe editing"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]
                   [lexicon.core.dynamic :refer [without-undo]]))

;;; =============================================================================
;;; Basic Undo Operations
;;; =============================================================================

(deftest ^:critical undo-reverses-insert
  "CRITICAL: Undo reverses text insertion.

  Emacs Semantics (undo.c):
  - record_insert records (BEG . END) cons
  - Undo deletes the range [BEG, END)

  Why this matters:
  - Most fundamental undo operation
  - Must work for basic editing"
  (testing "undo removes inserted text"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/undo-boundary)

      (helpers/undo)

      (is (= "" (helpers/buffer-string))
          "Inserted text should be removed"))))

(deftest ^:critical undo-reverses-delete
  "CRITICAL: Undo reverses text deletion.

  Emacs Semantics (undo.c):
  - record_delete records (TEXT . POS) where TEXT is deleted string
  - Undo re-inserts TEXT at POS

  Why this matters:
  - Must restore deleted text exactly"
  (testing "undo restores deleted text"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/undo-boundary)

      (helpers/delete-region 0 6)
      (helpers/undo-boundary)

      (helpers/undo)

      (is (= "Hello World" (helpers/buffer-string))
          "Deleted text should be restored"))))

(deftest ^:critical undo-boundary-groups-changes
  "CRITICAL: undo-boundary marks logical operation groups.

  Emacs Semantics (undo.c:251):
  - undo-boundary adds nil to buffer-undo-list
  - Multiple edits between boundaries undo as single unit
  - Commands automatically add boundaries

  Why this matters:
  - User expects C-z to undo whole command, not character by character"
  (testing "multiple edits between boundaries undo together"
    (with-test-buffer "*test*"
      (helpers/undo-boundary)

      ;; Multiple edits without boundary
      (helpers/insert "A")
      (helpers/insert "B")
      (helpers/insert "C")

      (helpers/undo-boundary)

      ;; Single undo should remove all three
      (helpers/undo)

      (is (= "" (helpers/buffer-string))
          "All edits between boundaries should undo together"))))

(deftest ^:critical undo-restores-point
  "CRITICAL: Undo restores point position.

  Emacs Semantics (undo.c:record_point):
  - Point position recorded at command start
  - Restored during undo
  - Only recorded at boundaries, not every edit

  Why this matters:
  - User expects cursor to return to original position"
  (testing "undo restores point"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 2)  ; Middle of 'Hello'
      (helpers/undo-boundary)

      (helpers/insert " World")  ; Point now at end
      (helpers/undo-boundary)

      (helpers/undo)

      (is (= 2 (helpers/point))
          "Point should be restored to pre-command position"))))

;;; =============================================================================
;;; Consecutive Undo
;;; =============================================================================

(deftest ^:high consecutive-undo-continues-chain
  "HIGH: Consecutive undo commands continue undoing.

  Emacs Semantics (simple.el:3447):
  - If last-command is undo, don't restart undo chain
  - Allows undoing many changes with repeated C-z
  - Non-undo command breaks the chain

  Why this matters:
  - Must undo entire session of work with C-z C-z C-z..."
  (testing "consecutive undos undo multiple boundaries"
    (with-test-buffer "*test*"
      (helpers/insert "A")
      (helpers/undo-boundary)
      (helpers/insert "B")
      (helpers/undo-boundary)
      (helpers/insert "C")
      (helpers/undo-boundary)

      (helpers/undo)  ; Remove C
      (helpers/undo)  ; Remove B
      (helpers/undo)  ; Remove A

      (is (= "" (helpers/buffer-string))
          "Consecutive undos should remove all insertions"))))

;;; =============================================================================
;;; Redo (Undo the Undo)
;;; =============================================================================

(deftest ^:high undo-creates-redo-record
  "HIGH: Undoing creates redo records.

  Emacs Semantics:
  - Undo operations are themselves recorded
  - These become 'redo' operations
  - Continuing undo past original state does redo

  Implementation Note:
  - Lexicon uses separate redo stack for simplicity"
  (testing "redo restores undone change"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/undo-boundary)

      (helpers/undo)
      (is (= "" (helpers/buffer-string)))

      (helpers/redo)
      (is (= "Hello" (helpers/buffer-string))
          "Redo should restore undone change"))))

(deftest ^:high new-edit-clears-redo
  "HIGH: New edit after undo clears redo history.

  Emacs Semantics:
  - After undo, new edit creates new branch
  - Cannot redo after making new changes

  Why this matters:
  - Standard undo/redo tree semantics"
  (testing "edit after undo clears redo"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/undo-boundary)
      (helpers/undo)

      ;; New edit
      (helpers/insert "Goodbye")
      (helpers/undo-boundary)

      ;; Redo should have nothing (or error/no-op)
      (helpers/redo)

      (is (= "Goodbye" (helpers/buffer-string))
          "Redo should be cleared after new edit"))))

;;; =============================================================================
;;; Undo Groups / Atomic Operations
;;; =============================================================================

(deftest ^:high with-undo-group-atomic
  "HIGH: with-undo-group makes multiple edits atomic.

  Emacs Semantics:
  - atomic-change-group macro
  - All changes succeed or all fail
  - Single undo undoes entire group

  Implementation Note:
  - Lexicon uses :group-start/:group-end markers"
  (testing "grouped edits undo atomically"
    (with-test-buffer "*test*"
      (helpers/undo-boundary)

      (helpers/with-undo-group
        (fn []
          (helpers/insert "One ")
          (helpers/insert "Two ")
          (helpers/insert "Three")))

      (helpers/undo-boundary)

      ;; Single undo should remove entire group
      (helpers/undo)

      (is (= "" (helpers/buffer-string))
          "Grouped edits should undo as single unit"))))

;;; =============================================================================
;;; Undo Recording Control
;;; =============================================================================

(deftest ^:high without-undo-disables-recording
  "HIGH: without-undo macro disables undo recording.

  Emacs Semantics:
  - buffer-undo-list set to t disables undo
  - Used for buffer initialization, macro expansion

  Implementation Note:
  - Lexicon uses :recording-enabled? flag"
  (testing "without-undo prevents undo recording"
    (with-test-buffer "*test*"
      (helpers/undo-boundary)

      (without-undo
        (helpers/insert "Not undoable"))

      (helpers/undo-boundary)
      (helpers/insert "Undoable")
      (helpers/undo-boundary)

      (helpers/undo)  ; Should only undo 'Undoable'

      (is (= "Not undoable" (helpers/buffer-string))
          "Text inserted without-undo should remain"))))

(deftest ^:medium buffer-disable-undo-for-buffer
  "MEDIUM: buffer-disable-undo disables undo for entire buffer.

  Emacs Semantics (buffer.c:1771):
  - Sets buffer-undo-list to t
  - All edits become unrecoverable
  - Used for scratch buffers, logs

  Implementation Note:
  - Per-buffer :undo-disabled flag"
  (testing "buffer-disable-undo stops recording"
    (with-test-buffer "*test*"
      (helpers/buffer-disable-undo)

      (helpers/insert "Not recorded")
      (helpers/undo-boundary)

      ;; Undo should do nothing
      (helpers/undo)

      (is (= "Not recorded" (helpers/buffer-string))
          "Edit should remain (no undo history)"))))

;;; =============================================================================
;;; Undo Stack Limits
;;; =============================================================================

(deftest ^:medium undo-limit-truncates-history
  "MEDIUM: Undo history respects size limits.

  Emacs Semantics:
  - undo-limit: soft limit (default 160000 bytes)
  - undo-strong-limit: hard limit (default 240000 bytes)
  - undo-outer-limit: per-command limit (default 24000000 bytes)

  Why this matters:
  - Prevents unbounded memory growth
  - Long sessions need truncation"
  (testing "old undo history is truncated"
    (with-test-buffer "*test*"
      ;; Generate lots of undo history
      (dotimes [i 200]
        (helpers/insert "Some text that should eventually get truncated ")
        (helpers/undo-boundary))

      ;; Should still be able to undo recent changes
      ;; But very old changes should be truncated
      (let [undo-count (helpers/undo-history-length)]
        (is (< undo-count 200)
            "Undo history should be truncated")))))

;;; =============================================================================
;;; Undo in Region
;;; =============================================================================

(deftest ^:low undo-in-region-selective
  "LOW: Undo in region only undoes changes within region.

  Emacs Semantics (simple.el:3455):
  - With active region, undo only affects that region
  - Changes outside region preserved
  - Uses undo-make-selective-list

  Why this matters:
  - Power user feature for selective undo"
  (testing "undo in region is selective"
    (with-test-buffer "*test*"
      (helpers/insert "AAABBBCCC")
      (helpers/undo-boundary)

      ;; Change middle section
      (helpers/delete-region 3 6)
      (helpers/goto-char 3)
      (helpers/insert "XXX")
      (helpers/undo-boundary)

      ;; Select only middle and undo
      (helpers/set-mark 3)
      (helpers/goto-char 6)
      (helpers/activate-mark)

      (helpers/undo-in-region)

      (is (= "AAABBBCCC" (helpers/buffer-string))
          "Only region changes should be undone"))))

;;; =============================================================================
;;; Edge Cases
;;; =============================================================================

(deftest ^:low undo-empty-buffer-noop
  "LOW: Undo on empty buffer does nothing.

  Implementation Note:
  - Should not error, just no-op"
  (testing "undo on empty undo stack"
    (with-test-buffer "*test*"
      ;; Clear any implicit undo history
      (helpers/buffer-disable-undo)
      (helpers/buffer-enable-undo)

      ;; Should not error
      (helpers/undo)

      (is (= "" (helpers/buffer-string))
          "Undo on empty buffer should be no-op"))))

(deftest ^:low undo-preserves-markers
  "LOW: Undo correctly adjusts markers.

  Emacs Semantics (undo.c:record_marker_adjustments):
  - Markers within deleted region need explicit adjustment
  - insertion-type affects restoration behavior

  Implementation Note:
  - WASM gap buffer handles most marker adjustment"
  (testing "markers adjusted during undo"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (let [marker (helpers/make-marker)]
        (helpers/set-marker marker 6)  ; At 'W'

        (helpers/undo-boundary)
        (helpers/delete-region 0 6)  ; Delete 'Hello '
        (helpers/undo-boundary)

        ;; Marker now at 0 (adjusted)
        (is (= 0 (helpers/marker-position marker)))

        (helpers/undo)

        ;; After undo, marker should be back at 6
        (is (= 6 (helpers/marker-position marker))
            "Marker should be restored after undo")))))
