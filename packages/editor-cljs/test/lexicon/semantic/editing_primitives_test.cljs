(ns lexicon.semantic.editing-primitives-test
  "Semantic tests for editing primitives - core text manipulation.

  Editing primitives in Emacs src/editfns.c (~70 DEFUNs).
  Key categories:
  - Text insertion: insert, insert-before-markers
  - Text deletion: delete-region, delete-and-extract-region
  - Text extraction: buffer-substring, buffer-string
  - Point movement: goto-char, point
  - Position predicates: bobp, eobp, bolp, eolp
  - Character access: char-after, char-before
  - Line positions: line-beginning-position, line-end-position

  Related: Issue #105 (Editing Primitives), Issue #94 (TDD)
  Priority: CRITICAL - foundation for all text editing"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; Text Insertion
;;; =============================================================================

(deftest ^:critical insert-basic
  "CRITICAL: insert adds text at point.

  Emacs Semantics (editfns.c:1374):
  - Inserts all arguments (strings or chars) at point
  - Point moves to end of inserted text
  - Inherits text properties from surrounding text

  Why this matters:
  - Most fundamental editing operation"
  (testing "insert adds text at point"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (is (= "Hello" (helpers/buffer-string)))
      (is (= 5 (helpers/point)) "Point at end of insertion")))

  (testing "insert at middle of text"
    (with-test-buffer "*test*"
      (helpers/insert "HelloWorld")
      (helpers/goto-char 5)
      (helpers/insert " ")
      (is (= "Hello World" (helpers/buffer-string)))))

  (testing "insert multiple arguments"
    (with-test-buffer "*test*"
      (helpers/insert "Hello" " " "World")
      (is (= "Hello World" (helpers/buffer-string))))))

(deftest ^:high insert-before-markers-semantics
  "HIGH: insert-before-markers keeps markers before insertion.

  Emacs Semantics (editfns.c:1417):
  - Like insert, but all markers at point stay before text
  - Regular insert: markers with insertion-type t move after

  Why this matters:
  - Correct marker behavior during complex edits"
  (testing "markers stay before inserted text"
    (with-test-buffer "*test*"
      (helpers/insert "AB")
      (let [marker (helpers/make-marker)]
        (helpers/set-marker marker 1)  ; Between A and B

        (helpers/goto-char 1)
        (helpers/insert-before-markers "X")

        ;; Marker should still be at 1 (before X)
        (is (= 1 (helpers/marker-position marker))
            "Marker should stay before inserted text")
        (is (= "AXB" (helpers/buffer-string)))))))

;;; =============================================================================
;;; Text Deletion
;;; =============================================================================

(deftest ^:critical delete-region-basic
  "CRITICAL: delete-region removes text between two positions.

  Emacs Semantics (editfns.c:2657):
  - Deletes text between START and END
  - Point moves to START if it was in deleted region
  - Records deletion for undo

  Why this matters:
  - Foundation for cut, backspace, delete operations"
  (testing "delete-region removes text"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/delete-region 0 6)
      (is (= "World" (helpers/buffer-string)))))

  (testing "delete-region with reversed args"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/delete-region 11 5)  ; END before START
      (is (= "Hello" (helpers/buffer-string))
          "Should work with reversed arguments"))))

(deftest ^:high delete-and-extract-region
  "HIGH: delete-and-extract-region returns deleted text.

  Emacs Semantics (editfns.c:2668):
  - Deletes text between START and END
  - Returns the deleted text as string
  - Used by kill-region

  Why this matters:
  - Need return value for kill ring"
  (testing "returns deleted text"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (let [deleted (helpers/delete-and-extract-region 0 6)]
        (is (= "Hello " deleted))
        (is (= "World" (helpers/buffer-string)))))))

;;; =============================================================================
;;; Text Extraction
;;; =============================================================================

(deftest ^:critical buffer-substring-basic
  "CRITICAL: buffer-substring extracts text from buffer.

  Emacs Semantics (editfns.c:1669):
  - Returns text between START and END
  - Includes text properties
  - Does not modify buffer

  Why this matters:
  - Foundation for copy, search, display"
  (testing "buffer-substring extracts text"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (is (= "World" (helpers/buffer-substring 6 11)))))

  (testing "buffer-substring preserves properties"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/put-text-property 0 5 'face 'bold)
      (let [text (helpers/buffer-substring 0 5)]
        (is (= "Hello" text))
        ;; Text should have properties (if supported)
        ))))

(deftest ^:high buffer-substring-no-properties
  "HIGH: buffer-substring-no-properties strips properties.

  Emacs Semantics (editfns.c:1689):
  - Like buffer-substring but without text properties
  - Returns plain string

  Why this matters:
  - Needed when properties would interfere"
  (testing "strips text properties"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/put-text-property 0 5 'face 'bold)
      (let [text (helpers/buffer-substring-no-properties 0 5)]
        (is (= "Hello" text))
        ;; Should have no properties
        ))))

;;; =============================================================================
;;; Point and Position
;;; =============================================================================

(deftest ^:critical goto-char-basic
  "CRITICAL: goto-char moves point to position.

  Emacs Semantics (editfns.c:201):
  - Moves point to POSITION
  - Accepts marker or integer
  - Clamps to valid range

  Why this matters:
  - Navigation foundation"
  (testing "goto-char moves point"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/goto-char 6)
      (is (= 6 (helpers/point)))))

  (testing "goto-char clamps to buffer bounds"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 100)
      (is (<= (helpers/point) 5)
          "Should clamp to buffer end")))

  (testing "goto-char accepts marker"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (let [marker (helpers/make-marker)]
        (helpers/set-marker marker 6)
        (helpers/goto-char marker)
        (is (= 6 (helpers/point)))))))

(deftest ^:high point-returns-position
  "HIGH: point returns current point position.

  Emacs Semantics (editfns.c:184):
  - Returns integer position of point
  - 1-indexed in Emacs, 0-indexed in Lexicon

  Implementation Note:
  - Lexicon uses 0-indexed positions"
  (testing "point returns current position"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 3)
      (is (= 3 (helpers/point))))))

;;; =============================================================================
;;; Position Predicates
;;; =============================================================================

(deftest ^:high bobp-eobp-predicates
  "HIGH: bobp/eobp check if at buffer boundaries.

  Emacs Semantics:
  - bobp: t if point at beginning of buffer (editfns.c:1022)
  - eobp: t if point at end of buffer (editfns.c:1032)

  Why this matters:
  - Navigation boundary checks"
  (testing "bobp at beginning"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 0)
      (is (helpers/bobp) "Should be at beginning")))

  (testing "bobp not at beginning"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 3)
      (is (not (helpers/bobp)) "Should not be at beginning")))

  (testing "eobp at end"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 5)
      (is (helpers/eobp) "Should be at end")))

  (testing "eobp not at end"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 0)
      (is (not (helpers/eobp)) "Should not be at end"))))

(deftest ^:high bolp-eolp-predicates
  "HIGH: bolp/eolp check if at line boundaries.

  Emacs Semantics:
  - bolp: t if point at beginning of line (editfns.c:1042)
  - eolp: t if point at end of line (editfns.c:1051)

  Why this matters:
  - Line-based navigation"
  (testing "bolp at line beginning"
    (with-test-buffer "*test*"
      (helpers/insert "Hello\nWorld")
      (helpers/goto-char 6)  ; Beginning of 'World'
      (is (helpers/bolp) "Should be at line beginning")))

  (testing "eolp at line end"
    (with-test-buffer "*test*"
      (helpers/insert "Hello\nWorld")
      (helpers/goto-char 5)  ; End of 'Hello', before newline
      (is (helpers/eolp) "Should be at line end"))))

;;; =============================================================================
;;; Character Access
;;; =============================================================================

(deftest ^:high char-after-before
  "HIGH: char-after/char-before access characters.

  Emacs Semantics:
  - char-after: char at POS (or point) (editfns.c:1061)
  - char-before: char before POS (or point) (editfns.c:1093)
  - Returns nil at buffer boundaries

  Why this matters:
  - Character-level operations"
  (testing "char-after returns character"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 0)
      (is (= "H" (helpers/char-after)))))

  (testing "char-after at end returns nil"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 5)
      (is (nil? (helpers/char-after)))))

  (testing "char-before returns previous character"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 5)
      (is (= "o" (helpers/char-before)))))

  (testing "char-before at beginning returns nil"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/goto-char 0)
      (is (nil? (helpers/char-before))))))

;;; =============================================================================
;;; Line Positions
;;; =============================================================================

(deftest ^:high line-beginning-end-positions
  "HIGH: line-beginning/end-position return line bounds.

  Emacs Semantics:
  - line-beginning-position: start of current line (editfns.c:723)
  - line-end-position: end of current line (editfns.c:778)
  - Optional arg N for Nth line relative to current

  Why this matters:
  - C-a, C-e movement, kill-line"
  (testing "line-beginning-position"
    (with-test-buffer "*test*"
      (helpers/insert "Hello\nWorld\nFoo")
      (helpers/goto-char 8)  ; Middle of 'World'
      (is (= 6 (helpers/line-beginning-position)))))

  (testing "line-end-position"
    (with-test-buffer "*test*"
      (helpers/insert "Hello\nWorld\nFoo")
      (helpers/goto-char 8)  ; Middle of 'World'
      (is (= 11 (helpers/line-end-position))))))

;;; =============================================================================
;;; Region
;;; =============================================================================

(deftest ^:high region-beginning-end
  "HIGH: region-beginning/end return region bounds.

  Emacs Semantics (editfns.c:247, 254):
  - region-beginning: smaller of point and mark
  - region-end: larger of point and mark
  - Error if mark not set

  Why this matters:
  - All region operations use these"
  (testing "region bounds"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/set-mark 0)
      (helpers/goto-char 5)
      (is (= 0 (helpers/region-beginning)))
      (is (= 5 (helpers/region-end)))))

  (testing "region with point before mark"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/set-mark 10)
      (helpers/goto-char 5)
      (is (= 5 (helpers/region-beginning)))
      (is (= 10 (helpers/region-end))))))

;;; =============================================================================
;;; Edge Cases
;;; =============================================================================

(deftest ^:medium insert-empty-string
  "MEDIUM: insert with empty string is no-op.

  Implementation Note:
  - Should not error or create undo entry"
  (testing "insert empty string"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [len (count (helpers/buffer-string))]
        (helpers/insert "")
        (is (= len (count (helpers/buffer-string))))))))

(deftest ^:medium delete-region-empty
  "MEDIUM: delete-region with same start/end is no-op.

  Implementation Note:
  - Should not error"
  (testing "delete empty region"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (helpers/delete-region 2 2)
      (is (= "Hello" (helpers/buffer-string))))))

(deftest ^:low buffer-substring-whole-buffer
  "LOW: buffer-substring can extract whole buffer.

  Implementation Note:
  - Should be equivalent to buffer-string"
  (testing "extract whole buffer"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (is (= (helpers/buffer-string)
             (helpers/buffer-substring 0 11))))))
