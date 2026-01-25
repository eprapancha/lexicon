(ns lexicon.semantic.markers-test
  "Semantic tests for markers - CORE primitive for position tracking.

  Markers are position references that automatically move with buffer edits.
  Used by: window-point, undo system, multi-window editing, region tracking.

  Related: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md
  Priority: CRITICAL - required for multi-window reliability"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; Markers - Core Primitive Tests
;;; =============================================================================

(deftest ^:critical markers-track-insertions
  "CRITICAL: Markers move when text inserted before them.

  Semantic Guarantee:
  - Marker at position N
  - Insert M characters before marker
  - Marker now at position N+M

  Why this matters:
  - Window-point must stay on same logical position
  - Undo system tracks positions across edits
  - Multi-buffer editing requires stable references

  Implementation Note:
  - Markers stored in :markers map in db
  - WASM gap buffer notifies on insert/delete
  - Markers adjust positions automatically
  - NOT implemented yet"
  (testing "Marker advances with insertion before it"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)  ; After "Hello"
        (is (= 5 (helpers/marker-position m)))

        ;; Insert before the marker
        (helpers/goto-char 0)
        (helpers/insert "XXX")

        ;; Marker should move forward by 3
        (is (= 8 (helpers/marker-position m))
            "Marker shifted by insertion length"))))

  (testing "Multiple markers all shift correctly"
    (with-test-buffer "*test*"
      (helpers/insert "ABCDEFGH")
      (let [m1 (helpers/make-marker)
            m2 (helpers/make-marker)
            m3 (helpers/make-marker)]
        (helpers/set-marker m1 2)  ; After "AB"
        (helpers/set-marker m2 5)  ; After "ABCDE"
        (helpers/set-marker m3 8)  ; At end

        ;; Insert at beginning
        (helpers/goto-char 0)
        (helpers/insert "XX")

        (is (= 4 (helpers/marker-position m1)))
        (is (= 7 (helpers/marker-position m2)))
        (is (= 10 (helpers/marker-position m3)))))))

(deftest ^:critical markers-survive-deletions
  "CRITICAL: Markers adjust when surrounding text deleted.

  Semantic Guarantee:
  - Marker before deleted region: unchanged
  - Marker within deleted region: moves to deletion start
  - Marker after deleted region: shifts back by deletion length

  Why this matters:
  - Window-point doesn't become invalid
  - Undo positions remain valid
  - Regions track correctly during edits

  Implementation Note:
  - Deletion notifies markers to adjust
  - Marker within deletion clamped to start
  - NOT implemented yet"
  (testing "Marker before deletion unaffected"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)  ; After "Hello"

        ;; Delete "World"
        (helpers/delete-region 6 11)

        ;; Marker should stay at 5
        (is (= 5 (helpers/marker-position m))
            "Marker before deletion unchanged"))))

  (testing "Marker within deletion moves to start"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 8)  ; Middle of "World"

        ;; Delete "World"
        (helpers/delete-region 6 11)

        ;; Marker should move to deletion start
        (is (= 6 (helpers/marker-position m))
            "Marker within deletion clamped to start"))))

  (testing "Marker after deletion shifts back"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World!")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 12)  ; After "!"

        ;; Delete "World"
        (helpers/delete-region 6 11)

        ;; Marker should shift back by 5
        (is (= 7 (helpers/marker-position m))
            "Marker after deletion shifted back")))))

(deftest ^:high markers-buffer-association
  "HIGH: Markers are associated with a specific buffer.

  Semantic Guarantee:
  - Marker knows which buffer it belongs to
  - Marker position is only valid in that buffer
  - Marker in killed buffer becomes invalid

  Why this matters:
  - Can't accidentally use marker from wrong buffer
  - Multi-buffer editing is safe
  - Cleanup when buffers are killed

  Implementation Note:
  - Marker stores :buffer-id
  - marker-buffer returns buffer
  - Buffer kill clears its markers
  - NOT implemented yet"
  (testing "Marker remembers its buffer"
    (with-test-buffer "*buf1*"
      (helpers/insert "Buffer 1")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)

        ;; Switch to different buffer
        (with-test-buffer "*buf2*"
          (helpers/insert "Buffer 2")

          ;; Marker should still point to buf1
          (is (= "*buf1*" (helpers/buffer-name (helpers/marker-buffer m)))
              "Marker associated with original buffer")))))

  (testing "Marker in killed buffer becomes nil"
    (with-test-buffer "*temp*"
      (helpers/insert "Temporary")
      (let [m (helpers/make-marker)
            buf-id (helpers/current-buffer-id)]
        (helpers/set-marker m 5)

        ;; Kill the buffer
        (helpers/kill-buffer buf-id)

        ;; Marker should be invalid
        (is (nil? (helpers/marker-buffer m))
            "Marker buffer is nil after kill")))))

(deftest ^:high markers-copy-marker
  "HIGH: copy-marker creates independent duplicate.

  Semantic Guarantee:
  - (copy-marker m) creates new marker at same position
  - Original and copy are independent
  - Moving one doesn't affect the other

  Why this matters:
  - Save position before complex operation
  - Multiple markers tracking same logical position
  - Temporary position references

  Implementation Note:
  - Creates new marker with same buffer and position
  - NOT implemented yet"
  (testing "Copy creates independent marker"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m1 (helpers/make-marker)]
        (helpers/set-marker m1 5)
        (let [m2 (helpers/copy-marker m1)]

          ;; Both at same position initially
          (is (= 5 (helpers/marker-position m1)))
          (is (= 5 (helpers/marker-position m2)))

          ;; Move m2
          (helpers/set-marker m2 3)

          ;; m1 should be unchanged
          (is (= 5 (helpers/marker-position m1))
              "Original marker unaffected")
          (is (= 3 (helpers/marker-position m2))
              "Copy marker moved independently"))))))

(deftest ^:high markers-move-marker
  "HIGH: move-marker repositions existing marker.

  Semantic Guarantee:
  - (move-marker m pos) changes marker position
  - Can move marker to different buffer
  - Returns the marker

  Why this matters:
  - Reuse marker objects
  - Change buffer association
  - Update tracked positions

  Implementation Note:
  - Updates marker's :buffer-id and :position
  - NOT implemented yet"
  (testing "Move marker to new position"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)

        ;; Move the marker
        (helpers/move-marker m 8)

        (is (= 8 (helpers/marker-position m))
            "Marker moved to new position"))))

  (testing "Move marker to different buffer"
    (with-test-buffer "*buf1*"
      (helpers/insert "Buffer 1")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)

        (with-test-buffer "*buf2*"
          (helpers/insert "Buffer 2")
          (let [buf2-id (helpers/current-buffer-id)]

            ;; Move marker to this buffer
            (helpers/move-marker m 3 buf2-id)

            (is (= 3 (helpers/marker-position m)))
            (is (= "*buf2*" (helpers/buffer-name (helpers/marker-buffer m)))
                "Marker now in different buffer")))))))

(deftest ^:medium markers-at-point-min-max
  "MEDIUM: Markers work at buffer boundaries.

  Semantic Guarantee:
  - Marker can be at position 0 (point-min)
  - Marker can be at end of buffer (point-max)
  - Boundary markers adjust correctly

  Why this matters:
  - Beginning/end of buffer are common positions
  - Region from point to end uses marker at point-max

  Implementation Note:
  - Position 0 and buffer-length are valid
  - NOT implemented yet"
  (testing "Marker at beginning of buffer"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 0)

        ;; Insert at beginning
        (helpers/goto-char 0)
        (helpers/insert "XXX")

        ;; Marker should move
        (is (= 3 (helpers/marker-position m))
            "Marker at start moved by insertion"))))

  (testing "Marker at end of buffer"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)  ; At end

        ;; Append text
        (helpers/goto-char 5)
        (helpers/insert " World")

        ;; Marker should stay at original end (depending on insertion mode)
        ;; In Emacs, markers at point-max don't advance
        (is (or (= 5 (helpers/marker-position m))
                (= 11 (helpers/marker-position m)))
            "Marker at end behaves consistently")))))

;;; =============================================================================
;;; Integration with Core Systems
;;; =============================================================================

(deftest ^:critical markers-with-undo
  "CRITICAL: Markers work correctly with undo/redo.

  Semantic Guarantee:
  - Undo restores marker positions
  - Redo restores markers again
  - Marker moves tracked through undo history

  Why this matters:
  - Window-point must survive undo
  - Region tracking must survive undo
  - Critical for reliable editing

  Implementation Note:
  - Undo system tracks marker state
  - Each undo entry includes marker adjustments
  - NOT implemented yet"
  (testing "Marker position restored on undo"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)

        ;; Insert text that moves marker
        (helpers/goto-char 0)
        (helpers/insert "XXX")
        (is (= 8 (helpers/marker-position m)))

        ;; Undo the insertion
        (helpers/undo)

        ;; Marker should be back at original position
        (is (= 5 (helpers/marker-position m))
            "Marker position restored by undo")))))

(deftest ^:critical markers-window-point
  "CRITICAL: Window-point implemented as marker.

  Semantic Guarantee:
  - Each window has window-point as a marker
  - Window-point moves with buffer edits
  - Multiple windows can have different points in same buffer

  Why this matters:
  - This is THE use case for markers
  - Multi-window editing requires stable positions
  - Dired refresh must preserve window positions

  Implementation Note:
  - :window-point in window record is marker, not integer
  - Buffer edits notify markers to adjust
  - NOT implemented yet - window-point probably just integer currently"
  (testing "Window-point survives buffer edits"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")

      ;; Set point to middle
      (helpers/goto-char 6)
      (let [initial-pos 6]

        ;; Insert before point
        (helpers/goto-char 0)
        (helpers/insert "XXX")

        ;; Window point should have moved
        (let [current-pos (helpers/point)]
          (is (= (+ initial-pos 3) current-pos)
              "Point moved with insertion"))))))

(deftest ^:high markers-with-multiple-windows
  "HIGH: Different windows have independent markers in same buffer.

  Semantic Guarantee:
  - Window A shows buffer at position X
  - Window B shows same buffer at position Y
  - Edits in one window update markers correctly in both

  Why this matters:
  - Split windows on same buffer
  - Dired with multiple views of same directory
  - Independent navigation in same content

  Implementation Note:
  - Each window stores its own window-point marker
  - All markers for a buffer update together
  - NOT implemented yet - needs window-point as marker"
  (testing "Two windows, same buffer, different markers"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")

      ;; Split window
      (let [win1 (helpers/current-window)
            win2 (helpers/split-window)]

        ;; Position windows differently
        (helpers/select-window win1)
        (helpers/goto-char 0)

        (helpers/select-window win2)
        (helpers/goto-char 6)

        ;; Insert at beginning
        (helpers/goto-char 0)
        (helpers/insert "XXX")

        ;; Both windows should adjust their points
        (helpers/select-window win1)
        (is (= 3 (helpers/point)) "Win1 point adjusted")

        (helpers/select-window win2)
        (is (= 9 (helpers/point)) "Win2 point adjusted")))))

;;; =============================================================================
;;; Marker Cleanup and Memory Management
;;; =============================================================================

(deftest ^:medium markers-gc
  "MEDIUM: Markers can be garbage collected or explicitly freed.

  Semantic Guarantee:
  - Unused markers don't leak memory
  - Can explicitly free marker with (set-marker m nil)
  - Freed marker returns nil for position

  Why this matters:
  - Long-lived buffers with many temp markers
  - Memory efficiency
  - Clean up after operations

  Implementation Note:
  - Markers stored in atom/map
  - set-marker to nil removes from map
  - NOT implemented yet"
  (testing "Set marker to nil frees it"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)
        (is (= 5 (helpers/marker-position m)))

        ;; Free the marker
        (helpers/set-marker m nil)

        ;; Marker should be invalid
        (is (nil? (helpers/marker-position m))
            "Freed marker returns nil")))))

;;; =============================================================================
;;; Insertion Type Tests (Emacs marker-insertion-type)
;;; =============================================================================

(deftest ^:critical markers-insertion-type-nil
  "CRITICAL: Marker with insertion-type nil stays BEFORE inserted text.

  Emacs Semantics (from marker.c):
  - (set-marker-insertion-type m nil)
  - Insert at marker position
  - Marker does NOT advance, text appears AFTER marker

  Why this matters:
  - save-excursion uses this to restore point correctly
  - Region bounds need predictable behavior
  - Undo position tracking

  Implementation Note:
  - WASM marker needs insertion_type field
  - update_markers_on_insert must check insertion_type
  - Default should be nil (don't advance)"
  (testing "Marker with insertion-type=nil stays before insertion"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)
        (helpers/set-marker-insertion-type m nil)

        ;; Insert at marker position
        (helpers/goto-char 5)
        (helpers/insert "XXX")

        ;; Marker should NOT have moved
        (is (= 5 (helpers/marker-position m))
            "Marker with insertion-type=nil stays at 5")
        (is (= "HelloXXX" (helpers/buffer-string))
            "Text inserted after marker")))))

(deftest ^:critical markers-insertion-type-t
  "CRITICAL: Marker with insertion-type t advances PAST inserted text.

  Emacs Semantics:
  - (set-marker-insertion-type m t)
  - Insert at marker position
  - Marker advances, text appears BEFORE marker

  Why this matters:
  - Some markers need to track 'end of region'
  - Process output markers should advance
  - Overlay end markers typically use this"
  (testing "Marker with insertion-type=t advances past insertion"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)
        (helpers/set-marker-insertion-type m true)

        ;; Insert at marker position
        (helpers/goto-char 5)
        (helpers/insert "XXX")

        ;; Marker should have advanced
        (is (= 8 (helpers/marker-position m))
            "Marker with insertion-type=t advances to 8")))))

(deftest ^:high markers-insertion-type-query
  "HIGH: Can query marker's insertion type.

  Emacs API:
  - (marker-insertion-type m) -> t or nil

  Implementation Note:
  - Store insertion_type in marker metadata
  - Default is nil"
  (testing "Query insertion type returns correct value"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)

        ;; Default should be nil
        (is (nil? (helpers/marker-insertion-type m))
            "Default insertion-type is nil")

        ;; Set to t
        (helpers/set-marker-insertion-type m true)
        (is (true? (helpers/marker-insertion-type m))
            "After setting to t, returns t")

        ;; Set back to nil
        (helpers/set-marker-insertion-type m nil)
        (is (nil? (helpers/marker-insertion-type m))
            "After setting to nil, returns nil")))))

;;; =============================================================================
;;; insert-before-markers Tests
;;; =============================================================================

(deftest ^:high insert-before-markers
  "HIGH: insert-before-markers makes ALL markers stay before insertion.

  Emacs Semantics:
  - Ignores individual marker insertion-types
  - All markers at insertion point do NOT advance
  - Used for special cases like shell output

  Why this matters:
  - Shell/process output insertion
  - Maintaining cursor stability during programmatic inserts"
  (testing "insert-before-markers keeps all markers before insertion"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m1 (helpers/make-marker)
            m2 (helpers/make-marker)]
        ;; Set up markers with different insertion types
        (helpers/set-marker m1 5)
        (helpers/set-marker-insertion-type m1 nil)
        (helpers/set-marker m2 5)
        (helpers/set-marker-insertion-type m2 true)

        ;; Use insert-before-markers
        (helpers/goto-char 5)
        (helpers/insert-before-markers "XXX")

        ;; BOTH markers should stay at 5
        (is (= 5 (helpers/marker-position m1))
            "m1 (insertion-type=nil) stays at 5")
        (is (= 5 (helpers/marker-position m2))
            "m2 (insertion-type=t) also stays at 5 with insert-before-markers")))))

;;; =============================================================================
;;; Type Predicates
;;; =============================================================================

(deftest ^:medium markerp-predicate
  "MEDIUM: markerp tests if object is a marker.

  Emacs API:
  - (markerp obj) -> t if marker, nil otherwise

  Implementation Note:
  - Check object type/metadata"
  (testing "markerp identifies markers correctly"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m (helpers/make-marker)]
        (helpers/set-marker m 5)

        (is (helpers/markerp m) "Marker returns true")
        (is (not (helpers/markerp 5)) "Number returns false")
        (is (not (helpers/markerp "hello")) "String returns false")
        (is (not (helpers/markerp nil)) "nil returns false")))))

;;; =============================================================================
;;; Copy Marker with Insertion Type
;;; =============================================================================

(deftest ^:high copy-marker-preserves-insertion-type
  "HIGH: copy-marker can optionally set insertion type on copy.

  Emacs API:
  - (copy-marker m) - same insertion-type as original
  - (copy-marker m t) - force insertion-type to t
  - (copy-marker m nil) - force insertion-type to nil

  Implementation Note:
  - copy-marker takes optional TYPE argument"
  (testing "copy-marker preserves insertion type by default"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m1 (helpers/make-marker)]
        (helpers/set-marker m1 5)
        (helpers/set-marker-insertion-type m1 true)

        (let [m2 (helpers/copy-marker m1)]
          (is (= 5 (helpers/marker-position m2)))
          (is (true? (helpers/marker-insertion-type m2))
              "Copy preserves insertion-type")))))

  (testing "copy-marker can override insertion type"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m1 (helpers/make-marker)]
        (helpers/set-marker m1 5)
        (helpers/set-marker-insertion-type m1 true)

        ;; Copy with explicit nil insertion-type
        (let [m2 (helpers/copy-marker m1 nil)]
          (is (= 5 (helpers/marker-position m2)))
          (is (nil? (helpers/marker-insertion-type m2))
              "Copy has overridden insertion-type"))))))

;;; =============================================================================
;;; Edge Cases from Emacs Source
;;; =============================================================================

(deftest ^:medium set-marker-with-marker-arg
  "MEDIUM: set-marker can take another marker as POSITION argument.

  Emacs API:
  - (set-marker m1 m2) - m1 gets same position as m2
  - Also inherits buffer if m2 is in different buffer

  Implementation Note:
  - Check if position arg is marker, extract its position/buffer"
  (testing "set-marker with marker argument copies position"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m1 (helpers/make-marker)
            m2 (helpers/make-marker)]
        (helpers/set-marker m1 5)
        (helpers/set-marker m2 m1)  ; Use m1 as position

        (is (= 5 (helpers/marker-position m2))
            "m2 copied position from m1")))))

(deftest ^:medium marker-position-clamping
  "MEDIUM: Marker positions are clamped to valid buffer range.

  Emacs Behavior:
  - Position < 0 becomes 0 (point-min)
  - Position > buffer-length becomes buffer-length (point-max)

  Implementation Note:
  - set_marker_internal in marker.c uses clip_to_bounds"
  (testing "Position clamped to buffer bounds"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")  ; 5 chars
      (let [m (helpers/make-marker)]
        ;; Try to set beyond buffer end
        (helpers/set-marker m 100)
        (is (= 5 (helpers/marker-position m))
            "Position clamped to buffer end")

        ;; Try to set before buffer start (negative)
        (helpers/set-marker m -10)
        (is (= 0 (helpers/marker-position m))
            "Position clamped to buffer start")))))

(deftest ^:low marker-equality
  "LOW: Marker equality semantics.

  Emacs Behavior:
  - (eq m1 m2) - false unless same object
  - (= m1 m2) - true if same position and buffer
  - (equal m1 m2) - same as =

  Implementation Note:
  - Markers are reference types with value equality on position"
  (testing "Marker equality"
    (with-test-buffer "*test*"
      (helpers/insert "Hello")
      (let [m1 (helpers/make-marker)
            m2 (helpers/make-marker)]
        (helpers/set-marker m1 5)
        (helpers/set-marker m2 5)

        ;; Same position, different objects
        (is (not (identical? m1 m2))
            "Different marker objects")
        (is (= (helpers/marker-position m1) (helpers/marker-position m2))
            "Same position")))))
