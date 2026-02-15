(ns lexicon.ui.editing.markers-test
  "E2E tests for mark (region anchor) behavior via keyboard.

  The mark is the Emacs concept of a position that defines a region with point.
  These tests verify user-visible behavior via keyboard simulation.

  For Lisp API marker tests (make-marker, marker-position, etc.),
  see lexicon.lisp.editing-test."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Mark (Region Anchor) - User-Visible Effects via Keyboard
;; =============================================================================

;; NOTE: test-mark-and-region removed - duplicate of
;; kill_ring_test.clj::test-kill-region-deletes-selection which tests
;; mark + region + kill with additional kill ring verification

(deftest test-mark-survives-cursor-movement
  (testing "Mark position is preserved when cursor moves"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type text
    (h/type-text "ABCDEFGHIJ")
    (Thread/sleep 100)

    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; Move to position 5
    (dotimes [_ 5]
      (h/press-ctrl "f")
      (Thread/sleep 20))
    (Thread/sleep 50)

    ;; Set mark
    (h/set-mark)
    (Thread/sleep 50)

    ;; Move to end
    (h/press-ctrl "e")
    (Thread/sleep 50)

    ;; Exchange point and mark (C-x C-x)
    (h/press-ctrl "x")
    (Thread/sleep 30)
    (h/press-ctrl "x")
    (Thread/sleep 100)

    ;; Should be back at position 5 (where mark was set)
    (is (= 5 (h/get-point*))
        "Exchange point and mark should return to mark position")))
