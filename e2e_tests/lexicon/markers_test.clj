(ns lexicon.markers-test
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

(deftest test-mark-and-region
  (testing "C-space sets mark for region selection"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; Set mark with C-space (mark command, not marker API)
    (h/set-mark)
    (Thread/sleep 50)

    ;; Move forward to select "Hello"
    (dotimes [_ 5]
      (h/press-ctrl "f")
      (Thread/sleep 20))
    (Thread/sleep 50)

    ;; Region should be active - verify by killing
    ;; Note: C-w is blocked by browser (close tab), use M-x instead
    (h/execute-command "kill-region")
    (Thread/sleep 100)

    (is (= " World" (h/get-buffer-text*))
        "C-space and movement should create region that can be killed")))

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
