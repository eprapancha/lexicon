(ns lexicon.ui.editing.save-excursion-test
  "E2E tests for save-excursion behavior - user-visible cursor restoration.

  Note: save-excursion is a Lisp macro, not a keyboard command.
  E2E tests focus on user-visible cursor behavior.
  API-specific tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Cursor Restoration - User-Visible Behavior
;; =============================================================================

(deftest test-cursor-position-preserved-after-search
  (testing "Cursor returns to original position after failed search"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Move to position 6
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (dotimes [_ 6]
      (h/press-ctrl "f")
      (Thread/sleep 20))
    (Thread/sleep 50)
    (let [original-pt (h/get-point*)]
      ;; Start incremental search
      (h/press-ctrl "s")
      (Thread/sleep 100)

      ;; Search for something that doesn't exist
      (h/type-text "ZZZZZ")
      (Thread/sleep 100)

      ;; Cancel search with C-g (should restore position)
      (h/press-ctrl "g")
      (Thread/sleep 100)

      (is (= original-pt (h/get-point*))
          "Cursor restored after cancelled search"))))

(deftest test-cursor-preserved-after-mark-operations
  (testing "Cursor position preserved after mark-related commands"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "ABCDEFGHIJ")
    (Thread/sleep 100)

    ;; Move to position 5
    (h/press-ctrl "a")
    (Thread/sleep 30)
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

    ;; Exchange point and mark (C-x C-x) - two separate key presses
    (h/press-ctrl "x")
    (Thread/sleep 30)
    (h/press-ctrl "x")
    (Thread/sleep 100)

    ;; Should be back at position 5
    (is (= 5 (h/get-point*))
        "C-x C-x exchanges point and mark")))

;; =============================================================================
;; Save-Excursion Lisp API Tests
;; =============================================================================
;;
;; Note: save-excursion is a Lisp macro. Tests for the Lisp API are in
;; lexicon.lisp.editing-test (test-save-excursion-* tests).
;; This file focuses on user-visible keyboard behavior that demonstrates
;; similar cursor restoration principles.
