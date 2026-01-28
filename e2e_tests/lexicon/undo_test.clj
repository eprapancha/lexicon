(ns lexicon.undo-test
  "E2E tests for undo system - core editing primitive (Issue #103).

  Tests USER undo operations via keyboard:
  - C-/ or C-/ for undo
  - Undo reverses text insertion
  - Undo reverses text deletion
  - Multiple undos work in sequence

  Related: Issue #103 (Undo System), Issue #94 (TDD)
  Priority: CRITICAL - required for safe editing"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Basic Undo Operations (Keyboard-driven)
;; =============================================================================

(deftest test-undo-reverses-insert
  (testing "C-/ removes inserted text"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello")
    (Thread/sleep 100)
    (is (= "Hello" (h/get-buffer-text*)) "Text should be inserted")

    ;; User presses C-/ to undo (Emacs keybinding)
    (h/press-ctrl "/")
    (Thread/sleep 100)

    (is (= "" (h/get-buffer-text*))
        "Inserted text should be removed by undo")))

(deftest test-undo-reverses-delete
  (testing "C-/ restores single deleted character"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello")
    (Thread/sleep 200)

    ;; User deletes ONE character with backspace
    (h/press-key "Backspace")
    (Thread/sleep 200)
    (is (= "Hell" (h/get-buffer-text*)) "One character should be deleted")

    ;; User presses C-/ to undo
    (h/press-ctrl "/")
    (Thread/sleep 200)

    (is (= "Hello" (h/get-buffer-text*))
        "Deleted character should be restored by undo")))

(deftest test-multiple-edits-undo-together
  (testing "Rapid edits undo as a group"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types rapidly (no pause between characters)
    (h/type-text "ABC")
    (Thread/sleep 200)  ; Pause creates implicit boundary

    ;; Single undo should remove the group
    (h/press-ctrl "/")
    (Thread/sleep 100)

    (is (= "" (h/get-buffer-text*))
        "Rapid edits should undo together")))

(deftest test-undo-restores-point
  (testing "Undo restores cursor position"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello")
    (Thread/sleep 100)

    ;; Move to position 2
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (h/press-ctrl "f")
    (h/press-ctrl "f")
    (Thread/sleep 50)
    (let [pt-before (h/get-point*)]
      ;; Insert more text
      (h/type-text " World")
      (Thread/sleep 100)

      ;; Undo
      (h/press-ctrl "/")
      (Thread/sleep 100)

      (is (= pt-before (h/get-point*))
          "Point should be restored after undo"))))

;; =============================================================================
;; Consecutive Undo
;; =============================================================================

(deftest test-consecutive-undo-continues-chain
  (testing "Multiple C-/ undos multiple changes"
    (h/setup-test*)
    (h/clear-buffer)

    ;; First edit
    (h/type-text "A")
    (Thread/sleep 200)  ; Pause for boundary

    ;; Second edit
    (h/type-text "B")
    (Thread/sleep 200)  ; Pause for boundary

    ;; Third edit
    (h/type-text "C")
    (Thread/sleep 200)

    (is (= "ABC" (h/get-buffer-text*)) "All text should be present")

    ;; Undo three times
    (h/press-ctrl "/")
    (Thread/sleep 100)
    (h/press-ctrl "/")
    (Thread/sleep 100)
    (h/press-ctrl "/")
    (Thread/sleep 100)

    (is (= "" (h/get-buffer-text*))
        "Consecutive undos should remove all insertions")))

;; =============================================================================
;; Redo (Undo the Undo)
;; =============================================================================

(deftest ^:skip test-redo-restores-undone-change
  (testing "Redo after undo restores text"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello")
    (Thread/sleep 100)

    ;; Undo
    (h/press-ctrl "/")
    (Thread/sleep 100)
    (is (= "" (h/get-buffer-text*)) "Text should be undone")

    ;; Redo (C-g C-/ or C-/ in some Emacs configs)
    ;; In Emacs, after undo, another undo undoes the undo (redo)
    ;; Or use C-g to break undo chain then undo again
    (h/press-ctrl "g")  ; Break undo chain
    (Thread/sleep 50)
    (h/press-ctrl "/")  ; Undo the undo = redo
    (Thread/sleep 100)

    (is (= "Hello" (h/get-buffer-text*))
        "Redo should restore undone change")))

(deftest ^:skip test-new-edit-clears-redo
  (testing "Edit after undo prevents redo"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello")
    (Thread/sleep 100)

    ;; Undo
    (h/press-ctrl "/")
    (Thread/sleep 100)
    (is (= "" (h/get-buffer-text*)))

    ;; New edit instead of redo
    (h/type-text "Goodbye")
    (Thread/sleep 100)

    (is (= "Goodbye" (h/get-buffer-text*))
        "New edit should replace undone content")))

;; =============================================================================
;; Undo Recording Control (API tests - placeholders)
;; =============================================================================

(deftest ^:skip test-buffer-disable-undo-for-buffer
  (testing "buffer-disable-undo stops recording - placeholder"
    ;; This tests internal API behavior
    ;; E2E tests cannot toggle undo recording via keyboard
    (is true "PENDING: Undo disable - needs E2E implementation")))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest ^:skip test-undo-empty-buffer-noop
  (testing "Undo on empty buffer is safe"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Should not error
    (h/press-ctrl "/")
    (Thread/sleep 100)

    (is (= "" (h/get-buffer-text*))
        "Undo on empty buffer should be no-op")))

(deftest ^:skip test-undo-preserves-markers
  (testing "Markers adjusted during undo - placeholder"
    ;; Marker manipulation requires internal API
    ;; E2E tests verify user-visible behavior
    (is true "PENDING: Marker preservation - needs E2E implementation")))

;; =============================================================================
;; Kill and Yank with Undo
;; =============================================================================

(deftest test-undo-kill-line
  (testing "Undo restores killed line"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Go to beginning and kill line
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (h/press-ctrl "k")
    (Thread/sleep 100)
    (is (= "" (h/get-buffer-text*)) "Line should be killed")

    ;; Undo
    (h/press-ctrl "/")
    (Thread/sleep 100)

    (is (= "Hello World" (h/get-buffer-text*))
        "Undo should restore killed line")))
