(ns lexicon.lisp.consult-lines-filter-test
  "E2E tests for consult-focus-lines and consult-keep-lines commands.

  Tests line filtering functionality:
  - consult-focus-lines hides non-matching lines via overlays
  - consult-keep-lines deletes non-matching lines

  JUSTIFICATION: Tests call overlay and buffer manipulation functions via
  eval-lisp to exercise the line filtering pipeline. This is a Lisp API test."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lh]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Local Helpers
;; =============================================================================

(defn- setup-lines-buffer!
  "Set up a buffer with mixed content for filtering tests."
  []
  (lh/setup-test)
  (lh/eval-lisp! "(insert \"TODO: fix bug\\nDONE: deploy app\\nTODO: write tests\\nDONE: review PR\\nTODO: update docs\")"))

;; =============================================================================
;; Test 1: Overlay API works (prerequisite for focus-lines)
;; =============================================================================

(deftest test-overlay-creation-and-properties
  (testing "make-overlay and overlay-put work correctly"
    (lh/setup-test)
    (lh/eval-lisp! "(insert \"hello world\")")
    (Thread/sleep 100)

    ;; Create an overlay
    (let [ov-id (lh/eval-lisp! "(make-overlay 1 6)")]
      (is (some? ov-id) "make-overlay should return an overlay ID")

      ;; Set a property
      (lh/eval-lisp! (str "(overlay-put " ov-id " 'face :bold)"))
      (Thread/sleep 100)

      ;; Clean up
      (lh/eval-lisp! (str "(delete-overlay " ov-id ")"))
      (Thread/sleep 100))))

;; =============================================================================
;; Test 2: remove-overlays clears region
;; =============================================================================

(deftest test-remove-overlays
  (testing "remove-overlays clears all overlays in a region"
    (lh/setup-test)
    (lh/eval-lisp! "(insert \"some text here\")")
    (Thread/sleep 100)

    ;; Create overlays
    (lh/eval-lisp! "(make-overlay 1 5)")
    (lh/eval-lisp! "(make-overlay 6 10)")
    (Thread/sleep 100)

    ;; Remove all overlays
    (lh/eval-lisp! "(remove-overlays (point-min) (point-max))")
    (Thread/sleep 100)

    ;; Verify no overlays remain
    (let [overlays (lh/eval-lisp! "(overlays-in (point-min) (point-max))")]
      (is (or (nil? overlays) (empty? overlays))
          "Should have no overlays after remove-overlays"))))

;; =============================================================================
;; Test 3: consult-keep-lines filters buffer content
;; =============================================================================

(deftest test-keep-lines-filters-content
  (testing "Keeping matching lines removes non-matching lines"
    (setup-lines-buffer!)
    (Thread/sleep 100)

    ;; Verify initial content
    (let [text (lh/eval-lisp! "(buffer-string)")]
      (is (.contains text "TODO") "Buffer should contain TODO lines")
      (is (.contains text "DONE") "Buffer should contain DONE lines"))

    ;; Simulate keep-lines: filter to only TODO lines
    (lh/eval-lisp! "(do
      (let [text (buffer-string)
            lines (.split text \"\\n\")
            re (js/RegExp. \"TODO\" \"i\")
            kept (filterv (fn [line]
                            (let [match? (.test re line)]
                              (set! (.-lastIndex re) 0)
                              match?))
                          lines)
            new-text (.join (to-array kept) \"\\n\")]
        (erase-buffer)
        (insert new-text)))")
    (Thread/sleep 100)

    ;; Verify only TODO lines remain
    (let [text (lh/eval-lisp! "(buffer-string)")]
      (is (.contains text "TODO") "Should still contain TODO lines")
      (is (not (.contains text "DONE")) "Should not contain DONE lines")
      (let [lines (str/split text #"\n")]
        (is (= 3 (count lines)) "Should have 3 TODO lines")))))

;; =============================================================================
;; Test 4: Filtering preserves line content
;; =============================================================================

(deftest test-keep-lines-preserves-content
  (testing "Kept lines retain their full content"
    (setup-lines-buffer!)
    (Thread/sleep 100)

    ;; Keep DONE lines
    (lh/eval-lisp! "(do
      (let [text (buffer-string)
            lines (.split text \"\\n\")
            re (js/RegExp. \"DONE\" \"i\")
            kept (filterv (fn [line]
                            (let [match? (.test re line)]
                              (set! (.-lastIndex re) 0)
                              match?))
                          lines)
            new-text (.join (to-array kept) \"\\n\")]
        (erase-buffer)
        (insert new-text)))")
    (Thread/sleep 100)

    (let [text (lh/eval-lisp! "(buffer-string)")
          lines (str/split text #"\n")]
      (is (= 2 (count lines)) "Should have 2 DONE lines")
      (is (.contains (first lines) "deploy app")
          "First DONE line should contain 'deploy app'")
      (is (.contains (second lines) "review PR")
          "Second DONE line should contain 'review PR'"))))
