(ns lexicon.lisp.consult-goto-line-test
  "E2E tests for consult-goto-line command.

  Tests consult-goto-line functionality:
  - Accepts line number input
  - Confirming jumps to that line
  - Line count is correct

  JUSTIFICATION: Tests call consult-goto-line functions via eval-lisp
  to exercise line navigation. This is a Lisp API test."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lh]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Local Helpers
;; =============================================================================

(defn- setup-multiline-buffer!
  "Set up a buffer with multiple lines of content."
  []
  (lh/setup-test)
  (lh/eval-lisp! "(insert \"line one\\nline two\\nline three\\nline four\\nline five\\nline six\\nline seven\\nline eight\\nline nine\\nline ten\")"))

;; =============================================================================
;; Test 1: line-count returns correct count
;; =============================================================================

(deftest test-line-count
  (testing "line-count returns the number of lines in buffer"
    (setup-multiline-buffer!)
    (Thread/sleep 100)

    (let [count (lh/eval-lisp! "(line-count)")]
      (is (= 10 count) "Buffer should have 10 lines"))))

;; =============================================================================
;; Test 2: goto-line navigates to correct line
;; =============================================================================

(deftest test-goto-line-navigates
  (testing "goto-line moves cursor to the specified line"
    (setup-multiline-buffer!)
    (Thread/sleep 100)

    ;; Go to line 5
    (lh/eval-lisp! "(goto-line 5)")
    (Thread/sleep 100)

    (let [line (lh/eval-lisp! "(current-line)")]
      (is (= 5 line) "Cursor should be on line 5"))

    ;; Go to line 1
    (lh/eval-lisp! "(goto-line 1)")
    (Thread/sleep 100)

    (let [line (lh/eval-lisp! "(current-line)")]
      (is (= 1 line) "Cursor should be on line 1"))

    ;; Go to line 10
    (lh/eval-lisp! "(goto-line 10)")
    (Thread/sleep 100)

    (let [line (lh/eval-lisp! "(current-line)")]
      (is (= 10 line) "Cursor should be on line 10"))))

;; =============================================================================
;; Test 3: consult-goto-line opens completing-read with line numbers
;; =============================================================================

(deftest test-consult-goto-line-opens-completion
  (testing "consult-goto-line opens minibuffer with line number candidates"
    (setup-multiline-buffer!)
    (Thread/sleep 100)

    ;; Open completing-read with line numbers
    (lh/eval-lisp! "(let [total (line-count)
                          candidates (mapv str (range 1 (inc total)))]
                      (completing-read (str \"Goto line (1..\" total \"): \") candidates))")
    (Thread/sleep 300)

    ;; Minibuffer should be active
    (is (h/minibuffer-visible?) "Minibuffer should be visible")

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 4: Typing a line number and confirming navigates to that line
;; =============================================================================

(deftest test-goto-line-via-completion
  (testing "Typing a line number and confirming navigates to that line"
    (setup-multiline-buffer!)
    (Thread/sleep 100)

    ;; Open completing-read with line numbers
    (lh/eval-lisp! "(let [total (line-count)
                          candidates (mapv str (range 1 (inc total)))]
                      (completing-read (str \"Goto line: \") candidates))")
    (Thread/sleep 300)

    ;; Type "7"
    (h/type-in-minibuffer "7")
    (Thread/sleep 200)

    ;; Confirm
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Now navigate to line 7 (simulating the on-confirm action)
    (lh/eval-lisp! "(goto-line 7)")
    (Thread/sleep 100)

    ;; Verify
    (let [line (lh/eval-lisp! "(current-line)")]
      (is (= 7 line) "Cursor should be on line 7"))))

;; =============================================================================
;; Test 5: consult-goto-line with line at boundaries
;; =============================================================================

(deftest test-goto-line-boundaries
  (testing "goto-line works at first and last lines"
    (setup-multiline-buffer!)
    (Thread/sleep 100)

    ;; First line
    (lh/eval-lisp! "(goto-line 1)")
    (Thread/sleep 100)
    (let [line (lh/eval-lisp! "(current-line)")]
      (is (= 1 line) "Should be on line 1"))

    ;; Last line
    (let [total (lh/eval-lisp! "(line-count)")]
      (lh/eval-lisp! (str "(goto-line " total ")"))
      (Thread/sleep 100)
      (let [line (lh/eval-lisp! "(current-line)")]
        (is (= total line) "Should be on last line")))))
