(ns lexicon.save-excursion-test
  "E2E tests for save-excursion - CORE macro for temporary navigation.

  Save-excursion saves point and mark, executes body, then restores them.
  Used by: virtually every command that 'looks around' without moving cursor.

  Related: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md
  Priority: CRITICAL - ergonomic requirement, used everywhere"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as test-helpers]))

;; Test configuration
(def app-url "http://localhost:8080")

;; Browser driver (will be set by fixture)
(def ^:dynamic *driver* nil)

;; Setup/teardown
(use-fixtures :once (partial test-helpers/with-driver-and-messages #'*driver*))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn eval-lisp
  "Evaluate Lisp code and return the result."
  [code]
  (let [result (e/js-execute *driver* (str "return window.evalLisp(`" code "`)"))
        success (:success result)]
    (if success
      {:success true :result (:result result)}
      {:success false :error (:error result)})))

(defn eval-lisp!
  "Evaluate Lisp code and return just the result (throws on error)"
  [code]
  (let [{:keys [success result error]} (eval-lisp code)]
    (if success
      result
      (throw (ex-info (str "Lisp eval failed: " error) {:code code})))))

(defn setup-test []
  "Standard test setup"
  (e/go *driver* app-url)
  (test-helpers/wait-for-editor-ready *driver*)
  (test-helpers/click-editor *driver*)
  (Thread/sleep 300)
  ;; Start with clean buffer
  (eval-lisp! "(erase-buffer)")
  (eval-lisp! "(set-buffer-modified-p nil)"))

;; =============================================================================
;; Save-Excursion - Core Macro Tests
;; =============================================================================

(deftest test-save-excursion-restores-point
  (testing "Point restored after moving in body"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(goto-char 6)")  ; Middle of buffer

    ;; save-excursion should restore point after body
    (eval-lisp! "(save-excursion (goto-char 0))")

    ;; Point should be restored
    (is (= 6 (eval-lisp! "(point)"))
        "Point restored after save-excursion"))

  (testing "Point restored even with multiple moves"
    (setup-test)
    (eval-lisp! "(insert \"ABCDEFGHIJ\")")
    (eval-lisp! "(goto-char 5)")

    (eval-lisp! "(save-excursion (goto-char 0) (goto-char 10) (goto-char 3))")

    (is (= 5 (eval-lisp! "(point)"))
        "Point restored despite multiple moves")))

(deftest test-save-excursion-restores-on-error
  (testing "Point restored even when body errors"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(goto-char 6)")

    ;; Execute save-excursion with error, catch via eval-lisp
    (let [result (eval-lisp "(save-excursion (goto-char 0) (error \"Test error\"))")]
      (is (not (:success result)) "Error propagates"))

    ;; Despite error, point should be restored
    (is (= 6 (eval-lisp! "(point)"))
        "Point restored even after error")))

(deftest test-save-excursion-restores-mark
  (testing "Mark restored after body"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp! "(set-mark 5)")  ; Mark at position 5

    ;; Change mark in save-excursion
    (eval-lisp! "(save-excursion (set-mark 10))")

    ;; Mark should be restored
    (is (= 5 (eval-lisp! "(mark)"))
        "Mark restored after save-excursion")))

(deftest test-save-excursion-return-value
  (testing "Return value from body"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")

    (let [result (eval-lisp! "(save-excursion (goto-char 0) (looking-at \"Hello\"))")]
      (is (= true result)
          "save-excursion returns body value"))))

(deftest test-save-excursion-nested
  (testing "Nested save-excursion"
    (setup-test)
    (eval-lisp! "(insert \"ABCDEFGHIJ\")")
    (eval-lisp! "(goto-char 5)")  ; Start at 'F'

    ;; Nested save-excursion
    (eval-lisp! "(save-excursion
                   (goto-char 3)
                   (save-excursion
                     (goto-char 7)))")

    ;; Outer restored, should be at original position
    (is (= 5 (eval-lisp! "(point)"))
        "Nested save-excursion restores correctly")))

(deftest test-save-excursion-with-buffer-switch
  (testing "Buffer restored after switch"
    (setup-test)
    (let [original-name (eval-lisp! "(buffer-name)")]
      ;; Create second buffer and switch to it inside save-excursion
      (eval-lisp! "(get-buffer-create \"*buf2*\")")

      (eval-lisp! "(save-excursion
                     (set-buffer \"*buf2*\")
                     (insert \"Buffer 2\"))")

      ;; Should be back in original buffer
      (is (= original-name (eval-lisp! "(buffer-name)"))
          "Buffer restored after save-excursion")

      ;; Cleanup
      (eval-lisp! "(kill-buffer \"*buf2*\")"))))

;; =============================================================================
;; Integration with Core Systems
;; =============================================================================

(deftest test-save-excursion-in-read-only-buffer
  (testing "Navigate in read-only buffer"
    (setup-test)
    (eval-lisp! "(insert \"Help text here\")")
    (eval-lisp! "(setq buffer-read-only t)")
    (eval-lisp! "(goto-char 5)")

    (eval-lisp! "(save-excursion (goto-char 0))")

    (is (= 5 (eval-lisp! "(point)"))
        "Point restored in read-only buffer")

    ;; Reset read-only
    (eval-lisp! "(setq buffer-read-only nil)")))

(deftest test-save-excursion-with-narrowing
  (testing "save-excursion with narrowing"
    (setup-test)
    (eval-lisp! "(insert \"AAABBBCCCDDDEEE\")")
    ;; Narrow to "BBBCCCDD" (positions 3-11)
    (eval-lisp! "(narrow-to-region 3 11)")
    (eval-lisp! "(goto-char 6)")  ; Position 6

    (eval-lisp! "(save-excursion (goto-char 3))")

    (is (= 6 (eval-lisp! "(point)"))
        "Point restored in narrowed buffer")

    ;; Widen to clean up
    (eval-lisp! "(widen)")))

;; =============================================================================
;; Usage Patterns from Emacs
;; =============================================================================

(deftest test-save-excursion-look-ahead-pattern
  (testing "Look ahead pattern"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(goto-char 0)")

    ;; Look ahead and return result
    (let [result (eval-lisp! "(save-excursion
                                (forward-word)
                                (buffer-substring 0 5))")]
      (is (= "Hello" result)
          "Extracted text ahead")
      (is (= 0 (eval-lisp! "(point)"))
          "Cursor didn't move"))))

(deftest test-save-excursion-scan-buffer-pattern
  (testing "Scan buffer pattern"
    (setup-test)
    (eval-lisp! "(insert \"foo bar foo baz foo\")")
    (eval-lisp! "(goto-char 10)")  ; Middle of buffer

    ;; Count occurrences using save-excursion
    (let [count (eval-lisp! "(save-excursion
                               (goto-char 0)
                               (let ((n 0))
                                 (while (search-forward \"foo\" nil t)
                                   (setq n (+ n 1)))
                                 n))")]
      (is (= 3 count) "Found 3 occurrences")
      (is (= 10 (eval-lisp! "(point)"))
          "Cursor back at original position"))))
