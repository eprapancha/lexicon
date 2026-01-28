(ns lexicon.sci-integration-test
  "Integration tests for SCI (Small Clojure Interpreter) evaluation API.

  IMPORTANT: This file intentionally uses evalLisp because it tests the
  Lisp evaluation system itself, not user-visible keyboard behavior.
  These are API integration tests that verify:
  - Lisp evaluation works correctly (eval-string)
  - Buffer/editing APIs are exposed to Lisp (insert, buffer-string, etc.)
  - Security constraints block dangerous access (js/eval, js/fetch)
  - Dynamic variables work correctly

  Key components:
  - sci-context: SCI evaluation environment
  - sci-namespace: Exposed Lisp API functions
  - Trust levels: :core, :local, :external

  Related: Issue #106 (SCI Integration), Issue #94 (TDD)
  Priority: CRITICAL - foundation for Emacs Lisp compatibility"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Helper Functions
;; NOTE: We intentionally use evalLisp here to test the Lisp API
;; =============================================================================

(defn eval-lisp
  "Evaluate Lisp code and return the result."
  [code]
  (let [result (e/js-execute h/*driver* (str "return window.evalLisp(`" code "`)"))
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
  (h/setup-test*)
  ;; Start with clean buffer - using Lisp API is intentional here
  (eval-lisp! "(erase-buffer)")
  (eval-lisp! "(set-buffer-modified-p nil)"))

;; =============================================================================
;; Basic Evaluation
;; =============================================================================

(deftest test-eval-string-basic
  (testing "eval-string returns result"
    (setup-test)
    (let [result (eval-lisp! "(+ 1 2)")]
      (is (= 3 result))))

  (testing "eval-string with multiple forms"
    (setup-test)
    (let [result (eval-lisp! "(setq x 5) (+ x 3)")]
      (is (= 8 result))))

  (testing "eval-string with string result"
    (setup-test)
    (let [result (eval-lisp! "(str \"hello\" \" \" \"world\")")]
      (is (= "hello world" result)))))

(deftest test-eval-string-error-handling
  (testing "syntax error returns error"
    (setup-test)
    (let [result (eval-lisp "(+ 1")]  ; Missing closing paren
      (is (not (:success result))
          "Should return error indicator")))

  (testing "runtime error returns error"
    (setup-test)
    (let [result (eval-lisp "(error \"test error\")")]
      (is (not (:success result))
          "Should handle gracefully"))))

;; =============================================================================
;; API Exposure
;; =============================================================================

(deftest test-sci-exposes-buffer-api
  (testing "point function works"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (let [result (eval-lisp! "(point)")]
      (is (number? result))))

  (testing "goto-char function works"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (eval-lisp! "(goto-char 0)")
    (is (= 0 (eval-lisp! "(point)"))))

  (testing "insert function works"
    (setup-test)
    (eval-lisp! "(insert \"test\")")
    (is (= "test" (eval-lisp! "(buffer-string)"))))

  (testing "buffer-string function works"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (let [result (eval-lisp! "(buffer-string)")]
      (is (= "Hello" result)))))

(deftest test-sci-exposes-message-api
  (testing "message function works"
    (setup-test)
    (eval-lisp! "(message \"Hello from SCI\")")
    ;; Message should be stored and accessible
    (is (= "Hello from SCI" (eval-lisp! "(current-message)")))))

(deftest test-sci-exposes-search-api
  (testing "search-forward available"
    (setup-test)
    (eval-lisp! "(insert \"Hello World Hello\")")
    (eval-lisp! "(goto-char 0)")
    (let [result (eval-lisp! "(search-forward \"World\")")]
      ;; Should return position after match or nil
      (is (or (number? result) (nil? result))))))

;; =============================================================================
;; Command Definition
;; =============================================================================

(deftest ^:skip test-defcommand-creates-command
  (testing "defcommand creates callable command"
    (setup-test)
    (eval-lisp!
     "(defcommand 'test-cmd (fn [] (insert \"executed\")) \"A test command\")")

    ;; Command should exist and be invokable
    (eval-lisp! "(test-cmd)")
    (is (= "executed" (eval-lisp! "(buffer-string)")))))

(deftest ^:skip test-defcommand-with-prefix-arg
  (testing "command can access prefix arg"
    (setup-test)
    (eval-lisp!
     "(defcommand 'test-prefix
        (fn [] (insert (str *prefix-arg*)))
        \"Show prefix arg\")")

    ;; Set prefix arg and invoke
    (eval-lisp! "(setq *prefix-arg* 4)")
    (eval-lisp! "(test-prefix)")

    (is (= "4" (eval-lisp! "(buffer-string)"))
        "Should insert prefix arg value")))

;; =============================================================================
;; Security
;; =============================================================================

(deftest test-sci-blocks-dangerous-access
  (testing "js/eval blocked"
    (setup-test)
    (let [result (eval-lisp "(js/eval \"alert('xss')\")")]
      (is (not (:success result))
          "js/eval should be blocked")))

  (testing "js/fetch blocked"
    (setup-test)
    (let [result (eval-lisp "(js/fetch \"http://evil.com\")")]
      (is (not (:success result))
          "js/fetch should be blocked"))))

(deftest ^:skip test-sci-allows-safe-js
  (testing "js/Math allowed"
    (setup-test)
    (let [result (eval-lisp! "(js/Math.sqrt 16)")]
      (is (= 4.0 result))))

  (testing "js/Date allowed"
    (setup-test)
    (let [result (eval-lisp "(js/Date.)")]
      ;; May or may not work depending on SCI config
      (is (some? result) "Should return something"))))

;; =============================================================================
;; Dynamic Variables
;; =============================================================================

(deftest ^:skip test-dynamic-variables-work
  (testing "*current-buffer* accessible"
    (setup-test)
    (let [result (eval-lisp! "(current-buffer)")]
      (is (some? result))))

  (testing "*prefix-arg* accessible"
    (setup-test)
    (eval-lisp! "(setq *prefix-arg* 7)")
    (let [result (eval-lisp! "*prefix-arg*")]
      (is (= 7 result)))))

;; =============================================================================
;; Lisp API Completeness
;; =============================================================================

(deftest test-buffer-operations-exposed
  (testing "buffer-string works"
    (setup-test)
    (eval-lisp! "(insert \"test\")")
    (is (= "test" (eval-lisp! "(buffer-string)"))))

  (testing "insert works"
    (setup-test)
    (eval-lisp! "(insert \"hello\")")
    (is (= "hello" (eval-lisp! "(buffer-string)"))))

  (testing "delete-region works"
    (setup-test)
    (eval-lisp! "(insert \"hello world\")")
    (eval-lisp! "(delete-region 0 6)")
    (is (= "world" (eval-lisp! "(buffer-string)")))))

(deftest test-navigation-exposed
  (testing "point works"
    (setup-test)
    (eval-lisp! "(insert \"hello\")")
    (is (number? (eval-lisp! "(point)"))))

  (testing "goto-char works"
    (setup-test)
    (eval-lisp! "(insert \"hello\")")
    (eval-lisp! "(goto-char 2)")
    (is (= 2 (eval-lisp! "(point)"))))

  (testing "forward-line works"
    (setup-test)
    (eval-lisp! "(insert \"line1\\nline2\\nline3\")")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp! "(forward-line 1)")
    ;; Should be at start of line2
    (is (>= (eval-lisp! "(point)") 6))))

(deftest test-modes-exposed
  (testing "major-mode accessible"
    (setup-test)
    (let [result (eval-lisp! "major-mode")]
      (is (some? result) "major-mode should be defined"))))

;; =============================================================================
;; Save-Excursion (Lisp API Tests)
;; =============================================================================
;;
;; save-excursion is a Lisp macro that saves point/mark, executes body,
;; then restores point/mark. These tests verify the Lisp API behavior.

(deftest test-save-excursion-restores-point
  (testing "Point restored after movement in save-excursion body"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(goto-char 5)")
    (let [pt-before (eval-lisp! "(point)")]
      ;; Move point inside save-excursion
      (eval-lisp! "(save-excursion (goto-char 0))")
      (is (= pt-before (eval-lisp! "(point)"))
          "Point should be restored after save-excursion"))))

(deftest test-save-excursion-restores-point-multiple-moves
  (testing "Point restored even after multiple moves"
    (setup-test)
    (eval-lisp! "(insert \"ABCDEFGHIJ\")")
    (eval-lisp! "(goto-char 5)")
    (let [pt-before (eval-lisp! "(point)")]
      ;; Multiple moves inside save-excursion
      (eval-lisp! "(save-excursion
                     (goto-char 0)
                     (goto-char 10)
                     (goto-char 3))")
      (is (= pt-before (eval-lisp! "(point)"))
          "Point restored after multiple moves"))))

(deftest test-save-excursion-return-value
  (testing "save-excursion returns body's value"
    (setup-test)
    (eval-lisp! "(insert \"Hello\")")
    (let [result (eval-lisp! "(save-excursion (goto-char 0) (+ 1 2))")]
      (is (= 3 result) "Should return last form's value"))))

(deftest test-save-excursion-restores-on-error
  (testing "Point restored even when body errors"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(goto-char 5)")
    (let [pt-before (eval-lisp! "(point)")]
      ;; Try to cause an error inside save-excursion
      (eval-lisp "(save-excursion (goto-char 0) (error \"test\"))")
      ;; Point should still be restored
      (is (= pt-before (eval-lisp! "(point)"))
          "Point restored even after error in body"))))

(deftest test-save-excursion-nested
  (testing "Nested save-excursion works correctly"
    (setup-test)
    (eval-lisp! "(insert \"ABCDEFGHIJ\")")
    (eval-lisp! "(goto-char 5)")
    (let [outer-pt (eval-lisp! "(point)")]
      (eval-lisp! "(save-excursion
                     (goto-char 2)
                     (save-excursion
                       (goto-char 8)))")
      (is (= outer-pt (eval-lisp! "(point)"))
          "Outer point restored after nested save-excursion"))))

(deftest test-save-excursion-look-ahead-pattern
  (testing "Look-ahead pattern - check content without moving cursor"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(goto-char 0)")
    (let [pt-before (eval-lisp! "(point)")
          ;; Look ahead to find "World" without moving point
          found? (eval-lisp! "(save-excursion
                               (search-forward \"World\"))")]
      (is (= pt-before (eval-lisp! "(point)"))
          "Point unchanged after look-ahead")
      (is (some? found?) "Should find 'World'"))))

;; =============================================================================
;; Buffer API (Lisp API Tests)
;; =============================================================================
;;
;; Tests for buffer-related Lisp API functions.

(deftest test-buffer-modified-p-api
  (testing "buffer-modified-p returns false for unmodified buffer"
    (setup-test)
    ;; setup-test already calls (set-buffer-modified-p nil)
    (let [result (eval-lisp! "(buffer-modified-p)")]
      (is (not result) "Fresh buffer should not be modified")))

  (testing "buffer-modified-p returns true after insert"
    (setup-test)
    (eval-lisp! "(insert \"test\")")
    (let [result (eval-lisp! "(buffer-modified-p)")]
      (is result "Buffer should be modified after insert"))))

(deftest test-set-buffer-modified-p-api
  (testing "set-buffer-modified-p clears modification flag"
    (setup-test)
    (eval-lisp! "(insert \"test\")")
    (eval-lisp! "(set-buffer-modified-p nil)")
    (let [result (eval-lisp! "(buffer-modified-p)")]
      (is (not result) "Buffer should not be modified after clearing flag")))

  (testing "set-buffer-modified-p can force modified state"
    (setup-test)
    (eval-lisp! "(set-buffer-modified-p t)")
    (let [result (eval-lisp! "(buffer-modified-p)")]
      (is result "Buffer should be modified when forced"))))

(deftest test-erase-buffer-api
  (testing "erase-buffer removes all content"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(erase-buffer)")
    (is (= "" (eval-lisp! "(buffer-string)"))
        "Buffer should be empty after erase-buffer"))

  (testing "erase-buffer resets point to beginning"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(erase-buffer)")
    (is (= 0 (eval-lisp! "(point)"))
        "Point should be at beginning after erase-buffer")))

(deftest test-current-buffer-api
  (testing "current-buffer returns buffer object"
    (setup-test)
    (let [result (eval-lisp! "(current-buffer)")]
      (is (some? result) "current-buffer should return something"))))

(deftest ^:skip test-buffer-live-p-api
  (testing "buffer-live-p checks if buffer is alive"
    (setup-test)
    ;; TODO: Implement buffer-live-p
    (is true "PENDING: buffer-live-p needs implementation")))

(deftest ^:skip test-get-buffer-create-api
  (testing "get-buffer-create returns or creates buffer"
    (setup-test)
    ;; TODO: Implement get-buffer-create
    (is true "PENDING: get-buffer-create needs implementation")))

(deftest ^:skip test-narrow-to-region-api
  (testing "narrow-to-region restricts buffer view"
    (setup-test)
    ;; TODO: Implement narrowing
    (is true "PENDING: narrow-to-region needs implementation")))

(deftest ^:skip test-save-restriction-api
  (testing "save-restriction preserves narrowing state"
    (setup-test)
    ;; TODO: Implement save-restriction
    (is true "PENDING: save-restriction needs implementation")))
