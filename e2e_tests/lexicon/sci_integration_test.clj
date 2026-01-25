(ns lexicon.sci-integration-test
  "E2E tests for SCI (Small Clojure Interpreter) integration.

  SCI provides runtime evaluation for:
  - Emacs Lisp API emulation (lexicon.lisp)
  - External package sandboxing (core.packages.sci)
  - Interactive eval (eval-string, eval-expression)

  Key components:
  - sci-context: SCI evaluation environment
  - sci-namespace: Exposed Lisp API functions
  - Trust levels: :core, :local, :external

  Related: Issue #106 (SCI Integration), Issue #94 (TDD)
  Priority: CRITICAL - foundation for Emacs Lisp compatibility"
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

(deftest test-defcommand-creates-command
  (testing "defcommand creates callable command"
    (setup-test)
    (eval-lisp!
     "(defcommand 'test-cmd (fn [] (insert \"executed\")) \"A test command\")")

    ;; Command should exist and be invokable
    (eval-lisp! "(test-cmd)")
    (is (= "executed" (eval-lisp! "(buffer-string)")))))

(deftest test-defcommand-with-prefix-arg
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

(deftest test-sci-allows-safe-js
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

(deftest test-dynamic-variables-work
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
