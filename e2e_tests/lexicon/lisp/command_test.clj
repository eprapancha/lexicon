(ns lexicon.lisp.command-test
  "Lisp API tests for command definition and invocation.

  Tests command-related Lisp functions:
  - defcommand: Define a new interactive command
  - Command invocation with prefix arguments
  - Dynamic variables (*prefix-arg*, *current-buffer*)

  JUSTIFICATION: Commands are Lisp entities. Testing command registration,
  metadata, and prefix argument handling requires Lisp evaluation.
  Keyboard invocation is tested separately in UI tests."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Command Definition
;; =============================================================================

(deftest test-defcommand-creates-command
  (testing "defcommand creates callable command"
    (lisp/setup-test)
    (lisp/eval-lisp!
     "(defcommand 'test-cmd (fn [] (insert \"executed\")) \"A test command\")")

    ;; Command should exist and be invokable via call-interactively
    (lisp/eval-lisp! "(call-interactively 'test-cmd)")
    (Thread/sleep 100) ;; Let async dispatch complete
    (is (= "executed" (lisp/eval-lisp! "(buffer-string)")))))

(deftest test-defcommand-with-prefix-arg
  (testing "command can access prefix arg"
    (lisp/setup-test)
    ;; Set prefix arg BEFORE defining command so it's available
    (lisp/eval-lisp! "(setq *prefix-arg* 4)")
    (lisp/eval-lisp!
     "(defcommand 'test-prefix
        (fn [] (insert (str (symbol-value '*prefix-arg*))))
        \"Show prefix arg\")")

    ;; Invoke via call-interactively
    (lisp/eval-lisp! "(call-interactively 'test-prefix)")
    (Thread/sleep 100) ;; Let async dispatch complete

    (is (= "4" (lisp/eval-lisp! "(buffer-string)"))
        "Should insert prefix arg value")))

;; =============================================================================
;; Dynamic Variables
;; =============================================================================

(deftest test-prefix-arg-variable
  (testing "*prefix-arg* is accessible and settable"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq *prefix-arg* 7)")
    (let [result (lisp/eval-lisp! "*prefix-arg*")]
      (is (= 7 result)))))

(deftest test-current-buffer-variable
  (testing "*current-buffer* is accessible"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(current-buffer)")]
      (is (some? result)))))

;; =============================================================================
;; Mode Variables
;; =============================================================================

(deftest test-major-mode-accessible
  (testing "major-mode variable is accessible"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "major-mode")]
      (is (some? result) "major-mode should be defined"))))

;; =============================================================================
;; Message API
;; =============================================================================

(deftest test-message-function
  (testing "message function stores message"
    (lisp/setup-test)
    (lisp/eval-lisp! "(message \"Hello from SCI\")")
    ;; Message should be stored and accessible
    (is (= "Hello from SCI" (lisp/eval-lisp! "(current-message)")))))
