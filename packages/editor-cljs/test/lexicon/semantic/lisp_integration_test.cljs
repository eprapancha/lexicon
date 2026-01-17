(ns lexicon.semantic.lisp-integration-test
  "SCI/Lisp integration tests from Epic #86.

  Tests command definition, error isolation, and Lisp evaluation."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

;; =============================================================================
;; Command Definition Tests
;; =============================================================================

(deftest ^:critical defcommand-registers-editor-command
  (testing "Emacs invariant: Commands defined from Lisp are first-class"
    (h/reset-editor-db!)
    ;; Define command via eval-lisp
    (h/eval-lisp "(defcommand 'hello (fn [] (message \"hello\")) \"Say hello\")")
    ;; Invoke the command
    (h/invoke-command 'hello)
    ;; Check that message was displayed
    (is (re-find #"hello" (h/echo-area-text))
        "Command defined via defcommand should execute and display message")))

;; =============================================================================
;; Error Isolation Tests
;; =============================================================================

(deftest ^:critical lisp-errors-do-not-corrupt-editor-state
  (testing "Emacs invariant: Errors in Lisp code must not corrupt editor state"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "test")]
      (h/insert-text buf "initial")
      ;; Capture state before error
      (let [text-before (h/buffer-text buf)
            point-before (h/point buf)]
        ;; Evaluate invalid Lisp code that will error
        (h/eval-lisp "(+ 1 :boom)")
        ;; Verify state unchanged despite error
        (is (= text-before (h/buffer-text buf))
            "Buffer text should be unchanged after Lisp error")
        (is (= point-before (h/point buf))
            "Point should be unchanged after Lisp error")))))
