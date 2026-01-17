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
    ;; TODO: Implement eval-lisp helper and defcommand support
    ;; For now, this will fail
    ;; (h/eval-lisp
    ;;  '(defcommand hello ()
    ;;     "Say hello"
    ;;     (message "hello")))
    ;; (h/invoke-command 'hello)
    ;; (is (re-find #"hello" (h/echo-area-text)))

    ;; Placeholder assertion until SCI integration is implemented
    (is false "SCI integration not yet implemented - defcommand unavailable")))

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
        ;; TODO: Implement eval-lisp and editor-snapshot helpers
        ;; (try
        ;;   (h/eval-lisp '(+ 1 :boom))
        ;;   (catch :default _))
        ;; Verify state unchanged
        (is (= text-before (h/buffer-text buf))
            "Buffer text should be unchanged after Lisp error")
        (is (= point-before (h/point buf))
            "Point should be unchanged after Lisp error")))

    ;; Placeholder - will fail until SCI is integrated
    (is false "SCI integration not yet implemented - error isolation untestable")))
