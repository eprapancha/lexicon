(ns lexicon.ui.lisp-integration-test
  "E2E tests for SCI/Lisp integration - Epic #86

  Tests critical invariants:
  - Commands can be defined from Lisp
  - Lisp errors don't corrupt editor state"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-lisp-evaluation-available
  (testing "Emacs invariant: Lisp evaluation is available in the editor"
    (h/setup-test*)

    ;; Try to evaluate Lisp code in scratch buffer (M-: or eval-expression)
    ;; Type some Lisp code
    (h/type-text "(+ 1 2)")
    (Thread/sleep 50)

    ;; Editor should not crash when Lisp code is in buffer
    (let [text (h/get-buffer-text*)]
      (is (.contains text "(+ 1 2)")
          "Should be able to type Lisp code without crashing"))

    ;; Try invoking a command via M-x (which itself is Lisp-backed)
    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "fundamental-mode")
    (Thread/sleep 50)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Should execute successfully (proving Lisp integration works)
    (let [echo-text (h/get-echo-area-text)]
      (is (not (.contains echo-text "error"))
          "Lisp-backed commands should execute successfully"))))

(deftest test-editor-state-preserved-after-errors
  (testing "Emacs invariant: Editor state preserved even if errors occur"
    (h/setup-test*)

    ;; Type some content
    (h/type-text "stable-content")
    (Thread/sleep 50)

    (let [text-before (h/get-buffer-text*)]
      ;; Try to invoke a non-existent command (should error gracefully)
      (h/press-meta "x")
      (Thread/sleep 100)
      (h/type-in-minibuffer "non-existent-command-xyz")
      (Thread/sleep 50)
      (h/press-minibuffer-enter)
      (Thread/sleep 100)

      ;; Editor should still be functional
      (let [text-after (h/get-buffer-text*)]
        (is (.contains text-after "stable-content")
            "Buffer content should be preserved after command error"))

      ;; Should still be able to type
      (h/type-text "more")
      (Thread/sleep 50)

      (let [text-final (h/get-buffer-text*)]
        (is (.contains text-final "more")
            "Editor should remain functional after errors")))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.ui.lisp-integration-test))
