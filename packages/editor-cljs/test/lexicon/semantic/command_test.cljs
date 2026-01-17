(ns lexicon.semantic.command-test
  "Emacs semantic compatibility tests for command system.

  Tests from Epic #86, Issue #87 - Core Editor Semantics"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical command-has-metadata-and-identity
  (testing "Emacs invariant: Commands are inspectable entities, not bare functions"
    (h/reset-editor-db!)
    (let [cmd (h/define-test-command
                {:name 'test/hello
                 :doc "Say hello"
                 :fn (fn [_] nil)})]
      (is (symbol? (h/command-name cmd)) "Command should have a symbol name")
      (is (string? (h/command-doc cmd)) "Command should have documentation")
      (is (fn? (h/command-fn cmd)) "Command should have a function"))))

(deftest ^:critical all-command-invocations-go-through-dispatch
  (testing "Emacs invariant: All command invocation must pass through the command dispatcher"
    (h/reset-editor-db!)
    (h/define-test-command
      {:name 'test/hello
       :doc "Say hello"
       :fn (fn [_] nil)})
    (h/with-instrumented-dispatch
      (fn []
        (h/invoke-command 'test/hello)
        (is (h/dispatch-was-used?) "Command invocation should use dispatch")))))
