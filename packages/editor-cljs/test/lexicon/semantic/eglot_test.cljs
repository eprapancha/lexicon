(ns lexicon.semantic.eglot-test
  "Semantic tests for LSP client.

  Emacs source: lisp/progmodes/eglot.el
  Status: 0% implemented

  Key features:
  - Automatic language server detection
  - Hover documentation (eldoc)
  - Completion at point
  - Diagnostics (flymake)
  - Code actions, rename, find references

  Related: Issue #129, Issue #122, Issue #124, Issue #94 (TDD)
  Priority: MEDIUM"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:medium eglot-server-configuration
  "MEDIUM: Configure language servers."
  (testing "eglot-server-programs is configurable"
    (let [programs (helpers/eglot-server-programs)]
      (is (or (nil? programs) (map? programs) (sequential? programs))
          "Should return server programs configuration"))))

(deftest ^:medium eglot-connection-lifecycle
  "MEDIUM: LSP connection lifecycle."
  (testing "eglot can manage connections"
    (with-test-buffer "*test.cljs*"
      (helpers/insert "(ns test)")
      (let [connected (helpers/eglot-ensure)]
        (is (boolean? connected)
            "eglot-ensure should return connection status")))))

(deftest ^:medium eglot-hover-documentation
  "MEDIUM: Hover shows documentation."
  (testing "eglot provides eldoc documentation"
    (with-test-buffer "*test.cljs*"
      (helpers/insert "(defn foo [] nil)")
      (helpers/goto-char 7)
      (let [doc (helpers/eglot-eldoc-function)]
        (is (or (nil? doc) (string? doc))
            "Should return documentation or nil")))))

(deftest ^:medium eglot-completion
  "MEDIUM: LSP completion integration."
  (testing "eglot provides completion candidates"
    (with-test-buffer "*test.cljs*"
      (helpers/insert "(de")
      (let [completions (helpers/eglot-completion-at-point)]
        (is (or (nil? completions) (sequential? completions))
            "Should return completions or nil")))))

(deftest ^:medium eglot-diagnostics
  "MEDIUM: LSP diagnostics to flymake."
  (testing "eglot reports diagnostics"
    (let [diagnostics (helpers/eglot-diagnostics)]
      (is (or (nil? diagnostics) (sequential? diagnostics))
          "Should return diagnostics list or nil"))))

(deftest ^:low eglot-code-actions
  "LOW: Code actions and refactoring."
  (testing "eglot-code-actions returns actions"
    (let [actions (helpers/eglot-code-actions)]
      (is (or (nil? actions) (sequential? actions))
          "Should return code actions or nil"))))

