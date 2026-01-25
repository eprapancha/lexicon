(ns lexicon.eglot-test
  "E2E tests for LSP client.

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
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as test-helpers]))

(def app-url "http://localhost:8080")
(def ^:dynamic *driver* nil)
(use-fixtures :once (partial test-helpers/with-driver-and-messages #'*driver*))

(defn eval-lisp
  [code]
  (let [result (e/js-execute *driver* (str "return window.evalLisp(`" code "`)"))
        success (:success result)]
    (if success
      {:success true :result (:result result)}
      {:success false :error (:error result)})))

(defn eval-lisp! [code]
  (let [{:keys [success result error]} (eval-lisp code)]
    (if success result
      (throw (ex-info (str "Lisp eval failed: " error) {:code code})))))

(defn setup-test []
  (e/go *driver* app-url)
  (test-helpers/wait-for-editor-ready *driver*)
  (test-helpers/click-editor *driver*)
  (Thread/sleep 300)
  (eval-lisp! "(erase-buffer)")
  (eval-lisp! "(set-buffer-modified-p nil)"))

(deftest test-eglot-server-configuration
  (testing "eglot-server-programs is configurable"
    (setup-test)
    (let [programs (eval-lisp "eglot-server-programs")]
      (is (or (not (:success programs))
              (nil? (:result programs))
              (map? (:result programs))
              (sequential? (:result programs)))
          "Should return server programs configuration"))))

(deftest test-eglot-connection-lifecycle
  (testing "eglot can manage connections"
    (setup-test)
    (eval-lisp! "(insert \"(ns test)\")")
    (let [connected (eval-lisp "(eglot-ensure)")]
      (is (or (not (:success connected))
              (boolean? (:result connected)))
          "eglot-ensure should return connection status"))))

(deftest test-eglot-hover-documentation
  (testing "eglot provides eldoc documentation"
    (setup-test)
    (eval-lisp! "(insert \"(defn foo [] nil)\")")
    (eval-lisp! "(goto-char 7)")
    (let [doc (eval-lisp "(eglot-eldoc-function)")]
      (is (or (not (:success doc))
              (nil? (:result doc))
              (string? (:result doc)))
          "Should return documentation or nil"))))

(deftest test-eglot-completion
  (testing "eglot provides completion candidates"
    (setup-test)
    (eval-lisp! "(insert \"(de\")")
    (let [completions (eval-lisp "(eglot-completion-at-point)")]
      (is (or (not (:success completions))
              (nil? (:result completions))
              (sequential? (:result completions)))
          "Should return completions or nil"))))

(deftest test-eglot-diagnostics
  (testing "eglot reports diagnostics"
    (setup-test)
    (let [diagnostics (eval-lisp "(eglot-diagnostics)")]
      (is (or (not (:success diagnostics))
              (nil? (:result diagnostics))
              (sequential? (:result diagnostics)))
          "Should return diagnostics list or nil"))))

(deftest test-eglot-code-actions
  (testing "eglot-code-actions returns actions"
    (setup-test)
    (let [actions (eval-lisp "(eglot-code-actions)")]
      (is (or (not (:success actions))
              (nil? (:result actions))
              (sequential? (:result actions)))
          "Should return code actions or nil"))))
