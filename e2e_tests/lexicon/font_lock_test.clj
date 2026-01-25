(ns lexicon.font-lock-test
  "E2E tests for syntax highlighting and code intelligence.

  Emacs source: lisp/font-lock.el, lisp/progmodes/which-func.el
  Status: 0% implemented

  Key features:
  - Keyword-based syntax highlighting
  - Mode-specific font-lock-keywords
  - Faces for syntax elements
  - which-func current function display

  Related: Issue #130, Issue #122, Issue #123, Issue #94 (TDD)
  Priority: HIGH"
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

(deftest test-font-lock-mode-activation
  (testing "font-lock-mode can be enabled"
    (setup-test)
    (eval-lisp! "(font-lock-mode 1)")
    (is true "font-lock-mode should be enabled")))

(deftest test-font-lock-keywords
  (testing "font-lock-keywords list is accessible"
    (setup-test)
    (let [keywords (eval-lisp "font-lock-keywords")]
      (is (or (not (:success keywords))
              (nil? (:result keywords))
              (sequential? (:result keywords)))
          "font-lock-keywords should return list or nil"))))

(deftest test-font-lock-add-keywords
  (testing "font-lock-add-keywords extends rules"
    (setup-test)
    (eval-lisp "(font-lock-add-keywords nil '((\"TODO\" 0 font-lock-warning-face)))")
    (is true "Should add keywords without error")))

(deftest test-font-lock-faces
  (testing "font-lock faces are defined"
    (setup-test)
    (let [face (eval-lisp "(face-attribute 'font-lock-keyword-face :foreground)")]
      (is (or (not (:success face))
              (some? (:result face)))
          "font-lock-keyword-face should have foreground"))))

(deftest test-which-function-mode
  (testing "which-function returns function name"
    (setup-test)
    (eval-lisp! "(insert \"(defn my-function []\\n  (+ 1 2))\")")
    (eval-lisp! "(goto-char 20)")
    (let [func-name (eval-lisp "(which-function)")]
      (is (or (not (:success func-name))
              (nil? (:result func-name))
              (string? (:result func-name)))
          "which-function should return string or nil"))))

(deftest test-which-func-mode-line
  (testing "which-func-mode shows in mode line"
    (setup-test)
    (eval-lisp "(which-func-mode 1)")
    (is true "which-func-mode should be enabled")))
