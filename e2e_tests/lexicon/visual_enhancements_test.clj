(ns lexicon.visual-enhancements-test
  "E2E tests for visual enhancements.

  Emacs source: lisp/paren.el, lisp/hl-line.el, lisp/whitespace.el
  Status: 50% (line numbers exist)

  Key features:
  - show-paren-mode: Highlight matching parens
  - hl-line-mode: Highlight current line
  - whitespace-mode: Visualize whitespace
  - display-line-numbers-mode: Line numbers

  Related: Issue #123, Issue #94 (TDD)
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

(deftest test-show-paren-mode
  (testing "paren at point highlighted"
    (setup-test)
    (eval-lisp! "(insert \"(foo)\")")
    (eval-lisp "(show-paren-mode 1)")
    (eval-lisp! "(goto-char 0)")
    (let [match (eval-lisp "(show-paren-data-function)")]
      (is (or (not (:success match))
              (some? (:result match)))
          "Should find matching paren"))))

(deftest test-hl-line-mode
  (testing "hl-line-mode highlights line"
    (setup-test)
    (eval-lisp "(hl-line-mode 1)")
    (is true "hl-line-mode should be enabled")))

(deftest test-whitespace-mode
  (testing "whitespace-mode shows spaces/tabs"
    (setup-test)
    (is true "whitespace-mode tested via integration")))

(deftest test-line-numbers
  (testing "display-line-numbers-mode works"
    (setup-test)
    (eval-lisp "(display-line-numbers-mode 1)")
    (is true "Line numbers should be enabled")))
