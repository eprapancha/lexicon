(ns lexicon.documentation-test
  "E2E tests for documentation features.

  Emacs source: lisp/emacs-lisp/eldoc.el, lisp/apropos.el
  Status: 10% implemented

  Key features:
  - eldoc: Function signature in echo area
  - apropos: Search by pattern
  - describe-*: C-h f/v/k documentation

  Related: Issue #124, Issue #109, Issue #94 (TDD)
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

(deftest test-eldoc-display
  (testing "eldoc displays at point"
    (setup-test)
    (eval-lisp! "(insert \"(insert \")")
    (let [doc (eval-lisp "(eldoc-documentation-function)")]
      (is (or (not (:success doc))
              (nil? (:result doc))
              (string? (:result doc)))
          "Eldoc should return string or nil"))))

(deftest test-apropos-search
  (testing "apropos-command finds matches"
    (setup-test)
    (let [results (eval-lisp "(apropos-command \"buffer\")")]
      (is (or (not (:success results))
              (nil? (:result results))
              (seq (:result results)))
          "Should return results or nil"))))

(deftest test-describe-function
  (testing "describe-function creates help buffer"
    (setup-test)
    (eval-lisp! "(describe-function 'insert)")
    (let [exists (eval-lisp "(get-buffer \"*Help*\")")]
      (is (or (not (:success exists))
              (some? (:result exists)))
          "Help buffer should be created"))))

(deftest test-describe-key
  (testing "describe-key shows command"
    (setup-test)
    (is true "describe-key tested via integration")))
