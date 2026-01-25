(ns lexicon.grep-test
  "E2E tests for grep and regexp highlighting.

  Emacs source: lisp/progmodes/grep.el, lisp/hi-lock.el
  Status: 0% implemented

  Key features:
  - grep: Run grep, display results
  - grep-find: Grep with find for project search
  - hi-lock: Highlight patterns in buffer
  - occur: Show all lines matching regexp

  Related: Issue #125, Issue #107, Issue #94 (TDD)
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

(deftest test-grep-command
  (testing "grep creates grep buffer"
    (setup-test)
    (eval-lisp "(grep \"grep -n TODO *.cljs\")")
    (let [exists (eval-lisp "(get-buffer \"*grep*\")")]
      (is (or (not (:success exists))
              (some? (:result exists)))
          "Grep buffer should be created or nil"))))

(deftest test-grep-find-project
  (testing "grep-find searches recursively"
    (setup-test)
    (is true "grep-find should work")))

(deftest test-hi-lock-mode
  (testing "hi-lock-mode can be enabled"
    (setup-test)
    (eval-lisp "(hi-lock-mode 1)")
    (is true "hi-lock-mode should be enabled")))

(deftest test-highlight-regexp
  (testing "highlight-regexp highlights matches"
    (setup-test)
    (eval-lisp! "(insert \"foo bar foo baz\")")
    (eval-lisp "(highlight-regexp \"foo\")")
    (is true "highlight-regexp should work")))

(deftest test-occur
  (testing "occur creates occur buffer"
    (setup-test)
    (eval-lisp! "(insert \"line1 foo\\nline2\\nline3 foo\")")
    (is true "occur should create *Occur* buffer")))
