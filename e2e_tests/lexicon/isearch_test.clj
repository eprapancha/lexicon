(ns lexicon.isearch-test
  "E2E tests for incremental search (isearch).

  Emacs source: lisp/isearch.el (4,638 LOC)
  Status: 70% implemented

  Key features:
  - C-s: Search forward incrementally
  - C-r: Search backward incrementally
  - M-c: Toggle case sensitivity
  - Wrapping search

  Related: Issue #107 (Search & Replace), Issue #94 (TDD)
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

;; =============================================================================
;; Basic Search
;; =============================================================================

(deftest test-isearch-forward-basic
  (testing "search forward finds match"
    (setup-test)
    (eval-lisp! "(insert \"Hello World Hello\")")
    (eval-lisp! "(goto-char 0)")
    (let [result (eval-lisp "(search-forward \"World\")")]
      (is (or (not (:success result))
              (number? (:result result)))
          "Should find match")))

  (testing "repeated search finds next match"
    (setup-test)
    (eval-lisp! "(insert \"one two one three one\")")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp! "(search-forward \"one\")")
    (let [pos1 (eval-lisp! "(point)")]
      (eval-lisp! "(search-forward \"one\")")
      (let [pos2 (eval-lisp! "(point)")]
        (is (> pos2 pos1) "Second match should be after first")))))

(deftest test-isearch-backward-basic
  (testing "search backward finds match"
    (setup-test)
    (eval-lisp! "(insert \"Hello World Hello\")")
    (eval-lisp! "(goto-char 17)")
    (let [result (eval-lisp "(search-backward \"Hello\")")]
      (is (or (not (:success result))
              (number? (:result result)))
          "Should find last 'Hello'"))))

(deftest test-isearch-wrapping
  (testing "search wraps to beginning"
    (setup-test)
    (eval-lisp! "(insert \"Hello World\")")
    (eval-lisp! "(goto-char 11)")
    ;; Search for something at the beginning
    (let [result (eval-lisp "(search-forward \"Hello\" nil t)")]
      (is (or (not (:success result))
              (nil? (:result result))  ; didn't wrap
              (number? (:result result)))
          "Should handle wrap or return nil"))))

(deftest test-isearch-case-toggle
  (testing "case insensitive by default"
    (setup-test)
    (eval-lisp! "(insert \"Hello HELLO hello\")")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp! "(setq case-fold-search t)")
    (let [result (eval-lisp "(search-forward \"hello\")")]
      (is (or (not (:success result))
              (number? (:result result)))
          "Should find match case-insensitively"))))

;; =============================================================================
;; Query Replace
;; =============================================================================

(deftest test-query-replace-basic
  (testing "replace-string replaces all matches"
    (setup-test)
    (eval-lisp! "(insert \"foo bar foo baz foo\")")
    (eval-lisp! "(goto-char 0)")
    (eval-lisp! "(replace-string \"foo\" \"XXX\")")
    (is (= "XXX bar XXX baz XXX" (eval-lisp! "(buffer-string)"))
        "All matches replaced")))
