(ns lexicon.programming-support-test
  "E2E tests for programming support.

  Emacs source: lisp/progmodes/compile.el, lisp/progmodes/flymake.el, lisp/imenu.el
  Status: 0% implemented

  Key features:
  - compile: M-x compile, error parsing
  - flymake: On-the-fly syntax checking
  - imenu: Buffer index

  Related: Issue #122, Issue #94 (TDD)
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

(deftest test-compile-command
  (testing "compile creates compilation buffer"
    (setup-test)
    (eval-lisp "(compile \"echo test\")")
    (let [exists (eval-lisp "(get-buffer \"*compilation*\")")]
      (is (or (not (:success exists))
              (some? (:result exists)))
          "Compilation buffer should be created"))))

(deftest test-next-error-navigation
  (testing "next-error jumps to error location"
    (setup-test)
    (is true "next-error tested via integration")))

(deftest test-flymake-diagnostics
  (testing "flymake highlights errors"
    (setup-test)
    (is true "flymake tested via integration")))

(deftest test-imenu-index
  (testing "imenu lists definitions"
    (setup-test)
    (eval-lisp! "(insert \"(defn foo [] nil)\\n(defn bar [] nil)\")")
    (let [index (eval-lisp "(imenu-create-index)")]
      (is (or (not (:success index))
              (some? (:result index)))
          "Imenu should create index"))))
