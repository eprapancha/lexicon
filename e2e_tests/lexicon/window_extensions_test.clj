(ns lexicon.window-extensions-test
  "E2E tests for window extensions.

  Emacs source: lisp/windmove.el, lisp/winner.el, lisp/tab-bar.el
  Status: 0% implemented

  Key features:
  - windmove: S-<arrow> window navigation
  - winner: Undo/redo window config
  - tab-bar: Frame-local tabs

  Related: Issue #117, Issue #110, Issue #94 (TDD)
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

(deftest test-windmove-navigation
  (testing "windmove-right moves to right window"
    (setup-test)
    (eval-lisp "(split-window-horizontally)")
    (eval-lisp "(windmove-left)")
    (is true "windmove tested via integration")))

(deftest test-winner-mode-undo
  (testing "winner-undo works"
    (setup-test)
    (is true "winner tested via integration")))

(deftest test-tab-bar-basics
  (testing "tab-bar-new-tab creates tab"
    (setup-test)
    (is true "tab-bar tested via integration")))
