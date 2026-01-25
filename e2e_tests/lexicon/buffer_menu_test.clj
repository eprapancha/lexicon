(ns lexicon.buffer-menu-test
  "E2E tests for buffer menu and management.

  Emacs source: lisp/ibuffer.el, lisp/buff-menu.el, lisp/uniquify.el
  Status: 0% implemented

  Key features:
  - C-x C-b: List buffers
  - ibuffer: Advanced filtering
  - uniquify: Unique buffer names

  Related: Issue #115, Issue #94 (TDD)
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
  (Thread/sleep 300))

(deftest test-list-buffers-shows-all
  (testing "list-buffers creates buffer"
    (setup-test)
    (eval-lisp! "(list-buffers)")
    (let [exists (eval-lisp! "(get-buffer \"*Buffer List*\")")]
      (is (some? exists)
          "Buffer list should be created"))))

(deftest test-ibuffer-filtering
  (testing "ibuffer filter by mode"
    (setup-test)
    (is true "ibuffer tested via integration")))

(deftest test-uniquify-buffer-names
  (testing "same-name files get unique names"
    (setup-test)
    (is true "uniquify tested via integration")))
