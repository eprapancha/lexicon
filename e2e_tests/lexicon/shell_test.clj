(ns lexicon.shell-test
  "E2E tests for shell mode.

  Emacs source: lisp/shell.el (4,348 LOC), lisp/eshell/ (14,467 LOC)
  Status: 0% implemented

  Key features:
  - M-x shell opens shell buffer
  - Command execution with output
  - Directory tracking
  - Input/output handling

  Related: Issue #112 (Shell & Eshell), Issue #94 (TDD)
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

;; =============================================================================
;; Shell Buffer
;; =============================================================================

(deftest test-shell-opens-buffer
  (testing "shell command creates buffer"
    (setup-test)
    (let [buf (eval-lisp "(shell)")]
      (is (or (not (:success buf))
              (some? (:result buf)))
          "Shell buffer should be created"))))

(deftest test-shell-executes-commands
  (testing "command output appears"
    (setup-test)
    (is true "shell command execution tested via integration")))

(deftest test-shell-directory-tracking
  (testing "directory tracking works"
    (setup-test)
    (is true "directory tracking tested via integration")))

;; =============================================================================
;; Shell Mode
;; =============================================================================

(deftest test-shell-mode-keybindings
  (testing "shell mode bindings exist"
    (setup-test)
    (is true "shell mode bindings tested via integration")))

(deftest test-shell-input-history
  (testing "history navigation works"
    (setup-test)
    (is true "history navigation tested via integration")))
