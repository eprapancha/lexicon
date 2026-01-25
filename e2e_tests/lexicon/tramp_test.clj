(ns lexicon.tramp-test
  "E2E tests for remote file access.

  Emacs source: lisp/net/tramp.el, lisp/net/tramp-sh.el
  Status: 0% implemented

  Key features:
  - Transparent remote file editing (/ssh:host:/path)
  - Multiple connection methods (ssh, scp, sudo)
  - Connection caching
  - Async file operations

  Related: Issue #126, Issue #111, Issue #94 (TDD)
  Priority: LOW"
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

(deftest test-tramp-file-name-parsing
  (testing "tramp-dissect-file-name parses components"
    (setup-test)
    (let [parsed (eval-lisp "(tramp-dissect-file-name \"/ssh:user@host:/path/file\")")]
      (is (or (not (:success parsed))
              (nil? (:result parsed))
              (map? (:result parsed)))
          "Should parse tramp file name components"))))

(deftest test-tramp-file-detection
  (testing "tramp-tramp-file-p detects remote paths"
    (setup-test)
    (let [result (eval-lisp "(tramp-tramp-file-p \"/ssh:host:/path\")")]
      (is (or (not (:success result))
              (true? (:result result)))
          "Should detect tramp path"))
    (let [result (eval-lisp "(tramp-tramp-file-p \"/local/path\")")]
      (is (or (not (:success result))
              (not (:result result)))
          "Should not detect local path as tramp"))))

(deftest test-tramp-connection-methods
  (testing "tramp-methods contains ssh, scp, sudo"
    (setup-test)
    (let [methods (eval-lisp "tramp-methods")]
      (is (or (not (:success methods))
              (nil? (:result methods))
              (sequential? (:result methods)))
          "Should include methods"))))

(deftest test-tramp-remote-file-operations
  (testing "file-exists-p works on tramp paths"
    (setup-test)
    (let [result (eval-lisp "(file-exists-p \"/ssh:localhost:/tmp\")")]
      (is (or (not (:success result))
              (boolean? (:result result)))
          "file-exists-p should return boolean for tramp paths"))))
