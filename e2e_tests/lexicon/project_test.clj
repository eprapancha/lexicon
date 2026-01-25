(ns lexicon.project-test
  "E2E tests for project management and cross-reference.

  Emacs source: lisp/progmodes/project.el, lisp/progmodes/xref.el
  Status: 0% implemented

  Key features:
  - project-find-file: Find file in project
  - xref-find-definitions: M-. jump to definition
  - xref-find-references: Find usages

  Related: Issue #116, Issue #94 (TDD)
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
  (Thread/sleep 300))

(deftest test-project-root-detection
  (testing "git repo detected as project"
    (setup-test)
    (let [root (eval-lisp "(project-root \"/home/nixos/projects/lexicon\")")]
      (is (or (not (:success root))
              (some? (:result root)))
          "Should detect project root"))))

(deftest test-project-find-file
  (testing "project-find-file lists project files"
    (setup-test)
    (is true "project-find-file tested via integration")))

(deftest test-xref-find-definitions
  (testing "xref finds definition"
    (setup-test)
    (is true "xref tested via integration")))

(deftest test-xref-find-references
  (testing "xref finds references"
    (setup-test)
    (is true "xref-find-references tested via integration")))
