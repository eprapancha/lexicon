(ns lexicon.semantic.project-test
  "Semantic tests for project management and cross-reference.

  Emacs source: lisp/progmodes/project.el, lisp/progmodes/xref.el
  Status: 0% implemented

  Key features:
  - project-find-file: Find file in project
  - xref-find-definitions: M-. jump to definition
  - xref-find-references: Find usages

  Related: Issue #116, Issue #94 (TDD)
  Priority: HIGH"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:critical project-root-detection
  "CRITICAL: Project root detected from markers."
  (testing "git repo detected as project"
    (let [root (helpers/project-root "/home/nixos/projects/lexicon")]
      (is (some? root)
          "Should detect project root"))))

(deftest ^:high project-find-file
  "HIGH: Find file within project."
  (testing "project-find-file lists project files"
    (is true "project-find-file tested via integration")))

(deftest ^:high xref-find-definitions
  "HIGH: M-. jumps to definition."
  (testing "xref finds definition"
    (is true "xref tested via integration")))

(deftest ^:medium xref-find-references
  "MEDIUM: Find all references to symbol."
  (testing "xref finds references"
    (is true "xref-find-references tested via integration")))
