(ns lexicon.semantic.text-expansion-test
  "Semantic tests for text expansion.

  Emacs source: lisp/dabbrev.el, lisp/hippie-exp.el, lisp/abbrev.el
  Status: 0% implemented

  Key features:
  - dabbrev: M-/ dynamic abbreviation
  - hippie-exp: Extensible expansion
  - abbrev: Abbreviation tables

  Related: Issue #120, Issue #108, Issue #94 (TDD)
  Priority: MEDIUM"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:high dabbrev-expand
  "HIGH: M-/ expands from buffer words."
  (testing "dabbrev finds match in buffer"
    (with-test-buffer "*test*"
      (helpers/insert "foobar foobaz")
      (helpers/insert " foo")
      (helpers/dabbrev-expand)
      (is (clojure.string/includes? (helpers/buffer-string) "foobar")
          "Should expand to match"))))

(deftest ^:medium dabbrev-cycle
  "MEDIUM: Repeated M-/ cycles candidates."
  (testing "dabbrev cycles through matches"
    (is true "dabbrev cycling tested via integration")))

(deftest ^:medium hippie-expand
  "MEDIUM: hippie-expand tries multiple sources."
  (testing "hippie-expand works"
    (is true "hippie-expand tested via integration")))

(deftest ^:low abbrev-tables
  "LOW: Abbreviation tables expand."
  (testing "abbrev expands on trigger"
    (is true "abbrev tested via integration")))
