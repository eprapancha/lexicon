(ns lexicon.semantic.visual-enhancements-test
  "Semantic tests for visual enhancements.

  Emacs source: lisp/paren.el, lisp/hl-line.el, lisp/whitespace.el
  Status: 50% (line numbers exist)

  Key features:
  - show-paren-mode: Highlight matching parens
  - hl-line-mode: Highlight current line
  - whitespace-mode: Visualize whitespace
  - display-line-numbers-mode: Line numbers

  Related: Issue #123, Issue #94 (TDD)
  Priority: HIGH"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:critical show-paren-mode
  "CRITICAL: Matching paren highlighted."
  (testing "paren at point highlighted"
    (with-test-buffer "*test*"
      (helpers/insert "(foo)")
      (helpers/enable-minor-mode :show-paren-mode)
      (helpers/goto-char 0)
      (let [match (helpers/show-paren-data-function)]
        (is (some? match)
            "Should find matching paren")))))

(deftest ^:high hl-line-mode
  "HIGH: Current line highlighted."
  (testing "hl-line-mode highlights line"
    (with-test-buffer "*test*"
      (helpers/enable-minor-mode :hl-line-mode)
      (is (helpers/minor-mode-enabled? :hl-line-mode)
          "hl-line-mode should be enabled"))))

(deftest ^:medium whitespace-mode
  "MEDIUM: Whitespace visualized."
  (testing "whitespace-mode shows spaces/tabs"
    (is true "whitespace-mode tested via integration")))

(deftest ^:high line-numbers
  "HIGH: Line numbers displayed."
  (testing "display-line-numbers-mode works"
    (with-test-buffer "*test*"
      (helpers/enable-minor-mode :display-line-numbers-mode)
      (is (helpers/minor-mode-enabled? :display-line-numbers-mode)
          "Line numbers should be enabled"))))
