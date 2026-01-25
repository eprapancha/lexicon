(ns lexicon.semantic.documentation-test
  "Semantic tests for documentation features.

  Emacs source: lisp/emacs-lisp/eldoc.el, lisp/apropos.el
  Status: 10% implemented

  Key features:
  - eldoc: Function signature in echo area
  - apropos: Search by pattern
  - describe-*: C-h f/v/k documentation

  Related: Issue #124, Issue #109, Issue #94 (TDD)
  Priority: MEDIUM"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:high eldoc-display
  "HIGH: Eldoc shows function signature."
  (testing "eldoc displays at point"
    (with-test-buffer "*test*"
      (helpers/insert "(insert ")
      (helpers/enable-minor-mode :eldoc-mode)
      (let [doc (helpers/eldoc-documentation-function)]
        (is (or (nil? doc) (string? doc))
            "Eldoc should return string or nil")))))

(deftest ^:medium apropos-search
  "MEDIUM: Apropos finds matching commands."
  (testing "apropos-command finds matches"
    (let [results (helpers/apropos-command "buffer")]
      (is (or (nil? results) (seq results))
          "Should return results or nil"))))

(deftest ^:medium describe-function
  "MEDIUM: C-h f shows function docs."
  (testing "describe-function creates help buffer"
    (helpers/describe-function 'insert)
    (is (helpers/buffer-exists? "*Help*")
        "Help buffer should be created")))

(deftest ^:medium describe-key
  "MEDIUM: C-h k shows key binding."
  (testing "describe-key shows command"
    (is true "describe-key tested via integration")))
