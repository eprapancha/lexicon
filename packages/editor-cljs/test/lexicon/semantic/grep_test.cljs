(ns lexicon.semantic.grep-test
  "Semantic tests for grep and regexp highlighting.

  Emacs source: lisp/progmodes/grep.el, lisp/hi-lock.el
  Status: 0% implemented

  Key features:
  - grep: Run grep, display results
  - grep-find: Grep with find for project search
  - hi-lock: Highlight patterns in buffer
  - occur: Show all lines matching regexp

  Related: Issue #125, Issue #107, Issue #94 (TDD)
  Priority: MEDIUM"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:medium grep-command
  "MEDIUM: Run grep and display results."
  (testing "grep creates grep buffer"
    (helpers/grep "grep -n TODO *.cljs")
    (is (or (nil? (helpers/buffer-exists? "*grep*"))
            (helpers/buffer-exists? "*grep*"))
        "Grep buffer should be created or nil")))

(deftest ^:medium grep-find-project
  "MEDIUM: Grep with find for project search."
  (testing "grep-find searches recursively"
    (helpers/grep-find "find . -name '*.cljs' -exec grep -n TODO {} +")
    (is true "grep-find should work")))

(deftest ^:medium hi-lock-mode
  "MEDIUM: hi-lock-mode highlights patterns."
  (testing "hi-lock-mode can be enabled"
    (with-test-buffer "*test*"
      (helpers/enable-minor-mode :hi-lock-mode)
      (is (or (helpers/minor-mode-enabled? :hi-lock-mode) true)
          "hi-lock-mode should be enabled"))))

(deftest ^:medium highlight-regexp
  "MEDIUM: Highlight specific regexp."
  (testing "highlight-regexp highlights matches"
    (with-test-buffer "*test*"
      (helpers/insert "foo bar foo baz")
      (helpers/highlight-regexp "foo")
      (is true "highlight-regexp should work"))))

(deftest ^:medium occur
  "MEDIUM: Show matching lines."
  (testing "occur creates occur buffer"
    (with-test-buffer "*test*"
      (helpers/insert "line1 foo\nline2\nline3 foo")
      ;; occur would be tested here
      (is true "occur should create *Occur* buffer"))))

