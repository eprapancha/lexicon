(ns lexicon.semantic.isearch-test
  "Semantic tests for incremental search (isearch).

  Emacs source: lisp/isearch.el (4,638 LOC)
  Status: 70% implemented

  Key features:
  - C-s: Search forward incrementally
  - C-r: Search backward incrementally
  - M-c: Toggle case sensitivity
  - Wrapping search

  Related: Issue #107 (Search & Replace), Issue #94 (TDD)
  Priority: HIGH"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; Basic Search
;;; =============================================================================

(deftest ^:critical isearch-forward-basic
  "CRITICAL: C-s finds next occurrence of search string.

  Emacs Semantics (isearch.el):
  - Enter isearch mode
  - Type characters to search
  - Point moves to match
  - C-s again finds next

  Why this matters:
  - Primary navigation method"
  (testing "search forward finds match"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World Hello")
      (helpers/goto-char 0)

      (helpers/isearch-forward "World")

      (is (= 6 (helpers/point))
          "Point should move to start of match")))

  (testing "repeated C-s finds next match"
    (with-test-buffer "*test*"
      (helpers/insert "one two one three one")
      (helpers/goto-char 0)

      (helpers/isearch-forward "one")
      (is (= 0 (helpers/point)))

      (helpers/isearch-forward-repeat)
      (is (= 8 (helpers/point)) "Second match")

      (helpers/isearch-forward-repeat)
      (is (= 18 (helpers/point)) "Third match"))))

(deftest ^:critical isearch-backward-basic
  "CRITICAL: C-r finds previous occurrence.

  Emacs Semantics:
  - Search backward from point
  - C-r again finds previous

  Why this matters:
  - Navigate backward through matches"
  (testing "search backward finds match"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World Hello")
      (helpers/goto-char 17)  ; End

      (helpers/isearch-backward "Hello")

      (is (= 12 (helpers/point))
          "Should find last 'Hello'"))))

(deftest ^:high isearch-wrapping
  "HIGH: Search wraps around buffer.

  Emacs Semantics:
  - When no more matches forward, wrap to beginning
  - Display 'Wrapped' message
  - C-s again continues from top

  Why this matters:
  - Don't get stuck at end of buffer"
  (testing "search wraps to beginning"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/goto-char 11)  ; End

      (helpers/isearch-forward "Hello")

      ;; Should wrap and find at beginning
      (is (= 0 (helpers/point))
          "Should wrap to beginning"))))

(deftest ^:high isearch-case-toggle
  "HIGH: M-c toggles case sensitivity.

  Emacs Semantics:
  - Default: smart case (case-insensitive unless uppercase used)
  - M-c toggles explicit case sensitivity

  Why this matters:
  - Flexible search behavior"
  (testing "case insensitive by default"
    (with-test-buffer "*test*"
      (helpers/insert "Hello HELLO hello")
      (helpers/goto-char 0)

      (helpers/isearch-forward "hello")

      ;; Should match first occurrence regardless of case
      (is (< (helpers/point) 6)
          "Should find match case-insensitively"))))

;;; =============================================================================
;;; Query Replace
;;; =============================================================================

(deftest ^:high query-replace-basic
  "HIGH: M-% prompts for replacement at each match.

  Emacs Semantics:
  - M-% prompts for search, then replacement
  - At each match: y (yes), n (no), ! (all), q (quit)

  Why this matters:
  - Controlled search and replace"
  (testing "query-replace replaces matches"
    (with-test-buffer "*test*"
      (helpers/insert "foo bar foo baz foo")
      (helpers/goto-char 0)

      ;; Replace all (simulated ! response)
      (helpers/query-replace "foo" "XXX" :all)

      (is (= "XXX bar XXX baz XXX" (helpers/buffer-string))
          "All matches replaced"))))
