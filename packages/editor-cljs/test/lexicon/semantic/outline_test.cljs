(ns lexicon.semantic.outline-test
  "Semantic tests for outline and code folding.

  Emacs source: lisp/outline.el, lisp/progmodes/hideshow.el
  Status: 0% implemented

  Key features:
  - Outline headings can be collapsed
  - hs-minor-mode hides code blocks
  - Uses text properties (invisible)

  Related: Issue #114 (Outline & Folding), Issue #94 (TDD)
  Priority: LOW"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; Outline Mode
;;; =============================================================================

(deftest ^:medium outline-mode-basics
  "MEDIUM: Outline mode collapses headings.

  Emacs Semantics (outline.el):
  - Headings identified by outline-regexp
  - Children can be hidden/shown
  - Uses invisible text property

  Why this matters:
  - Hierarchical document navigation"
  (testing "outline headings identified"
    (with-test-buffer "*test*"
      (helpers/insert "* Heading 1\nContent\n** Heading 2\nMore content")
      (helpers/enable-minor-mode :outline-minor-mode)

      (is (helpers/minor-mode-enabled? :outline-minor-mode)
          "Outline minor mode should be enabled")))

  (testing "heading can be collapsed"
    (with-test-buffer "*test*"
      (helpers/insert "* Heading\nHidden content\nMore hidden")
      (helpers/enable-minor-mode :outline-minor-mode)
      (helpers/goto-char 0)

      (helpers/outline-hide-subtree)

      ;; Content should be invisible
      (is (not= (helpers/buffer-string) (helpers/visible-text))
          "Some content should be hidden"))))

(deftest ^:medium outline-navigation
  "MEDIUM: Navigate between outline headings.

  Commands:
  - C-c C-n: next heading
  - C-c C-p: previous heading
  - C-c C-f: next same level
  - C-c C-u: up heading

  Why this matters:
  - Efficient document navigation"
  (testing "next heading navigation"
    (with-test-buffer "*test*"
      (helpers/insert "* H1\nContent\n* H2\nContent\n* H3")
      (helpers/enable-minor-mode :outline-minor-mode)
      (helpers/goto-char 0)

      (helpers/outline-next-heading)

      (is (> (helpers/point) 0)
          "Should move to next heading"))))

;;; =============================================================================
;;; Hideshow (Code Folding)
;;; =============================================================================

(deftest ^:medium hs-minor-mode-basics
  "MEDIUM: hs-minor-mode folds code blocks.

  Emacs Semantics (hideshow.el):
  - Detects block delimiters ({}, begin/end)
  - Hides block content
  - Shows ellipsis indicator

  Why this matters:
  - Code navigation in large files"
  (testing "hs-minor-mode enables"
    (with-test-buffer "*test*"
      (helpers/insert "function foo() {\n  body\n}")
      (helpers/enable-minor-mode :hs-minor-mode)

      (is (helpers/minor-mode-enabled? :hs-minor-mode)
          "hs-minor-mode should be enabled")))

  (testing "code block can be hidden"
    (with-test-buffer "*test*"
      (helpers/insert "function foo() {\n  body\n}")
      (helpers/enable-minor-mode :hs-minor-mode)
      (helpers/goto-char 0)

      (helpers/hs-hide-block)

      ;; Block content should be invisible
      (is (clojure.string/includes? (helpers/visible-text) "function")
          "Function declaration visible")
      (is (not (clojure.string/includes? (helpers/visible-text) "body"))
          "Block body should be hidden"))))

(deftest ^:low hs-toggle-hiding
  "LOW: Toggle visibility of code block.

  Emacs Semantics:
  - C-c @ C-c toggles block at point
  - Remembers fold state

  Why this matters:
  - Quick show/hide"
  (testing "toggle hiding works"
    (with-test-buffer "*test*"
      (helpers/insert "function foo() {\n  body\n}")
      (helpers/enable-minor-mode :hs-minor-mode)
      (helpers/goto-char 0)

      (helpers/hs-toggle-hiding)  ; Hide
      (let [hidden-text (helpers/visible-text)]
        (helpers/hs-toggle-hiding)  ; Show
        (is (not= hidden-text (helpers/visible-text))
            "Toggle should change visibility")))))

;;; =============================================================================
;;; Integration with Text Properties
;;; =============================================================================

(deftest ^:high folding-uses-invisible-property
  "HIGH: Folding uses invisible text property.

  Emacs Semantics:
  - Hidden text has (invisible . t) property
  - Rendering layer respects property
  - Text still in buffer for search

  Why this matters:
  - Consistent with text property system
  - Depends on Issue #102 implementation"
  (testing "hidden text has invisible property"
    (with-test-buffer "*test*"
      (helpers/insert "* Heading\nHidden content")
      (helpers/enable-minor-mode :outline-minor-mode)
      (helpers/goto-char 0)
      (helpers/outline-hide-subtree)

      ;; Check invisible property on hidden text
      (let [prop (helpers/get-text-property 10 'invisible)]
        (is (some? prop)
            "Hidden text should have invisible property")))))
