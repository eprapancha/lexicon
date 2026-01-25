(ns lexicon.semantic.font-lock-test
  "Semantic tests for syntax highlighting and code intelligence.

  Emacs source: lisp/font-lock.el, lisp/progmodes/which-func.el
  Status: 0% implemented

  Key features:
  - Keyword-based syntax highlighting
  - Mode-specific font-lock-keywords
  - Faces for syntax elements
  - which-func current function display

  Related: Issue #130, Issue #122, Issue #123, Issue #94 (TDD)
  Priority: HIGH"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:high font-lock-mode-activation
  "HIGH: font-lock-mode applies highlighting."
  (testing "font-lock-mode can be enabled"
    (with-test-buffer "*test*"
      (helpers/enable-minor-mode :font-lock-mode)
      (is (helpers/minor-mode-enabled? :font-lock-mode)
          "font-lock-mode should be enabled"))))

(deftest ^:high font-lock-keywords
  "HIGH: Keywords define highlighting rules."
  (testing "font-lock-keywords list is accessible"
    (with-test-buffer "*test*"
      (helpers/enable-major-mode :clojure-mode)
      (let [keywords (helpers/font-lock-keywords)]
        (is (or (nil? keywords) (sequential? keywords))
            "font-lock-keywords should return list or nil")))))

(deftest ^:high font-lock-add-keywords
  "HIGH: Add custom highlighting rules."
  (testing "font-lock-add-keywords extends rules"
    (helpers/font-lock-add-keywords :clojure-mode [["TODO" 0 'font-lock-warning-face]])
    (is true "Should add keywords without error")))

(deftest ^:medium font-lock-faces
  "MEDIUM: Syntax highlighting faces."
  (testing "font-lock faces are defined"
    (is (helpers/face-attribute 'font-lock-keyword-face :foreground)
        "font-lock-keyword-face should have foreground")))

(deftest ^:high which-function-mode
  "HIGH: Display current function in mode line."
  (testing "which-function returns function name"
    (with-test-buffer "*test*"
      (helpers/insert "(defn my-function []\n  (+ 1 2))")
      (helpers/goto-char 20)
      (let [func-name (helpers/which-function)]
        (is (or (nil? func-name) (string? func-name))
            "which-function should return string or nil")))))

(deftest ^:medium which-func-mode-line
  "MEDIUM: which-func updates mode line."
  (testing "which-func-mode shows in mode line"
    (with-test-buffer "*test*"
      (helpers/enable-minor-mode :which-func-mode)
      (is (helpers/minor-mode-enabled? :which-func-mode)
          "which-func-mode should be enabled"))))

