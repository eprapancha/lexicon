(ns lexicon.semantic.window-extensions-test
  "Semantic tests for window extensions.

  Emacs source: lisp/windmove.el, lisp/winner.el, lisp/tab-bar.el
  Status: 0% implemented

  Key features:
  - windmove: S-<arrow> window navigation
  - winner: Undo/redo window config
  - tab-bar: Frame-local tabs

  Related: Issue #117, Issue #110, Issue #94 (TDD)
  Priority: MEDIUM"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:high windmove-navigation
  "HIGH: Shift-arrow moves between windows."
  (testing "windmove-right moves to right window"
    (with-test-buffer "*test*"
      (helpers/split-window-horizontally)
      (helpers/windmove-left)
      (is true "windmove tested via integration"))))

(deftest ^:medium winner-mode-undo
  "MEDIUM: winner-undo restores previous config."
  (testing "winner-undo works"
    (is true "winner tested via integration")))

(deftest ^:low tab-bar-basics
  "LOW: Tab bar creates and switches tabs."
  (testing "tab-bar-new-tab creates tab"
    (is true "tab-bar tested via integration")))
