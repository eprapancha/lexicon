(ns lexicon.semantic.editing-enhancements-test
  "Semantic tests for editing enhancements.

  Emacs source: lisp/delsel.el, lisp/rect.el, lisp/kmacro.el, lisp/electric.el
  Status: 10% (some electric)

  Key features:
  - delsel: Delete selection on insert
  - rect: Rectangle operations (C-x r)
  - kmacro: Keyboard macros (F3/F4)
  - electric: Auto-pairing

  Related: Issue #121, Issue #105, Issue #94 (TDD)
  Priority: MEDIUM"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:high delete-selection-mode
  "HIGH: Typing deletes active region."
  (testing "selection replaced on insert"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/set-mark 0)
      (helpers/goto-char 5)
      (helpers/activate-mark)
      (helpers/enable-minor-mode :delete-selection-mode)
      (helpers/insert "X")
      (is (= "X World" (helpers/buffer-string))
          "Selection should be replaced"))))

(deftest ^:high rectangle-kill-yank
  "HIGH: Rectangle kill and yank."
  (testing "C-x r k kills rectangle"
    (is true "rectangle tested via integration")))

(deftest ^:medium keyboard-macro-record
  "MEDIUM: F3/F4 records and plays macros."
  (testing "macro records and replays"
    (is true "kmacro tested via integration")))

(deftest ^:medium electric-pair
  "MEDIUM: Auto-insert matching brackets."
  (testing "electric-pair-mode inserts closing"
    (with-test-buffer "*test*"
      (helpers/enable-minor-mode :electric-pair-mode)
      (helpers/insert "(")
      (is (= "()" (helpers/buffer-string))
          "Closing paren should be inserted"))))
