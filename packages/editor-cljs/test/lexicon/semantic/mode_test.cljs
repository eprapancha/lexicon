(ns lexicon.semantic.mode-test
  "Emacs semantic compatibility tests for major/minor modes.

  Tests from Epic #86, Issue #90 - Interaction Semantics"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h])
  (:require-macros [lexicon.semantic.helpers-macros :refer [with-buffer]]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical exactly-one-major-mode-per-buffer
  (testing "Emacs invariant: Each buffer has exactly one major mode"
    (h/reset-editor-db!)
    (with-buffer "a"
      (h/enable-major-mode 'text-mode)
      (is (= 'text-mode (h/current-major-mode)) "Major mode should be text-mode")
      (h/enable-major-mode 'prog-mode)
      (is (= 'prog-mode (h/current-major-mode)) "Setting new major mode replaces old one"))))

(deftest ^:critical multiple-minor-modes-can-coexist
  (testing "Emacs invariant: Minor modes are orthogonal, composable features"
    (h/reset-editor-db!)
    (h/enable-minor-mode 'line-numbers-mode)
    (h/enable-minor-mode 'whitespace-mode)
    (is (h/minor-mode-enabled? 'line-numbers-mode) "First minor mode enabled")
    (is (h/minor-mode-enabled? 'whitespace-mode) "Second minor mode enabled")
    (is (h/minor-mode-enabled? 'line-numbers-mode) "First minor mode still enabled")))
