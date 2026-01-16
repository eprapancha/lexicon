(ns lexicon.interaction.critical.major-minor-modes-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical exactly-one-major-mode-per-buffer
  "Emacs invariant:
   Each buffer has exactly one major mode.

   Required for:
   - syntax
   - indentation
   - Evil state defaults

   Status: ACTIVE"
  (h/with-editor
    (h/with-buffer \"a\"
      (h/enable-major-mode 'text-mode)
      (is (= 'text-mode (h/current-major-mode)))
      (h/enable-major-mode 'prog-mode)
      (is (= 'prog-mode (h/current-major-mode))))))

(deftest ^:critical multiple-minor-modes-can-coexist
  "Emacs invariant:
   Minor modes are orthogonal, composable features.

   Required for:
   - Evil
   - Flycheck
   - Company/Corfu

   Status: ACTIVE"
  (h/with-editor
    (h/enable-minor-mode 'line-numbers-mode)
    (h/enable-minor-mode 'whitespace-mode)
    (is (h/minor-mode-enabled? 'line-numbers-mode))
    (is (h/minor-mode-enabled? 'whitespace-mode))))
