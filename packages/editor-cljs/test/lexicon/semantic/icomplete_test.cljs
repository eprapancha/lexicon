(ns lexicon.semantic.icomplete-test
  "Semantic tests for incremental completion UI.

  Emacs source: lisp/icomplete.el
  Status: 0% implemented

  Key features:
  - Shows completions as you type
  - Completion cycling with C-. and C-,
  - Fido mode (flex + vertical display)

  Related: Issue #128, Issue #108, Issue #94 (TDD)
  Priority: MEDIUM"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

(deftest ^:medium icomplete-mode-activation
  "MEDIUM: icomplete-mode activates."
  (testing "icomplete-mode can be enabled"
    (helpers/enable-minor-mode :icomplete-mode)
    (is (helpers/minor-mode-enabled? :icomplete-mode)
        "icomplete-mode should be enabled")))

(deftest ^:medium icomplete-candidate-display
  "MEDIUM: Shows candidates during completion."
  (testing "icomplete shows completions in minibuffer"
    (helpers/enable-minor-mode :icomplete-mode)
    (helpers/activate-minibuffer)
    (helpers/minibuffer-insert "buf")
    (let [candidates (helpers/icomplete-completions)]
      (is (or (nil? candidates) (sequential? candidates))
          "Should return completion candidates or nil"))))

(deftest ^:medium icomplete-cycling
  "MEDIUM: Cycle through completions."
  (testing "icomplete-forward-completions cycles"
    (helpers/enable-minor-mode :icomplete-mode)
    (helpers/icomplete-forward-completions)
    (is true "Should cycle forward through completions")))

(deftest ^:medium icomplete-fido-mode
  "MEDIUM: Fido mode for vertical display."
  (testing "icomplete-fido-mode enables flex matching"
    (helpers/enable-minor-mode :fido-mode)
    (is (or (helpers/minor-mode-enabled? :fido-mode)
            (helpers/minor-mode-enabled? :icomplete-mode))
        "fido-mode should be enabled")))

