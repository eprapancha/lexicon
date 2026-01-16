(ns lexicon.e2e.editor-semantics.minibuffer-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical minibuffer-is-a-buffer-with-keymap
  "Emacs invariant:
   The minibuffer is a buffer with its own keymap and participates
   in normal command/keymap resolution.

   Required for:
   - M-x
   - Vertico
   - Orderless
   - Embark

   Status: PENDING"
  (is false))
