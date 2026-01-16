(ns lexicon.history.high.undo-integration-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:high undo-restores-point-and-mark
  "Emacs invariant:
   Undo restores cursor and region state.

   Required for:
   - usability
   - Evil correctness

   Status: PENDING"
  (is false))
