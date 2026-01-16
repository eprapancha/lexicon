(ns lexicon.core.high.completion-tables-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical completion-table-is-a-function
  "Emacs invariant:
   Completion sources are functions, not static lists.

   Required for:
   - Orderless
   - Vertico
   - Corfu

   Status: PENDING"
  (is false))

(deftest ^:high completion-style-can-be-swapped
  "Emacs invariant:
   Completion logic must be decoupled from UI and swappable.

   Status: PENDING"
  (is false))
