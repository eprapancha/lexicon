(ns lexicon.history.high.command-history-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:high command-history-is-queryable
  "Emacs invariant:
   Command history is persistent and inspectable.

   Required for:
   - M-x
   - Vertico
   - command repetition

   Status: PENDING"
  (is false))
