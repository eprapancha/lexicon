(ns lexicon.interaction.high.narrowing-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:high narrowing-restricts-visible-region-only
  "Emacs invariant:
   Narrowing does not delete text — it limits visibility.

   Required for:
   - Org
   - code folding

   Status: PENDING"
  (is false))
