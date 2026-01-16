(ns lexicon.filesystem.high.dired-readiness-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:high buffer-contents-can-be-regenerated
  "Emacs invariant:
   Some buffers derive contents from computation and can be refreshed
   without losing identity.

   Required for:
   - Dired
   - Magit
   - Help buffers
   - Org agenda

   Status: PENDING"
  (is false))
