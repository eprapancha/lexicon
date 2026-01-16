(ns lexicon.filesystem.high.project-semantics-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:high project-membership-is-derived
  "Emacs invariant:
   Project membership is derived from filesystem context,
   not intrinsic to buffers.

   Required for:
   - project.el
   - refactoring
   - multi-project workflows

   Status: PENDING"
  (is false))
