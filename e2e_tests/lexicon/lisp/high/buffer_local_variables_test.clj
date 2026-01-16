(ns lexicon.lisp.high.buffer-local-variables-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:high variables-can-be-buffer-local
  "Emacs invariant:
   Variables may have buffer-local bindings.

   Required for:
   - major modes
   - minor modes
   - configuration

   Status: PENDING"
  (is false))
