(ns lexicon.lisp.high.introspection-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:high symbols-are-inspectable
  "Emacs invariant:
   Symbols expose value, function, documentation, and bindings.

   Required for:
   - help
   - completion
   - discoverability

   Status: PENDING"
  (is false))
