(ns lexicon.lisp.high.hooks-and-advice-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:high hooks-run-in-dynamic-context
  "Emacs invariant:
   Hooks run with full dynamic binding and editor context.

   Required for:
   - mode hooks
   - project hooks
   - Magit

   Status: PENDING"
  (is false))

(deftest ^:high advice-wraps-commands-not-functions
  "Emacs invariant:
   Advice applies to command invocation, not raw functions.

   Required for:
   - instrumentation
   - debugging
   - packages

   Status: PENDING"
  (is false))
