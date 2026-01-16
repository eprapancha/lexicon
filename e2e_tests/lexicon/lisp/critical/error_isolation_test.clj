(ns lexicon.lisp.critical.error-isolation-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical lisp-errors-do-not-corrupt-editor-state
  "Emacs invariant:
   Errors in Lisp code must not corrupt editor state.

   Required for:
   - trust
   - live hacking
   - resilience

   Status: ACTIVE"
  (h/with-editor
    (let [before (h/editor-snapshot)]
      (try
        (h/eval-lisp '(+ 1 :boom))
        (catch :default _))
      (is (= before (h/editor-snapshot))))))
