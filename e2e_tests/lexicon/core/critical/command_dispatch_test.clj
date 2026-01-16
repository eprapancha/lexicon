(ns lexicon.core.critical.command-dispatch-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical command-has-metadata-and-identity
  "Emacs invariant:
   Commands are inspectable entities, not bare functions.
   They must carry identity, documentation, and invocation semantics.

   Required for:
   - M-x
   - help
   - completion
   - Embark

   Status: ACTIVE"
  (let [cmd (h/define-test-command
              {:name 'test/hello
               :doc  "\"Say hello\""
               :fn   (fn [_] nil)})]
    (is (symbol? (h/command-name cmd)))
    (is (string? (h/command-doc cmd)))
    (is (fn? (h/command-fn cmd)))))

(deftest ^:critical all-command-invocations-go-through-dispatch
  "Emacs invariant:
   No command may be executed directly. All invocation must pass
   through the command dispatcher.

   Required for:
   - interactivity
   - advice
   - hooks
   - undo

   Status: ACTIVE"
  (h/with-instrumented-dispatch
    (h/invoke-command 'test/hello)
    (is (h/dispatch-was-used?))))
