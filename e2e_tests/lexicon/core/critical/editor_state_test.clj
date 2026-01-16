(ns lexicon.core.critical.editor-state-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical editor-state-mutations-are-serialized
  "Emacs invariant:
   All editor state mutations must pass through a single, explicit
   mutation boundary. No component may mutate state implicitly.

   This invariant enables:
   - undo / redo
   - replay
   - deterministic tests
   - fast mode

   Status: ACTIVE"
  (h/with-instrumented-editor
    (h/insert-text "x")
    (is (h/all-mutations-went-through-dispatch?))))
