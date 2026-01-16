(ns lexicon.history.critical.undo-boundary-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical commands-create-undo-boundaries
  "Emacs invariant:
   Undo groups operations by command invocation, not keystrokes.

   Required for:
   - sane undo
   - Evil
   - macros

   Status: ACTIVE"
  (h/with-editor
    (h/execute-command
     (fn []
       (h/insert-text \"a\")
       (h/insert-text \"b\")))
    (h/undo)
    (is (= \"\" (h/current-buffer-text)))))

(deftest ^:critical undo-boundary-can-be-manually-inserted
  "Emacs invariant:
   Code can explicitly insert undo boundaries.

   Required for:
   - complex commands
   - packages

   Status: PENDING"
  (is false))
