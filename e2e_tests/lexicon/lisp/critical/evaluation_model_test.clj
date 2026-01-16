(ns lexicon.lisp.critical.evaluation-model-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical evaluation-has-editor-context
  "Emacs invariant:
   Lisp evaluation always occurs within an editor context
   (current buffer, point, variables).

   Required for:
   - interactive commands
   - hooks
   - advice
   - buffer-local behavior

   Status: PENDING"
  (h/with-editor
    (h/with-buffer \"a\"
      (h/eval-lisp
       '(is (= \"a\" (buffer-name)))))))
