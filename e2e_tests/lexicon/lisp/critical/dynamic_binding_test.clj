(ns lexicon.lisp.critical.dynamic-binding-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical let-is-dynamically-scoped
  "Emacs invariant:
   Most variables are dynamically scoped by default.

   Required for:
   - hooks
   - advice
   - configuration patterns
   - package compatibility

   Status: PENDING"
  (h/with-editor
    (h/eval-lisp
     '(progn
       (setq x 1)
       (let ((x 2))
         (is (= 2 x)))
       (is (= 1 x))))))
