(ns lexicon.history.critical.undo-model-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical undo-is-buffer-local
  "Emacs invariant:
   Undo history belongs to a buffer, not the editor.

   Required for:
   - multiple buffers
   - Magit
   - indirect buffers

   Status: ACTIVE"
  (h/with-editor
    (let [a (h/create-buffer \"a\")
          b (h/create-buffer \"b\")]
      (h/insert-text a \"x\")
      (h/insert-text b \"y\")
      (h/undo a)
      (is (= \"\" (h/buffer-text a)))
      (is (= \"y\" (h/buffer-text b))))))

(deftest ^:critical undo-is-not-destructive
  "Emacs invariant:
   Undo does not delete history — it creates new history entries.

   Required for:
   - redo (implicit)
   - undo-tree style behavior

   Status: PENDING"
  (is false))
