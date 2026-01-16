(ns lexicon.filesystem.critical.save-and-revert-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical save-buffer-writes-buffer-contents
  "Emacs invariant:
   Saving is a buffer operation, not a file operation.

   Required for:
   - hooks
   - advice
   - Magit interception

   Status: ACTIVE"
  (h/with-temp-file \"hello.txt\"
    (h/with-editor
      (h/find-file \"hello.txt\")
      (h/insert-text \"hello\")
      (h/save-buffer)
      (is (= \"hello\" (h/read-file \"hello.txt\")))
      (is (false? (h/buffer-modified?))))))

(deftest ^:high revert-buffer-preserves-identity
  "Emacs invariant:
   Reverting reloads contents without replacing the buffer.

   Required for:
   - file watchers
   - project.el
   - Magit refresh

   Status: PENDING"
  (is false))
