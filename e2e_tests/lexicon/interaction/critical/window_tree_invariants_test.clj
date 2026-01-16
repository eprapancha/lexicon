(ns lexicon.interaction.critical.window-tree-invariants-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical window-tree-is-a-binary-partition
  "Emacs invariant:
   Windows form a recursive tree of splits.

   Required for:
   - window navigation
   - display-buffer
   - popups

   Status: PENDING"
  (is false))

(deftest ^:critical window-deletion-preserves-buffer-identity
  "Emacs invariant:
   Deleting a window never kills the buffer by default.

   Required for:
   - Magit
   - help buffers
   - undo-window-configuration

   Status: ACTIVE"
  (h/with-editor
    (let [buf (h/create-buffer \"x\")]
      (h/show-buffer-in-new-window buf)
      (h/delete-other-windows)
      (is (h/buffer-exists? \"x\")))))
