(ns lexicon.interaction.critical.point-mark-semantics-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical point-is-buffer-local
  "Emacs invariant:
   Point belongs to buffer, not window.

   Required for:
   - multiple windows
   - indirect buffers

   Status: ACTIVE"
  (h/with-editor
    (let [buf (h/create-buffer \"a\")]
      (h/show-buffer-in-two-windows buf)
      (h/set-point buf 3)
      (is (= 3 (h/point buf))))))

(deftest ^:critical mark-is-pushable-and-stack-based
  "Emacs invariant:
   Mark participates in a ring/stack.

   Required for:
   - region
   - Evil motions
   - navigation

   Status: PENDING"
  (is false))
