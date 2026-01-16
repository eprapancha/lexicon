(ns lexicon.history.critical.kill-ring-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical kill-ring-is-global
  "Emacs invariant:
   The kill ring is global across buffers.

   Required for:
   - yank
   - Evil paste
   - cross-buffer workflows

   Status: ACTIVE"
  (h/with-editor
    (h/with-buffer \"a\"
      (h/kill-region 0 1))
    (h/with-buffer \"b\"
      (h/yank)
      (is (seq (h/buffer-text \"b\"))))))

(deftest ^:critical consecutive-kills-append
  "Emacs invariant:
   Consecutive kills append to the same kill-ring entry.

   Required for:
   - predictable text manipulation

   Status: PENDING"
  (is false))
