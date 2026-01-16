(ns lexicon.core.critical.keymap-resolution-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical local-keymap-shadows-global-keymap
  "Emacs invariant:
   Local keymaps override global keymaps.

   Required for:
   - major modes
   - minibuffer
   - Evil

   Status: ACTIVE"
  (h/with-editor
    (h/bind-global "C-x" 'global-cmd)
    (h/with-buffer
      (h/bind-local "C-x" 'local-cmd)
      (h/press "C-x")
      (is (= 'local-cmd (h/last-command))))))

(deftest ^:critical prefix-key-waits-for-completion
  "Emacs invariant:
   Prefix keys do not invoke commands eagerly.

   Required for:
   - C-x
   - Transient
   - Magit

   Status: ACTIVE"
  (h/with-editor
    (h/define-prefix "C-x")
    (h/press "C-x")
    (is (nil? (h/last-command)))))
