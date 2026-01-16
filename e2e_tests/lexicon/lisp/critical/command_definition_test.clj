(ns lexicon.lisp.critical.command-definition-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical defcommand-registers-editor-command
  "Emacs invariant:
   Commands defined from Lisp are first-class editor commands,
   indistinguishable from native ones.

   Required for:
   - M-x
   - packages
   - keybindings

   Status: PENDING"
  (h/with-editor
    (h/eval-lisp
     '(defcommand hello ()
        \"Say hello\"
        (message \"hello\")))
    (h/invoke-command 'hello)
    (is (re-find #"hello" (h/messages-text)))))
