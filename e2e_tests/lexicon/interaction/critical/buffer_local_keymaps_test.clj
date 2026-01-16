(ns lexicon.interaction.critical.buffer-local-keymaps-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical major-mode-keymap-overrides-global
  "Emacs invariant:
   Major mode keymaps override global bindings.

   Required for:
   - language modes
   - Evil normal/insert states

   Status: ACTIVE"
  (h/with-editor
    (h/bind-global \"TAB\" 'global-tab)
    (h/with-buffer \"a\"
      (h/define-major-mode 'text-mode
        {:keymap {\"TAB\" 'text-tab}})
      (h/enable-major-mode 'text-mode)
      (h/press \"TAB\")
      (is (= 'text-tab (h/last-command))))))

(deftest ^:critical minor-mode-keymaps-are-layered
  "Emacs invariant:
   Minor mode keymaps stack predictably.

   Required for:
   - Evil + other modes
   - transient maps

   Status: PENDING"
  (is false))
