(ns lexicon.core.completion.command
  "Command Completion Table - Completion for M-x command names.

  Used by:
    - M-x (execute-extended-command)

  Features:
    - Completes from registered interactive commands
    - Annotations show keybinding (if any)
    - Filters to only interactive commands

  See Issue #137."
  (:require [lexicon.core.completion.table :as table]))

(defn command-annotation
  "Generate annotation for command completion.
   Shows keybinding if available."
  [command-name keybindings-map]
  (when-let [binding (get keybindings-map command-name)]
    (str " (" binding ")")))

(defn find-keybinding-for-command
  "Find the keybinding string for a command, if any.
   Searches through keymaps to find first binding."
  [_command-name _keymaps]
  ;; This would search keymaps for the command
  ;; For now, return nil - can be enhanced later
  nil)

(defn make-command-table
  "Create a completion table for command names.

  Takes the commands registry and returns a completion table.
  Only includes interactive commands (those with :interactive metadata).

  Example:
    (make-command-table (:commands @db))"
  [commands]
  (let [;; Filter to only interactive commands
        interactive-commands (filter (fn [[_name cmd]]
                                       (or (:interactive cmd)
                                           (:interactive? cmd)))
                                     commands)
        command-names (map (comp name first) interactive-commands)
        ;; Sort alphabetically by default
        sorted-names (sort command-names)]
    (table/make-completion-table
     sorted-names
     {:category :command
      :annotation-fn nil  ; Can be enhanced to show keybindings
      :sort-fn nil})))

(defn command-completions
  "Get command name completions for the given input.
   Simple helper for M-x."
  [commands input]
  (let [table (make-command-table commands)]
    (table/all-completions table input nil)))
