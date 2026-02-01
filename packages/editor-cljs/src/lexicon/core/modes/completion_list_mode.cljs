(ns lexicon.core.modes.completion-list-mode
  "Completion List Mode - Mode for *Completions* buffer.

  This mode is derived from special-mode and provides interactive
  selection of completions displayed in the *Completions* buffer.

  Standard bindings:
  - RET: Choose completion at point
  - q: Quit *Completions* and return to minibuffer
  - n/down: Next completion
  - p/up: Previous completion
  - SPC: Page down
  - DEL: Page up

  The *Completions* buffer is displayed when pressing TAB twice
  in the minibuffer (vanilla Emacs completion behavior).

  Note: Event handlers for this mode are defined in lexicon.core.events.ui
  to avoid circular dependencies. This namespace only defines the mode
  keymap and initialization.

  See Issue #136."
  (:require [re-frame.core :as rf]
            [lexicon.core.modes.special-mode :as special-mode]))

;; =============================================================================
;; Completion List Mode Keymap
;; =============================================================================

(def completion-list-mode-map
  "Keymap for completion-list-mode. Extends special-mode-map.
   Note: 'q' is inherited from special-mode-map and calls :special-mode/quit-buffer.
   We override it with :completion-list/quit to return to minibuffer."
  (merge special-mode/special-mode-map
         {:RET   [:completion-list/choose-at-point]
          :n     [:completion-list/next-completion]
          :p     [:completion-list/previous-completion]
          :down  [:completion-list/next-completion]
          :up    [:completion-list/previous-completion]
          :q     [:completion-list/quit]}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-completion-list-mode!
  "Initialize completion-list-mode."
  []
  ;; Register mode as derived from special-mode
  (rf/dispatch-sync
   [:special-mode/define-derived-mode
    :completion-list-mode
    "Completion"
    :keymap completion-list-mode-map]))
