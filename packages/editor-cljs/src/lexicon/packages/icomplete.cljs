(ns lexicon.packages.icomplete
  "Incremental completion feedback in the minibuffer (icomplete).

  When icomplete-mode is enabled, prospective completions are shown
  inline in the minibuffer as you type.

  Display format:
  - {...} - multiple candidates, separated by icomplete-separator
  - (...) - single match, matching is required
  - [...] - single match, matching is optional

  Key bindings (when minibuffer active):
  - C-. : icomplete-forward-completions (cycle to next)
  - C-, : icomplete-backward-completions (cycle to previous)

  Based on Emacs lisp/icomplete.el

  This package uses only lisp.cljs primitives.
  UI integration (subscriptions/events) is in core/events/icomplete.cljs"
  (:require [lexicon.lisp :as lisp]))

;; =============================================================================
;; Commands
;; =============================================================================

(defn icomplete-mode!
  "Toggle icomplete mode."
  []
  ;; Dispatch to core event
  (lisp/dispatch-event :icomplete/enable nil)
  (if (lisp/icomplete-enabled?)
    (lisp/message "Icomplete mode enabled")
    (lisp/message "Icomplete mode disabled")))

(defn icomplete-forward!
  "Cycle forward through icomplete candidates."
  []
  (lisp/dispatch-event :icomplete/forward-completions))

(defn icomplete-backward!
  "Cycle backward through icomplete candidates."
  []
  (lisp/dispatch-event :icomplete/backward-completions))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize icomplete mode commands and keybindings."
  []
  ;; Register icomplete-mode command
  (lisp/define-command 'icomplete-mode
    icomplete-mode!
    "Toggle incremental completion feedback in minibuffer")

  ;; Register cycling commands
  (lisp/define-command 'icomplete-forward-completions
    icomplete-forward!
    "Cycle forward through icomplete candidates (C-.)")

  (lisp/define-command 'icomplete-backward-completions
    icomplete-backward!
    "Cycle backward through icomplete candidates (C-,)")

  ;; Set up key bindings (these are active when minibuffer is active)
  (lisp/global-set-key "C-." :icomplete-forward-completions)
  (lisp/global-set-key "C-," :icomplete-backward-completions))
