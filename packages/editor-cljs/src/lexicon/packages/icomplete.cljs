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

  fido-mode is an enhanced icomplete with:
  - Flex matching by default (characters match in order, not necessarily contiguous)
  - ido-like behavior (M-d to enter dired, M-j to force match)

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
  (if (lisp/icomplete-toggle!)
    (lisp/message "Icomplete mode enabled")
    (lisp/message "Icomplete mode disabled")))

(defn icomplete-forward!
  "Cycle forward through icomplete candidates."
  []
  (lisp/icomplete-forward!))

(defn icomplete-backward!
  "Cycle backward through icomplete candidates."
  []
  (lisp/icomplete-backward!))

(defn fido-mode!
  "Toggle fido mode (flex-matching icomplete)."
  []
  (if (lisp/fido-toggle!)
    (lisp/message "Fido mode enabled (flex matching)")
    (lisp/message "Fido mode disabled")))

(defn fido-exit!
  "Exit minibuffer with current input, even if it doesn't match."
  []
  (lisp/dispatch-event :minibuffer/confirm))

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

  ;; Register fido-mode command
  (lisp/define-command 'fido-mode
    fido-mode!
    "Toggle fido mode (flex-matching icomplete)")

  ;; Register fido-specific commands
  (lisp/define-command 'icomplete-fido-exit
    fido-exit!
    "Exit with current input (M-j in fido-mode)")

  ;; Set up key bindings (these are active when minibuffer is active)
  (lisp/global-set-key "C-." :icomplete-forward-completions)
  (lisp/global-set-key "C-," :icomplete-backward-completions)
  ;; M-j for fido force-exit (force complete with current input)
  (lisp/global-set-key "M-j" :icomplete-fido-exit)

  ;; Register with minibuffer hooks for icomplete integration
  ;; This ensures icomplete state is reset when minibuffer activates/deactivates
  (lisp/add-hook 'minibuffer-setup-hook
    (fn [_] (lisp/dispatch-event :icomplete/minibuffer-setup)))

  (lisp/add-hook 'minibuffer-exit-hook
    (fn [_] (lisp/dispatch-event :icomplete/minibuffer-exit))))
