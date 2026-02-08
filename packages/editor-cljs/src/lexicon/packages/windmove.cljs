(ns lexicon.packages.windmove
  "Directional window selection (windmove).

  S-<arrow> keys select the window in that direction:
  - S-left:  windmove-left
  - S-right: windmove-right
  - S-up:    windmove-up
  - S-down:  windmove-down

  Based on Emacs lisp/windmove.el

  This package demonstrates the clean package architecture:
  - Only depends on lexicon.lisp (the public API)
  - No direct access to re-frame, state, events, or subscriptions
  - All functionality through the lisp.cljs abstraction layer"
  (:require [lexicon.lisp :as lisp]))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-windmove!
  "Initialize windmove commands. Keybindings are NOT set by default.
   Call windmove-default-keybindings to enable S-arrow keybindings."
  []
  ;; Register directional commands
  (lisp/define-command
    'windmove-left
    lisp/windmove-left
    "Select the window to the left")

  (lisp/define-command
    'windmove-right
    lisp/windmove-right
    "Select the window to the right")

  (lisp/define-command
    'windmove-up
    lisp/windmove-up
    "Select the window above")

  (lisp/define-command
    'windmove-down
    lisp/windmove-down
    "Select the window below")

  ;; Register the keybindings command (like Emacs windmove-default-keybindings)
  (lisp/define-command
    'windmove-default-keybindings
    (fn []
      (lisp/global-set-key "S-ArrowLeft" :windmove-left)
      (lisp/global-set-key "S-ArrowRight" :windmove-right)
      (lisp/global-set-key "S-ArrowUp" :windmove-up)
      (lisp/global-set-key "S-ArrowDown" :windmove-down)
      (lisp/message "Windmove keybindings enabled (S-arrow)"))
    "Enable S-arrow keybindings for windmove"))
