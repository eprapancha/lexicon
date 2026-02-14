(ns lexicon.packages.winner
  "Window configuration undo/redo (winner-mode).

  Winner-mode tracks window configuration changes and allows
  restoring previous configurations:
  - C-c <left>:  winner-undo (restore previous config)
  - C-c <right>: winner-redo (redo undone config)

  Based on Emacs lisp/winner.el

  This package uses only lisp.cljs primitives, demonstrating clean
  package architecture without re-frame dependencies."
  (:require [lexicon.lisp :as lisp]))

;; =============================================================================
;; Package-local State
;; =============================================================================

(def default-history-size 50)

(defonce winner-mode-enabled? (atom false))
(defonce winner-history (atom (lisp/make-ring default-history-size)))
(defonce winner-redo-stack (atom []))
(defonce winner-last-config (atom nil))

;; =============================================================================
;; Core Functions
;; =============================================================================

(defn record-window-config!
  "Record current window configuration if it changed."
  []
  (when @winner-mode-enabled?
    (let [current-config (lisp/current-window-configuration)
          changed? (not (lisp/window-configuration-equal? current-config @winner-last-config))]
      (when changed?
        (when @winner-last-config
          (swap! winner-history #(lisp/ring-insert % @winner-last-config)))
        (reset! winner-redo-stack [])
        (reset! winner-last-config current-config)))))

(defn reset-winner-state!
  "Reset winner state."
  []
  (reset! winner-history (lisp/make-ring default-history-size))
  (reset! winner-redo-stack [])
  (reset! winner-last-config nil))

(defn winner-undo!
  "Undo to previous window configuration."
  []
  (if-not @winner-mode-enabled?
    (lisp/message "Winner mode is not enabled")
    (let [history @winner-history
          length (lisp/ring-length history)]
      (lisp/message (str "Winner: undo called, history-size=" length))
      (if (zero? length)
        (lisp/message "No previous window configuration")
        (let [current-config (lisp/current-window-configuration)
              previous-config (lisp/ring-ref history 0)
              [_ new-history] (lisp/ring-remove history 0)]
          (swap! winner-redo-stack conj current-config)
          (reset! winner-history new-history)
          (reset! winner-last-config previous-config)
          (lisp/set-window-configuration previous-config)
          (lisp/message "Winner: restored previous configuration"))))))

(defn winner-redo!
  "Redo undone window configuration."
  []
  (if-not @winner-mode-enabled?
    (lisp/message "Winner mode is not enabled")
    (if (empty? @winner-redo-stack)
      (lisp/message "No configuration to redo")
      (let [current-config (lisp/current-window-configuration)
            redo-config (peek @winner-redo-stack)]
        (swap! winner-history #(lisp/ring-insert % current-config))
        (swap! winner-redo-stack pop)
        (reset! winner-last-config redo-config)
        (lisp/set-window-configuration redo-config)
        (lisp/message "Winner: redone configuration")))))

(defn winner-mode!
  "Toggle or set winner-mode."
  [enable?]
  (let [new-state (if (nil? enable?)
                    (not @winner-mode-enabled?)
                    (boolean enable?))]
    (reset! winner-mode-enabled? new-state)
    (if new-state
      (do
        (reset-winner-state!)
        (reset! winner-last-config (lisp/current-window-configuration))
        ;; Register hook to record window configuration changes
        (lisp/add-hook 'window-configuration-change-hook record-window-config!)
        (lisp/message "Winner mode enabled"))
      (do
        ;; Unregister hook
        (lisp/remove-hook 'window-configuration-change-hook record-window-config!)
        (reset-winner-state!)
        (lisp/message "Winner mode disabled")))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-winner!
  "Initialize winner-mode commands and keybindings."
  []
  (lisp/define-command
    'winner-mode
    (fn [] (winner-mode! nil))
    "Toggle winner-mode (window configuration undo/redo)")

  (lisp/define-command
    'winner-undo
    winner-undo!
    "Restore previous window configuration (C-c <left>)")

  (lisp/define-command
    'winner-redo
    winner-redo!
    "Redo window configuration (C-c <right>)")

  ;; Set up key bindings
  (lisp/global-set-key "C-c <left>" :winner-undo)
  (lisp/global-set-key "C-c <right>" :winner-redo))
