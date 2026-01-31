(ns lexicon.core.winner
  "Window configuration undo/redo (winner-mode).

  Winner-mode tracks window configuration changes and allows
  restoring previous configurations:
  - C-c <left>:  winner-undo (restore previous config)
  - C-c <right>: winner-redo (redo undone config)

  Based on Emacs lisp/winner.el"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; State
;; =============================================================================

;; Whether winner-mode is enabled
(defonce winner-mode-enabled? (atom false))

;; History of window configurations (list of window-trees)
;; Most recent at the front
(defonce winner-history (atom []))

;; Redo stack (configs undone that can be redone)
(defonce winner-redo-stack (atom []))

;; Maximum history size
(defonce winner-history-max (atom 50))

;; Last recorded window tree (to detect changes)
(defonce winner-last-tree (atom nil))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn trees-equal?
  "Check if two window trees are structurally equal (ignoring cursor/viewport)."
  [tree1 tree2]
  (cond
    (and (nil? tree1) (nil? tree2)) true
    (or (nil? tree1) (nil? tree2)) false
    (not= (:type tree1) (:type tree2)) false
    (= (:type tree1) :leaf)
    (and (= (:id tree1) (:id tree2))
         (= (:buffer-id tree1) (:buffer-id tree2)))
    :else
    (and (trees-equal? (:first tree1) (:first tree2))
         (trees-equal? (:second tree1) (:second tree2)))))

(defn record-window-config!
  "Record current window configuration if it changed."
  [window-tree]
  (when @winner-mode-enabled?
    (when-not (trees-equal? window-tree @winner-last-tree)
      ;; Push current to history before recording new
      (when @winner-last-tree
        (swap! winner-history
               (fn [history]
                 (vec (take @winner-history-max
                            (cons @winner-last-tree history))))))
      ;; Clear redo stack on new change
      (reset! winner-redo-stack [])
      ;; Update last tree
      (reset! winner-last-tree window-tree))))

(defn reset-winner-state!
  "Reset winner state."
  []
  (reset! winner-history [])
  (reset! winner-redo-stack [])
  (reset! winner-last-tree nil))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :winner/undo
 (fn [{:keys [db]} [_]]
   "Restore previous window configuration (C-c <left>)."
   (if-not @winner-mode-enabled?
     {:db db
      :fx [[:dispatch [:echo/message "Winner mode is not enabled"]]]}
     (if (empty? @winner-history)
       {:db db
        :fx [[:dispatch [:echo/message "No previous window configuration"]]]}
       (let [current-tree (:window-tree db)
             previous-tree (first @winner-history)]
         ;; Push current to redo stack
         (swap! winner-redo-stack conj current-tree)
         ;; Pop from history
         (swap! winner-history rest)
         ;; Update last tree without recording
         (reset! winner-last-tree previous-tree)
         {:db (assoc db :window-tree previous-tree)
          :fx [[:dispatch [:echo/message "Winner: restored previous configuration"]]]})))))

(rf/reg-event-fx
 :winner/redo
 (fn [{:keys [db]} [_]]
   "Redo undone window configuration (C-c <right>)."
   (if-not @winner-mode-enabled?
     {:db db
      :fx [[:dispatch [:echo/message "Winner mode is not enabled"]]]}
     (if (empty? @winner-redo-stack)
       {:db db
        :fx [[:dispatch [:echo/message "No configuration to redo"]]]}
       (let [current-tree (:window-tree db)
             redo-tree (peek @winner-redo-stack)]
         ;; Push current back to history
         (swap! winner-history #(cons current-tree %))
         ;; Pop from redo stack
         (swap! winner-redo-stack pop)
         ;; Update last tree without recording
         (reset! winner-last-tree redo-tree)
         {:db (assoc db :window-tree redo-tree)
          :fx [[:dispatch [:echo/message "Winner: redone configuration"]]]})))))

(rf/reg-event-fx
 :winner-mode
 (fn [{:keys [db]} [_ enable?]]
   "Toggle or set winner-mode."
   (let [new-state (if (nil? enable?)
                     (not @winner-mode-enabled?)
                     (boolean enable?))]
     (reset! winner-mode-enabled? new-state)
     (if new-state
       (do
         (reset-winner-state!)
         (reset! winner-last-tree (:window-tree db))
         {:db db
          :fx [[:dispatch [:echo/message "Winner mode enabled"]]]})
       (do
         (reset-winner-state!)
         {:db db
          :fx [[:dispatch [:echo/message "Winner mode disabled"]]]})))))

;; Hook to record window changes
(rf/reg-event-fx
 :winner/record-change
 (fn [{:keys [db]} [_]]
   "Record window configuration change (called after split/delete)."
   (record-window-config! (:window-tree db))
   {:db db}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-winner!
  "Initialize winner-mode commands and keybindings."
  []
  ;; Register commands
  (rf/dispatch [:register-command :winner-mode
                {:docstring "Toggle winner-mode (window configuration undo/redo)"
                 :handler [:winner-mode nil]}])

  (rf/dispatch [:register-command :winner-undo
                {:docstring "Restore previous window configuration (C-c <left>)"
                 :handler [:winner/undo]}])

  (rf/dispatch [:register-command :winner-redo
                {:docstring "Redo window configuration (C-c <right>)"
                 :handler [:winner/redo]}])

  ;; Set up key bindings
  (rf/dispatch [:keymap/set-global "C-c <left>" :winner-undo])
  (rf/dispatch [:keymap/set-global "C-c <right>" :winner-redo]))
