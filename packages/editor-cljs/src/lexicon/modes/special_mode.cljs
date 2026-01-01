(ns lexicon.modes.special-mode
  "Special Mode - Base mode for read-only buffers.

  Special-mode is a parent mode for buffers that are read-only and
  have special interactive commands. Examples:
  - *Help* buffers
  - *Buffer List*
  - *Compilation*
  - Package list buffers

  Standard bindings:
  - q: Quit/bury buffer
  - g: Refresh/revert buffer
  - SPC: Scroll down (page down)
  - DEL/Backspace: Scroll up (page up)
  - <, >: Beginning/end of buffer
  - TAB: Forward button
  - S-TAB: Backward button"
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]))

;; -- Special Mode Keymap --

(def special-mode-map
  "Keymap for special-mode. All child modes inherit these bindings."
  {:q     [:special-mode/quit-buffer]
   :g     [:special-mode/revert-buffer]
   :SPC   [:editor/scroll-down]
   :DEL   [:editor/scroll-up]
   :S-SPC [:editor/scroll-up]
   :<     [:editor/beginning-of-buffer]
   :>     [:editor/end-of-buffer]
   :TAB   [:special-mode/forward-button]
   :S-TAB [:special-mode/backward-button]})

;; -- Event Handlers --

;; Quit/bury the current buffer (like Emacs quit-window).
;; For special buffers, this typically means hiding the buffer.
(rf/reg-event-fx
  :special-mode/quit-buffer
  (fn [{:keys [db]} _]
    (let [active-window-id (:active-window-id db)
          active-window (db/find-window-in-tree (:window-tree db) active-window-id)
          buffer-id (:buffer-id active-window)
          buffer (get-in db [:buffers buffer-id])
          buffer-name (:name buffer)]
      (println "🚪 Quitting special buffer:" buffer-name)
      ;; For now, just show a message. In future, implement buffer switching.
      {:fx [[:dispatch [:echo/message (str "Quit " buffer-name " (buffer switching not yet implemented)")]]]})))

;; Revert/refresh the buffer content.
;; Child modes should override this to implement their specific refresh logic.
(rf/reg-event-fx
  :special-mode/revert-buffer
  (fn [{:keys [db]} _]
    (let [active-window-id (:active-window-id db)
          active-window (db/find-window-in-tree (:window-tree db) active-window-id)
          buffer-id (:buffer-id active-window)
          buffer (get-in db [:buffers buffer-id])
          buffer-name (:name buffer)
          ;; Get the mode-specific revert function from buffer metadata
          revert-fn (get-in buffer [:mode-data :revert-fn])]
      (if revert-fn
        (do
          (println "🔄 Reverting buffer:" buffer-name)
          {:fx [[:dispatch revert-fn]]})
        (do
          (println "⚠️ No revert function for buffer:" buffer-name)
          {:fx [[:dispatch [:echo/message (str buffer-name " has no revert function")]]]})))))

;; Navigate to the next button/link in the buffer.
;; Buttons are typically implemented as text properties with :button or :link.
(rf/reg-event-fx
  :special-mode/forward-button
  (fn [{:keys [db]} _]
    ;; Placeholder - will be implemented when we add button support
    {:fx [[:dispatch [:echo/message "Forward button (not yet implemented)"]]]}))

;; Navigate to the previous button/link in the buffer.
(rf/reg-event-fx
  :special-mode/backward-button
  (fn [{:keys [db]} _]
    ;; Placeholder - will be implemented when we add button support
    {:fx [[:dispatch [:echo/message "Backward button (not yet implemented)"]]]}))

;; -- Mode Activation --

;; Enable special-mode for a buffer.
;; This sets the buffer to read-only and installs the special-mode keymap.
(rf/reg-event-db
  :special-mode/enable
  (fn [db [_ buffer-id]]
    (let [buffer-name (get-in db [:buffers buffer-id :name])]
      (println "🔒 Enabling special-mode for buffer:" buffer-name)
      (-> db
          ;; Make buffer read-only
          (assoc-in [:buffers buffer-id :is-read-only?] true)
          ;; Set mode to special-mode
          (assoc-in [:buffers buffer-id :major-mode] :special-mode)
          ;; Install special-mode keymap
          (assoc-in [:buffers buffer-id :mode-keymap] special-mode-map)))))

;; Define a special-mode derived mode.
;; This creates a new mode that inherits from special-mode.
;; Usage: (rf/dispatch [:special-mode/define-derived-mode
;;                      :help-mode
;;                      "Help"
;;                      {:keymap {:RET [:help/follow-link]}
;;                       :revert-fn [:help/revert]}])
(rf/reg-event-db
  :special-mode/define-derived-mode
  (fn [db [_ mode-name mode-line-name & {:keys [keymap revert-fn]}]]
    (let [;; Merge parent keymap with child keymap (child wins)
          merged-keymap (merge special-mode-map keymap)]
      (println "📝 Defining derived mode:" mode-name "from special-mode")
      (assoc-in db [:modes mode-name]
                {:parent-mode :special-mode
                 :mode-line-name mode-line-name
                 :keymap merged-keymap
                 :revert-fn revert-fn}))))

;; Activate a special-mode derived mode for a buffer.
(rf/reg-event-db
  :special-mode/activate-derived-mode
  (fn [db [_ buffer-id mode-name]]
    (let [buffer-name (get-in db [:buffers buffer-id :name])
          mode-spec (get-in db [:modes mode-name])
          mode-line-name (:mode-line-name mode-spec)
          keymap (:keymap mode-spec)
          revert-fn (:revert-fn mode-spec)]
      (when-not mode-spec
        (println "⚠️ Unknown mode:" mode-name))
      (println "🔧 Activating" mode-name "for buffer:" buffer-name)
      (-> db
          ;; Make buffer read-only (special-mode requirement)
          (assoc-in [:buffers buffer-id :is-read-only?] true)
          ;; Set mode
          (assoc-in [:buffers buffer-id :major-mode] mode-name)
          ;; Install mode keymap
          (assoc-in [:buffers buffer-id :mode-keymap] keymap)
          ;; Store mode-specific data
          (assoc-in [:buffers buffer-id :mode-data :mode-line-name] mode-line-name)
          (assoc-in [:buffers buffer-id :mode-data :revert-fn] revert-fn)))))
