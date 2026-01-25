(ns lexicon.core.modes.help-mode
  "Help Mode - Display help documentation in read-only buffers.

  Help-mode is used for displaying documentation, function help,
  variable descriptions, and other reference material.

  Key bindings (inherited from special-mode):
  - q: Quit help buffer
  - g: Revert/refresh help
  - SPC: Scroll down
  - DEL: Scroll up"
  (:require [re-frame.core :as rf]
            [lexicon.core.ui.mode-line :as mode-line]))

;; -- Help Buffer Management --

;; Create or update a help buffer with content
(rf/reg-event-fx
  :help/show
  (fn [{:keys [db]} [_ buffer-name content]]
    (let [;; Find or create help buffer
          existing-buffer-id (some (fn [[id buf]]
                                    (when (= (:name buf) buffer-name) id))
                                  (:buffers db))
          buffer-id (or existing-buffer-id
                       (inc (apply max 0 (keys (:buffers db)))))

          ;; Create WASM instance for buffer content
          wasm-constructor (get-in db [:wasm :constructor])
          wasm-instance (when wasm-constructor (new wasm-constructor content))]

      (println "📖 Creating help buffer:" buffer-name)

      {:db (-> db
               ;; Create/update buffer
               (assoc-in [:buffers buffer-id]
                        {:id buffer-id
                         :name buffer-name
                         :wasm-instance wasm-instance
                         :major-mode :help-mode
                         :is-read-only? true
                         :modified? false
                         :mode-line-format mode-line/help-mode-line-format
                         :mode-data {:mode-line-name "Help"
                                    :revert-fn [:help/revert buffer-id]}
                         :text-properties {}
                         :overlays {}
                         :next-overlay-id 1})
               ;; Activate help-mode
               )
       ;; Switch to help buffer (placeholder - buffer switching not fully implemented)
       :fx [[:dispatch [:echo/message (str "Help buffer created: " buffer-name)]]]})))

;; Revert/refresh a help buffer
(rf/reg-event-fx
  :help/revert
  (fn [{:keys [db]} [_ buffer-id]]
    (let [buffer (get-in db [:buffers buffer-id])
          buffer-name (:name buffer)]
      (println "🔄 Reverting help buffer:" buffer-name)
      ;; For now, just show a message
      ;; In the future, this would re-fetch the help content
      {:fx [[:dispatch [:echo/message (str "Revert " buffer-name " (not yet implemented)")]]]})))

;; -- Help Content Generators --

;; Show help for describe-function
(rf/reg-event-fx
  :help/describe-function
  (fn [_ [_ function-name]]
    (let [help-content (str "Function: " function-name "\n\n"
                           "Documentation not yet available.\n\n"
                           "Help system is under development.")]
      {:fx [[:dispatch [:help/show (str "*Help: " function-name "*") help-content]]]})))

;; Show help for describe-variable
(rf/reg-event-fx
  :help/describe-variable
  (fn [_ [_ variable-name]]
    (let [help-content (str "Variable: " variable-name "\n\n"
                           "Documentation not yet available.\n\n"
                           "Help system is under development.")]
      {:fx [[:dispatch [:help/show (str "*Help: " variable-name "*") help-content]]]})))

;; Show general help
(rf/reg-event-fx
  :help/show-help
  (fn [_ _]
    (let [help-content (str "Lexicon Editor Help\n"
                           "===================\n\n"
                           "This is the Lexicon editor, an Emacs-like editor built with ClojureScript.\n\n"
                           "Key Bindings:\n"
                           "- In this help buffer:\n"
                           "  - q: Quit help\n"
                           "  - g: Refresh help\n"
                           "  - SPC: Scroll down\n"
                           "  - DEL: Scroll up\n\n"
                           "More documentation coming soon.")]
      {:fx [[:dispatch [:help/show "*Help*" help-content]]]})))
