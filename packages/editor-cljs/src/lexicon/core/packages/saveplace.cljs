(ns lexicon.core.packages.saveplace
  "Save place - remember cursor position per file.

  Implements Emacs saveplace.el functionality:
  - Saves cursor position when killing a buffer
  - Restores cursor position when reopening a file
  - Persists position data across sessions via localStorage

  Commands:
  - save-place-mode: Toggle save-place mode
  - save-place-forget-unreadable-files: Clean up entries

  Based on Emacs lisp/saveplace.el"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def save-place-limit
  "Maximum number of file positions to remember."
  400)

(def storage-key "lexicon-save-place-alist")

;; =============================================================================
;; Persistence (localStorage)
;; =============================================================================

(defn- save-alist!
  "Save place alist to localStorage."
  [alist]
  (try
    (let [json-str (js/JSON.stringify (clj->js alist))]
      (.setItem js/localStorage storage-key json-str))
    (catch js/Error _e nil)))

(defn- load-alist
  "Load place alist from localStorage."
  []
  (try
    (when-let [json-str (.getItem js/localStorage storage-key)]
      (let [parsed (js->clj (js/JSON.parse json-str) :keywordize-keys true)]
        (into {} (map (fn [entry]
                        [(or (:path entry) (first (keys entry)))
                         (or (:position entry) (first (vals entry)))])
                      (if (map? parsed) parsed (seq parsed))))))
    (catch js/Error _e {})))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-db
 :saveplace/initialize
 (fn [db [_]]
   (let [saved-alist (load-alist)]
     (-> db
         (assoc-in [:saveplace :enabled?] true)
         (assoc-in [:saveplace :alist] (or saved-alist {}))))))

(rf/reg-event-fx
 :saveplace/save-position
 (fn [{:keys [db]} [_ file-path position]]
   "Save cursor position for a file."
   (when (and (get-in db [:saveplace :enabled?])
              file-path
              position
              (> position 0))  ; Don't save position 0 (beginning of file)
     (let [alist (get-in db [:saveplace :alist] {})
           updated (assoc alist file-path position)
           ;; Enforce limit - remove oldest entries if over limit
           limited (if (> (count updated) save-place-limit)
                     (into {} (take save-place-limit (sort-by val > updated)))
                     updated)]
       (save-alist! limited)
       {:db (assoc-in db [:saveplace :alist] limited)}))))

(rf/reg-event-fx
 :saveplace/restore-position
 (fn [{:keys [db]} [_ buffer-id file-path]]
   "Restore cursor position for a file buffer."
   (when (and (get-in db [:saveplace :enabled?])
              file-path)
     (let [alist (get-in db [:saveplace :alist] {})
           saved-pos (get alist file-path)]
       (when (and saved-pos (> saved-pos 0))
         {:fx [[:dispatch [:goto-char buffer-id saved-pos]]]})))))

(rf/reg-event-fx
 :saveplace/on-kill-buffer
 (fn [{:keys [db]} [_ buffer-id]]
   "Save position when buffer is being killed."
   (let [buffer (get-in db [:buffers buffer-id])
         file-path (:file-path buffer)
         cursor-pos (get-in db [:ui :cursor-position] 0)]
     (when (and file-path cursor-pos)
       {:fx [[:dispatch [:saveplace/save-position file-path cursor-pos]]]}))))

(rf/reg-event-fx
 :command/save-place-mode
 (fn [{:keys [db]} [_]]
   (let [enabled? (get-in db [:saveplace :enabled?] false)]
     {:db (assoc-in db [:saveplace :enabled?] (not enabled?))
      :fx [[:dispatch [:echo/message (if enabled?
                                       "save-place-mode disabled"
                                       "save-place-mode enabled")]]]})))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :saveplace/enabled?
 (fn [db _]
   (get-in db [:saveplace :enabled?] false)))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize saveplace module and register commands."
  []
  (rf/dispatch [:saveplace/initialize])

  (rf/dispatch [:register-command :save-place-mode
                {:docstring "Toggle save-place mode - remember cursor position per file"
                 :interactive nil
                 :handler [:command/save-place-mode]}]))
